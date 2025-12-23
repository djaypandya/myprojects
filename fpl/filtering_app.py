import streamlit as st
import sqlite3
import pandas as pd
import altair as alt

import plotly.graph_objects as go
import time
import league_replay
import player_consistency
import visualizations

import config
import fpl_api

# --- Constants ---
# Imported from config


# --- Database Schema & Assumptions ---
# Tables used:
# 1. elements: Core player data
# 2. teams: Team info (strength used as difficulty proxy)
# 3. element_types: Position info
# 4. player_history: Used for consistency metrics (appearances, minutes)
# 5. events: Current gameweek info
# 6. fixtures: Upcoming opponents

def get_connection():
    conn = sqlite3.connect(config.DB_PATH)
    conn.row_factory = sqlite3.Row
    return conn

def get_current_gameweek(conn):
    """Fetch the current gameweek ID."""
    query = "SELECT id FROM events WHERE is_current = 1"
    cursor = conn.cursor()
    cursor.execute(query)
    row = cursor.fetchone()
    if row:
        return row['id']
    
    query_fallback = "SELECT id FROM events WHERE finished = 1 ORDER BY id DESC LIMIT 1"
    cursor.execute(query_fallback)
    row = cursor.fetchone()
    return row['id'] if row else 0

def get_player_data(conn, min_minutes_threshold, season_id=config.DEFAULT_SEASON_ID):
    """
    Fetch player stats using SQL with consistency metrics.
    Calculates:
    - total_appearances: Games with minutes > 0
    - full_appearances: Games with minutes >= min_minutes_threshold
    - pct_full_appearances: full / total
    """
    query = """
    WITH history_stats AS (
        SELECT
            element_id,
            SUM(CASE WHEN minutes > 0 THEN 1 ELSE 0 END) as total_appearances,
            SUM(CASE WHEN minutes >= ? THEN 1 ELSE 0 END) as full_appearances
        FROM player_history
        WHERE season_id = ?
        GROUP BY element_id
    )
    SELECT 
        e.id,
        e.web_name AS Name,
        t.short_name AS Team,
        t.id AS team_id,
        et.singular_name AS Position,
        e.now_cost / 10.0 AS Cost,
        e.total_points AS "Total Points",
        e.minutes AS "Total Minutes",
        COALESCE(h.total_appearances, 0) as games_played,
        COALESCE(h.full_appearances, 0) as full_appearances
    FROM elements e
    JOIN teams t ON e.team_id = t.id AND t.season_id = e.season_id
    JOIN element_types et ON e.element_type = et.id AND et.season_id = e.season_id
    LEFT JOIN history_stats h ON e.id = h.element_id
    WHERE e.season_id = ?
    """
    return pd.read_sql_query(query, conn, params=(min_minutes_threshold, season_id, season_id))

def get_future_fixtures(conn, current_gw, season_id=config.DEFAULT_SEASON_ID):
    """Fetch future fixtures to calculate difficulty."""
    query = """
    SELECT 
        f.event,
        f.team_h,
        f.team_a,
        f.team_h_difficulty,
        f.team_a_difficulty
    FROM fixtures f
    WHERE f.event > ? AND f.season_id = ?
    ORDER BY f.event
    """
    return pd.read_sql_query(query, conn, params=(current_gw, season_id))

def calculate_fixture_difficulty(team_id, fixtures_df, num_fixtures):
    """
    Calculate the sum of difficulty for the next N fixtures.
    Difficulty = Opponent Strength + Home/Away Adjustment.
    Home: -0.1 (Easier)
    Away: +0.1 (Harder)
    """
    team_fixtures = fixtures_df[
        (fixtures_df['team_h'] == team_id) | (fixtures_df['team_a'] == team_id)
    ].head(num_fixtures)
    
    difficulty_score = 0
    for _, row in team_fixtures.iterrows():
        if row['team_h'] == team_id:
            # Playing Home: Use Home Difficulty (Easier)
            difficulty_score += (row['team_h_difficulty'] + config.DIFFICULTY_HOME_ADJ)
        else:
            # Playing Away: Use Away Difficulty (Harder)
            difficulty_score += (row['team_a_difficulty'] + config.DIFFICULTY_AWAY_ADJ)
            
    return difficulty_score

def calculate_ranks(df):
    """
    Calculate Composite Rank per Position.
    Composite Rank = Average of (Pts/Match Rank) and (Total Points Rank).
    This balances 'Form' (PPG) with 'Reliability' (Total Points).
    """
    df = df.copy()
    
    # 1. Rank by Pts/Match (Form)
    df['PPG_Rank'] = df.groupby('Position')['Pts/Match'].rank(method='min', ascending=False)
    
    # 2. Rank by Total Points (Reliability)
    df['Total_Points_Rank'] = df.groupby('Position')['Total Points'].rank(method='min', ascending=False)
    
    # 3. Calculate Average Rank
    df['Avg_Rank_Score'] = (df['PPG_Rank'] + df['Total_Points_Rank']) / 2.0
    
    # 4. Final Rank based on the Average Score (ascending, lower score is better)
    df['Rank'] = df.groupby('Position')['Avg_Rank_Score'].rank(method='min', ascending=True)
    
    # Calculate Total Players per Position
    position_counts = df['Position'].value_counts().to_dict()
    
    # Create Display Column (e.g. "2 / 86")
    df['Rank Display'] = df.apply(
        lambda row: f"{int(row['Rank'])} / {position_counts.get(row['Position'], 0)}", 
        axis=1
    )
    
    return df

def get_teams(conn, season_id=config.DEFAULT_SEASON_ID):
    """Fetch all teams with their short names."""
    query = "SELECT id, name, short_name FROM teams WHERE season_id = ?"
    return pd.read_sql_query(query, conn, params=(season_id,))

def get_difficulty_color(difficulty):
    """
    Returns a hex color based on difficulty score (2.0 to 5.0+).
    Green (Easy) -> Yellow -> Red (Hard).
    """
    # Normalize difficulty (approx range 2.0 to 5.0)
    # Using a 3-stop gradient: Green(2) -> Yellow(3.5) -> Red(5)
    
    # Safe bounds
    d = max(config.DIFFICULTY_THRESHOLD_EASY, min(config.DIFFICULTY_THRESHOLD_HARD, float(difficulty)))
    
    # Interpolation logic
    if d <= config.DIFFICULTY_THRESHOLD_MEDIUM:
        # Green to Yellow
        # Range: EASY to MEDIUM
        span = config.DIFFICULTY_THRESHOLD_MEDIUM - config.DIFFICULTY_THRESHOLD_EASY
        ratio = (d - config.DIFFICULTY_THRESHOLD_EASY) / span
        # RGB Green: (0, 255, 0) -> Yellow: (255, 255, 0)
        r = int(0 + ratio * 255)
        g = 255
        b = 0
    else:
        # Yellow to Red
        # Range: MEDIUM to HARD
        span = config.DIFFICULTY_THRESHOLD_HARD - config.DIFFICULTY_THRESHOLD_MEDIUM
        ratio = (d - config.DIFFICULTY_THRESHOLD_MEDIUM) / span
        # RGB Yellow: (255, 255, 0) -> Red: (255, 0, 0)
        r = 255
        g = int(255 - ratio * 255)
        b = 0
        
    return f'rgb({r}, {g}, {b})'

def get_team_picks(entry_id, gw):
    """Fetch picks for a specific entry and gameweek."""
    try:
        data = fpl_api.fetch_entry_picks(entry_id, gw)
        return [p['element'] for p in data['picks']]
    except Exception as e:
        st.error(f"Error fetching team picks: {e}")
        return []

def render_fixture_analysis(conn, current_gw, num_fixtures, season_id=config.DEFAULT_SEASON_ID):
    st.header(f"Fixture Difficulty Analysis (Next {num_fixtures} GWs)")
    
    # --- Team Selector ---
    col_input, _ = st.columns([1, 2])
    with col_input:
        entry_id_input = st.text_input("Enter Team ID (for squad analysis)", help="Leave empty for general team analysis")
    
    # 1. Get Data
    teams_df = get_teams(conn, season_id)
    fixtures_df = get_future_fixtures(conn, current_gw, season_id)
    
    # --- GENERAL ANALYSIS (ALWAYS VISIBLE) ---
    st.subheader("General Team Analysis")
    
    # Process Data for each team
    team_data = []
    team_colors = []
    
    for _, team in teams_df.iterrows():
        team_id = team['id']
        team_short = team['short_name']
        
        # Calculate Total Difficulty
        difficulty = calculate_fixture_difficulty(team_id, fixtures_df, num_fixtures)
        
        # Get Opponents for the table
        team_fixtures = fixtures_df[
            (fixtures_df['team_h'] == team_id) | (fixtures_df['team_a'] == team_id)
        ].head(num_fixtures)
        
        opponents = {}
        row_colors = {}
        
        for _, fix in team_fixtures.iterrows():
            gw = fix['event']
            
            if fix['team_h'] == team_id:
                # Home Game
                opp_id = fix['team_a']
                opp_row = teams_df[teams_df['id'] == opp_id].iloc[0]
                opp_name = opp_row['short_name'].upper()
                # Difficulty
                diff_val = fix['team_h_difficulty'] + config.DIFFICULTY_HOME_ADJ
            else:
                # Away Game
                opp_id = fix['team_h']
                opp_row = teams_df[teams_df['id'] == opp_id].iloc[0]
                opp_name = opp_row['short_name'].lower()
                # Difficulty
                diff_val = fix['team_a_difficulty'] + config.DIFFICULTY_AWAY_ADJ
            
            opponents[gw] = opp_name
            row_colors[gw] = diff_val
            
        # Create row
        row = {
            'Team': team_short,
            'Total Difficulty': difficulty,
            **opponents
        }
        
        color_row = {
            'Team': 0, 
            'Total Difficulty': 0,
            **row_colors
        }
        
        team_data.append(row)
        team_colors.append(color_row)
        
    # Create DataFrames
    df_analysis = pd.DataFrame(team_data)
    df_colors = pd.DataFrame(team_colors)
    
    # Sort by Difficulty (Ascending - Easiest first)
    df_analysis = df_analysis.sort_values('Total Difficulty', ascending=True)
    df_colors = df_colors.reindex(df_analysis.index)
    
    # Display General Analysis
    st.caption("KEY: UPPERCASE = HOME, lowercase = away")
    
    col1, col2 = st.columns([1, 2])
    
    with col1:
        st.subheader("Difficulty Score")
        chart = alt.Chart(df_analysis).mark_bar(color="#ffaa00").encode(
            x=alt.X('Total Difficulty', title='Difficulty Score'),
            y=alt.Y('Team', sort='x', title=None),
            tooltip=['Team', 'Total Difficulty']
        ).properties(
            height=500
        )
        st.altair_chart(chart, use_container_width=True)
        
    with col2:
        st.subheader("Upcoming Opponents")
        
        gw_cols = [col for col in df_analysis.columns if col not in ['Team', 'Total Difficulty']]
        gw_cols = sorted(gw_cols)
        final_cols = ['Team'] + gw_cols
        
        df_display = df_analysis[final_cols]
        df_color_subset = df_colors[final_cols]
        
        def style_heatmap(df):
            styles = pd.DataFrame('', index=df.index, columns=df.columns)
            for col in df.columns:
                if col == 'Team':
                    continue
                series = df_color_subset[col]
                styles[col] = series.map(lambda x: f'background-color: {get_difficulty_color(x)}; color: black')
            return styles

        st.dataframe(
            df_display.style.apply(style_heatmap, axis=None),
            hide_index=True,
            height=500,
            width=1000,
            column_config={
                "Team": st.column_config.TextColumn("Team", width="small")
            }
        )

    # --- SQUAD ANALYSIS MODE (CONDITIONAL) ---
    squad_player_ids = []
    if entry_id_input and entry_id_input.isdigit():
        # Get latest finished GW
        cursor = conn.cursor()
        cursor.execute("SELECT MAX(id) FROM events WHERE finished = 1")
        last_finished_gw = cursor.fetchone()[0]
        
        if last_finished_gw:
            # Avoid spinner conflict if possible, but fine here
            squad_player_ids = get_team_picks(entry_id_input, last_finished_gw)
    
    if squad_player_ids:
        st.markdown("---")
        st.subheader("My Team Analysis (Sorted by Difficulty)")
        st.caption(f"Based on Squad from GW {last_finished_gw}")
        
        # Get Player Details
        placeholders = ','.join(['?'] * len(squad_player_ids))
        query = f"""
            SELECT e.id, e.web_name, e.team_id, e.element_type, t.short_name as team_short
            FROM elements e 
            JOIN teams t ON e.team_id = t.id
            WHERE e.id IN ({placeholders}) AND e.season_id = ?
        """
        params = squad_player_ids + [season_id]
        players_df = pd.read_sql_query(query, conn, params=params)
        
        player_data = []
        player_colors = []
        
        for _, player in players_df.iterrows():
            web_name = player['web_name']
            team_short = player['team_short']
            team_id = player['team_id']
            
            # Calculate fixtures for this player's team
            difficulty = calculate_fixture_difficulty(team_id, fixtures_df, num_fixtures)
            
            # Get upcoming fixtures
            team_fixtures = fixtures_df[
                (fixtures_df['team_h'] == team_id) | (fixtures_df['team_a'] == team_id)
            ].head(num_fixtures)
            
            opponents = {}
            row_colors = {}
            
            for _, fix in team_fixtures.iterrows():
                gw = fix['event']
                
                if fix['team_h'] == team_id:
                    # Home
                    diff_val = fix['team_h_difficulty'] + config.DIFFICULTY_HOME_ADJ
                else:
                    # Away
                    diff_val = fix['team_a_difficulty'] + config.DIFFICULTY_AWAY_ADJ
                
                opponents[gw] = diff_val 
                row_colors[gw] = diff_val

            row = {
                'web_name': web_name,
                'short_name': team_short,
                'Total Difficulty': difficulty, # Added for sorting
                **opponents
            }
            
            color_row = {
                'web_name': 0, 'short_name': 0, 'Total Difficulty': 0,
                **row_colors
            }
            
            player_data.append(row)
            player_colors.append(color_row)
            
        df_display = pd.DataFrame(player_data)
        df_colors = pd.DataFrame(player_colors)
        
        # SORTING: Easiest (Low Difficulty) to Hardest (High Difficulty)
        df_display = df_display.sort_values('Total Difficulty', ascending=True)
        # Reindex colors to match
        df_colors = df_colors.reindex(df_display.index)
        
        # Display
        # Filter columns to show (Exclude Total Difficulty if not wanted, or keep it?)
        # User said "team with most difficult fixtures is the one I would want to replace"
        # So sorting is key. Showing the score might be helpful but heatmap is main request.
        # Let's hide 'Total Difficulty' from the view to keep it clean like the heatmap.
        
        gw_cols = [col for col in df_display.columns if str(col).isdigit()]
        gw_cols = sorted(gw_cols)
        final_cols = ['web_name', 'short_name'] + gw_cols
        
        df_show = df_display[final_cols]
        df_col_show = df_colors[final_cols]
        
        def style_squad_heatmap(df):
            styles = pd.DataFrame('', index=df.index, columns=df.columns)
            for col in df.columns:
                if col in ['web_name', 'short_name']:
                    continue
                series = df_col_show[col]
                styles[col] = series.map(lambda x: f'background-color: {get_difficulty_color(x)}; color: black')
            return styles

        st.dataframe(
            df_show.style.apply(style_squad_heatmap, axis=None).format(precision=1),
            hide_index=True,
            height=600,
            use_container_width=True
        )

def render_player_analysis(conn, current_gw, num_fixtures, max_rank, min_minutes_threshold):
    st.header("Player Selection")
    
    # 1. Get Base Player Data (with dynamic minutes threshold)
    df_players = get_player_data(conn, min_minutes_threshold)
    
    # Calculate Derived Metrics
    df_players['games_played'] = df_players['games_played'].replace(0, 1) # Avoid div by zero
    df_players['Pts/Match'] = df_players['Total Points'] / df_players['games_played']
    df_players['Avg Minutes'] = df_players['Total Minutes'] / df_players['games_played']
    
    # Consistency Metric
    df_players['pct_full_appearances'] = df_players['full_appearances'] / df_players['games_played']
    
    # Calculate Ranks (BEFORE filtering, to keep ranks accurate relative to all players)
    df_players = calculate_ranks(df_players)
    
    # 2. Get Fixture Data & Calculate Scores
    df_fixtures = get_future_fixtures(conn, current_gw)
    
    team_ids = df_players['team_id'].unique()
    team_difficulty_map = {
        tid: calculate_fixture_difficulty(tid, df_fixtures, num_fixtures) 
        for tid in team_ids
    }
    
    df_players['Fixture Score'] = df_players['team_id'].map(team_difficulty_map)
    
    # --- Filtering Logic ---
    
    # Filter 1: Consistency (Minutes)
    df_filtered = df_players[df_players['pct_full_appearances'] >= config.MIN_FULL_APPEARANCE_RATIO].copy()
    
    # Filter 2: Max Rank
    df_filtered = df_filtered[df_filtered['Rank'] <= max_rank]
    
    # --- Display ---
    st.markdown(f"### Analysis for Next {num_fixtures} Gameweeks (Starting GW {current_gw + 1})")
    st.markdown(f"Showing players ranked in **Top {max_rank}** (Composite Rank: Pts/Match + Total Pts) who play **{min_minutes_threshold}+ mins** in at least **{int(config.MIN_FULL_APPEARANCE_RATIO*100)}%** of their games.")
    
    cols = st.columns(4)
    
    pos_map = {
        'Goalkeeper': 'GKP',
        'Defender': 'DEF',
        'Midfielder': 'MID',
        'Forward': 'FWD'
    }
    
    positions = ['Goalkeeper', 'Defender', 'Midfielder', 'Forward']
    
    for i, pos in enumerate(positions):
        with cols[i]:
            st.subheader(pos_map[pos])
            
            pos_df = df_filtered[df_filtered['Position'] == pos].copy()
            
            if not pos_df.empty:
                # Sort: Easiest Fixtures -> Best Rank -> High Total Points
                pos_df = pos_df.sort_values(
                    by=['Fixture Score', 'Rank', 'Total Points'], 
                    ascending=[True, True, False]
                )
                
                # Display Columns
                display_cols = ['Name', 'Team', 'Cost', 'Total Points', 'Pts/Match', 'Rank Display', 'Fixture Score']
                
                st.dataframe(
                    pos_df[display_cols].style.format({
                        'Cost': '{:.1f}',
                        'Pts/Match': '{:.1f}', # Changed format to match user example (4.8)
                        'Fixture Score': '{:.1f}'
                    }),
                    hide_index=True,
                    width=1000
                )
            else:
                st.info("No players meet the criteria.")

def render_league_replay():
    st.header("League Position Replay")
    st.markdown("Visualize your league ranking progression over the season.")
    
    # Input Step
    col1, col2 = st.columns([1, 2])
    with col1:
        entry_id = st.text_input("Enter your Team ID (Entry ID)", help="Found in your FPL URL: fantasy.premierleague.com/entry/XXXXXX/...")
    
    if not entry_id:
        st.info("Please enter your Team ID to begin.")
        return

    if not entry_id.isdigit():
        st.error("Team ID must be a number.")
        return
        
    entry_id = int(entry_id)
    
    # Fetch User Metadata to get Leagues
    with st.spinner("Fetching team details..."):
        meta = league_replay.fetch_team_metadata(entry_id)
        
    if not meta:
        st.error("Could not fetch team details. Check the ID.")
        return
        
    leagues = league_replay.get_classic_leagues(meta)
    
    if not leagues:
        st.warning("No classic leagues found for this team.")
        return
        
    # League Selector
    league_options = {l['name']: l['id'] for l in leagues}
    selected_league_name = st.selectbox("Select a League", list(league_options.keys()))
    selected_league_id = league_options[selected_league_name]
    
    # Fetch League Data
    if st.button("Load League Data"):
        st.session_state['league_data_loaded'] = True
        st.session_state['current_league_id'] = selected_league_id
        
        with st.spinner("Fetching league history (this may take a moment for large leagues)..."):
            progress_bar = st.progress(0)
            df_league, error = league_replay.process_league_history(
                selected_league_id, 
                progress_callback=lambda p: progress_bar.progress(p)
            )
            progress_bar.empty()
            
            if error:
                st.error(error)
                st.session_state['league_df'] = None
            else:
                st.session_state['league_df'] = df_league

    # If data is loaded and matches the selected league (simple check)
    if st.session_state.get('league_data_loaded') and st.session_state.get('current_league_id') == selected_league_id:
        df = st.session_state.get('league_df')
        
        if df is None:
            return

        max_gw = df['GW'].max()
        all_entries = df['Entry ID'].unique()
        
        # --- Multi-Manager Highlighting ---
        
        # Create a mapping of Names -> IDs for the multiselect
        # We need a clean name map
        name_to_id = {}
        id_to_name = {}
        for eid in all_entries:
            row = df[df['Entry ID'] == eid].iloc[0]
            # Unique name: Name (Entry Name) [ID] to avoid collisions
            label = f"{row['Name']} [{eid}]"
            name_to_id[label] = eid
            id_to_name[eid] = label
            
        # Default selection: The current user
        default_label = id_to_name.get(entry_id)
        default_selection = [default_label] if default_label else []
        
        st.subheader("Highlight Managers")
        selected_labels = st.multiselect(
            "Select managers to compare:", 
            options=name_to_id.keys(),
            default=default_selection
        )
        
        selected_ids = [name_to_id[l] for l in selected_labels]
        
        # Color Palette (Distinct colors)
        PALETTE = [
            "#00FFFF", # Cyan (User default usually)
            "#FF00FF", # Magenta
            "#FFFF00", # Yellow
            "#00FF00", # Lime
            "#FF4500", # OrangeRed
            "#1E90FF", # DodgerBlue
            "#FF1493", # DeepPink
            "#ADFF2F", # GreenYellow
        ]
        
        # Map ID -> Color
        highlight_config = {}
        for idx, eid in enumerate(selected_ids):
            color = PALETTE[idx % len(PALETTE)]
            highlight_config[eid] = color
            
        # --- Animation Settings ---
        # --- Animation Settings ---
        metric = st.radio("Metric to Replay", ["Rank", "Gap to Leader"], horizontal=True)
        
        fig = visualizations.create_league_animation_chart(df, highlight_config, metric)
        st.plotly_chart(fig, use_container_width=True)

        # --- GIF Export ---
        st.divider()
        st.subheader("Export Animation")
        col_export, _ = st.columns([1, 4])
        with col_export:
            if st.button("Generate Animation (GIF)"):
                with st.spinner("Generating GIF... This can take 30-60 seconds."):
                    progress_bar = st.progress(0)
                    gif_bytes = league_replay.generate_league_gif(
                        df, 
                        highlight_config, 
                        progress_callback=lambda p: progress_bar.progress(p)
                    )
                    progress_bar.empty()
                    
                    st.success("GIF Generated!")
                    st.download_button(
                        label="Download GIF",
                        data=gif_bytes,
                        file_name="league_replay.gif",
                        mime="image/gif"
                    )

        # --- GW Snapshot (Template vs Differentials) ---
        st.divider()
        st.header("GW Snapshot: Template vs Differentials")
        
        # Snapshot Controls (Collapsible)
        with st.expander("Snapshot Settings", expanded=True):
            snap_col1, snap_col2 = st.columns(2)
            with snap_col1:
                # Manager selector (defaults to current user if in list, else first)
                # We need the list of managers from the league logic.
                # member_map was created above but is local.
                # We can re-fetch or pass it down? 
                # Ideally we reuse the existing data.
                # 'entry_ids' and 'member_map' are local in render_league_replay.
                # Let's assume we can re-use the sorted list.
                
                # Fetch league members for the dropdown
                league_data = league_replay.fetch_league_standings(selected_league_id)
                member_map = {}
                if league_data and 'standings' in league_data:
                    results = league_data['standings'].get('results', [])
                    # Limit to 50 for consistency, or take all?
                    # The snapshot logic fetches picks for ALL provided.
                    # Let's use the same logic as the replay if possible, or just take first page.
                    for m in results:
                        member_map[m['entry']] = m['player_name']
                
                # Create a list of (id, name) tuples for the selectbox
                manager_options = [(eid, name) for eid, name in member_map.items()]
                
                # Default index
                default_idx = 0
                if entry_id:
                    my_id = entry_id
                    for i, (eid, _) in enumerate(manager_options):
                        if eid == my_id:
                            default_idx = i
                            break
                            
                selected_manager_tuple = st.selectbox(
                    "Select Focus Manager",
                    options=manager_options,
                    format_func=lambda x: x[1],
                    index=default_idx
                )
                selected_manager_id = selected_manager_tuple[0]
                
            with snap_col2:
                impact_metric = st.selectbox(
                    "Impact Metric (Y-Axis)",
                    ["Form", "Points Per Game", "Cost"],
                    index=1
                )
        
        # Fetch Snapshot Data
        if st.button("Generate Snapshot"):
            with st.spinner(f"Fetching live picks for {len(member_map)} managers..."):
                snap_data, err = league_replay.get_league_snapshot_data(selected_league_id, selected_manager_id)
                
            if err:
                st.error(err)
            else:
                df_snap = snap_data['df']
                current_gw = snap_data['gw']
                num_mgrs = snap_data['num_managers']
                
                st.subheader(f"Gameweek {current_gw} Analysis ({num_mgrs} Managers)")
                
                
                fig_snap = visualizations.create_snapshot_chart(df_snap, impact_metric)
                st.plotly_chart(fig_snap, use_container_width=True)
                
                # --- Key Takeaways ---
                st.markdown("### Key Takeaways")
                
                col_k1, col_k2, col_k3 = st.columns(3)
                
                # 1. Most Owned
                top_owned = df_snap.sort_values('ownership_pct', ascending=False).head(5)
                with col_k1:
                    st.write("**Top 5 Owned**")
                    for _, r in top_owned.iterrows():
                        st.write(f"- {r['web_name']}: {r['ownership_pct']:.1f}%")
                        
                # 2. My Differentials (Owned by me, < 15% owned)
                my_diffs = df_snap[(df_snap['is_mine']) & (df_snap['ownership_pct'] < config.SNAPSHOT_DIFF_THRESHOLD)]
                with col_k2:
                    st.write("**Your Differentials (<15%)**")
                    if not my_diffs.empty:
                        for _, r in my_diffs.iterrows():
                            st.write(f"- {r['web_name']}: {r['ownership_pct']:.1f}%")
                    else:
                        st.write("No low-ownership differentials.")
                        
                # 3. Scary Differentials (Not owned by me, High owned?) 
                # Or "Top Differentials in League" (Not owned by me, High Form, Low Ownership)
                # Let's do "Threats" (Not owned by me, > 40% owned)
                threats = df_snap[(~df_snap['is_mine']) & (df_snap['ownership_pct'] > 40)].sort_values('ownership_pct', ascending=False).head(5)
                with col_k3:
                    st.write("**Main Threats (Not Owned, >40%)**")
                    if not threats.empty:
                        for _, r in threats.iterrows():
                            st.write(f"- {r['web_name']}: {r['ownership_pct']:.1f}%")
                    else:
                        st.write("You own the main template players.")

def render_player_consistency(conn, current_gw):
    st.header("Player Consistency Analysis")
    
    # 1. Data Freshness Check
    with st.spinner("Checking data freshness..."):
        player_consistency.check_and_update_data()
        
    # 2. Controls
    col1, col2, col3 = st.columns(3)
    
    with col1:
        # Metric Selector
        metric_map = {
            "Total Points": "consistency_points",
            "Defensive Contributions": "consistency_defensive",
            "BPS": "consistency_bps",
            "xG": "consistency_xg",
            "xGI": "consistency_xgi",
            "Minutes": "consistency_minutes"
        }
        selected_metric_label = st.selectbox("Select Metric", list(metric_map.keys()))
        selected_metric_col = metric_map[selected_metric_label]
        
    with col2:
        # Top X Teams Filter
        num_fixtures = st.slider("Next N Fixtures (Difficulty)", 1, 10, 5)
        top_x = st.slider("Top X Easiest Teams", 1, 20, 10)
        
    with col3:
        # Max Cost
        max_cost = st.slider("Max Price (£)", 4.0, 15.0, 15.0, 0.5)

    col4, col5 = st.columns(2)
    with col4:
        # Position Filter
        positions = st.multiselect("Positions", ["GKP", "DEF", "MID", "FWD"], default=["MID", "FWD"])
    
    with col5:
        # Availability Filter
        min_chance = st.selectbox(
            "Min Chance of Playing Next Round",
            options=[0, 25, 50, 75, 100],
            index=3, # Default 75%
            format_func=lambda x: f"{x}%+"
        )
    
    # 3. Get Data
    df_stats = player_consistency.get_consistency_stats()
    
    if df_stats.empty:
        st.warning("No data available.")
        return

    # Fetch Availability
    availability_map = player_consistency.fetch_player_availability()
    
    # Merge Availability
    # df_stats has 'element_id'
    def get_chance(eid):
        return availability_map.get(eid, {}).get('chance', 100) # Default to 100 if missing
        
    def get_news(eid):
        return availability_map.get(eid, {}).get('news', "")

    df_stats['chance_of_playing'] = df_stats['element_id'].map(get_chance)
    df_stats['news'] = df_stats['element_id'].map(get_news)

    # 4. Filter Data
    
    # Availability
    df_filtered = df_stats[df_stats['chance_of_playing'] >= min_chance]
    
    # Cost
    df_filtered = df_filtered[df_filtered['now_cost'] / 10.0 <= max_cost]
    
    # Position
    # Map full names to short names for filtering
    pos_map = {'Goalkeeper': 'GKP', 'Defender': 'DEF', 'Midfielder': 'MID', 'Forward': 'FWD'}
    df_filtered['position_short'] = df_filtered['position_name'].map(pos_map)
    df_filtered = df_filtered[df_filtered['position_short'].isin(positions)]
    
    # Top X Teams (Easiest Fixtures)
    # Reuse calculate_fixture_difficulty logic
    fixtures_df = get_future_fixtures(conn, current_gw)
    teams_df = get_teams(conn)
    
    team_difficulties = []
    for _, team in teams_df.iterrows():
        diff = calculate_fixture_difficulty(team['id'], fixtures_df, num_fixtures)
        team_difficulties.append({'team_name': team['name'], 'difficulty': diff})
        
    df_difficulty = pd.DataFrame(team_difficulties).sort_values('difficulty').head(top_x)
    allowed_teams = df_difficulty['team_name'].tolist()
    
    df_filtered = df_filtered[df_filtered['team_name'].isin(allowed_teams)]
    
    if df_filtered.empty:
        st.warning("No players match the criteria.")
        return
        
    # Show filtered out count?
    total_count = len(df_stats)
    filtered_count = len(df_filtered)
    st.caption(f"Showing {filtered_count} of {total_count} players.")

    # 5. Visualization
    st.subheader(f"Top Consistent Players by {selected_metric_label}")
    st.caption("Consistency Score = Median - Standard Deviation (Last 5 GWs)")
    
    # Sort by consistency score
    df_chart = df_filtered.sort_values(selected_metric_col, ascending=False).head(20)
    
    chart = alt.Chart(df_chart).mark_bar().encode(
        x=alt.X(selected_metric_col, title='Consistency Score'),
        y=alt.Y('web_name', sort='-x', title='Player'),
        color=alt.Color('position_name', legend=alt.Legend(title="Position")),
        tooltip=['web_name', 'team_name', 'now_cost', 'chance_of_playing', 'news', selected_metric_col]
    ).properties(
        height=600
    )
    
    st.altair_chart(chart, use_container_width=True)


def main():
    st.set_page_config(layout="wide", page_title="FPL Tool")
    
    st.title("FPL Analysis Tool")
    
    # --- Sidebar Controls ---
    st.sidebar.header("Global Settings")
    
    num_fixtures = st.sidebar.slider(
        "Number of upcoming fixtures", 
        min_value=1, max_value=10, value=5,
        help="How many future gameweeks to consider for fixture difficulty."
    )
    
    st.sidebar.header("Player Filters")
    
    max_rank = st.sidebar.number_input(
        "Show Top N Players by Composite Rank", 
        min_value=10, max_value=200, value=50,
        help="Rank is based on a mix of Pts/Match and Total Points."
    )
    
    min_minutes_threshold = st.sidebar.number_input(
        "Min Minutes Threshold", 
        min_value=30, max_value=90, value=60,
        help=f"Players must meet this minutes threshold in at least {int(config.MIN_FULL_APPEARANCE_RATIO*100)}% of their appearances."
    )

    # --- Data Loading ---
    conn = get_connection()
    current_gw = get_current_gameweek(conn)
    
    # --- Tabs ---
    tab1, tab2, tab3, tab4 = st.tabs(["Fixture Analysis", "Player Selection", "League Position Replay", "Player Consistency"])
    
    with tab1:
        render_fixture_analysis(conn, current_gw, num_fixtures)
        
    with tab2:
        render_player_analysis(conn, current_gw, num_fixtures, max_rank, min_minutes_threshold)
        
    with tab3:
        render_league_replay()
        
    with tab4:
        render_player_consistency(conn, current_gw)

    conn.close()

if __name__ == "__main__":
    main()
