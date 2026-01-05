import streamlit as st
import pandas as pd
import plotly.graph_objects as go
from datetime import datetime
import league_replay
import config
import visualizations


def render_overview_scorecard(entry_id, league_id, current_gw, meta):
    """
    Render the Mini-league Snapshot scorecard with 8 KPI tiles.
    
    Tiles (in order):
    1. GW Points (Live)
    2. League Rank (Live GW) - conditional
    3. Overall Points (Season)
    4. Mini-league Rank (Season)
    5. Gap to 1st (Season)
    6. Gap to 3rd (Season)
    7. Players Left
    8. Captain Status
    """
    
    with st.container():
        st.subheader("📊 Mini-League Snapshot")
        
        # Track if any API calls fail
        has_warnings = False
        
        # Fetch all required data
        try:
            bootstrap = league_replay.fetch_bootstrap_static()
        except:
            bootstrap = None
            has_warnings = True
            
        try:
            standings_data = league_replay.fetch_league_standings_cached(league_id)
        except:
            standings_data = None
            has_warnings = True
            
        try:
            live_data = league_replay.fetch_event_live_cached(current_gw)
        except:
            live_data = None
            has_warnings = True
            
        try:
            fixtures_data = league_replay.fetch_fixtures_cached(current_gw)
        except:
            fixtures_data = None
            has_warnings = True
            
        try:
            picks_data = league_replay.fetch_picks_cached(entry_id, current_gw)
        except:
            picks_data = None
            has_warnings = True
        
        if has_warnings:
            st.warning("⚠️ Some live data unavailable; showing latest known values.")
        
        # Compute metrics
        # 1. GW Points (Live) - Effective (Raw - Hits)
        raw_points = league_replay.compute_live_gw_points(picks_data, live_data)
        gw_cost = picks_data.get('entry_history', {}).get('event_transfers_cost', 0) if picks_data else 0
        
        gw_points_display = "N/A"
        gw_delta = None
        
        if raw_points is not None:
            effective_points = raw_points - gw_cost
            gw_points_display = effective_points
            if gw_cost > 0:
                gw_delta = f"-{gw_cost} cost"
        elif meta:
             # Fallback
             gw_points_display = meta.get('summary_event_points', 'N/A')
        
        # 2. Standings info (for season metrics)
        standings_info = league_replay.get_standings_info(standings_data, entry_id)
        
        # 3. Overall Points (Season)
        overall_points = meta.get('summary_overall_points', 'N/A') if meta else 'N/A'
        
        # 4. Mini-league Rank
        league_rank = standings_info['my_rank'] if standings_info else 'N/A'
        
        # 5. Gap to 1st
        gap_to_1st = standings_info['gap_to_1st'] if standings_info else 'N/A'
        
        # 6. Gap to 3rd
        gap_to_3rd = standings_info['gap_to_3rd'] if standings_info else 'N/A'
        
        # 7. Players Left
        fixture_status = league_replay.get_fixture_status_map(fixtures_data, bootstrap)
        players_left = league_replay.compute_players_left(picks_data, fixture_status, bootstrap)
        
        # 8. Captain Status
        captain_info = league_replay.get_captain_status(picks_data, fixture_status, bootstrap)
        
        # 9. Live League Rank (conditional)
        league_size = standings_info['league_size'] if standings_info else 0
        show_live_rank = league_size <= league_replay.LIVE_RANK_MAX_MANAGERS and league_size > 0
        live_league_rank = None
        if show_live_rank:
            live_league_rank = league_replay.compute_live_league_rank(
                standings_data, current_gw, entry_id, live_data
            )
        
        # Render tiles
        if show_live_rank:
            # 8 tiles: 4 + 4
            cols = st.columns(4)
            with cols[0]:
                st.metric("GW Points (Live)", gw_points_display, delta=gw_delta, delta_color="inverse")
            with cols[1]:
                st.metric("Live GW Rank", live_league_rank if live_league_rank else 'N/A')
            with cols[2]:
                st.metric("Overall Points", overall_points)
            with cols[3]:
                st.metric("League Rank", league_rank)
            
            cols2 = st.columns(4)
            with cols2[0]:
                gap_1_display = f"{gap_to_1st:+d}" if isinstance(gap_to_1st, int) and gap_to_1st != 0 else str(gap_to_1st)
                st.metric("Gap to 1st", gap_1_display if gap_to_1st != 0 else "Leader! 🏆")
            with cols2[1]:
                if isinstance(gap_to_3rd, int):
                    gap_3_display = f"{gap_to_3rd:+d}" if gap_to_3rd != 0 else "0"
                else:
                    gap_3_display = str(gap_to_3rd)
                st.metric("Gap to 3rd", gap_3_display)
            with cols2[2]:
                st.metric("Players Left", players_left if players_left is not None else 'N/A')
            with cols2[3]:
                captain_display = f"{captain_info['name']} {captain_info['symbol']}"
                st.metric("Captain", captain_display)
        else:
            # 7 tiles (no live rank): 4 + 3
            cols = st.columns(4)
            with cols[0]:
                st.metric("GW Points (Live)", gw_points_display, delta=gw_delta, delta_color="inverse")
            with cols[1]:
                st.metric("Overall Points", overall_points)
            with cols[2]:
                st.metric("League Rank", league_rank)
            with cols[3]:
                gap_1_display = f"{gap_to_1st:+d}" if isinstance(gap_to_1st, int) and gap_to_1st != 0 else str(gap_to_1st)
                st.metric("Gap to 1st", gap_1_display if gap_to_1st != 0 else "Leader! 🏆")
            
            cols2 = st.columns(4)
            with cols2[0]:
                if isinstance(gap_to_3rd, int):
                    gap_3_display = f"{gap_to_3rd:+d}" if gap_to_3rd != 0 else "0"
                else:
                    gap_3_display = str(gap_to_3rd)
                st.metric("Gap to 3rd", gap_3_display)
            with cols2[1]:
                st.metric("Players Left", players_left if players_left is not None else 'N/A')
            with cols2[2]:
                captain_display = f"{captain_info['name']} {captain_info['symbol']}"
                st.metric("Captain", captain_display)
        
        # Last refreshed timestamp
        st.caption(f"Last refreshed: {datetime.now().strftime('%H:%M')}")
        
        st.divider()


def render_team_overview(entry_id, current_gw, bootstrap):
    """
    Renders the 'Current Week Team Overview' section.
    """
    st.header(f"📋 Team Overview (GW {current_gw})")
    
    # Fetch Data
    with st.spinner("Loading team status..."):
        try:
            live_data = league_replay.fetch_event_live_cached(current_gw)
            fixtures_data = league_replay.fetch_fixtures_cached(current_gw)
            picks_data = league_replay.fetch_picks_cached(entry_id, current_gw)
        except Exception as e:
            st.error(f"Could not load team data: {e}")
            return

    if not picks_data or not live_data:
        st.warning("Team data unavailable.")
        return

    # Process Data
    rows = league_replay.get_gw_team_details(picks_data, live_data, bootstrap, fixtures_data)
    transfer_info = league_replay.get_gw_transfer_summary(picks_data)
    
    if not rows:
        st.info("No player data found.")
        return

    # Calculate Summary Metrics
    starters = [r for r in rows if r['Mult'] > 0]
    
    # Played Status (Starters Only)
    n_finished = sum(1 for r in starters if r['Status'] == 'Finished')
    n_playing = sum(1 for r in starters if r['Status'] == 'Playing')
    total_starters = len(starters)
    played_str = f"{n_finished}/{total_starters}"
    if n_playing > 0:
        played_str += f" (+{n_playing} playing)"
    
    # Points
    gw_points = sum(r['Contrib'] for r in rows) # Raw
    cost = transfer_info['cost']
    effective_points = gw_points - cost
    
    # Render Container
    with st.container(border=True):
        # A. Summary Bar
        k1, k2, k3, k4, k5 = st.columns(5)
        
        with k1:
            st.metric("GW Points", effective_points, delta=f"-{cost} cost" if cost > 0 else None, delta_color="inverse")
        with k2:
            st.metric("Played", played_str, help="Starters finished / Total starters")
        with k3:
            chip = transfer_info['active_chip']
            chip_lbl = chip.upper() if chip else "None"
            st.metric("Chip", chip_lbl)
        with k4:
            # Transfers
            n_trans = transfer_info['transfers']
            st.metric("Transfers", f"{n_trans}", delta=f"-{cost} cost" if cost > 0 else None, delta_color="inverse")
        with k5:
            # Value
            val = picks_data.get('entry_history', {}).get('value', 0) / 10.0
            bank = picks_data.get('entry_history', {}).get('bank', 0) / 10.0
            st.metric("Team Value", f"£{val}m", f"£{bank}m bank")
            
        st.divider()
        
        # B. Detailed Table
        df = pd.DataFrame(rows)
        
        # Sorting: Mult descending, then Status
        status_rank = {'Finished': 1, 'Playing': 2, 'Upcoming': 3}
        df['status_rank'] = df['Status'].map(status_rank)
        df = df.sort_values(by=['Mult', 'status_rank', 'Contrib'], ascending=[False, True, False])
        
        # Styling / Config
        disp_cols = ['Player', 'Pos', 'Status', 'Mult', 'GW Pts', 'Stats', 'Contrib']
        
        st.dataframe(
            df[disp_cols],
            use_container_width=True,
            hide_index=True,
            column_config={
                "Player": st.column_config.TextColumn("Player", width="large"),
                "Pos": st.column_config.TextColumn("Pos", width="small"),
                "Status": st.column_config.TextColumn("Status", width="medium"),
                "Mult": st.column_config.NumberColumn("Mult", format="%d"),
                "GW Pts": st.column_config.NumberColumn("Raw Pts", format="%d"),
                "Stats": st.column_config.TextColumn("Breakdown", width="medium"),
                "Contrib": st.column_config.NumberColumn("Total", format="%d"),
            },
            height=560 
        )
        
    st.write("")
def render_league_race(league_id, entry_id, current_gw):
    """
    Render the interactive League Race visual.
    Includes simplified toggle, race chart, and drilldown panel.
    """
    st.header("🏁 League Race")
    
    # 1. Controls
    race_mode = st.radio(
        "Metric", 
        ["Total Points (Season)", "GW Points (Live)"],
        horizontal=True,
        index=0,
        key="race_mode_toggle"
    )
    
    mode = 'total' if race_mode.startswith("Total") else 'gw'
    
    # 2. Fetch Data
    with st.spinner(f"Analyzing {race_mode}..."):
        df_race, err = league_replay.get_league_race_data(league_id, current_gw, mode)
        
    if err:
        st.warning(f"Could not load race data: {err}")
        return
        
    # 3. Render Chart
    if df_race is not None and not df_race.empty:
        fig_race = visualizations.create_race_bar_chart(df_race, entry_id, mode)
        
        # Use selection event (Streamlit 1.35+)
        selection = st.plotly_chart(fig_race, width='stretch', on_select="rerun")
        
        # 4. Drilldown Panel
        # Determine selected manager
        selected_manager_id = entry_id # Default to user
        
        if selection and selection['selection']['points']:
            # Try to get customdata from point selection
            points = selection['selection']['points']
            if points and 'customdata' in points[0]:
                 selected_manager_id = points[0]['customdata']
                 
        # Fetch drilldown details
        drilldown_mgr = df_race[df_race['entry_id'] == selected_manager_id]
        
        if not drilldown_mgr.empty:
            mgr_row = drilldown_mgr.iloc[0]
            mgr_name = mgr_row['player_name']
            entry_name = mgr_row['entry_name']
            rank = mgr_row['rank']
            points = mgr_row['points']
            gap = points - df_race[df_race['entry_id'] == entry_id].iloc[0]['points']
            
            with st.container():
                st.markdown(f"### 🔎 Manager Drilldown: {mgr_name}")
                st.caption(f"Rank: #{rank}")
                
                # Metrics
                c1, c2, c3, c4 = st.columns(4)
                with c1: st.metric(race_mode.split()[0], points)
                with c2: st.metric("Gap from You", f"{gap:+d}" if gap != 0 and selected_manager_id != entry_id else "-")
                
                # Fetch deeper history for sparkline and chips if selection changes
                # (Simple caching handles this efficiency)
                history = league_replay.fetch_entry_history(selected_manager_id)
                
                if history:
                    # Chips
                    chips_used = league_replay.get_chip_usage(history)
                    with c3:
                        st.markdown("**Chips**")
                        if chips_used:
                            badges = ""
                            for chip in chips_used:
                                badges += f"<span style='background-color:#333; color:#ddd; padding:2px 6px; border-radius:4px; font-size:0.8em; margin-right:4px; display:inline-block; margin-bottom:4px;'>{chip}</span>"
                            st.markdown(badges, unsafe_allow_html=True)
                        else:
                            st.caption("None")
                            
                    # Sparkline (Last 5 GW)
                    current_hist = history.get('current', [])
                    if current_hist:
                        last_5 = current_hist[-5:]
                        gws = [x['event'] for x in last_5]
                        pts = [x['points'] for x in last_5]
                        
                        # Create mini sparkline
                        fig_spark = go.Figure(go.Scatter(x=gws, y=pts, mode='lines+markers', line_color='#00ff87'))
                        fig_spark.update_layout(
                            title="Last 5 GWs", margin=dict(t=30, l=10, r=10, b=10), height=100,
                            xaxis=dict(showgrid=False, zeroline=False),
                            yaxis=dict(showgrid=False, zeroline=False, showticklabels=False),
                            template='plotly_dark'
                        )
                        with c4:
                            st.plotly_chart(fig_spark, width='stretch', config={'displayModeBar': False})
                else:
                    with c3: st.write("History unavailable.")
                    



def render_season_trend(league_id, entry_id):
    """
    Render Season Trend section.
    """
    st.header("📈 Season Trend")
    
    # Controls
    c1, c2, c3 = st.columns([2, 2, 1])
    with c1:
        metric = st.radio("View", ["Total Points", "Overall Rank"], horizontal=True, key="trend_view")
    with c2:
        range_opt = st.radio("Range", ["Last 6", "Last 10", "All Season"], horizontal=True, index=2, key="trend_range")
    with c3:
        st.write("")
        st.write("")
        show_notes = st.checkbox("Show Annotations", value=False, key="trend_notes")
        
    metric_map = "Total Points" if metric == "Total Points" else "Rank"
    
    # Fetch Data
    with st.spinner("Analyzing season history..."):
        df_trend, err = league_replay.get_season_trend_data(league_id, entry_id)
        
    if err:
        st.warning(f"Could not load trend data: {err}")
        return
        
    if df_trend is not None and not df_trend.empty:
        # Companion Metrics (Calculated on Full Data or Window? Window seems better for "in range")
        # Creating a view for calculation
        df_view = df_trend.copy()
        if range_opt == "Last 6": df_view = df_view.tail(6)
        if range_opt == "Last 10": df_view = df_view.tail(10)
        
        # 1. Net Gain vs Leader
        metric_cols = st.columns(4)
        
        # 1. Net Gain vs Leader
        metric_cols = st.columns(4)
        
        user_start = df_view.iloc[0]['user_points']
        user_end = df_view.iloc[-1]['user_points']
        user_gain = user_end - user_start
        
        color_gain = "green" if user_gain >= 0 else "red"
        
        with metric_cols[0]:
            if 'leader_points' in df_view.columns and df_view['leader_points'].notnull().all():
                leader_start = df_view.iloc[0]['leader_points']
                leader_end = df_view.iloc[-1]['leader_points']
                leader_gain = leader_end - leader_start
                net_gain = user_gain - leader_gain
                color_net = "green" if net_gain >= 0 else "red"
                st.markdown(f"**Net vs Leader**<br><span style='font-size:1.2em; color:{color_net}'>{net_gain:+d}</span>", unsafe_allow_html=True)
            else:
                st.markdown(f"**Points Gained**<br><span style='font-size:1.2em; color:{color_gain}'>{user_gain}</span>", unsafe_allow_html=True)

        # 2. Best GW
        best_gw = df_view.loc[df_view['user_gw_points'].idxmax()]
        with metric_cols[1]:
             st.markdown(f"**Best GW**<br>{best_gw['user_gw_points']} (GW{best_gw['gw']})", unsafe_allow_html=True)
            
        # 3. Worst GW
        worst_gw = df_view.loc[df_view['user_gw_points'].idxmin()]
        with metric_cols[2]:
             st.markdown(f"**Worst GW**<br>{worst_gw['user_gw_points']} (GW{worst_gw['gw']})", unsafe_allow_html=True)
            
        # 4. Streak (Last 3)
        if len(df_view) >= 3 and 'leader_points' in df_view.columns:
            last_3 = df_view.tail(3)
            wins = 0
            for i in range(len(last_3) - 1):
                u_gw = last_3.iloc[i+1]['user_points'] - last_3.iloc[i]['user_points']
                l_gw = last_3.iloc[i+1]['leader_points'] - last_3.iloc[i]['leader_points']
                if u_gw > l_gw: wins += 1
            
            streak_label = "Improving" if wins >= 2 else "Declining"
            color_streak = "green" if wins >= 2 else "grey"
            with metric_cols[3]:
                 st.markdown(f"**Recent Form**<br><span style='color:{color_streak}'>{streak_label}</span>", unsafe_allow_html=True)

        # Visual
        fig = visualizations.create_trend_line_chart(df_trend, metric_map, show_notes, range_opt)
        st.plotly_chart(fig, width='stretch')
        
    st.divider()


def render_manager_comparison(league_id, entry_id, current_gw, member_map):
    """
    Render Manager Comparison (Head-to-Head) section.
    """
    st.header("⚔️ Manager Comparison (H2H)")
    
    # 1. Inputs
    c1, c2, c3, c4 = st.columns([1, 2, 1, 1])
    
    with c1:
        # GW Selector (MVP: just current GW or Input?)
        # For MVP, simplify to Current GW or simple dropdown if we had list.
        # We'll use current_gw as default.
        selected_gw = st.number_input("Gameweek", min_value=1, max_value=38, value=current_gw)
        
    with c2:
        # Rival Selector
        # Filter member_map to exclude user
        rival_options = {k: v for k, v in member_map.items() if k != entry_id}
        default_rival = []
        if rival_options:
             # Default to first available (usually rank 1 if member map ordered by rank)
             default_rival = [list(rival_options.keys())[0]]
             
        selected_rivals = st.multiselect(
            "Select Rival(s)", 
            options=rival_options.keys(),
            format_func=lambda x: rival_options[x],
            default=default_rival,
            max_selections=5
        )
        
    with c3:
        include_bench = st.checkbox("Include Bench", value=False)
        
    with c4:
        # Auto-refresh / Manual Refresh
        if st.button("Refresh Live"):
            st.rerun()
            
    if not selected_rivals:
        st.info("Select a rival to compare.")
        return

    # 2. Fetch Data
    with st.spinner("Fetching Head-to-Head data..."):
        h2h_data, err = league_replay.get_h2h_comparison_data(entry_id, selected_rivals, selected_gw, include_bench)
        
    if err:
        st.error(err)
        return
        
    # 3. Render Comparisons
    for rival_id in selected_rivals:
        rival_name = rival_options.get(rival_id, str(rival_id))
        data = h2h_data.get(rival_id, {})
        
        if 'error' in data:
            st.warning(f"Could not load data for {rival_name}")
            continue
            
        summary = data.get('summary', {})
        shared = data.get('shared', [])
        user_diff = data.get('user_diff', [])
        rival_diff = data.get('rival_diff', [])
        
        # Determine container border color based on winner?
        delta = summary['delta']
        win_color = "green" if delta > 0 else "red" if delta < 0 else "grey"
        
        with st.container(border=True):
            # A. Header Scoreboard
            col_score1, col_score2, col_score3 = st.columns([1, 1, 1])
            
            u_cost = summary.get('user_cost', 0)
            r_cost = summary.get('rival_cost', 0)
            
            u_delta = f"-{u_cost} hit" if u_cost > 0 else None
            r_delta = f"-{r_cost} hit" if r_cost > 0 else None
            
            with col_score1:
                st.metric("You", f"{summary['user_total']} pts", delta=u_delta, delta_color="inverse")
            with col_score2:
                st.metric("Delta", f"{delta:+d}", delta_color="normal")
            with col_score3:
                st.metric(rival_name, f"{summary['rival_total']} pts", delta=r_delta, delta_color="inverse")
            
            # B. Differentials (The Key Driver)
            st.subheader("Differentials")
            diff_col1, diff_col2 = st.columns(2)
            
            def render_diff_table(diff_list, owner_name):
                if not diff_list:
                    st.caption("No unique players.")
                    return
                # Formatting
                disp_data = []
                total_contrib = 0
                for d in diff_list:
                    # Icon for status
                    status = "✅" # Placeholder for fixture status
                    if d['is_bench']: status = "🪑"
                    if d['is_captain']: status += " (C)"
                    
                    disp_data.append({
                        "Player": f"{d['web_name']} {status}",
                        "Pts": d['points'],
                        "Contrib": d['contrib']
                    })
                    total_contrib += d['contrib']
                
                st.markdown(f"**{owner_name}** (Impact: {total_contrib} pts)")
                st.dataframe(
                    pd.DataFrame(disp_data),
                    use_container_width=True,
                    hide_index=True,
                    column_config={
                        "Contrib": st.column_config.NumberColumn("Contrib", format="%d")
                    }
                )
                
            with diff_col1:
                render_diff_table(user_diff, "Only You")
            with diff_col2:
                render_diff_table(rival_diff, f"Only {rival_name}")
                
            # C. Shared Players
            with st.expander(f"Shared Players ({len(shared)})", expanded=(len(shared) <= 5 and len(shared) > 0)):
                if shared:
                    # Prepare DF
                    shared_disp = []
                    for s in shared:
                        # Net Impact Label
                        net = s['net_impact']
                        net_str = f"{net:+d}" if net != 0 else "-"
                        
                        # Captain Markers
                        u_info = f"{s['web_name']}"
                        r_info = f"{s['web_name']}"
                        if s['u_cap']: u_info += " (C)"
                        if s['r_cap']: r_info += " (C)"
                        
                        shared_disp.append({
                            "Player": s['web_name'],
                            "Pts": s['points'],
                            "Your Contrib": s['u_contrib'],
                            "Rival Contrib": s['r_contrib'],
                            "Net": net
                        })
                    
                    st.dataframe(
                        pd.DataFrame(shared_disp),
                        use_container_width=True,
                        hide_index=True,
                        column_config={
                             "Net": st.column_config.NumberColumn("Your Gain", format="%+d")
                        }
                    ) 
                else:
                    st.caption("No shared players.")

    st.divider()

def render_template_analysis(league_id, entry_id):
    """
    Render Template Analysis chart (PPG vs Ownership) for the user.
    """
    st.header("🛡️ Squad Template Analysis")
    st.markdown("Assess your squad against the league template based on ownership and player quality.")
    
    # Defaults
    impact_metric = "Points Per Game"
    
    with st.spinner(f"Analyzing league ownership..."):
        snap_data, err = league_replay.get_league_snapshot_data(league_id, entry_id)
        
    if err:
        st.error(err)
        return
        
    df_snap = snap_data['df']
    
    # Chart
    fig_snap = visualizations.create_snapshot_chart(df_snap, impact_metric)
    st.plotly_chart(fig_snap, width='stretch')
    
    # Insights
    st.markdown("### 🧠 Key Takeaways")
    c1, c2, c3 = st.columns(3)
    
    # 1. Top Owned
    top_owned = df_snap.sort_values('ownership_pct', ascending=False).head(5)
    with c1:
        st.caption("Top 5 Owned in League")
        for _, r in top_owned.iterrows():
            st.write(f"**{r['web_name']}**: {r['ownership_pct']:.1f}%")
            
    # 2. My Differentials (Low owned players I have)
    # Threshold < 15% usually
    my_diffs = df_snap[(df_snap['is_mine']) & (df_snap['ownership_pct'] < 15)]
    with c2:
        st.caption("Your Differentials (<15%)")
        if not my_diffs.empty:
            for _, r in my_diffs.iterrows():
                st.write(f"**{r['web_name']}**: {r['ownership_pct']:.1f}%")
        else:
             st.write("No low-ownership differentials.")
             
    # 3. Main Threats (High owned players I don't have)
    threats = df_snap[(~df_snap['is_mine']) & (df_snap['ownership_pct'] > 40)].sort_values('ownership_pct', ascending=False).head(5)
    with c3:
        st.caption("Main Threats (>40% Owned)")
        if not threats.empty:
            for _, r in threats.iterrows():
                st.write(f"**{r['web_name']}**: {r['ownership_pct']:.1f}%")
        else:
            st.write("You own all the template players.")
            
    st.divider()


def main():
    st.set_page_config(layout="wide", page_title="My FPL Mini Leagues")
    
    st.title("My FPL Mini Leagues")
    st.markdown("Deep dive into your mini-leagues. Compare your team and track the title race.")
    
    # --- Persistent Team ID using URL query params ---
    # Check for saved Team ID in URL
    query_params = st.query_params
    saved_team_id = query_params.get("team_id", "")
    
    # --- Step 1: User Input ---
    with st.container():
        col1, col2 = st.columns([1, 2])
        with col1:
            entry_id_input = st.text_input(
                "Enter your Team ID", 
                value=saved_team_id,
                help="Found in your FPL URL. Your ID will be saved for next time."
            )
            
    if not entry_id_input:
        st.info("Please enter your Team ID to begin.")
        return

    if not entry_id_input.isdigit():
        st.error("Team ID must be a number.")
        return
        
    entry_id = int(entry_id_input)
    
    # Save Team ID to URL for persistence
    if entry_id_input != saved_team_id:
        st.query_params["team_id"] = entry_id_input
    
    # --- Step 2: League Selection ---
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
    
    # Get current GW for scorecard
    bootstrap = league_replay.fetch_bootstrap_static()
    current_gw = league_replay.get_current_gw(bootstrap) if bootstrap else None
    
    # --- Render Overview Scorecard (always visible after league selection) ---
    if current_gw:
        render_overview_scorecard(entry_id, selected_league_id, current_gw, meta)
        
        # --- Team Overview (Hub) ---
        render_team_overview(entry_id, current_gw, bootstrap)
        
        # --- Render League Race (new feature) ---
        render_league_race(selected_league_id, entry_id, current_gw)
        
        # --- Render Season Trend (new feature) ---
        render_season_trend(selected_league_id, entry_id)
        
        # --- Manager Comparison ---
        member_map = {}
        s_data = league_replay.fetch_league_standings_cached(selected_league_id)
        if s_data and 'standings' in s_data:
             for r in s_data['standings'].get('results', []):
                 member_map[r['entry']] = r['player_name']
                 
        if member_map:
             render_manager_comparison(selected_league_id, entry_id, current_gw, member_map)

        # --- Template Analysis ---
        st.divider()
        render_template_analysis(selected_league_id, entry_id)

if __name__ == "__main__":
    main()
