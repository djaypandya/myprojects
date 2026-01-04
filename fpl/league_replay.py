
import pandas as pd
import concurrent.futures
import streamlit as st
import time
import io
from PIL import Image
import plotly.graph_objects as go

import config
import fpl_api

# Check if kaleido is available for GIF export
try:
    import kaleido
    KALEIDO_AVAILABLE = True
except ImportError:
    KALEIDO_AVAILABLE = False

# Constants
# Imported from config

def generate_league_gif(df, highlight_config, progress_callback=None):
    """
    Generate a GIF of the league history.
    
    Args:
        df: DataFrame with columns [GW, Entry ID, Name, Rank, Total Points]
        highlight_config: Dict mapping {entry_id: hex_color_string}. 
                          Entries in this dict will be highlighted.
                          Others will be grey.
        progress_callback: Function to call with progress (0.0 to 1.0)
        
    Returns:
        bytes: The GIF file content
    """
    max_gw = df['GW'].max()
    all_entries = df['Entry ID'].unique()
    frames = []
    
    # Pre-calculate entry styles
    entry_styles = {}
    for eid in all_entries:
        # Get Name
        entry_rows = df[df['Entry ID'] == eid]
        name = entry_rows['Name'].iloc[0] if not entry_rows.empty else str(eid)
        
        is_highlighted = eid in highlight_config
        custom_color = highlight_config.get(eid, "#808080")
        
        entry_styles[eid] = {
            'name': name,
            'color': custom_color if is_highlighted else "#808080",
            'opacity': 1.0 if is_highlighted else 0.3,
            'width': 4 if is_highlighted else 1,
            'marker_size': 12 if is_highlighted else 6,
            'show_legend': is_highlighted # Only show legend for highlighted
        }
        
    # Iterate GWs
    for gw in range(1, max_gw + 1):
        if progress_callback:
            progress_callback(gw / max_gw)
            
        # Create Figure for this GW
        fig = go.Figure()
        
        # Filter Data
        # We need lines up to 'gw'
        # And dots at 'gw'
        
        # Performance Note: Re-creating figure every loop is safer for static export 
        # than updating traces, to ensure clean state.
        
        for eid in all_entries:
            style = entry_styles[eid]
            
            # Data up to current loop GW
            entry_history = df[(df['Entry ID'] == eid) & (df['GW'] <= gw)].sort_values('GW')
            
            if entry_history.empty:
                continue
                
            # Line
            fig.add_trace(go.Scatter(
                x=entry_history['GW'],
                y=entry_history['Rank'],
                mode='lines+markers',
                name=style['name'],
                line=dict(color=style['color'], width=style['width']),
                opacity=style['opacity'],
                marker=dict(size=4, color=style['color']), # Small markers on line history? Or just line?
                showlegend=style['show_legend']
            ))
            
            # Current Tip (Big Dot)
            current = entry_history[entry_history['GW'] == gw]
            if not current.empty:
                fig.add_trace(go.Scatter(
                    x=current['GW'],
                    y=current['Rank'],
                    mode='markers',
                    marker=dict(color=style['color'], size=style['marker_size']),
                    showlegend=False
                ))

        # Layout
        fig.update_layout(
            title=f"Gameweek {gw}",
            xaxis_title="Gameweek",
            yaxis_title="Position",
            yaxis_autorange="reversed",
            xaxis=dict(range=[0.5, max_gw + 0.5]),
            yaxis=dict(range=[df['Rank'].max() + 1, 0]),
            template="plotly_dark",
            margin=dict(l=50, r=50, t=50, b=50),
            width=600,
            height=450
        )
        
        # Render to Image
        img_bytes = fig.to_image(format="png", engine="kaleido")
        frames.append(Image.open(io.BytesIO(img_bytes)))
        
    # Save as GIF
    output_buffer = io.BytesIO()
    if frames:
        frames[0].save(
            output_buffer,
            format="GIF",
            save_all=True,
            append_images=frames[1:],
            duration=500, # 500ms per frame
            loop=0
        )
        
    return output_buffer.getvalue()

@st.cache_data(ttl=3600)
def fetch_team_metadata(entry_id):
    """Fetch manager details using fpl_api (cached)."""
    try:
        return fpl_api.fetch_entry(entry_id)
    except Exception as e:
        print(f"Error fetching metadata for {entry_id}: {e}")
        return None

@st.cache_data(ttl=3600)
def fetch_league_standings(league_id):
    """Fetch league standings using fpl_api (cached)."""
    try:
        return fpl_api.fetch_league_standings(league_id)
    except Exception as e:
        print(f"Error fetching league {league_id}: {e}")
        return None

@st.cache_data(ttl=3600)
def fetch_entry_history(entry_id):
    """Fetch history using fpl_api (cached)."""
    try:
        # returns {'current': [...], ...} or None
        return fpl_api.fetch_entry_history(entry_id)
    except Exception as e:
        print(f"Error fetching history for {entry_id}: {e}")
        return None

def get_classic_leagues(entry_data):
    """
    Extract strictly 'invitational classic' leagues from the entry metadata.
    Filters for:
    - scoring='c' (Classic) which is implicit in the 'classic' key
    - league_type='x' (Private/Invitational)
    
    Returns a list of dicts: [{'id': 123, 'name': 'League Name'}, ...]
    """
    if not entry_data or 'leagues' not in entry_data or 'classic' not in entry_data['leagues']:
        return []
    
    # Filter for league_type == 'x' (Private/Invitational)
    # Public leagues usually have league_type='s' (Global) or similar.
    invitational_leagues = [
        {'id': league['id'], 'name': league['name']}
        for league in entry_data['leagues']['classic']
        if league.get('league_type') == 'x'
    ]
    
    return invitational_leagues

# =========================================================
# SCORECARD FUNCTIONS
# =========================================================

# Constant for live rank calculation threshold
LIVE_RANK_MAX_MANAGERS = 40

@st.cache_data(ttl=60)
def fetch_event_live_cached(event_id):
    """Fetch live event data with short TTL for live scores."""
    try:
        return fpl_api.fetch_event_live(event_id)
    except Exception as e:
        print(f"Error fetching live data for event {event_id}: {e}")
        return None

@st.cache_data(ttl=600)
def fetch_fixtures_cached(event_id):
    """Fetch fixtures for an event with 10min TTL."""
    try:
        return fpl_api.fetch_fixtures(event=event_id)
    except Exception as e:
        print(f"Error fetching fixtures for event {event_id}: {e}")
        return None

@st.cache_data(ttl=120)
def fetch_picks_cached(entry_id, event_id):
    """Fetch picks with 2min TTL for relatively fresh data."""
    try:
        return fpl_api.fetch_entry_picks(entry_id, event_id)
    except Exception as e:
        print(f"Error fetching picks for entry {entry_id}, event {event_id}: {e}")
        return None

@st.cache_data(ttl=300)
def fetch_league_standings_cached(league_id):
    """Fetch league standings with 5min TTL."""
    try:
        return fpl_api.fetch_league_standings(league_id)
    except Exception as e:
        print(f"Error fetching standings for league {league_id}: {e}")
        return None

def compute_live_gw_points(picks_data, live_data):
    """
    Compute live GW points for starting XI only.
    
    Args:
        picks_data: from entry/{id}/event/{gw}/picks/
        live_data: from event/{gw}/live/
        
    Returns:
        int: Total live GW points
    """
    if not picks_data or not live_data:
        return None
    
    picks = picks_data.get('picks', [])
    elements = live_data.get('elements', [])
    
    # Build element_id -> live_points map
    live_points_map = {}
    for el in elements:
        el_id = el.get('id')
        stats = el.get('stats', {})
        live_points_map[el_id] = stats.get('total_points', 0)
    
    total = 0
    for pick in picks:
        # Only count starting XI (multiplier > 0)
        multiplier = pick.get('multiplier', 0)
        if multiplier > 0:
            element_id = pick.get('element')
            points = live_points_map.get(element_id, 0)
            total += points * multiplier
    
    return total

def get_fixture_status_map(fixtures_data, bootstrap_data):
    """
    Build a map of team_id -> finished status.
    
    Returns:
        dict: {team_id: {'finished': bool, 'started': bool}}
    """
    if not fixtures_data or not bootstrap_data:
        return {}
    
    team_status = {}
    teams = bootstrap_data.get('teams', [])
    
    # Initialize all teams as not started
    for team in teams:
        team_status[team['id']] = {'finished': False, 'started': False}
    
    # Update based on fixtures
    for fixture in fixtures_data:
        home_team = fixture.get('team_h')
        away_team = fixture.get('team_a')
        finished = fixture.get('finished', False)
        started = fixture.get('started', False)
        
        if home_team:
            team_status[home_team] = {'finished': finished, 'started': started}
        if away_team:
            team_status[away_team] = {'finished': finished, 'started': started}
    
    return team_status

def compute_players_left(picks_data, fixture_status, bootstrap_data):
    """
    Count starting XI players with unfinished fixtures.
    
    Returns:
        int: Number of players yet to play or still playing
    """
    if not picks_data or not fixture_status or not bootstrap_data:
        return None
    
    picks = picks_data.get('picks', [])
    elements = bootstrap_data.get('elements', [])
    
    # Build element_id -> team_id map
    element_team_map = {el['id']: el['team'] for el in elements}
    
    players_left = 0
    for pick in picks:
        multiplier = pick.get('multiplier', 0)
        if multiplier > 0:  # Starting XI only
            element_id = pick.get('element')
            team_id = element_team_map.get(element_id)
            if team_id:
                status = fixture_status.get(team_id, {})
                if not status.get('finished', False):
                    players_left += 1
    
    return players_left

def get_captain_status(picks_data, fixture_status, bootstrap_data):
    """
    Get captain name and played status.
    
    Returns:
        dict: {'name': str, 'played': bool, 'symbol': str}
    """
    if not picks_data or not bootstrap_data:
        return {'name': 'Unknown', 'played': False, 'symbol': '❓'}
    
    picks = picks_data.get('picks', [])
    elements = bootstrap_data.get('elements', [])
    
    # Build element lookups
    element_name_map = {el['id']: el['web_name'] for el in elements}
    element_team_map = {el['id']: el['team'] for el in elements}
    
    # Find captain
    captain_pick = next((p for p in picks if p.get('is_captain')), None)
    if not captain_pick:
        return {'name': 'No Captain', 'played': False, 'symbol': '❓'}
    
    captain_id = captain_pick.get('element')
    captain_name = element_name_map.get(captain_id, 'Unknown')
    captain_team = element_team_map.get(captain_id)
    
    # Check if captain's fixture is finished
    played = False
    symbol = '⏳'
    if fixture_status and captain_team:
        status = fixture_status.get(captain_team, {})
        played = status.get('finished', False)
        symbol = '✅' if played else '⏳'
    
    return {'name': captain_name, 'played': played, 'symbol': symbol}

def get_standings_info(standings_data, entry_id):
    """
    Extract user's standing info from league standings.
    
    Returns:
        dict: {
            'my_rank': int,
            'my_total': int,
            'gap_to_1st': int,
            'gap_to_3rd': int or 'N/A',
            'league_size': int,
            'first_place_total': int
        }
    """
    if not standings_data:
        return None
    
    standings = standings_data.get('standings', {})
    results = standings.get('results', [])
    
    if not results:
        return None
    
    league_size = len(results)
    
    # Find user
    my_row = next((r for r in results if r.get('entry') == entry_id), None)
    if not my_row:
        return None
    
    my_rank = my_row.get('rank', 0)
    my_total = my_row.get('total', 0)
    
    # Find 1st place
    first_place = next((r for r in results if r.get('rank') == 1), None)
    first_total = first_place.get('total', 0) if first_place else my_total
    gap_to_1st = first_total - my_total
    
    # Find 3rd place
    third_place = next((r for r in results if r.get('rank') == 3), None)
    if third_place:
        third_total = third_place.get('total', 0)
        gap_to_3rd = third_total - my_total  # Can be negative if user is ahead
    else:
        gap_to_3rd = 'N/A'
    
    return {
        'my_rank': my_rank,
        'my_total': my_total,
        'gap_to_1st': gap_to_1st,
        'gap_to_3rd': gap_to_3rd,
        'league_size': league_size,
        'first_place_total': first_total
    }

def compute_live_league_rank(standings_data, event_id, entry_id, live_data):
    """
    Compute live GW rank within the league.
    Only called if league_size <= LIVE_RANK_MAX_MANAGERS.
    
    Returns:
        int: User's live GW rank, or None on failure
    """
    if not standings_data or not live_data:
        return None
    
    standings = standings_data.get('standings', {})
    results = standings.get('results', [])
    
    if len(results) > LIVE_RANK_MAX_MANAGERS:
        return None
    
    # Fetch picks for all managers and compute live points
    manager_scores = []
    
    for manager in results:
        mgr_entry_id = manager.get('entry')
        mgr_total = manager.get('total', 0)
        
        picks = fetch_picks_cached(mgr_entry_id, event_id)
        live_pts = compute_live_gw_points(picks, live_data) if picks else 0
        
        manager_scores.append({
            'entry_id': mgr_entry_id,
            'live_gw_points': live_pts or 0,
            'total_points': mgr_total
        })
    
    # Sort by live GW points (desc), then total points (desc) as tiebreaker
    manager_scores.sort(key=lambda x: (-x['live_gw_points'], -x['total_points']))
    
    # Find user's rank
    for idx, mgr in enumerate(manager_scores):
        if mgr['entry_id'] == entry_id:
            return idx + 1  # 1-indexed rank
    
    return None

def get_league_race_data(league_id, event_id, mode='total'):
    """
    Get data for the League Race bar chart.
    
    Args:
        league_id: League ID
        event_id: Current GW ID
        mode: 'total' (Season points) or 'gw' (Live GW points)
        
    Returns:
        DataFrame: [entry_id, player_name, entry_name, points, rank, gap_to_user]
    """
    # 1. Fetch Standings
    standings_data = fetch_league_standings_cached(league_id)
    if not standings_data:
        return None, "Could not fetch standings."
        
    results = standings_data.get('standings', {}).get('results', [])
    if not results:
        return None, "No specific members found in this league."
    
    # Simple list of dicts to eventually turn into DF
    managers = []
    
    if mode == 'total':
        # Total Points Mode: Simpler, use standings data directly
        for rank_idx, res in enumerate(results):
            managers.append({
                'entry_id': res['entry'],
                'player_name': res['player_name'],
                'entry_name': res['player_name'], # Use Manager Name as fallback
                'points': res['total'],
                'rank': res['rank']
            })
            
    else:
        # GW Points Mode: Need live fetching
        # Limit to top 50 if necessary to save API calls
        race_results = results[:50]
        
        # We need live data for point calculation
        live_data = fetch_event_live_cached(event_id)
        if not live_data:
            return None, "Could not fetch live event data."
            
        # Helper to fetch and compute
        def fetch_and_compute(entry_id):
            picks = fetch_picks_cached(entry_id, event_id)
            if not picks:
                return 0
            return compute_live_gw_points(picks, live_data)
        
        # Concurrently fetch
        with concurrent.futures.ThreadPoolExecutor(max_workers=10) as executor:
            future_to_entry = {executor.submit(fetch_and_compute, r['entry']): r for r in race_results}
            
            for future in concurrent.futures.as_completed(future_to_entry):
                r = future_to_entry[future]
                try:
                    gw_points = future.result() or 0
                except:
                    gw_points = 0
                
                managers.append({
                    'entry_id': r['entry'],
                    'player_name': r['player_name'],
                    'entry_name': r['player_name'], # Use Manager Name as fallback for entry_name
                    'points': gw_points,
                    'rank': r['rank'] # Keep season rank for reference
                })
        
        # Sort by GW points desc
        managers.sort(key=lambda x: x['points'], reverse=True)
        # Re-assign rank based on GW performance (1 to N)
        for i, m in enumerate(managers):
            m['gw_rank'] = i + 1

    df_race = pd.DataFrame(managers)
    return df_race, None

def get_chip_usage(history_data):
    """
    Analyze chip usage from history.
    
    Returns:
        tuple: (list of used chips, list of remaining chips)
    """
    if not history_data or 'chips' not in history_data:
        return [], ["WC1", "WC2", "FH", "BB", "TC"] # Approximate defaults
        
    used_chips = []
    # Identify used chips
    for chip in history_data['chips']:
        chip_name = chip.get('name')
        chip_gw = chip.get('event')
        label = f"{chip_name} (GW{chip_gw})"
        used_chips.append(label)
        
    # Ideally logic for remaining chips requires knowing what was available.
    # Simplified: Just return used list for now.
    return used_chips

    return used_chips

def get_season_trend_data(league_id, user_entry_id):
    """
    Fetch data for Season Trend chart: User vs Leader vs 3rd Place.
    """
    # 1. Fetch Standings to find rivals
    standings_data = fetch_league_standings_cached(league_id)
    if not standings_data:
        return None, "Could not fetch standings."
        
    results = standings_data.get('standings', {}).get('results', [])
    if not results:
        return None, "No results in standings."
        
    # Find Leader and 3rd Place
    leader_entry = None
    third_entry = None
    
    # Sort just in case API didn't
    sorted_res = sorted(results, key=lambda x: x['rank'])
    
    if len(sorted_res) > 0:
        leader_entry = sorted_res[0]
    if len(sorted_res) > 2:
        third_entry = sorted_res[2]
        
    entries_to_fetch = {}
    entries_to_fetch['user'] = user_entry_id
    
    if leader_entry and leader_entry['entry'] != user_entry_id:
        entries_to_fetch['leader'] = leader_entry['entry']
        
    if third_entry and third_entry['entry'] != user_entry_id:
        # Avoid duplicate if leader is 3rd (impossible) or user is 3rd
        if 'leader' not in entries_to_fetch or entries_to_fetch['leader'] != third_entry['entry']:
             entries_to_fetch['3rd'] = third_entry['entry']
             
    # 2. Fetch Histories Concurrently
    histories = {}
    with concurrent.futures.ThreadPoolExecutor(max_workers=3) as executor:
        future_map = {executor.submit(fetch_entry_history, eid): key for key, eid in entries_to_fetch.items()}
        for future in concurrent.futures.as_completed(future_map):
            key = future_map[future]
            try:
                histories[key] = future.result()
            except Exception as e:
                print(f"Failed to fetch history for {key}: {e}")
                
    if 'user' not in histories or not histories['user']:
        return None, "Could not fetch user history."
        
    # 3. Align Data
    # Base on User's GWs
    user_gw_data = histories['user'].get('current', [])
    records = []
    
    # Helper to clean lookups
    def get_gw_stats(hist, gw_id):
        if not hist: return None
        for g in hist.get('current', []):
            if g['event'] == gw_id:
                return g
        return None
        
    user_chips = get_chip_usage(histories['user']) # List of strings "WC (GWX)"
    # Parse chips back to dict for eaasy lookup
    user_chip_map = {}
    if histories['user'] and 'chips' in histories['user']:
        for c in histories['user']['chips']:
            user_chip_map[c['event']] = c['name']

    for g in user_gw_data:
        gw = g['event']
        row = {
            'gw': gw,
            'user_points': g['total_points'],
            'user_rank': g['overall_rank'],
            'user_gw_points': g['points'],
            'user_cost': g['event_transfers_cost'],
            'user_chip': user_chip_map.get(gw)
        }
        
        # Leader
        if 'leader' in entries_to_fetch and 'leader' in histories:
             l_stats = get_gw_stats(histories['leader'], gw)
             if l_stats:
                 row['leader_points'] = l_stats['total_points']
                 row['leader_rank'] = l_stats['overall_rank']
                 
        # 3rd Place
        if '3rd' in entries_to_fetch and '3rd' in histories:
             t_stats = get_gw_stats(histories['3rd'], gw)
             if t_stats:
                 row['third_points'] = t_stats['total_points']
                 row['third_rank'] = t_stats['overall_rank']

        records.append(row)
        
    df = pd.DataFrame(records)
    return df, None

    return df, None

def get_h2h_comparison_data(user_entry_id, rival_ids, event_id, include_bench=False):
    """
    Fetch and compute Head-to-Head comparison data.
    
    Args:
        user_entry_id: User's Entry ID
        rival_ids: List of Rival Entry IDs
        event_id: GW ID
        include_bench: Boolean to include bench players (visual only usually)
        
    Returns:
        dict: {rival_id: {summary: {}, shared: [], user_diff: [], rival_diff: []}}
    """
    # 1. Fetch Picks Concurrently
    all_managers = [user_entry_id] + rival_ids
    picks_map = {}
    
    with concurrent.futures.ThreadPoolExecutor(max_workers=5) as executor:
        future_map = {executor.submit(fetch_picks_cached, eid, event_id): eid for eid in all_managers}
        for future in concurrent.futures.as_completed(future_map):
            eid = future_map[future]
            try:
                picks_map[eid] = future.result()
            except:
                picks_map[eid] = None
                
    if not picks_map.get(user_entry_id):
        return None, "Could not fetch your picks."
        
    # 2. Fetch Live Data & Static
    live_data = fetch_event_live_cached(event_id)
    bootstrap = fetch_bootstrap_static()
    
    if not live_data or not bootstrap:
        return None, "Could not fetch live/static data."
        
    # Helper to process a manager's picks into a set and detailed map
    def process_picks(picks_json):
        if not picks_json: return set(), {}
        player_set = set()
        details = {} # {element_id: {multiplier, is_captain, ...}}
        
        for pick in picks_json.get('picks', []):
            el_id = pick['element']
            mult = pick['multiplier']
            is_cap = pick['is_captain']
            is_vc = pick['is_vice_captain']
            
            # Bench handling: usually bench has multiplier 0. 
            # If include_bench is True, we treat them as part of the set.
            # However, for points calculation, we usually stick to multiplier.
            # User Constraint: "include bench (multiplier == 0) but label as bench"
            
            # Logic: If multiplier > 0, always include.
            # If multiplier == 0: include ONLY IF include_bench is True.
            
            if mult > 0 or include_bench:
                player_set.add(el_id)
                details[el_id] = {
                    'multiplier': mult,
                    'is_captain': is_cap,
                    'is_vice_captain': is_vc,
                    'is_bench': mult == 0
                }
        return player_set, details

    user_set, user_details = process_picks(picks_map.get(user_entry_id))
    
    # 3. Compare with each Rival
    comparison_results = {}
    
    # Helper to look up player info
    def get_player_info(el_id):
        # bootstrap 'elements' list
        for p in bootstrap['elements']:
            if p['id'] == el_id:
                team = next((t for t in bootstrap['teams'] if t['id'] == p['team']), {})
                return {
                    'web_name': p['web_name'],
                    'position': p['element_type'], # 1=GKP, etc
                    'team_short_name': team.get('short_name', ''),
                    'cost': p['now_cost'] / 10.0
                }
        return {'web_name': str(el_id), 'position': 0, 'team_short_name': '', 'cost': 0}

    # Helper to get live points
    def get_points(el_id):
        # live_data['elements'] is a list of dicts {id: X, stats: {total_points: Y}}
        for el in live_data.get('elements', []):
            if el['id'] == el_id:
                return el['stats']['total_points']
        return 0

    for r_id in rival_ids:
        r_picks = picks_map.get(r_id)
        if not r_picks:
             comparison_results[r_id] = {'error': 'Could not fetch picks'}
             continue
             
        rival_set, rival_details = process_picks(r_picks)
        
        shared = user_set.intersection(rival_set)
        user_only = user_set - rival_set
        rival_only = rival_set - user_set
        
        # Build Data Lists
        shared_list = []
        user_diff_list = []
        rival_diff_list = []
        
        user_total_score = 0
        rival_total_score = 0
        
        # --- SHARED ---
        for el in shared:
            info = get_player_info(el)
            pts = get_points(el)
            
            u_mult = user_details[el]['multiplier']
            r_mult = rival_details[el]['multiplier']
            
            u_contrib = pts * u_mult
            r_contrib = pts * r_mult
            
            # Only add to total if it's not bench (unless logic dictates otherwise, but std scoring implies multiplier usage)
            user_total_score += u_contrib
            rival_total_score += r_contrib
            
            shared_list.append({
                'web_name': info['web_name'],
                'team': info['team_short_name'],
                'position': info['position'],
                'points': pts,
                'u_mult': u_mult,
                'r_mult': r_mult,
                'u_contrib': u_contrib,
                'r_contrib': r_contrib,
                'net_impact': u_contrib - r_contrib,
                'u_cap': user_details[el]['is_captain'],
                'r_cap': rival_details[el]['is_captain']
            })
            
        # --- USER DIFF ---
        for el in user_only:
            info = get_player_info(el)
            pts = get_points(el)
            mult = user_details[el]['multiplier']
            contrib = pts * mult
            user_total_score += contrib
            
            user_diff_list.append({
                'web_name': info['web_name'],
                'team': info['team_short_name'],
                'position': info['position'],
                'points': pts,
                'contrib': contrib,
                'is_bench': user_details[el]['is_bench'],
                'is_captain': user_details[el]['is_captain']
            })
            
        # --- RIVAL DIFF ---
        for el in rival_only:
            info = get_player_info(el)
            pts = get_points(el)
            mult = rival_details[el]['multiplier']
            contrib = pts * mult
            rival_total_score += contrib
            
            rival_diff_list.append({
                'web_name': info['web_name'],
                'team': info['team_short_name'],
                'position': info['position'],
                'points': pts,
                'contrib': contrib,
                'is_bench': rival_details[el]['is_bench'],
                'is_captain': rival_details[el]['is_captain']
            })
            
        comparison_results[r_id] = {
            'summary': {
                'user_total': user_total_score,
                'rival_total': rival_total_score,
                'delta': user_total_score - rival_total_score
            },
            'shared': sorted(shared_list, key=lambda x: abs(x['net_impact']), reverse=True),
            'user_diff': sorted(user_diff_list, key=lambda x: x['contrib'], reverse=True),
            'rival_diff': sorted(rival_diff_list, key=lambda x: x['contrib'], reverse=True)
        }
        
    return comparison_results, None

def process_league_history(league_id, progress_callback=None):
    """
    Orchestrates the data fetching and processing for the league replay.
    
    1. Fetch League Members
    2. Fetch History for each member (Concurrent)
    3. Compute Cumulative Points per GW
    4. Rank per GW
    5. Return DataFrame
    """
    
    # 1. Fetch League Standings
    league_data = fetch_league_standings(league_id)

    if not league_data or 'standings' not in league_data:
        return None, "Could not fetch league data."
        
    # Handle pagination if necessary? 
    # For MVP, we'll implement fetching just the first page of standings (up to 50 managers).
    # If 'results' is in standings, use that.
    standings_results = league_data['standings'].get('results', [])
    
    if not standings_results:
        return None, "No specific members found in this league."

    # Limit to reasonable number to prevent massive API spam on large leagues during demo
    # Let's cap at 50 for safety/speed unless pagination logic is added.
    MAX_MEMBERS = 50
    members = standings_results[:MAX_MEMBERS]
    
    member_map = {m['entry']: m['player_name'] for m in members}
    entry_ids = [m['entry'] for m in members]
    
    # 2. Fetch Histories Concurrently
    histories = {}
    
    with concurrent.futures.ThreadPoolExecutor(max_workers=10) as executor:
        future_to_entry = {executor.submit(fetch_entry_history, eid): eid for eid in entry_ids}
        
        count = 0
        total = len(entry_ids)
        
        for future in concurrent.futures.as_completed(future_to_entry):
            eid = future_to_entry[future]
            try:
                data = future.result()
                if data and 'current' in data:
                    histories[eid] = data['current']
            except Exception as exc:
                print(f"Entry {eid} generated an exception: {exc}")
            
            count += 1
            if progress_callback:
                progress_callback(count / total)

    if not histories:
        return None, "Could not retrieve history for any members."

    # 3. Process Data
    # We need to build a single DataFrame: [GW, EntryID, Name, TotalPoints, Position]
    
    # Determine max GW
    all_gws = set()
    for eid, history in histories.items():
        for event in history:
            all_gws.add(event['event'])
            
    if not all_gws:
        return None, "No gameweek data found."
        
    max_gw = max(all_gws)
    
    records = []
    
    # We need to compute rank at EACH gw.
    # Structure: dict of gw -> list of (entry_id, total_points)
    gw_standings = {gw: [] for gw in range(1, max_gw + 1)}
    
    for eid, history in histories.items():
        # history is a list of events. 
        # API returns 'total_points' as cumulative? No, API 'history'['current'] has 'total_points' which IS cumulative.
        # Let's verify: entry/history -> current -> [{event: 1, points: 50, total_points: 50}, {event: 2, points: 40, total_points: 90}]
        # Yes, 'total_points' in the history object is cumulative.
        
        for event in history:
            gw = event['event']
            pts = event['total_points']
            gw_standings[gw].append({'entry_id': eid, 'points': pts})
            
    # Now assign ranks
    final_data = []
    
    for gw in range(1, max_gw + 1):
        if not gw_standings[gw]:
            continue
            
        # Sort desc by points
        # Secondary sort could be transfers cost? For now just points.
        current_standings = sorted(gw_standings[gw], key=lambda x: x['points'], reverse=True)
        
        leader_points = current_standings[0]['points'] if current_standings else 0
        
        for rank_idx, row in enumerate(current_standings):
            real_rank = rank_idx + 1 # 1-based rank
            entry_id = row['entry_id']
            points = row['points']
            name = member_map.get(entry_id, f"Manager {entry_id}")
            
            delta = leader_points - points
            
            final_data.append({
                'GW': gw,
                'Entry ID': entry_id,
                'Name': name,
                'Total Points': points,
                'Rank': real_rank,
                'Delta': delta
            })
            
    df = pd.DataFrame(final_data)
    return df, None

@st.cache_data(ttl=3600)
def fetch_bootstrap_static():
    """Fetch bootstrap data using fpl_api (cached)."""
    try:
        return fpl_api.fetch_bootstrap_static()
    except Exception as e:
        print(f"Error fetching bootstrap-static: {e}")
        return None

def get_current_gw(bootstrap_data):
    """
    Determine the current gameweek from bootstrap data.
    Prioritizes 'is_current', checks 'is_next', or falls back to max finished.
    """
    if not bootstrap_data or 'events' not in bootstrap_data:
        return None
        
    events = bootstrap_data['events']
    
    # 1. Try is_current
    current = next((e for e in events if e.get('is_current', False)), None)
    if current:
        return current['id']
        
    # 2. Try is_next (minus 1, if we are mid-week but no active GW)
    # Actually, during a GW, is_current is True.
    # Between GWs, is_current might be False, and next is is_next.
    # If is_current is False, we probably want the *upcoming* GW if we are looking for picks?
    # No, we want the *LIVE* or *LATEST* GW to show ownership for.
    # If the deadline has passed, is_current=True.
    # If season is over or paused, max(finished) is best.
    
    finished_events = [e for e in events if e.get('finished', False)]
    if finished_events:
        return max(e['id'] for e in finished_events)
        
    return 1 # Fallback

@st.cache_data(ttl=600)
def fetch_picks(entry_id, gw):
    """Fetch picks using fpl_api (cached)."""
    try:
        # returns {'picks': [...], ...}
        return fpl_api.fetch_entry_picks(entry_id, gw)
    except Exception as e:
        # print(f"Error fetching picks for {entry_id}: {e}") 
        return None

def get_league_snapshot_data(league_id, my_entry_id=None):
    """
    Orchestrator for the GW Snapshot.
    
    Returns:
        dict: {
            'player_stats': DataFrame (ownership, form, etc),
            'current_gw': int
        }
    """
    # 1. Bootstrap
    bootstrap = fetch_bootstrap_static()
    if not bootstrap:
        return None, "Could not fetch FPL data."
        
    current_gw = get_current_gw(bootstrap)
    if not current_gw:
        return None, "Could not determine current gameweek."
        
    # Prepare Player Map (id -> name, team, cost, form, ppg)
    elements = bootstrap['elements']
    teams = bootstrap['teams']
    team_code_map = {t['id']: t['short_name'] for t in teams}
    
    player_map = {}
    for p in elements:
        player_map[p['id']] = {
            'web_name': p['web_name'],
            'team': team_code_map.get(p['team'], ""),
            'now_cost': p['now_cost'] / 10.0,
            'form': float(p['form']),
            'points_per_game': float(p['points_per_game']),
            'total_points': p['total_points'],
            'element_type': p['element_type']
        }
        
    # 2. League Managers
    league_data = fetch_league_standings(league_id)
    if not league_data:
        return None, "Could not fetch league standings."
        
    standings_results = league_data['standings'].get('results', [])
    if not standings_results:
        return None, "No managers found in league."
        
    # Extract entries (handle pagination? For snapshot, top 50 is usually sufficient for 'template', but full is better)
    # Let's stick to the list we got.
    entries = [{'id': m['entry'], 'name': m['player_name']} for m in standings_results]
    
    # 3. Fetch Picks Concurrently
    picks_data = {} # entry_id -> {picks: set(ids), captain: id}
    
    with concurrent.futures.ThreadPoolExecutor(max_workers=20) as executor: # Higher workers for small payloads
        future_to_entry = {executor.submit(fetch_picks, e['id'], current_gw): e['id'] for e in entries}
        
        for future in concurrent.futures.as_completed(future_to_entry):
            eid = future_to_entry[future]
            try:
                data = future.result()
                if data and 'picks' in data:
                    pick_ids = {p['element'] for p in data['picks']}
                    captain_id = next((p['element'] for p in data['picks'] if p['is_captain']), None)
                    picks_data[eid] = {'picks': pick_ids, 'captain': captain_id}
            except Exception:
                pass
                
    if not picks_data:
        return None, "Could not fetch picks for any managers."
        
    num_managers = len(picks_data)
    
    # 4. Compute Ownership
    # Universe of active players in this league
    active_elements = set()
    for d in picks_data.values():
        active_elements.update(d['picks'])
        
    snapshot_rows = []
    
    my_picks = set()
    my_captain = None
    if my_entry_id and my_entry_id in picks_data:
        my_picks = picks_data[my_entry_id]['picks']
        my_captain = picks_data[my_entry_id]['captain']
    
    for pid in active_elements:
        idx_info = player_map.get(pid, {})
        if not idx_info:
            continue
            
        owners = [eid for eid, d in picks_data.items() if pid in d['picks']]
        count = len(owners)
        pct = (count / num_managers) * 100.0
        
        is_mine = pid in my_picks
        is_my_captain = (pid == my_captain)
        
        snapshot_rows.append({
            'element_id': pid,
            'web_name': idx_info['web_name'],
            'team': idx_info['team'],
            'position_id': idx_info['element_type'],
            'cost': idx_info['now_cost'],
            'form': idx_info['form'],
            'ppg': idx_info['points_per_game'],
            'ownership_pct': pct,
            'owners_count': count,
            'is_mine': is_mine,
            'is_captain': is_my_captain
        })
        
    df = pd.DataFrame(snapshot_rows)
    return {'df': df, 'gw': current_gw, 'num_managers': num_managers}, None
