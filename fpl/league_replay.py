
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
    Extract strictly 'classic' leagues from the entry metadata.
    Returns a list of dicts: [{'id': 123, 'name': 'League Name'}, ...]
    """
    if not entry_data or 'leagues' not in entry_data or 'classic' not in entry_data['leagues']:
        return []
    
    return [
        {'id': league['id'], 'name': league['name']}
        for league in entry_data['leagues']['classic']
    ]

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
    
    member_map = {m['entry']: m['player_name'] + f" ({m['entry_name']})" for m in members}
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
