import sqlite3
import pandas as pd
import subprocess
import streamlit as st
import numpy as np

# Constants
import config
import fpl_api

# Constants
# Imported from config

def get_db_connection():
    conn = sqlite3.connect(config.DB_PATH)
    conn.row_factory = sqlite3.Row
    return conn

def check_and_update_data():
    """
    Checks if the local DB is up to date with the latest finished gameweek.
    If not, runs ingest.py.
    """
    try:
        # 1. Get latest finished GW from API
        data = fpl_api.fetch_bootstrap_static()
        
        current_gw_index = 0
        latest_finished_gw = 0
        
        for i, event in enumerate(data['events']):
            if event['is_current']:
                current_gw_index = i
            if event['finished']:
                latest_finished_gw = event['id']
                
        # 2. Get max round from DB
        conn = get_db_connection()
        cursor = conn.cursor()
        
        # Check if table exists first
        cursor.execute("SELECT name FROM sqlite_master WHERE type='table' AND name='player_history'")
        if not cursor.fetchone():
            print("Table player_history not found. Running ingestion...")
            conn.close()
            run_ingestion()
            return

        cursor.execute("SELECT MAX(round) FROM player_history WHERE season_id = ?", (config.DEFAULT_SEASON_ID,))
        result = cursor.fetchone()
        db_max_round = result[0] if result[0] is not None else 0
        conn.close()
        
        print(f"Latest Finished GW (API): {latest_finished_gw}")
        print(f"Max Round (DB): {db_max_round}")

        # 3. Compare and Update
        # If DB is behind the latest FINISHED gameweek
        if db_max_round < latest_finished_gw:
            st.warning(f"Data out of date (DB: GW{db_max_round}, API: GW{latest_finished_gw}). Updating database...")
            run_ingestion()
            st.success("Database updated successfully!")
            st.rerun() # Rerun app to load new data
            
    except Exception as e:
        st.error(f"Error checking data freshness: {e}")

def run_ingestion():
    """Runs the ingest.py script."""
    try:
        # Using subprocess to run the script in the same environment
        result = subprocess.run(["python3", "ingest.py"], capture_output=True, text=True)
        if result.returncode != 0:
            st.error(f"Ingestion failed: {result.stderr}")
            print(result.stderr)
        else:
            print(result.stdout)
    except Exception as e:
        st.error(f"Failed to run ingestion script: {e}")

def get_consistency_stats(season_id=config.DEFAULT_SEASON_ID):
    """
    Calculates consistency stats for all players based on the last 5 gameweeks.
    Returns a DataFrame.
    """
    conn = get_db_connection()
    
    # Fetch last 5 GWs data for each player
    # We need: total_points, bps, minutes, proper defensive stats, and the new xG columns
    
    # We'll fetch all history first then filter/group in Pandas for flexibility
    # Optimization: Filter by season_id
    
    query = """
    SELECT 
        ph.element_id,
        el.web_name,
        el.element_type as position_id,
        et.singular_name as position_name,
        el.now_cost,
        el.team_id,
        t.name as team_name,
        ph.round,
        ph.total_points,
        ph.bps,
        ph.minutes,
        ph.clean_sheets,
        ph.saves,
        ph.penalties_saved,
        ph.expected_goals,
        ph.expected_goal_involvements
    FROM player_history ph
    JOIN elements el ON ph.element_id = el.id AND ph.season_id = el.season_id
    JOIN element_types et ON el.element_type = et.id AND el.season_id = et.season_id
    JOIN teams t ON el.team_id = t.id AND el.season_id = t.season_id
    WHERE ph.season_id = ?
    """
    
    df = pd.read_sql(query, conn, params=(season_id,))
    conn.close()
    
    if df.empty:
        return pd.DataFrame()

    # Calculate Defensive Contribution
    # Formula: (Clean Sheets * 4) + Saves + (Penalties Saved * 5)
    df['defensive_contribution'] = (df['clean_sheets'] * 4) + df['saves'] + (df['penalties_saved'] * 5)
    
    # Filter for last 5 available gameweeks PER PLAYER or GLOBAL?
    # Requirement: "Use the past 5 gameweeks of data for each player"
    # Usually implies the last 5 GWs played or the last 5 global GWs.
    # Let's use the last 5 global GWs to ensure fair comparison (form over same period).
    
    max_round = df['round'].max()
    min_round = max(1, max_round - 4) # Last 5 (inclusive)
    
    df_recent = df[df['round'] >= min_round].copy()
    
    # Group by Player and Calculate Consistency (Median - Std)
    # We need at least X games? Let's say at least 1 to return data, but consistency needs variance.
    # StdDev of 1 value is NaN (ddof=1). 
    
    stats = df_recent.groupby(['element_id', 'web_name', 'position_name', 'now_cost', 'team_name']).agg(
        # Total Points
        points_median=('total_points', 'median'),
        points_std=('total_points', 'std'),
        
        # Defensive
        defensive_median=('defensive_contribution', 'median'),
        defensive_std=('defensive_contribution', 'std'),
        
        # BPS
        bps_median=('bps', 'median'),
        bps_std=('bps', 'std'),
        
        # xG
        xg_median=('expected_goals', 'median'),
        xg_std=('expected_goals', 'std'),
        
        # xGI
        xgi_median=('expected_goal_involvements', 'median'),
        xgi_std=('expected_goal_involvements', 'std'),
        
        # Minutes
        minutes_median=('minutes', 'median'),
        minutes_std=('minutes', 'std'),
        
        # Count games played in this period
        games_played=('round', 'count')
    ).reset_index()
    
    # Fill NaN std (for single games) with 0? Or leave as is?
    # If 1 game, consistency is "Median" essentially (or undefined).
    # Let's fill with 0 to allow calculation, assuming consistent if only 1 game? 
    # Or maybe high penalty?
    # Standard approach: 0 std dev if 1 sample.
    stats = stats.fillna(0)
    
    # Calculate Consistency Scores (Median - Std)
    stats['consistency_points'] = stats['points_median'] - stats['points_std']
    stats['consistency_defensive'] = stats['defensive_median'] - stats['defensive_std']
    stats['consistency_bps'] = stats['bps_median'] - stats['bps_std']
    stats['consistency_xg'] = stats['xg_median'] - stats['xg_std']
    stats['consistency_xgi'] = stats['xgi_median'] - stats['xgi_std']
    stats['consistency_minutes'] = stats['minutes_median'] - stats['minutes_std']
    
    return stats

@st.cache_data(ttl=3600)
def fetch_player_availability():
    """
    Fetches the latest player availability data from bootstrap-static.
    Returns a dictionary mapping element_id -> {chance_of_playing_next_round, news}
    """
    try:
        data = fpl_api.fetch_bootstrap_static()
        
        availability_map = {}
        for p in data['elements']:
            # chance_of_playing_next_round can be None (assumed 100%), 0, 25, 50, 75, 100
            chance = p.get('chance_of_playing_next_round')
            if chance is None:
                chance = 100
            
            availability_map[p['id']] = {
                'chance': int(chance),
                'news': p.get('news', "")
            }
        return availability_map
    except Exception as e:
        print(f"Error fetching availability: {e}")
        return {}
