import sqlite3
import pandas as pd
from filtering_app import get_connection, get_teams, get_future_fixtures, calculate_fixture_difficulty, get_current_gameweek

def debug_columns():
    print("Debugging Column Types...")
    conn = get_connection()
    current_gw = get_current_gameweek(conn)
    num_fixtures = 5
    season_id = '2025-26'
    
    # 1. Get Data
    teams_df = get_teams(conn, season_id)
    fixtures_df = get_future_fixtures(conn, current_gw, season_id)
    
    # 2. Process Data
    team_data = []
    for _, team in teams_df.iterrows():
        team_id = team['id']
        team_short = team['short_name']
        
        # Get Opponents
        team_fixtures = fixtures_df[
            (fixtures_df['team_h'] == team_id) | (fixtures_df['team_a'] == team_id)
        ].head(num_fixtures)
        
        opponents = {}
        for _, fix in team_fixtures.iterrows():
            gw = fix['event']
            opponents[gw] = "OPP"
            
        row = {
            'Team': team_short,
            'Total Difficulty': 0,
            **opponents
        }
        team_data.append(row)
        
    df_analysis = pd.DataFrame(team_data)
    
    print("DataFrame Columns:", df_analysis.columns.tolist())
    print("Column Types:", [type(c) for c in df_analysis.columns])
    
    # Check the filtering logic
    gw_cols_int = sorted([col for col in df_analysis.columns if isinstance(col, int)])
    print("Detected Integer GW Cols:", gw_cols_int)
    
    gw_cols_str = sorted([col for col in df_analysis.columns if isinstance(col, str) and col.isdigit()])
    print("Detected String Digit GW Cols:", gw_cols_str)
    
    conn.close()

if __name__ == "__main__":
    debug_columns()
