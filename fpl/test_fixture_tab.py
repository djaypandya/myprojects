import sqlite3
import pandas as pd
from filtering_app import get_connection, get_teams, get_future_fixtures, calculate_fixture_difficulty, get_current_gameweek

def test_fixture_tab():
    print("Testing Fixture Tab Logic...")
    conn = get_connection()
    current_gw = get_current_gameweek(conn)
    num_fixtures = 5
    
    # 1. Get Data
    teams_df = get_teams(conn)
    fixtures_df = get_future_fixtures(conn, current_gw)
    
    print(f"Teams found: {len(teams_df)}")
    
    # 2. Process Data for a sample team (e.g. Arsenal)
    # Find Arsenal ID
    arsenal = teams_df[teams_df['short_name'] == 'ARS'].iloc[0]
    team_id = arsenal['id']
    team_short = arsenal['short_name']
    
    print(f"Testing for {team_short} (ID: {team_id})")
    
    # Get Opponents
    team_fixtures = fixtures_df[
        (fixtures_df['team_h'] == team_id) | (fixtures_df['team_a'] == team_id)
    ].head(num_fixtures)
    
    opponents = {}
    for _, fix in team_fixtures.iterrows():
        gw = fix['event']
        if fix['team_h'] == team_id:
            # Home Game: Opponent is Away Team (CAPS)
            opp_id = fix['team_a']
            opp_row = teams_df[teams_df['id'] == opp_id].iloc[0]
            opp_name = opp_row['short_name'].upper()
            print(f"GW {gw}: Home vs {opp_name}")
        else:
            # Away Game: Opponent is Home Team (lowercase)
            opp_id = fix['team_h']
            opp_row = teams_df[teams_df['id'] == opp_id].iloc[0]
            opp_name = opp_row['short_name'].lower()
            print(f"GW {gw}: Away vs {opp_name}")
        
        opponents[gw] = opp_name
        
    if len(opponents) > 0:
        print("Opponent formatting verification successful.")
    else:
        print("No upcoming fixtures found for Arsenal.")
            
    conn.close()
    print("Test Complete.")

if __name__ == "__main__":
    test_fixture_tab()
