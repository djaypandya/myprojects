import sqlite3
import pandas as pd
from filtering_app import get_connection, get_player_data, get_future_fixtures, calculate_fixture_difficulty, get_current_gameweek

def test_logic():
    print("Testing App Logic...")
    conn = get_connection()
    
    # 1. Test Current GW
    gw = get_current_gameweek(conn)
    print(f"Current GW: {gw}")
    
    # 2. Test Player Data
    print("Fetching player data...")
    df_players = get_player_data(conn)
    print(f"Players found: {len(df_players)}")
    if not df_players.empty:
        print(df_players.head())
        print("Columns:", df_players.columns)
    
    # 3. Test Fixtures
    print("Fetching future fixtures...")
    df_fixtures = get_future_fixtures(conn, gw)
    print(f"Future fixtures found: {len(df_fixtures)}")
    if not df_fixtures.empty:
        print(df_fixtures.head())
    
    # 4. Test Difficulty Calculation
    if not df_players.empty and not df_fixtures.empty:
        team_id = df_players.iloc[0]['team_id']
        print(f"Testing difficulty for Team ID {team_id} (Next 5 fixtures)...")
        diff = calculate_fixture_difficulty(team_id, df_fixtures, 5)
        print(f"Difficulty Score: {diff}")
        
    conn.close()
    print("Test Complete.")

if __name__ == "__main__":
    test_logic()
