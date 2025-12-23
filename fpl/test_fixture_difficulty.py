import sqlite3
import pandas as pd
from filtering_app import get_connection, get_future_fixtures, calculate_fixture_difficulty, get_current_gameweek

def test_fixture_difficulty():
    print("Testing Fixture Difficulty Adjustment...")
    conn = get_connection()
    current_gw = get_current_gameweek(conn)
    
    # Fetch fixtures
    df_fixtures = get_future_fixtures(conn, current_gw)
    
    if not df_fixtures.empty:
        # Pick a team and calculate manually
        # Let's pick Arsenal (Team ID 1 usually, or check DB)
        # We'll just pick the first team in the first fixture
        first_fix = df_fixtures.iloc[0]
        team_h = first_fix['team_h']
        team_a = first_fix['team_a']
        
        print(f"Checking Fixture: Team {team_h} (Home) vs Team {team_a} (Away)")
        print(f"Opponent Strength (Away Team): {first_fix['team_a_strength']}")
        
        # Calculate for Home Team (1 fixture)
        score_h = calculate_fixture_difficulty(team_h, df_fixtures, 1)
        expected_h = first_fix['team_a_strength'] - 0.1
        print(f"Home Team Score: {score_h} (Expected: {expected_h})")
        
        # Calculate for Away Team (1 fixture)
        score_a = calculate_fixture_difficulty(team_a, df_fixtures, 1)
        expected_a = first_fix['team_h_strength'] + 0.1
        print(f"Away Team Score: {score_a} (Expected: {expected_a})")
        
        if abs(score_h - expected_h) < 0.001 and abs(score_a - expected_a) < 0.001:
            print("SUCCESS: Adjustments applied correctly.")
        else:
            print("FAILURE: Calculation mismatch.")
            
    conn.close()
    print("Test Complete.")

if __name__ == "__main__":
    test_fixture_difficulty()
