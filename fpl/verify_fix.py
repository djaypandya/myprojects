import pandas as pd
import sqlite3
from filtering_app import get_future_fixtures, calculate_fixture_difficulty

DB_PATH = 'fpl_data.db'

def get_connection():
    conn = sqlite3.connect(DB_PATH)
    conn.row_factory = sqlite3.Row
    return conn

def main():
    conn = get_connection()
    
    # 1. Fetch Fixtures using the updated function
    # Assume current GW is 0 to get all future fixtures
    print("Fetching fixtures...")
    fixtures_df = get_future_fixtures(conn, 0)
    
    # 2. Find Liverpool (12) vs West Ham (19)
    # We know from previous steps this is Event 28 (Liverpool Home) or Event 13 (West Ham Home)
    # Let's check Event 28: Liverpool (Home) vs West Ham (Away)
    # In DB: Event 28 | Home: 12 | Away: 19 | H_Diff: 2 | A_Diff: 4
    
    # Wait, previous DB check:
    # 28|12|19|2|4
    # Home (12) difficulty is 2. Away (19) difficulty is 4.
    
    # Let's check Event 13: West Ham (Home) vs Liverpool (Away)
    # 13|19|12|4|2
    # Home (19) difficulty is 4. Away (12) difficulty is 2.
    
    # So for Liverpool (12):
    # vs West Ham (Home, Event 28): Should be 2.
    # vs West Ham (Away, Event 13): Should be 2.
    
    print("\n--- Verifying Liverpool (12) vs West Ham (19) ---")
    
    # Filter for just these games to isolate the calculation
    liv_whu_games = fixtures_df[
        ((fixtures_df['team_h'] == 12) & (fixtures_df['team_a'] == 19)) |
        ((fixtures_df['team_h'] == 19) & (fixtures_df['team_a'] == 12))
    ]
    
    for _, row in liv_whu_games.iterrows():
        gw = row['event']
        # Calculate for Liverpool
        # We need to pass a DF to calculate_fixture_difficulty, but it filters by team_id
        # So we can pass the single row DF
        
        single_game_df = pd.DataFrame([row])
        diff = calculate_fixture_difficulty(12, single_game_df, 1)
        
        print(f"GW {gw}: Difficulty for Liverpool = {diff:.1f}")
        
        # GW 13 (Away): 2 + 0.1 = 2.1
        # GW 28 (Home): 2 - 0.1 = 1.9
        
        expected = 2.1 if gw == 13 else 1.9
        
        if abs(diff - expected) < 0.01:
            print(f"  -> CORRECT (Expected {expected})")
        else:
            print(f"  -> INCORRECT (Expected {expected}, Got {diff:.1f})")

    conn.close()

if __name__ == "__main__":
    main()
