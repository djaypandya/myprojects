import sqlite3
import pandas as pd
from filtering_app import get_connection, get_player_data

def test_refinement():
    print("Testing Refined App Logic...")
    conn = get_connection()
    
    # Test with a minutes threshold of 60
    min_minutes = 60
    print(f"Fetching player data with min_minutes_threshold={min_minutes}...")
    df = get_player_data(conn, min_minutes)
    
    print(f"Players found: {len(df)}")
    if not df.empty:
        print("Columns:", df.columns)
        
        # Check if new columns exist
        if 'full_appearances' in df.columns and 'games_played' in df.columns:
            print("Aggregation columns present.")
            
            # Calculate pct
            df['games_played'] = df['games_played'].replace(0, 1)
            df['pct'] = df['full_appearances'] / df['games_played']
            
            # Show sample of players with high consistency
            consistent_players = df[df['pct'] >= 0.7]
            print(f"Players with >= 70% full appearances ({min_minutes}+ mins): {len(consistent_players)}")
            print(consistent_players[['Name', 'Team', 'games_played', 'full_appearances', 'pct']].head())
        else:
            print("ERROR: Aggregation columns missing!")
            
    conn.close()
    print("Test Complete.")

if __name__ == "__main__":
    test_refinement()
