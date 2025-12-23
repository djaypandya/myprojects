import sqlite3
import pandas as pd
from filtering_app import get_connection, get_player_data, calculate_ranks

def test_rank_logic():
    print("Testing Rank Logic...")
    conn = get_connection()
    
    # Fetch data
    df = get_player_data(conn, 60)
    
    # Calculate metrics needed for rank
    df['games_played'] = df['games_played'].replace(0, 1)
    df['Pts/Match'] = df['Total Points'] / df['games_played']
    
    # Calculate Ranks
    df = calculate_ranks(df)
    
    print(f"Players found: {len(df)}")
    if not df.empty:
        print("Columns:", df.columns)
        
        # Check Rank columns
        if 'Rank' in df.columns and 'Rank Display' in df.columns:
            print("Rank columns present.")
            
            # Check specific players if possible, or just top ranks
            # Filter for GKP to match user example
            gkp_df = df[df['Position'] == 'Goalkeeper'].sort_values('Rank')
            print("\nTop 5 GKPs by Pts/Match Rank:")
            print(gkp_df[['Name', 'Pts/Match', 'Rank', 'Rank Display']].head())
            
            # Check FWD
            fwd_df = df[df['Position'] == 'Forward'].sort_values('Rank')
            print("\nTop 5 FWDs by Pts/Match Rank:")
            print(fwd_df[['Name', 'Pts/Match', 'Rank', 'Rank Display']].head())
            
        else:
            print("ERROR: Rank columns missing!")
            
    conn.close()
    print("Test Complete.")

if __name__ == "__main__":
    test_rank_logic()
