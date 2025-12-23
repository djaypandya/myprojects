import sqlite3
import pandas as pd
from filtering_app import get_connection, get_player_data, calculate_ranks

def test_composite_rank():
    print("Testing Composite Rank Logic...")
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
        # Check columns
        if 'PPG_Rank' in df.columns and 'Total_Points_Rank' in df.columns and 'Rank' in df.columns:
            print("Composite Rank columns present.")
            
            # Check Ben White (Defender)
            white = df[df['Name'] == 'White']
            if not white.empty:
                print("\nBen White Stats:")
                print(white[['Name', 'Total Points', 'Pts/Match', 'PPG_Rank', 'Total_Points_Rank', 'Rank Display']].to_string(index=False))
            
            # Check a consistent defender (e.g. Saliba or Gabriel)
            gabriel = df[df['Name'] == 'Gabriel']
            if not gabriel.empty:
                print("\nGabriel Stats:")
                print(gabriel[['Name', 'Total Points', 'Pts/Match', 'PPG_Rank', 'Total_Points_Rank', 'Rank Display']].to_string(index=False))
                
            # Check Top 5 DEFs
            print("\nTop 5 DEFs by Composite Rank:")
            def_df = df[df['Position'] == 'Defender'].sort_values('Rank')
            print(def_df[['Name', 'Total Points', 'Pts/Match', 'PPG_Rank', 'Total_Points_Rank', 'Rank Display']].head())
            
        else:
            print("ERROR: Composite Rank columns missing!")
            
    conn.close()
    print("Test Complete.")

if __name__ == "__main__":
    test_composite_rank()
