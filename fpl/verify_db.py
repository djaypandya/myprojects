import sqlite3
import pandas as pd

DB_NAME = "fpl_data.db"

def get_db_connection():
    conn = sqlite3.connect(DB_NAME)
    return conn

def verify_data():
    conn = get_db_connection()
    
    tables = ['seasons', 'teams', 'element_types', 'elements', 'events', 'fixtures', 'fixture_stats', 'player_history', 'player_history_past']
    
    print("--- Row Counts ---")
    for table in tables:
        count = pd.read_sql(f"SELECT COUNT(*) as count FROM {table}", conn).iloc[0]['count']
        print(f"{table}: {count}")
        
    print("\n--- Sample Data (Teams) ---")
    print(pd.read_sql("SELECT * FROM teams LIMIT 5", conn))

    print("\n--- Sample Data (Players) ---")
    print(pd.read_sql("SELECT web_name, team_id, now_cost FROM elements LIMIT 5", conn))
    
    print("\n--- Sample Data (Fixtures) ---")
    print(pd.read_sql("SELECT * FROM fixtures LIMIT 5", conn))

    print("\n--- Sample Data (Player History) ---")
    print(pd.read_sql("SELECT element_id, fixture_id, total_points, minutes FROM player_history LIMIT 5", conn))

    print("\n--- Sample Data (Player History Past) ---")
    print(pd.read_sql("SELECT element_id, season_name, total_points FROM player_history_past LIMIT 5", conn))

    conn.close()

if __name__ == "__main__":
    verify_data()
