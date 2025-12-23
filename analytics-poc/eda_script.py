import sqlite3
import pandas as pd
import os

DB_PATH = 'fraud_demo.db'

def get_schema(conn):
    print("## Database Schema")
    cursor = conn.cursor()
    cursor.execute("SELECT name FROM sqlite_master WHERE type='table';")
    tables = cursor.fetchall()
    
    for table in tables:
        table_name = table[0]
        print(f"\n### Table: {table_name}")
        cursor.execute(f"PRAGMA table_info({table_name})")
        columns = cursor.fetchall()
        df_cols = pd.DataFrame(columns, columns=['cid', 'name', 'type', 'notnull', 'dflt_value', 'pk'])
        print(df_cols[['name', 'type', 'notnull', 'pk']].to_markdown(index=False))
        
        # Get row count
        cursor.execute(f"SELECT COUNT(*) FROM {table_name}")
        count = cursor.fetchone()[0]
        print(f"\nTotal Rows: {count}")

def analyze_data(conn):
    print("\n## Data Analysis")
    cursor = conn.cursor()
    cursor.execute("SELECT name FROM sqlite_master WHERE type='table';")
    tables = cursor.fetchall()
    
    for table in tables:
        table_name = table[0]
        print(f"\n### Analysis for Table: {table_name}")
        
        df = pd.read_sql_query(f"SELECT * FROM {table_name}", conn)
        
        print("\n#### Summary Statistics")
        print(df.describe().to_markdown())
        
        print("\n#### Missing Values")
        missing = df.isnull().sum()
        print(missing[missing > 0].to_markdown())
        
        if 'Class' in df.columns:
            print("\n#### Class Distribution")
            print(df['Class'].value_counts(normalize=True).to_markdown())

def main():
    if not os.path.exists(DB_PATH):
        print(f"Error: {DB_PATH} not found.")
        return

    try:
        conn = sqlite3.connect(DB_PATH)
        get_schema(conn)
        analyze_data(conn)
    except Exception as e:
        print(f"An error occurred: {e}")
    finally:
        if conn:
            conn.close()

if __name__ == "__main__":
    main()
