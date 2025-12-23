import sqlite3
import pandas as pd
import os

DB_NAME = 'fraud_demo.db'
CSV_FILE = 'creditcard.csv'

def setup_database():
    print(f"Connecting to SQLite database: {DB_NAME}...")
    conn = sqlite3.connect(DB_NAME)
    cursor = conn.cursor()

    print("Creating 'transactions' table...")
    # Dropping table if exists to ensure clean state
    cursor.execute("DROP TABLE IF EXISTS transactions")
    
    # Create table schema matching the CSV structure
    # Time: float, V1-V28: float, Amount: float, Class: int
    create_table_sql = """
    CREATE TABLE transactions (
        Time FLOAT,
        V1 FLOAT, V2 FLOAT, V3 FLOAT, V4 FLOAT, V5 FLOAT, V6 FLOAT, V7 FLOAT, V8 FLOAT, V9 FLOAT,
        V10 FLOAT, V11 FLOAT, V12 FLOAT, V13 FLOAT, V14 FLOAT, V15 FLOAT, V16 FLOAT, V17 FLOAT, V18 FLOAT, V19 FLOAT,
        V20 FLOAT, V21 FLOAT, V22 FLOAT, V23 FLOAT, V24 FLOAT, V25 FLOAT, V26 FLOAT, V27 FLOAT, V28 FLOAT,
        Amount FLOAT,
        Class INTEGER
    );
    """
    cursor.execute(create_table_sql)
    conn.commit()
    print("Table 'transactions' created.")

    print(f"Loading data from {CSV_FILE}...")
    if not os.path.exists(CSV_FILE):
        raise FileNotFoundError(f"{CSV_FILE} not found.")

    # Load CSV in chunks to manage memory if needed, but dataset is small enough for direct load
    # Using pandas to_sql is efficient for SQLite
    df = pd.read_csv(CSV_FILE)
    print(f"Read {len(df)} rows from CSV.")
    
    df.to_sql('transactions', conn, if_exists='append', index=False)
    print("Data loaded successfully.")
    
    # Verify count
    cursor.execute("SELECT COUNT(*) FROM transactions")
    count = cursor.fetchone()[0]
    print(f"Total rows in database: {count}")
    
    conn.close()

if __name__ == "__main__":
    setup_database()
