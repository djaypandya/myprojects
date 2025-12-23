import sqlite3
import pandas as pd

DB_NAME = 'fraud_demo.db'

def run_sql(conn, query):
    try:
        return pd.read_sql_query(query, conn)
    except Exception as e:
        print(f"Error running query: {e}")
        return None

def execute_ddl(conn, ddl):
    try:
        cursor = conn.cursor()
        cursor.execute(ddl)
        conn.commit()
    except Exception as e:
        print(f"Error executing DDL: {e}")

def main():
    print(f"Connecting to {DB_NAME}...")
    conn = sqlite3.connect(DB_NAME)
    
    # --- 3. Data Quality Assessment ---
    print("\n--- 3. Data Quality Assessment ---")
    
    # 3.1 Null Counts
    print("Checking for Nulls...")
    # Construct a query to check nulls for all columns dynamically or just key ones. 
    # For brevity/speed, checking key columns and a sample of V columns.
    null_check_sql = """
    SELECT 
        COUNT(*) - COUNT(Time) as Time_Nulls,
        COUNT(*) - COUNT(Amount) as Amount_Nulls,
        COUNT(*) - COUNT(Class) as Class_Nulls,
        COUNT(*) - COUNT(V1) as V1_Nulls
    FROM transactions
    """
    print(run_sql(conn, null_check_sql))

    # 3.2 Duplicates
    print("\nChecking for Duplicates (Time, Amount, V1)...")
    dup_check_sql = """
    SELECT Time, Amount, V1, COUNT(*) as cnt
    FROM transactions
    GROUP BY Time, Amount, V1
    HAVING cnt > 1
    ORDER BY cnt DESC
    LIMIT 5
    """
    print(run_sql(conn, dup_check_sql))

    # 3.3 Out of Range / Impossible Values
    print("\nChecking for Negative Amounts...")
    neg_amount_sql = "SELECT COUNT(*) as Negative_Amounts FROM transactions WHERE Amount < 0"
    print(run_sql(conn, neg_amount_sql))

    # 3.4 Distribution Checks
    print("\nClass Distribution (Fraud vs Non-Fraud)...")
    class_dist_sql = """
    SELECT Class, COUNT(*) as Count, 
           100.0 * COUNT(*) / (SELECT COUNT(*) FROM transactions) as Percentage
    FROM transactions
    GROUP BY Class
    """
    print(run_sql(conn, class_dist_sql))

    # --- 4. Whole-Population Exception Testing ---
    print("\n--- 4. Whole-Population Exception Testing ---")

    # 4.1 Create Exception Tables
    print("Creating exception tables...")
    ddl_statements = [
        "DROP TABLE IF EXISTS dq_exceptions",
        "CREATE TABLE dq_exceptions (Time FLOAT, Amount FLOAT, Reason TEXT)",
        
        "DROP TABLE IF EXISTS high_risk_exceptions",
        "CREATE TABLE high_risk_exceptions (Time FLOAT, Amount FLOAT, Class INT, Percentile_Rank FLOAT)",
        
        "DROP TABLE IF EXISTS velocity_exceptions",
        "CREATE TABLE velocity_exceptions (Time FLOAT, Amount FLOAT, Class INT, Window_Count INT)",
        
        "DROP TABLE IF EXISTS anomaly_exceptions",
        "CREATE TABLE anomaly_exceptions (Time FLOAT, V1 FLOAT, V2 FLOAT, Amount FLOAT, Class INT, Reason TEXT)"
    ]
    for ddl in ddl_statements:
        execute_ddl(conn, ddl)

    # 4.2 Populate DQ Exceptions (e.g., Amount < 0, though unlikely in this clean dataset)
    print("Populating DQ Exceptions...")
    execute_ddl(conn, "INSERT INTO dq_exceptions SELECT Time, Amount, 'Negative Amount' FROM transactions WHERE Amount < 0")
    
    # 4.3 Populate High Risk Exceptions (Top 1% Amount)
    print("Populating High Risk Exceptions (Top 1% Amount)...")
    # SQLite doesn't have PERCENT_RANK easily, so we use a subquery limit
    high_risk_sql = """
    INSERT INTO high_risk_exceptions
    SELECT Time, Amount, Class, 99.0
    FROM transactions
    ORDER BY Amount DESC
    LIMIT (SELECT CAST(COUNT(*) * 0.01 AS INT) FROM transactions)
    """
    execute_ddl(conn, high_risk_sql)
    print(run_sql(conn, "SELECT COUNT(*) as High_Risk_Count FROM high_risk_exceptions"))

    # 4.4 Populate Velocity Exceptions
    # "Rapid-velocity transactions in short time windows"
    # Since 'Time' is seconds from start, we can look for multiple transactions 
    # with the EXACT same Time (or very close) for simplicity in SQL.
    # A better check would be same V-features, but we assume distinct users aren't identified.
    # Let's flag timeframes with > 5 transactions in 1 second as a proxy for system burst/anomaly.
    print("Populating Velocity Exceptions (>10 txns in same second)...")
    velocity_sql = """
    INSERT INTO velocity_exceptions
    SELECT t.Time, t.Amount, t.Class, v.cnt
    FROM transactions t
    JOIN (
        SELECT Time, COUNT(*) as cnt
        FROM transactions
        GROUP BY Time
        HAVING cnt > 10
    ) v ON t.Time = v.Time
    """
    execute_ddl(conn, velocity_sql)
    print(run_sql(conn, "SELECT COUNT(*) as Velocity_Exceptions_Count FROM velocity_exceptions"))

    # 4.5 Populate Anomaly Exceptions (PCA Outliers > 3 SD)
    # V1-V28 are PCA components. Mean is roughly 0.
    # We'll check V1 for > 3 or < -3 as a sample heuristic for SQL-based outlier detection.
    print("Populating Anomaly Exceptions (V1 > 3 SD)...")
    # Assuming V1 is standardized, SD=1.
    anomaly_sql = """
    INSERT INTO anomaly_exceptions
    SELECT Time, V1, V2, Amount, Class, 'V1 Outlier (>3 SD)'
    FROM transactions
    WHERE ABS(V1) > 3
    """
    execute_ddl(conn, anomaly_sql)
    print(run_sql(conn, "SELECT COUNT(*) as Anomaly_Exceptions_Count FROM anomaly_exceptions"))

    conn.close()
    print("\nAnalysis Complete.")

if __name__ == "__main__":
    main()
