import sqlite3
import pandas as pd

DB_NAME = 'fraud_demo.db'

def main():
    conn = sqlite3.connect(DB_NAME)
    
    stats = {}
    
    # Total Transactions
    stats['total_txns'] = pd.read_sql("SELECT COUNT(*) FROM transactions", conn).iloc[0,0]
    
    # Fraud Count (Class=1)
    stats['fraud_count'] = pd.read_sql("SELECT COUNT(*) FROM transactions WHERE Class=1", conn).iloc[0,0]
    
    # Exception Counts
    stats['dq_exceptions'] = pd.read_sql("SELECT COUNT(*) FROM dq_exceptions", conn).iloc[0,0]
    stats['high_risk'] = pd.read_sql("SELECT COUNT(*) FROM high_risk_exceptions", conn).iloc[0,0]
    stats['velocity'] = pd.read_sql("SELECT COUNT(*) FROM velocity_exceptions", conn).iloc[0,0]
    stats['anomaly_v1'] = pd.read_sql("SELECT COUNT(*) FROM anomaly_exceptions", conn).iloc[0,0]
    stats['ml_anomalies'] = pd.read_sql("SELECT COUNT(*) FROM ml_anomalies", conn).iloc[0,0]
    
    # Overlap: ML Anomalies that are also actual Fraud
    # We need to join ml_anomalies with transactions on Time and Amount (proxy for ID)
    overlap_sql = """
    SELECT COUNT(*) 
    FROM ml_anomalies m
    JOIN transactions t ON m.Time = t.Time AND m.Amount = t.Amount
    WHERE t.Class = 1
    """
    stats['ml_fraud_overlap'] = pd.read_sql(overlap_sql, conn).iloc[0,0]
    
    # Monetary Exposure (Sum of Amount for High Risk)
    stats['high_risk_exposure'] = pd.read_sql("SELECT SUM(Amount) FROM high_risk_exceptions", conn).iloc[0,0]
    
    print("--- REPORT STATS ---")
    for k, v in stats.items():
        print(f"{k}: {v}")
        
    conn.close()

if __name__ == "__main__":
    main()
