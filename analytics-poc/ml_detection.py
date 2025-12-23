import sqlite3
import pandas as pd
from sklearn.ensemble import IsolationForest
import numpy as np

DB_NAME = 'fraud_demo.db'

def main():
    print(f"Connecting to {DB_NAME}...")
    conn = sqlite3.connect(DB_NAME)
    
    # Load data for ML
    # We use V1-V28 and Amount. Time is usually not predictive in this raw form, 
    # and Class is the target (which we ignore for unsupervised anomaly detection).
    print("Loading data for training...")
    df = pd.read_sql_query("SELECT Time, V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12, V13, V14, V15, V16, V17, V18, V19, V20, V21, V22, V23, V24, V25, V26, V27, V28, Amount FROM transactions", conn)
    
    # Features for model
    features = [c for c in df.columns if c not in ['Time']]
    X = df[features]
    
    print(f"Training Isolation Forest on {len(X)} records...")
    # Contamination = 0.02 (flagging top 2% as anomalies as requested)
    iso_forest = IsolationForest(n_estimators=100, contamination=0.02, random_state=42, n_jobs=-1)
    iso_forest.fit(X)
    
    print("Scoring transactions...")
    # -1 for outliers, 1 for inliers
    preds = iso_forest.predict(X)
    scores = iso_forest.decision_function(X)
    
    df['anomaly_score'] = scores
    df['is_anomaly'] = preds
    
    # Filter for anomalies
    anomalies = df[df['is_anomaly'] == -1].copy()
    print(f"Identified {len(anomalies)} anomalies (Top 2%).")
    
    # Save to SQL
    print("Saving ML anomalies to database...")
    
    # Create table
    cursor = conn.cursor()
    cursor.execute("DROP TABLE IF EXISTS ml_anomalies")
    
    # We'll save Time, Amount, Score, and top contributing features (simplified)
    # Saving the full row for the anomaly table
    anomalies_to_save = anomalies[['Time', 'Amount', 'anomaly_score']]
    
    anomalies_to_save.to_sql('ml_anomalies', conn, if_exists='replace', index=False)
    
    print("ML Anomaly Detection Complete.")
    conn.close()

if __name__ == "__main__":
    try:
        # Check for sklearn
        import sklearn
        main()
    except ImportError:
        print("scikit-learn not found. Please install it.")
