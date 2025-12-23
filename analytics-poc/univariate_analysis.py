import sqlite3
import pandas as pd
import os
from scipy.stats import skew, kurtosis

DB_PATH = 'fraud_demo.db'

def analyze_column(df, col_name):
    print(f"\n## Column: {col_name}")
    
    # Basic Stats
    desc = df[col_name].describe(percentiles=[0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99])
    print("\n### Statistics")
    print(desc.to_markdown())
    
    # Distribution Metrics
    if pd.api.types.is_numeric_dtype(df[col_name]):
        s = skew(df[col_name].dropna())
        k = kurtosis(df[col_name].dropna())
        print(f"\n- **Skewness**: {s:.4f}")
        print(f"- **Kurtosis**: {k:.4f}")
        
        # Outliers (Simple IQR check)
        Q1 = desc['25%']
        Q3 = desc['75%']
        IQR = Q3 - Q1
        lower_bound = Q1 - 1.5 * IQR
        upper_bound = Q3 + 1.5 * IQR
        outliers = df[(df[col_name] < lower_bound) | (df[col_name] > upper_bound)][col_name].count()
        print(f"- **Outliers (IQR method)**: {outliers} ({outliers/len(df)*100:.2f}%)")

def main():
    if not os.path.exists(DB_PATH):
        print(f"Error: {DB_PATH} not found.")
        return

    try:
        conn = sqlite3.connect(DB_PATH)
        df = pd.read_sql_query("SELECT * FROM transactions", conn)
        
        print("# Univariate Analysis Report")
        print(f"\nTotal Rows: {len(df)}")
        
        for col in df.columns:
            analyze_column(df, col)
            
    except Exception as e:
        print(f"An error occurred: {e}")
    finally:
        if conn:
            conn.close()

if __name__ == "__main__":
    main()
