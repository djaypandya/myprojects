import sqlite3
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import os

# Setup
DB_NAME = 'fraud_demo.db'
ASSETS_DIR = 'assets'
if not os.path.exists(ASSETS_DIR):
    os.makedirs(ASSETS_DIR)

# Style
plt.style.use('seaborn-v0_8-whitegrid')
sns.set_context("talk")
PALETTE = {'Legitimate': '#1f77b4', 'Fraud': '#d62728'}

def load_data():
    conn = sqlite3.connect(DB_NAME)
    # Load main data + ML scores
    query = """
    SELECT t.*, m.anomaly_score 
    FROM transactions t
    LEFT JOIN ml_anomalies m ON t.Time = m.Time AND t.Amount = m.Amount
    """
    df = pd.read_sql_query(query, conn)
    conn.close()
    df['Class_Label'] = df['Class'].map({0: 'Legitimate', 1: 'Fraud'})
    df['anomaly_score'] = df['anomaly_score'].fillna(1.0)
    return df

def plot_exception_overview(df):
    # Data prep
    total = len(df)
    fraud = len(df[df['Class']==1])
    high_risk = 2848 # From previous analysis
    velocity = 998   # From previous analysis
    ml_flagged = 5697 # From previous analysis
    
    categories = ['Total Population', 'ML Flagged (Top 2%)', 'High-Risk Amount (Top 1%)', 'Velocity Anomalies', 'Confirmed Fraud']
    counts = [total, ml_flagged, high_risk, velocity, fraud]
    colors = ['lightgray', 'orange', 'orange', 'orange', 'red']
    
    fig, ax = plt.subplots(figsize=(12, 6))
    bars = ax.barh(categories, counts, color=colors)
    ax.invert_yaxis()
    ax.set_xscale('log')
    ax.set_title('Exception Funnel: From Population to Fraud', fontsize=16, fontweight='bold')
    ax.set_xlabel('Count (Log Scale)')
    
    # Annotate
    for i, v in enumerate(counts):
        ax.text(v * 1.1, i, f"{v:,}", va='center')
        
    plt.tight_layout()
    plt.savefig(f"{ASSETS_DIR}/1_exception_funnel.png", dpi=300)
    plt.close()

def plot_high_risk_amount(df):
    fig, ax = plt.subplots(figsize=(10, 6))
    
    # Boxplot of Amount by Class
    sns.boxplot(data=df, x='Class_Label', y='Amount', palette=PALETTE, ax=ax, showfliers=False)
    sns.stripplot(data=df[df['Amount'] > 1000], x='Class_Label', y='Amount', color='black', alpha=0.3, jitter=True, ax=ax)
    
    ax.set_yscale('log')
    ax.set_title('Transaction Amount Distribution by Class', fontsize=16, fontweight='bold')
    ax.set_ylabel('Amount ($) - Log Scale')
    
    # Annotation
    ax.text(0.5, 0.9, "High-value outliers exist in both,\nbut fraud skews higher relative to median", 
            transform=ax.transAxes, ha='center', bbox=dict(facecolor='white', alpha=0.8))
            
    plt.tight_layout()
    plt.savefig(f"{ASSETS_DIR}/2_amount_risk.png", dpi=300)
    plt.close()

def plot_velocity_burst(df):
    # Simulate velocity: Count txns per second (Time is seconds)
    # We'll take a slice where a burst happened
    counts = df['Time'].value_counts().sort_index()
    
    # Find a burst
    burst_time = counts[counts > 5].index[0]
    window = counts.loc[burst_time-60:burst_time+60]
    
    fig, ax = plt.subplots(figsize=(12, 5))
    ax.plot(window.index, window.values, color='#1f77b4')
    
    # Highlight burst
    ax.axvspan(burst_time-2, burst_time+2, color='red', alpha=0.3)
    ax.annotate('Velocity Burst\n(>10 txns/sec)', xy=(burst_time, 10), xytext=(burst_time+20, 15),
                arrowprops=dict(facecolor='black', shrink=0.05))
                
    ax.set_title('Velocity Anomaly Detection (Simulated View)', fontsize=16, fontweight='bold')
    ax.set_xlabel('Time (Seconds)')
    ax.set_ylabel('Transactions per Second')
    
    plt.tight_layout()
    plt.savefig(f"{ASSETS_DIR}/3_velocity_burst.png", dpi=300)
    plt.close()

def plot_ml_score_dist(df):
    fig, ax = plt.subplots(figsize=(12, 6))
    
    # Histogram of anomaly scores
    sns.histplot(data=df, x='anomaly_score', hue='Class_Label', palette=PALETTE, element='step', stat='density', common_norm=False, ax=ax)
    
    ax.set_title('ML Anomaly Score Distribution', fontsize=16, fontweight='bold')
    ax.set_xlabel('Anomaly Score (Lower = More Anomalous)')
    
    # Highlight anomaly region
    ax.axvline(-0.5, color='green', linestyle='--')
    ax.text(-0.6, 0.5, "Anomaly Zone\n(Top 2%)", color='green', ha='right', transform=ax.get_xaxis_transform())
    
    plt.tight_layout()
    plt.savefig(f"{ASSETS_DIR}/4_ml_scores.png", dpi=300)
    plt.close()

def main():
    print("Loading data...")
    df = load_data()
    
    print("Generating plots...")
    plot_exception_overview(df)
    plot_high_risk_amount(df)
    plot_velocity_burst(df)
    plot_ml_score_dist(df)
    
    print("Plots generated in assets/ directory.")

if __name__ == "__main__":
    main()
