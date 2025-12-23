import streamlit as st
import pandas as pd
import sqlite3
import plotly.express as px
import plotly.graph_objects as go

# --- Config & Setup ---
st.set_page_config(page_title="Fraud Analytics Dashboard", layout="wide")
DB_NAME = 'fraud_demo.db'

# --- Data Loading ---
@st.cache_data
def load_data():
    conn = sqlite3.connect(DB_NAME)
    # Load main transaction data (sampling if too large for fast interactivity, but 280k is manageable)
    # We join with ML anomalies to get scores
    query = """
    SELECT t.*, m.anomaly_score 
    FROM transactions t
    LEFT JOIN ml_anomalies m ON t.Time = m.Time AND t.Amount = m.Amount
    """
    df = pd.read_sql_query(query, conn)
    conn.close()
    
    # Preprocessing
    df['anomaly_score'] = df['anomaly_score'].fillna(1.0) # Fill non-anomalies with high score (inliers)
    df['Hour'] = (df['Time'] / 3600) % 24
    df['Class_Label'] = df['Class'].map({0: 'Legitimate', 1: 'Fraud'})
    return df

try:
    df = load_data()
except Exception as e:
    st.error(f"Error loading data: {e}")
    st.stop()

# --- Sidebar ---
st.sidebar.title("🔍 Filters")

# Filter: Class
class_filter = st.sidebar.multiselect(
    "Transaction Class",
    options=['Legitimate', 'Fraud'],
    default=['Legitimate', 'Fraud']
)

# Filter: Amount
min_amt, max_amt = int(df['Amount'].min()), int(df['Amount'].max())
amount_range = st.sidebar.slider(
    "Amount Range ($)",
    min_value=min_amt,
    max_value=max_amt,
    value=(min_amt, max_amt)
)

# Filter: Anomaly Score (only if ML ran)
score_min, score_max = df['anomaly_score'].min(), df['anomaly_score'].max()
score_range = st.sidebar.slider(
    "Anomaly Score Range (Lower = More Anomalous)",
    min_value=float(score_min),
    max_value=float(score_max),
    value=(float(score_min), float(score_max))
)

# Apply Filters
filtered_df = df[
    (df['Class_Label'].isin(class_filter)) &
    (df['Amount'].between(amount_range[0], amount_range[1])) &
    (df['anomaly_score'].between(score_range[0], score_range[1]))
]

# --- Main Layout ---
st.title("🛡️ Credit Card Fraud Analytics Dashboard")

# KPI Row
col1, col2, col3, col4 = st.columns(4)
col1.metric("Total Transactions", f"{len(filtered_df):,}")
fraud_count = len(filtered_df[filtered_df['Class'] == 1])
col2.metric("Fraud Cases", f"{fraud_count:,}")
fraud_rate = (fraud_count / len(filtered_df)) * 100 if len(filtered_df) > 0 else 0
col3.metric("Fraud Rate", f"{fraud_rate:.2f}%")
exposure = filtered_df[filtered_df['Class'] == 1]['Amount'].sum()
col4.metric("Fraud Exposure", f"${exposure:,.2f}")

# Tabs
tab1, tab2, tab3 = st.tabs(["📈 Overview", "🕵️ Anomaly Explorer", "🔬 Deep Dive"])

with tab1:
    st.subheader("Temporal Patterns")
    
    # Time of Day Heatmap (Histogram)
    fig_time = px.histogram(
        filtered_df, 
        x="Hour", 
        color="Class_Label", 
        nbins=24,
        barmode="overlay",
        title="Transaction Distribution by Hour of Day",
        color_discrete_map={'Legitimate': 'blue', 'Fraud': 'red'},
        opacity=0.7
    )
    st.plotly_chart(fig_time, use_container_width=True)
    
    st.subheader("Amount Distribution")
    fig_amt = px.box(
        filtered_df, 
        x="Class_Label", 
        y="Amount", 
        color="Class_Label",
        title="Monetary Value Distribution (Log Scale)",
        log_y=True,
        color_discrete_map={'Legitimate': 'blue', 'Fraud': 'red'}
    )
    st.plotly_chart(fig_amt, use_container_width=True)

with tab2:
    st.subheader("Anomaly Detection Results")
    
    # Scatter: Amount vs Anomaly Score
    # Color by Class to see if anomalies align with fraud
    fig_scatter = px.scatter(
        filtered_df,
        x="anomaly_score",
        y="Amount",
        color="Class_Label",
        title="Anomaly Score vs. Transaction Amount",
        hover_data=['Time', 'V1', 'V2'],
        color_discrete_map={'Legitimate': 'blue', 'Fraud': 'red'},
        opacity=0.6
    )
    fig_scatter.add_vline(x=-0.5, line_dash="dash", line_color="green", annotation_text="Anomaly Threshold")
    st.plotly_chart(fig_scatter, use_container_width=True)
    
    st.markdown("""
    **Interpretation**:
    - **Lower Scores** (left) indicate higher abnormality.
    - **Red Points** are confirmed fraud.
    - The goal is to see if red points cluster in the low-score region.
    """)

with tab3:
    st.subheader("Feature Analysis (PCA Components)")
    
    feature = st.selectbox("Select Feature to Inspect", [f"V{i}" for i in range(1, 29)])
    
    fig_feat = px.histogram(
        filtered_df,
        x=feature,
        color="Class_Label",
        title=f"Distribution of {feature} by Class",
        barmode="overlay",
        color_discrete_map={'Legitimate': 'blue', 'Fraud': 'red'},
        histnorm='probability density'
    )
    st.plotly_chart(fig_feat, use_container_width=True)
    
    st.subheader("Parallel Coordinates (Sample)")
    if len(filtered_df) > 1000:
        st.info("Downsampling to 500 points for Parallel Coordinates plot...")
        pc_df = filtered_df.sample(500)
    else:
        pc_df = filtered_df
        
    fig_par = px.parallel_coordinates(
        pc_df,
        dimensions=['V1', 'V2', 'V3', 'V4', 'Amount'],
        color="Class",
        color_continuous_scale=px.colors.diverging.Tealrose,
        title="Multivariate Outlier Analysis (V1-V4)"
    )
    st.plotly_chart(fig_par, use_container_width=True)

# Footer
st.markdown("---")
st.caption("Generated by Agentic AI Analyst | Data Source: Kaggle Credit Card Fraud Detection")
