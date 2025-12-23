# Exploratory Data Analysis Report: fraud_demo.db

## Executive Summary
This report outlines the structure and statistical properties of the `fraud_demo.db` database. The database contains transaction data, various exception tables, and machine learning anomaly scores. The dataset is highly imbalanced, typical of fraud detection scenarios.

## Database Schema
The database consists of the following tables:

### 1. `transactions`
The main table containing credit card transactions.
- **Rows**: 284,807
- **Columns**: 31
    - `Time`: Time elapsed since the first transaction.
    - `V1` - `V28`: Anonymized PCA features.
    - `Amount`: Transaction amount.
    - `Class`: Target variable (0 = Legitimate, 1 = Fraud).

### 2. `dq_exceptions`
Table for data quality exceptions.
- **Rows**: 0 (Empty)

### 3. `high_risk_exceptions`
Transactions flagged as high risk based on amount and percentile.
- **Rows**: 2,848
- **Key Columns**: `Time`, `Amount`, `Class`, `Percentile_Rank`.

### 4. `velocity_exceptions`
Transactions flagged for high velocity (frequency).
- **Rows**: 998
- **Key Columns**: `Time`, `Amount`, `Class`, `Window_Count`.

### 5. `anomaly_exceptions`
Transactions flagged by rule-based anomaly detection on V-features.
- **Rows**: 13,398
- **Key Columns**: `Time`, `V1`, `V2`, `Amount`, `Class`, `Reason`.

### 6. `ml_anomalies`
Results from a machine learning anomaly detection model.
- **Rows**: 5,697
- **Key Columns**: `Time`, `Amount`, `anomaly_score`.

---

## Detailed Analysis

### Transaction Data (`transactions`)
- **Class Imbalance**: The dataset is highly imbalanced.
    - Legitimate (0): 99.83%
    - Fraud (1): 0.17%
- **Amount**:
    - Mean: 88.35
    - Max: 25,691.20
    - Median: 22.00
- **Missing Values**: None.

### Exception Tables
- **High Risk**: Captures top percentile transactions by amount. All entries have `Percentile_Rank` of 99.
- **Velocity**: Captures high-frequency transactions. `Window_Count` ranges from 11 to 36.
- **Anomaly Exceptions**: Captures outliers in V1 and V2 features.

### ML Anomalies (`ml_anomalies`)
- **Anomaly Score**:
    - Mean: -0.05
    - Range: -0.26 to ~0.00
    - Lower scores indicate higher likelihood of anomaly (Isolation Forest typical output).

## Recommendations for Analyst
1.  **Focus on Imbalance**: Any modeling must account for the 99.8/0.2 split.
2.  **Investigate Exceptions**: The `high_risk_exceptions` and `velocity_exceptions` tables provide good starting points for rule-based filtering.
3.  **ML Validation**: Compare `ml_anomalies` with actual `Class` labels in `transactions` to validate the unsupervised model's performance.
