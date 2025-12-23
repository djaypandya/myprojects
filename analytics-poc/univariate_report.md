# Univariate Analysis Report

## Overview
This report provides a detailed column-by-column analysis of the `transactions` table in `fraud_demo.db`.
Total Rows: 284,807

## Column Analysis

### Time
- **Description**: Time elapsed in seconds since the first transaction.
- **Distribution**: Bimodal distribution (likely day/night cycles).
- **Skewness**: -0.0356
- **Kurtosis**: -1.2935
- **Outliers**: 0 (0.00%)

### V1 - V28 (PCA Features)
These features are result of a PCA transformation.
- **General Observation**: Most are centered around 0 with standard deviation close to 1, but some have significant outliers.
- **Skewness**: Many features exhibit high skewness (e.g., V1, V2, V8, V28), indicating non-normal distributions.
- **Kurtosis**: High kurtosis in several features (e.g., V8, V20, V21, V28) suggests heavy tails (frequent outliers).

### Amount
- **Description**: Transaction amount.
- **Statistics**:
    - Mean: 88.35
    - Median: 22.00
    - Max: 25,691.20
- **Skewness**: 16.9777 (Highly right-skewed)
- **Kurtosis**: 845.0926 (Extremely heavy tail)
- **Outliers**: 31,904 (11.20%)
- **Insight**: The vast majority of transactions are small, but there are significant high-value outliers.

### Class
- **Description**: Target variable (0 = Legitimate, 1 = Fraud).
- **Statistics**:
    - Mean: 0.0017
- **Distribution**:
    - 0: 284,315 (99.83%)
    - 1: 492 (0.17%)
- **Insight**: Highly imbalanced dataset.

## Detailed Statistics
(Refer to `univariate_output.txt` for raw statistical tables for every column)
