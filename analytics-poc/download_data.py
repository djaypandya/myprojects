import os
import zipfile
import pandas as pd
from kaggle.api.kaggle_api_extended import KaggleApi

def download_and_validate():
    print("Authenticating with Kaggle...")
    api = KaggleApi()
    api.authenticate()

    dataset = 'mlg-ulb/creditcardfraud'
    file_name = 'creditcard.csv'
    
    print(f"Downloading {dataset}...")
    # Download to current directory
    api.dataset_download_files(dataset, path='.', unzip=True)
    
    if not os.path.exists(file_name):
        raise FileNotFoundError(f"{file_name} not found after download.")

    print(f"Validating {file_name}...")
    df = pd.read_csv(file_name)
    
    print(f"Shape: {df.shape}")
    print(f"Columns: {list(df.columns)}")
    
    expected_shape = (284807, 31)
    if df.shape != expected_shape:
        print(f"WARNING: Shape {df.shape} does not match expected {expected_shape}")
    else:
        print("SUCCESS: Shape matches expected dimensions.")
        
    # Check for 'Class' column which is critical
    if 'Class' not in df.columns:
        raise ValueError("Critical column 'Class' missing.")
        
    print("Data acquisition complete and validated.")

if __name__ == "__main__":
    download_and_validate()
