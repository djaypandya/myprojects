import requests
import subprocess
import sys
import datetime

BASE_URL = "https://fantasy.premierleague.com/api"

def fetch_bootstrap_static():
    url = f"{BASE_URL}/bootstrap-static/"
    response = requests.get(url)
    response.raise_for_status()
    return response.json()

def check_and_update():
    print(f"[{datetime.datetime.now()}] Checking for updates...")
    
    try:
        data = fetch_bootstrap_static()
    except Exception as e:
        print(f"Error fetching data: {e}")
        sys.exit(1)

    current_event = None
    for event in data['events']:
        if event['is_current']:
            current_event = event
            break
    
    if not current_event:
        print("No current event found.")
        return

    print(f"Current Gameweek: {current_event['name']} (ID: {current_event['id']})")
    print(f"Status: Finished={current_event['finished']}, Data Checked={current_event['data_checked']}")

    if current_event['finished'] and current_event['data_checked']:
        print("Gameweek is complete and data is checked. Starting ingestion...")
        try:
            # Run ingest.py
            # You might want to pass specific arguments if needed, but defaults work for current season
            subprocess.run(["python3", "ingest.py"], check=True)
            print("Update completed successfully.")
        except subprocess.CalledProcessError as e:
            print(f"Error running ingestion: {e}")
            sys.exit(1)
    else:
        print("Gameweek not yet complete or data not checked. No update needed.")

if __name__ == "__main__":
    check_and_update()
