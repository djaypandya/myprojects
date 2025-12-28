"""
FPL API Interaction Module.
Aligns with AGENTS.md Rule 3 (Abstraction) and Rule 4 (No Silent Failure).
"""
import requests
import time
import logging
from config import FPL_BASE_URL, USER_AGENT, API_TIMEOUT_SECONDS

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

HEADERS = {"User-Agent": USER_AGENT}

def _make_request(endpoint, retries=3, backoff_factor=1):
    """
    Internal helper to make requests with retries and defined timeout.
    
    Args:
        endpoint (str): The API endpoint path (e.g., "bootstrap-static/").
        retries (int): Number of retries on failure.
        backoff_factor (int): Multiplier for sleep time between retries.
        
    Returns:
        dict: Parsed JSON response.
        
    Raises:
        requests.exceptions.RequestException: If the request fails after retries.
    """
    url = f"{FPL_BASE_URL}/{endpoint}"
    attempt = 0
    
    while attempt <= retries:
        try:
            response = requests.get(url, headers=HEADERS, timeout=API_TIMEOUT_SECONDS)
            response.raise_for_status()
            return response.json()
        except requests.exceptions.RequestException as e:
            attempt += 1
            if attempt > retries:
                logger.error(f"Failed to fetch {url} after {retries} attempts: {e}")
                raise e # Fail loudly (Rule 4)
            
            wait_time = backoff_factor * (2 ** (attempt - 1))
            logger.warning(f"Request to {url} failed. Retrying in {wait_time}s... Error: {e}")
            time.sleep(wait_time)

def fetch_bootstrap_static():
    """Fetch general FPL data."""
    return _make_request("bootstrap-static/")

def fetch_entry(entry_id):
    """Fetch manager details."""
    return _make_request(f"entry/{entry_id}/")

def fetch_entry_history(entry_id):
    """Fetch entry history."""
    return _make_request(f"entry/{entry_id}/history/")

def fetch_entry_picks(entry_id, gw):
    """Fetch picks for a specific entry and gameweek."""
    return _make_request(f"entry/{entry_id}/event/{gw}/picks/")

def fetch_league_standings(league_id, page_new_entries=1, page_standings=1):
    """Fetch classic league standings."""
    # Note: FPL API Pagination logic might differ, usually ?page_new_entries=1&page_standings=1
    return _make_request(f"leagues-classic/{league_id}/standings/?page_new_entries={page_new_entries}&page_standings={page_standings}")

def fetch_fixtures(event=None):
    """Fetch fixtures. If event is provided, fetches specifically for that event if supported, else all."""
    # FPL API 'fixtures/' returns all. 'fixtures/?event=X' returns for GW.
    endpoint = "fixtures/"
    if event:
        endpoint += f"?event={event}"
    return _make_request(endpoint)

def fetch_element_summary(element_id):
    """Fetch all stats and history for a specific player."""
    return _make_request(f"element-summary/{element_id}/")

def fetch_event_live(event_id):
    """Fetch live points data for an event/gameweek."""
    return _make_request(f"event/{event_id}/live/")
