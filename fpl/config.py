"""
Central configuration for FPL Analysis Tool.
Aligns with AGENTS.md Rule 6: No Magic Numbers.
"""

# --- Paths & Database ---
DB_PATH = 'fpl_data.db'
DEFAULT_SEASON_ID = '2025-26'

# --- API Configuration ---
FPL_BASE_URL = "https://fantasy.premierleague.com/api"
USER_AGENT = "FPL-Analysis-Tool/1.0"
API_TIMEOUT_SECONDS = 10

# --- App Settings ---
MIN_FULL_APPEARANCE_RATIO = 0.7
MINUTES_THRESHOLD_DEFAULT = 60
DEFAULT_GAMES_HISTORY = 5  # For consistency check

# --- Scoring / Analysis Constants ---
# Difficulty Adjustments
DIFFICULTY_HOME_ADJ = -0.1
DIFFICULTY_AWAY_ADJ = 0.1

# Color Gradient Thresholds
DIFFICULTY_THRESHOLD_EASY = 2.0
DIFFICULTY_THRESHOLD_MEDIUM = 3.5
DIFFICULTY_THRESHOLD_HARD = 5.0

# Gameweek Snapshot Zones
SNAPSHOT_DIFF_THRESHOLD = 15  # Ownership % for Differentials
SNAPSHOT_TEMP_THRESHOLD = 60  # Ownership % for Template

# Chart Settings
CHART_Y_AXIS_BUFFER_RATIO = 1.2
CHART_Y_AXIS_DTICK = 2
