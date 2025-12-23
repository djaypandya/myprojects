
#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Refactored Understat Script using `understat` library and Dynamic Mapping
"""

import asyncio
import aiohttp
import ssl
import certifi
import pandas as pd
import json
import time
import warnings
import os
from sqlalchemy import create_engine
from understat import Understat
from tenacity import retry, stop_after_attempt, wait_exponential

# Import helper
import build_team_mapping
import config

# =============================================================================
# GLOBAL CONFIG
# =============================================================================

start_time = time.perf_counter()
warnings.simplefilter(action='ignore', category=FutureWarning)

LEAGUE = 'EPL'
# Target Season Year (Integer) -> Used for Understat Logic
TARGET_SEASON_YEAR = int(config.DEFAULT_SEASON_ID.split('-')[0]) 
# Season String for Output/Filenames -> '2025_2026' or '2025' depending on user preference?
# User asked: "ensure that it maps SEASON = '2025' in understat to 2025/26 for FPL"
# The Output filenames in the previous script used 'SEASON' variable directly.
# I will create a display version for FPL context if needed, but the filenames usually used the single year.
# I will keep SEASON as string '2025' for Understat queries.

SEASON = str(TARGET_SEASON_YEAR)

# =============================================================================
# HELPERS
# =============================================================================

# Retry decorator for robust network calls
@retry(stop=stop_after_attempt(3), wait=wait_exponential(multiplier=1, min=2, max=10))
async def get_team_players_safe(understat, team_name, season):
    # Understat library expects the "Title" e.g. "Arsenal", "Manchester United"
    # We map our keys to that if possible, or pass directly if they match
    # Mapping logic:
    map_to_lib = {
        'Manchester_City': 'Manchester City',
        'Manchester_United': 'Manchester United', 
        'West_Ham': 'West Ham',
        'Newcastle_United': 'Newcastle United',
        'Aston_Villa': 'Aston Villa',
        'Leeds_United': 'Leeds',
        'Wolverhampton_Wanderers': 'Wolverhampton Wanderers',
        'Nottingham_Forest': 'Nottingham Forest',
        'Crystal_Palace': 'Crystal Palace',
        'Brighton': 'Brighton',  
        'Tottenham': 'Tottenham',
        'Sheffield_United': 'Sheffield United',
        'Luton_Town': 'Luton',
        'Ipswich_Town': 'Ipswich',
        'Leicester_City': 'Leicester'
    }
    
    query_name = map_to_lib.get(team_name, team_name.replace("_", " "))
    return await understat.get_team_players(query_name, season)

@retry(stop=stop_after_attempt(3), wait=wait_exponential(multiplier=1, min=2, max=10))
async def get_player_matches_safe(sem, understat, player_id, season):
    async with sem:
        return await understat.get_player_matches(player_id, season=season)

@retry(stop=stop_after_attempt(3), wait=wait_exponential(multiplier=1, min=2, max=10))
async def get_teams_safe(understat, league, season):
    return await understat.get_teams(league, season)

def get_fpl_season_string(year):
    """
    Maps 2025 -> "2025/26" for FPL compatibility.
    """
    next_year = str(year + 1)[-2:]
    return f"{year}/{next_year}"

# =============================================================================
# MAIN FETCH LOGIC
# =============================================================================

async def fetch_players_and_team_data(session, teams_list, understat_team_code_dict):
    understat = Understat(session)
    # Semaphore to limit concurrent requests
    sem = asyncio.Semaphore(10) 
    
    master_player_data_list = []
    team_players_list = []
    
    # Calculate FPL Season String
    fpl_season_str = get_fpl_season_string(int(SEASON))
    print(f"Targeting FPL Season: {fpl_season_str} (Understat {SEASON})")
    
    print(f"Fetching data for {len(teams_list)} teams...")
    
    for TEAM in teams_list:
        try:
            # 1. Get Squad Data
            players = await get_team_players_safe(understat, TEAM, SEASON)
            if not players:
                print(f"Warning: No players found for {TEAM}")
                continue
                
            for p in players:
                p['team_title'] = TEAM
                p['fpl_season'] = fpl_season_str # Add future proof column
            
            team_players_list.extend(players)
            
            team_title_for_players = TEAM 
            understat_team_code = understat_team_code_dict[TEAM]
            
            # 2. Get Individual Player Data (Parallelized)
            print(f"Processing {TEAM} ({len(players)} players)...")
            
            tasks = []
            for p in players:
                pid = p['id']
                tasks.append(get_player_matches_safe(sem, understat, pid, SEASON))
            
            results = await asyncio.gather(*tasks, return_exceptions=True)
            
            for i, res in enumerate(results):
                if isinstance(res, Exception):
                    print(f"Error fetching player {players[i]['player_name']}: {res}")
                    continue
                    
                matches = res
                if matches:
                    for m in matches:
                        m['player_id'] = players[i]['id']
                        m['team_name'] = team_title_for_players
                        m['understat_team_code'] = understat_team_code
                        m['fpl_season'] = fpl_season_str # Add future proof column
                        master_player_data_list.append(m)
                                
        except Exception as e:
            print(f"Error processing team {TEAM}: {e}")

    # Convert to DataFrames
    team_players = pd.DataFrame(team_players_list)
    master_player_data = pd.DataFrame(master_player_data_list)
    
    return team_players, master_player_data

async def create_teams_master(session):
    understat = Understat(session)
    
    teams_data = await get_teams_safe(understat, "epl", SEASON)
    
    all_history_list = []
    fpl_season_str = get_fpl_season_string(int(SEASON))
    
    for team in teams_data:
        # team is dict: {'id': '88', 'title': 'Manchester City', 'history': [...]}
        tid = team['id']
        title = team['title']
        history = team.get('history', [])
        
        for i, h in enumerate(history):
            h['understat_team_code'] = tid
            h['team_name'] = title
            h['gameweek'] = i + 1
            h['fpl_season'] = fpl_season_str
            all_history_list.append(h)
    
    understat_teams_master = pd.json_normalize(all_history_list)
    return understat_teams_master


# =============================================================================
# MAIN FUNC
# =============================================================================

async def main():
    # 0. Dynamic Team Mapping
    print("Generating Dynamic Team Mapping...")
    try:
        # Assuming bootstrap_sample.json is the source
        TEAMS_CODE_DICT = await build_team_mapping.generate_teams_dict("bootstrap_sample.json", int(SEASON))
        print("Mapping Generated Successfully.")
        print(json.dumps(TEAMS_CODE_DICT, indent=2))
    except Exception as e:
        print(f"CRITICAL ERROR: Failed to generate team mapping: {e}")
        return

    TEAMS = list(TEAMS_CODE_DICT.keys())
    UNDERSTAT_TEAM_CODE_DICT = {team: values['UNDERSTAT_CODE'] for team, values in TEAMS_CODE_DICT.items()}

    # SSL Context
    ssl_context = ssl.create_default_context(cafile=certifi.where())
    connector = aiohttp.TCPConnector(ssl=ssl_context)
    
    async with aiohttp.ClientSession(connector=connector) as session:
        
        # 1. Fetch Data
        team_players, master_player_data = await fetch_players_and_team_data(session, TEAMS, UNDERSTAT_TEAM_CODE_DICT)
        understat_teams_master = await create_teams_master(session)
        
        end_time = time.perf_counter()
        print(f"Total time: {round((end_time-start_time)/60,2)} minutes")
        
        # 2. Database Export
        db_path = f'sqlite:///{config.DB_PATH}'
        engine = create_engine(db_path)
        
        print(f"Exporting to database: {db_path}...")
        
        try:
            # We rely on Pandas type inference which is generally sufficient.
            # Use 'replace' to overwrite tables for that season if they exist, or 'append' if accumulating?
            # User workflow seems to imply full refresh of these tables.
            # Original script used 'replace' in commented code.
            
            master_player_table = f"{SEASON}_understat_player_master"
            team_players_table = f"{SEASON}_understat_team_squad_master"
            teams_table = f"{SEASON}_understat_teams_master"
            
            master_player_data.to_sql(master_player_table, engine, if_exists='replace', index=False)
            print(f"Saved table: {master_player_table}")
            
            team_players.to_sql(team_players_table, engine, if_exists='replace', index=False)
            print(f"Saved table: {team_players_table}")
            
            understat_teams_master.to_sql(teams_table, engine, if_exists='replace', index=False)
            print(f"Saved table: {teams_table}")
            
        except Exception as e:
            print(f"Error saving to database: {e}")
            
        # 3. Clean up engine
        engine.dispose()


if __name__ == "__main__":
    try:
        asyncio.run(main())
    except KeyboardInterrupt:
        print("Script cancelled by user.")
