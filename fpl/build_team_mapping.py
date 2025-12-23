
import json
import asyncio
import aiohttp
import ssl
import certifi
from understat import Understat

# Mapping from FPL JSON name to User Key
# JSON Name -> User Key
NAME_MAP = {
    "Man City": "Manchester_City",
    "Chelsea": "Chelsea",
    "West Ham": "West_Ham",
    "Spurs": "Tottenham",
    "Liverpool": "Liverpool",
    "Man Utd": "Manchester_United",
    "Everton": "Everton",
    "Burnley": "Burnley",
    "Sunderland": "Sunderland",
    "Crystal Palace": "Crystal_Palace",
    "Brentford": "Brentford",
    "Arsenal": "Arsenal",
    "Leeds": "Leeds_United",
    "Newcastle": "Newcastle_United",
    "Aston Villa": "Aston_Villa",
    "Fulham": "Fulham",
    "Wolves": "Wolverhampton_Wanderers",
    "Bournemouth": "Bournemouth",
    "Nott'm Forest": "Nottingham_Forest",
    "Brighton": "Brighton",
    "Sheff Utd": "Sheffield_United",
    "Luton": "Luton_Town",
    "Ipswich": "Ipswich_Town",
    "Southampton": "Southampton",
    "Leicester": "Leicester_City"
}

# Manual map for differences between User Key and Understat Title
# User Key -> Understat Title
USER_TO_UNDERSTAT = {
    "Manchester_City": "Manchester City",
    "Chelsea": "Chelsea",
    "West_Ham": "West Ham",
    "Tottenham": "Tottenham",
    "Liverpool": "Liverpool",
    "Manchester_United": "Manchester United",
    "Everton": "Everton",
    "Burnley": "Burnley",
    "Sunderland": "Sunderland",
    "Crystal_Palace": "Crystal Palace",
    "Brentford": "Brentford",
    "Arsenal": "Arsenal",
    "Leeds_United": "Leeds",
    "Newcastle_United": "Newcastle United",
    "Aston_Villa": "Aston Villa",
    "Fulham": "Fulham",
    "Wolverhampton_Wanderers": "Wolverhampton Wanderers",
    "Bournemouth": "Bournemouth",
    "Nottingham_Forest": "Nottingham Forest",
    "Brighton": "Brighton",
    "Sheffield_United": "Sheffield United",
    "Luton_Town": "Luton",
    "Ipswich_Town": "Ipswich",
    "Southampton": "Southampton",
    "Leicester_City": "Leicester"
}

def load_fpl_codes(filepath):
    """
    Loads FPL codes from the bootstrap-static JSON file.
    """
    with open(filepath, 'r') as f:
        data = json.load(f)
    
    mapping = {}
    for team in data['teams']:
        fpl_name = team['name']
        if fpl_name in NAME_MAP:
            user_key = NAME_MAP[fpl_name]
            mapping[user_key] = {'FPL_CODE': team['code']}
        else:
            # Fallback or log warning?
            # For now, let's try to infer simple names or skip
            pass
            
    return mapping

async def get_understat_teams_data(seasons=[2025, 2024, 2023, 2022]):
    """
    Fetches team data from Understat across multiple seasons to catch relegated/promoted teams.
    """
    ssl_context = ssl.create_default_context(cafile=certifi.where())
    connector = aiohttp.TCPConnector(ssl=ssl_context)
    
    understat_teams = {}
    
    async with aiohttp.ClientSession(connector=connector) as session:
        understat = Understat(session)
        
        for season in seasons:
            try:
                teams = await understat.get_teams("epl", season)
                if not teams:
                    continue
                    
                for team in teams:
                    # team is {'id': '88', 'title': 'Manchester City', ...}
                    title = team['title']
                    uid = team['id']
                    
                    if title not in understat_teams:
                        understat_teams[title] = uid
            except Exception as e:
                # Silently fail for older seasons or connection issues in helper
                # print(f"Debug: Failed to fetch season {season}: {e}")
                pass

    return understat_teams

def match_teams(fpl_map, understat_teams):
    """
    Combines the FPL map with Understat IDs.
    """
    final_dict = {}
    
    for user_key, data in fpl_map.items():
        understat_title = USER_TO_UNDERSTAT.get(user_key)
        understat_code = understat_teams.get(understat_title)
        
        if understat_code:
            data['UNDERSTAT_CODE'] = int(understat_code)
        else:
            # If we can't find it, we leave it None or fill if known fallback
            # But the user wants a clean dict.
            data['UNDERSTAT_CODE'] = None 
            
        final_dict[user_key] = data
        
    return final_dict

async def generate_teams_dict(fpl_json_path, target_season_year):
    """
    Main entry point to generate the complete dictionary.
    target_season_year: e.g. 2025
    """
    # 1. Load FPL
    fpl_map = load_fpl_codes(fpl_json_path)
    
    # 2. Fetch Understat (check target season + a few back for relegated teams)
    seasons_to_check = [target_season_year, target_season_year - 1, target_season_year - 2, 2016] # 2016 for Sunderland if needed
    understat_teams = await get_understat_teams_data(seasons_to_check)
    
    # 3. Merge
    final_dict = match_teams(fpl_map, understat_teams)
    
    # Sort by key
    sorted_dict = dict(sorted(final_dict.items()))
    
    return sorted_dict

if __name__ == "__main__":
    # Test run
    loop = asyncio.new_event_loop()
    asyncio.set_event_loop(loop)
    result = loop.run_until_complete(generate_teams_dict("bootstrap_sample.json", 2025))
    print(json.dumps(result, indent=4))
