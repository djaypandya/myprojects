import sqlite3

import argparse
import os
import json

import config
import fpl_api

# Constants
# Imported from config

def get_db_connection():
    conn = sqlite3.connect(config.DB_PATH)
    conn.row_factory = sqlite3.Row
    return conn

def create_tables(conn):
    cursor = conn.cursor()
    
    # Seasons
    cursor.execute('''
    CREATE TABLE IF NOT EXISTS seasons (
        id TEXT PRIMARY KEY,
        name TEXT
    )
    ''')

    # Teams
    cursor.execute('''
    CREATE TABLE IF NOT EXISTS teams (
        id INTEGER,
        season_id TEXT,
        code INTEGER,
        name TEXT,
        short_name TEXT,
        strength INTEGER,
        pulse_id INTEGER,
        strength_overall_home INTEGER,
        strength_overall_away INTEGER,
        strength_attack_home INTEGER,
        strength_attack_away INTEGER,
        strength_defence_home INTEGER,
        strength_defence_away INTEGER,
        played INTEGER,
        win INTEGER,
        loss INTEGER,
        draw INTEGER,
        points INTEGER,
        position INTEGER,
        form TEXT,
        team_division TEXT,
        unavailable BOOLEAN,
        PRIMARY KEY (id, season_id),
        FOREIGN KEY (season_id) REFERENCES seasons (id)
    )
    ''')

    # Element Types (Positions)
    cursor.execute('''
    CREATE TABLE IF NOT EXISTS element_types (
        id INTEGER,
        season_id TEXT,
        singular_name TEXT,
        plural_name_short TEXT,
        PRIMARY KEY (id, season_id),
        FOREIGN KEY (season_id) REFERENCES seasons (id)
    )
    ''')

    # Elements (Players)
    cursor.execute('''
    CREATE TABLE IF NOT EXISTS elements (
        id INTEGER,
        season_id TEXT,
        code INTEGER,
        web_name TEXT,
        first_name TEXT,
        second_name TEXT,
        team_id INTEGER,
        element_type INTEGER,
        now_cost INTEGER,
        total_points INTEGER,
        minutes INTEGER,
        goals_scored INTEGER,
        assists INTEGER,
        clean_sheets INTEGER,
        goals_conceded INTEGER,
        own_goals INTEGER,
        penalties_saved INTEGER,
        penalties_missed INTEGER,
        yellow_cards INTEGER,
        red_cards INTEGER,
        saves INTEGER,
        bonus INTEGER,
        bps INTEGER,
        influence REAL,
        creativity REAL,
        threat REAL,
        ict_index REAL,
        PRIMARY KEY (id, season_id),
        FOREIGN KEY (season_id) REFERENCES seasons (id),
        FOREIGN KEY (team_id, season_id) REFERENCES teams (id, season_id),
        FOREIGN KEY (element_type, season_id) REFERENCES element_types (id, season_id)
    )
    ''')

    # Events (Gameweeks)
    cursor.execute('''
    CREATE TABLE IF NOT EXISTS events (
        id INTEGER,
        season_id TEXT,
        name TEXT,
        deadline_time TEXT,
        finished BOOLEAN,
        is_current BOOLEAN,
        PRIMARY KEY (id, season_id),
        FOREIGN KEY (season_id) REFERENCES seasons (id)
    )
    ''')

    # Fixtures
    cursor.execute('''
    CREATE TABLE IF NOT EXISTS fixtures (
        id INTEGER,
        season_id TEXT,
        event INTEGER,
        team_h INTEGER,
        team_a INTEGER,
        team_h_score INTEGER,
        team_a_score INTEGER,
        kickoff_time TEXT,
        finished BOOLEAN,
        minutes INTEGER,
        code INTEGER,
        finished_provisional BOOLEAN,
        provisional_start_time BOOLEAN,
        started BOOLEAN,
        team_h_difficulty INTEGER,
        team_a_difficulty INTEGER,
        pulse_id INTEGER,
        PRIMARY KEY (id, season_id),
        FOREIGN KEY (season_id) REFERENCES seasons (id),
        FOREIGN KEY (event, season_id) REFERENCES events (id, season_id),
        FOREIGN KEY (team_h, season_id) REFERENCES teams (id, season_id),
        FOREIGN KEY (team_a, season_id) REFERENCES teams (id, season_id)
    )
    ''')

    # Fixture Stats
    cursor.execute('''
    CREATE TABLE IF NOT EXISTS fixture_stats (
        fixture_id INTEGER,
        season_id TEXT,
        identifier TEXT,
        player_id INTEGER,
        value INTEGER,
        home_away TEXT,
        FOREIGN KEY (fixture_id, season_id) REFERENCES fixtures (id, season_id),
        FOREIGN KEY (season_id) REFERENCES seasons (id),
        FOREIGN KEY (player_id, season_id) REFERENCES elements (id, season_id)
    )
    ''')

    # Player History (Gameweek History)
    cursor.execute('''
    CREATE TABLE IF NOT EXISTS player_history (
        element_id INTEGER,
        season_id TEXT,
        fixture_id INTEGER,
        opponent_team INTEGER,
        total_points INTEGER,
        was_home BOOLEAN,
        kickoff_time TEXT,
        team_h_score INTEGER,
        team_a_score INTEGER,
        round INTEGER,
        minutes INTEGER,
        goals_scored INTEGER,
        assists INTEGER,
        clean_sheets INTEGER,
        goals_conceded INTEGER,
        own_goals INTEGER,
        penalties_saved INTEGER,
        penalties_missed INTEGER,
        yellow_cards INTEGER,
        red_cards INTEGER,
        saves INTEGER,
        bonus INTEGER,
        bps INTEGER,
        influence REAL,
        creativity REAL,
        threat REAL,
        ict_index REAL,
        expected_goals REAL,
        expected_goal_involvements REAL,
        value INTEGER,
        transfers_balance INTEGER,
        selected INTEGER,
        transfers_in INTEGER,
        transfers_out INTEGER,
        PRIMARY KEY (element_id, fixture_id, season_id),
        FOREIGN KEY (element_id, season_id) REFERENCES elements (id, season_id),
        FOREIGN KEY (season_id) REFERENCES seasons (id)
    )
    ''')
    
    # Simple migration for existing tables (safe to run if columns exist)
    try:
        cursor.execute("ALTER TABLE player_history ADD COLUMN expected_goals REAL")
    except sqlite3.OperationalError:
        pass # Column likely exists
        
    try:
        cursor.execute("ALTER TABLE player_history ADD COLUMN expected_goal_involvements REAL")
    except sqlite3.OperationalError:
        pass # Column likely exists

    # Player History Past (Previous Seasons)
    cursor.execute('''
    CREATE TABLE IF NOT EXISTS player_history_past (
        element_id INTEGER,
        season_name TEXT,
        element_code INTEGER,
        start_cost INTEGER,
        end_cost INTEGER,
        total_points INTEGER,
        minutes INTEGER,
        goals_scored INTEGER,
        assists INTEGER,
        clean_sheets INTEGER,
        goals_conceded INTEGER,
        own_goals INTEGER,
        penalties_saved INTEGER,
        penalties_missed INTEGER,
        yellow_cards INTEGER,
        red_cards INTEGER,
        saves INTEGER,
        bonus INTEGER,
        bps INTEGER,
        influence REAL,
        creativity REAL,
        threat REAL,
        ict_index REAL,
        PRIMARY KEY (element_id, season_name),
        FOREIGN KEY (element_id) REFERENCES elements (id)
    )
    ''')

    conn.commit()

# Fetch functions replaced by fpl_api

def ingest_data(season_id, season_name):
    print(f"Ingesting data for season: {season_name} ({season_id})")
    
    conn = get_db_connection()
    create_tables(conn)
    cursor = conn.cursor()

    # Insert Season
    cursor.execute('INSERT OR REPLACE INTO seasons (id, name) VALUES (?, ?)', (season_id, season_name))

    # Fetch Data
    bootstrap = fpl_api.fetch_bootstrap_static()
    fixtures = fpl_api.fetch_fixtures()

    # Ingest Teams
    print("Ingesting teams...")
    for team in bootstrap['teams']:
        cursor.execute('''
        INSERT OR REPLACE INTO teams (
            id, season_id, code, name, short_name, strength, pulse_id,
            strength_overall_home, strength_overall_away, strength_attack_home, strength_attack_away,
            strength_defence_home, strength_defence_away, played, win, loss, draw, points,
            position, form, team_division, unavailable
        )
        VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
        ''', (
            team['id'], season_id, team['code'], team['name'], team['short_name'], 
            team['strength'], team.get('pulse_id'),
            team.get('strength_overall_home'), team.get('strength_overall_away'),
            team.get('strength_attack_home'), team.get('strength_attack_away'),
            team.get('strength_defence_home'), team.get('strength_defence_away'),
            team.get('played'), team.get('win'), team.get('loss'), team.get('draw'),
            team.get('points'), team.get('position'), team.get('form'),
            team.get('team_division'), team.get('unavailable')
        ))

    # Ingest Element Types
    print("Ingesting element types...")
    for et in bootstrap['element_types']:
        cursor.execute('''
        INSERT OR REPLACE INTO element_types (id, season_id, singular_name, plural_name_short)
        VALUES (?, ?, ?, ?)
        ''', (et['id'], season_id, et['singular_name'], et['plural_name_short']))

    # Ingest Elements (Players)
    print("Ingesting players...")
    player_ids = []
    for el in bootstrap['elements']:
        player_ids.append(el['id'])
        cursor.execute('''
        INSERT OR REPLACE INTO elements (
            id, season_id, code, web_name, first_name, second_name, team_id, element_type,
            now_cost, total_points, minutes, goals_scored, assists, clean_sheets,
            goals_conceded, own_goals, penalties_saved, penalties_missed, yellow_cards,
            red_cards, saves, bonus, bps, influence, creativity, threat, ict_index
        ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
        ''', (
            el['id'], season_id, el['code'], el['web_name'], el['first_name'], el['second_name'],
            el['team'], el['element_type'], el['now_cost'], el['total_points'], el['minutes'],
            el['goals_scored'], el['assists'], el['clean_sheets'], el['goals_conceded'],
            el['own_goals'], el['penalties_saved'], el['penalties_missed'], el['yellow_cards'],
            el['red_cards'], el['saves'], el['bonus'], el['bps'], el['influence'],
            el['creativity'], el['threat'], el['ict_index']
        ))

    # Ingest Events
    print("Ingesting events...")
    for event in bootstrap['events']:
        cursor.execute('''
        INSERT OR REPLACE INTO events (id, season_id, name, deadline_time, finished, is_current)
        VALUES (?, ?, ?, ?, ?, ?)
        ''', (
            event['id'], season_id, event['name'], event['deadline_time'], 
            event['finished'], event['is_current']
        ))

    # Ingest Fixtures
    print("Ingesting fixtures...")
    # Clear existing stats for this season to avoid duplicates if re-running
    cursor.execute('DELETE FROM fixture_stats WHERE season_id = ?', (season_id,))
    
    for fix in fixtures:
        cursor.execute('''
        INSERT OR REPLACE INTO fixtures (
            id, season_id, event, team_h, team_a, team_h_score, team_a_score,
            kickoff_time, finished, minutes, code, finished_provisional,
            provisional_start_time, started, team_h_difficulty, team_a_difficulty, pulse_id
        ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
        ''', (
            fix['id'], season_id, fix['event'], fix['team_h'], fix['team_a'],
            fix['team_h_score'], fix['team_a_score'], fix['kickoff_time'],
            fix['finished'], fix['minutes'], fix.get('code'), fix.get('finished_provisional'),
            fix.get('provisional_start_time'), fix.get('started'),
            fix.get('team_h_difficulty'), fix.get('team_a_difficulty'), fix.get('pulse_id')
        ))

        # Ingest Fixture Stats
        if fix.get('stats'):
            for stat in fix['stats']:
                identifier = stat['identifier']
                for side in ['h', 'a']:
                    for item in stat[side]:
                        cursor.execute('''
                        INSERT INTO fixture_stats (fixture_id, season_id, identifier, player_id, value, home_away)
                        VALUES (?, ?, ?, ?, ?, ?)
                        ''', (
                            fix['id'], season_id, identifier, item['element'], item['value'], side
                        ))

    # Ingest Player History
    print(f"Ingesting player history for {len(player_ids)} players (this may take a while)...")
    for i, pid in enumerate(player_ids):
        if i % 50 == 0:
            print(f"Processing player {i}/{len(player_ids)}...")
        
        try:
            summary = fpl_api.fetch_element_summary(pid)
            
            # History (Current Season)
            for h in summary['history']:
                cursor.execute('''
                INSERT OR REPLACE INTO player_history (
                    element_id, season_id, fixture_id, opponent_team, total_points, was_home,
                    kickoff_time, team_h_score, team_a_score, round, minutes, goals_scored,
                    assists, clean_sheets, goals_conceded, own_goals, penalties_saved,
                    penalties_missed, yellow_cards, red_cards, saves, bonus, bps,
                    influence, creativity, threat, ict_index, expected_goals, expected_goal_involvements, 
                    value, transfers_balance,
                    selected, transfers_in, transfers_out
                ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
                ''', (
                    pid, season_id, h['fixture'], h['opponent_team'], h['total_points'], h['was_home'],
                    h['kickoff_time'], h['team_h_score'], h['team_a_score'], h['round'], h['minutes'],
                    h['goals_scored'], h['assists'], h['clean_sheets'], h['goals_conceded'],
                    h['own_goals'], h['penalties_saved'], h['penalties_missed'], h['yellow_cards'],
                    h['red_cards'], h['saves'], h['bonus'], h['bps'], h['influence'],
                    h['creativity'], h['threat'], h['ict_index'], 
                    h.get('expected_goals'), h.get('expected_goal_involvements'),
                    h['value'], h['transfers_balance'],
                    h['selected'], h['transfers_in'], h['transfers_out']
                ))
            
            # History Past (Previous Seasons)
            for hp in summary['history_past']:
                cursor.execute('''
                INSERT OR REPLACE INTO player_history_past (
                    element_id, season_name, element_code, start_cost, end_cost, total_points,
                    minutes, goals_scored, assists, clean_sheets, goals_conceded, own_goals,
                    penalties_saved, penalties_missed, yellow_cards, red_cards, saves, bonus,
                    bps, influence, creativity, threat, ict_index
                ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
                ''', (
                    pid, hp['season_name'], hp['element_code'], hp['start_cost'], hp['end_cost'],
                    hp['total_points'], hp['minutes'], hp['goals_scored'], hp['assists'],
                    hp['clean_sheets'], hp['goals_conceded'], hp['own_goals'], hp['penalties_saved'],
                    hp['penalties_missed'], hp['yellow_cards'], hp['red_cards'], hp['saves'],
                    hp['bonus'], hp['bps'], hp['influence'], hp['creativity'], hp['threat'],
                    hp['ict_index']
                ))
                
        except Exception as e:
            print(f"Error fetching history for player {pid}: {e}")

    conn.commit()
    conn.close()
    print("Ingestion complete.")

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Ingest FPL data into SQLite.')
    parser.add_argument('--season-id', type=str, default=config.DEFAULT_SEASON_ID, help='ID for the season (e.g., 2023-24)')
    parser.add_argument('--season-name', type=str, default='2025/26', help='Display name for the season')
    
    args = parser.parse_args()
    
    ingest_data(args.season_id, args.season_name)
