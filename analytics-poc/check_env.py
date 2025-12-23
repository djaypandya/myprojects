import sys
import subprocess

def install(package):
    subprocess.check_call([sys.executable, "-m", "pip", "install", package])

def check_postgres():
    try:
        import psycopg2
    except ImportError:
        print("Installing psycopg2-binary...")
        install("psycopg2-binary")
        import psycopg2

    try:
        # Try default connection
        conn = psycopg2.connect(dbname="postgres", user="djay", host="localhost")
        print("SUCCESS: Connected to PostgreSQL as 'djay'.")
        conn.close()
        return True
    except Exception as e:
        print(f"WARNING: Could not connect as 'djay': {e}")
        try:
            conn = psycopg2.connect(dbname="postgres", user="postgres", host="localhost")
            print("SUCCESS: Connected to PostgreSQL as 'postgres'.")
            conn.close()
            return True
        except Exception as e2:
            print(f"FAILURE: Could not connect to PostgreSQL: {e2}")
            return False

if __name__ == "__main__":
    print("Checking Python dependencies...")
    try:
        import pandas
        print("pandas: OK")
    except ImportError:
        print("Installing pandas...")
        install("pandas")
    
    try:
        import sqlalchemy
        print("sqlalchemy: OK")
    except ImportError:
        print("Installing sqlalchemy...")
        install("sqlalchemy")

    print("\nChecking PostgreSQL server...")
    pg_status = check_postgres()
    
    if not pg_status:
        sys.exit(1)
