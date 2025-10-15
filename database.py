"""
Database integration for Energy Forecasting Web Application
Provides SQLite database support for better data management and caching
"""

import sqlite3
import pandas as pd
import json
import os
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Any
import hashlib

class EnergyForecastDB:
    """SQLite database manager for energy forecasting data"""
    
    def __init__(self, db_path: str = "energy_forecast.db"):
        self.db_path = db_path
        self.init_database()
    
    def init_database(self):
        """Initialize database tables"""
        with sqlite3.connect(self.db_path) as conn:
            cursor = conn.cursor()
            
            # Countries table
            cursor.execute('''
                CREATE TABLE IF NOT EXISTS countries (
                    id INTEGER PRIMARY KEY AUTOINCREMENT,
                    name TEXT UNIQUE NOT NULL,
                    code TEXT,
                    region TEXT,
                    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
                )
            ''')
            
            # Energy data table
            cursor.execute('''
                CREATE TABLE IF NOT EXISTS energy_data (
                    id INTEGER PRIMARY KEY AUTOINCREMENT,
                    country_id INTEGER,
                    year INTEGER,
                    primary_energy_consumption REAL,
                    fossil_fuel_consumption REAL,
                    renewables_consumption REAL,
                    oil_consumption REAL,
                    nuclear_consumption REAL,
                    gdp REAL,
                    population REAL,
                    average_temperature REAL,
                    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                    FOREIGN KEY (country_id) REFERENCES countries (id),
                    UNIQUE(country_id, year)
                )
            ''')
            
            # Model results table
            cursor.execute('''
                CREATE TABLE IF NOT EXISTS model_results (
                    id INTEGER PRIMARY KEY AUTOINCREMENT,
                    country_id INTEGER,
                    model_name TEXT,
                    rmse REAL,
                    mape REAL,
                    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                    FOREIGN KEY (country_id) REFERENCES countries (id),
                    UNIQUE(country_id, model_name)
                )
            ''')
            
            # Forecasts cache table
            cursor.execute('''
                CREATE TABLE IF NOT EXISTS forecast_cache (
                    id INTEGER PRIMARY KEY AUTOINCREMENT,
                    cache_key TEXT UNIQUE,
                    country_id INTEGER,
                    model_name TEXT,
                    energy_type TEXT,
                    horizon INTEGER,
                    forecast_data TEXT,
                    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                    expires_at TIMESTAMP,
                    FOREIGN KEY (country_id) REFERENCES countries (id)
                )
            ''')
            
            # API usage tracking
            cursor.execute('''
                CREATE TABLE IF NOT EXISTS api_usage (
                    id INTEGER PRIMARY KEY AUTOINCREMENT,
                    endpoint TEXT,
                    country TEXT,
                    model TEXT,
                    response_time REAL,
                    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
                )
            ''')
            
            conn.commit()
    
    def get_country_id(self, country_name: str) -> Optional[int]:
        """Get country ID by name"""
        with sqlite3.connect(self.db_path) as conn:
            cursor = conn.cursor()
            cursor.execute("SELECT id FROM countries WHERE name = ?", (country_name,))
            result = cursor.fetchone()
            return result[0] if result else None
    
    def add_country(self, country_name: str, code: str = None, region: str = None) -> int:
        """Add a new country to the database"""
        with sqlite3.connect(self.db_path) as conn:
            cursor = conn.cursor()
            try:
                cursor.execute(
                    "INSERT INTO countries (name, code, region) VALUES (?, ?, ?)",
                    (country_name, code, region)
                )
                return cursor.lastrowid
            except sqlite3.IntegrityError:
                # Country already exists, return existing ID
                return self.get_country_id(country_name)
    
    def import_energy_data(self, df: pd.DataFrame):
        """Import energy data from DataFrame"""
        with sqlite3.connect(self.db_path) as conn:
            cursor = conn.cursor()
            
            for _, row in df.iterrows():
                country_name = row['country']
                country_id = self.add_country(country_name)
                
                # Prepare data for insertion
                data = {
                    'country_id': country_id,
                    'year': int(row['year']) if pd.notna(row['year']) else None,
                    'primary_energy_consumption': row.get('primary_energy_consumption'),
                    'fossil_fuel_consumption': row.get('fossil_fuel_consumption'),
                    'renewables_consumption': row.get('renewables_consumption'),
                    'oil_consumption': row.get('oil_consumption'),
                    'nuclear_consumption': row.get('nuclear_consumption'),
                    'gdp': row.get('gdp'),
                    'population': row.get('population'),
                    'average_temperature': row.get('average_temperature')
                }
                
                # Insert or update energy data
                cursor.execute('''
                    INSERT OR REPLACE INTO energy_data 
                    (country_id, year, primary_energy_consumption, fossil_fuel_consumption,
                     renewables_consumption, oil_consumption, nuclear_consumption,
                     gdp, population, average_temperature)
                    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
                ''', (
                    data['country_id'], data['year'], data['primary_energy_consumption'],
                    data['fossil_fuel_consumption'], data['renewables_consumption'],
                    data['oil_consumption'], data['nuclear_consumption'],
                    data['gdp'], data['population'], data['average_temperature']
                ))
            
            conn.commit()
    
    def import_model_results(self, df: pd.DataFrame):
        """Import model results from DataFrame"""
        with sqlite3.connect(self.db_path) as conn:
            cursor = conn.cursor()
            
            for _, row in df.iterrows():
                country_name = row['country']
                country_id = self.add_country(country_name)
                
                cursor.execute('''
                    INSERT OR REPLACE INTO model_results 
                    (country_id, model_name, rmse, mape)
                    VALUES (?, ?, ?, ?)
                ''', (country_id, row['model'], row['RMSE'], row['MAPE']))
            
            conn.commit()
    
    def get_country_data(self, country_name: str) -> pd.DataFrame:
        """Get energy data for a specific country"""
        with sqlite3.connect(self.db_path) as conn:
            query = '''
                SELECT e.year, e.primary_energy_consumption, e.fossil_fuel_consumption,
                       e.renewables_consumption, e.oil_consumption, e.nuclear_consumption,
                       e.gdp, e.population, e.average_temperature
                FROM energy_data e
                JOIN countries c ON e.country_id = c.id
                WHERE c.name = ?
                ORDER BY e.year
            '''
            return pd.read_sql_query(query, conn, params=(country_name,))
    
    def get_all_countries(self) -> List[str]:
        """Get list of all countries"""
        with sqlite3.connect(self.db_path) as conn:
            cursor = conn.cursor()
            cursor.execute("SELECT name FROM countries ORDER BY name")
            return [row[0] for row in cursor.fetchall()]
    
    def get_model_results(self, country_name: str = None) -> pd.DataFrame:
        """Get model results, optionally filtered by country"""
        with sqlite3.connect(self.db_path) as conn:
            if country_name:
                query = '''
                    SELECT c.name as country, mr.model_name as model, mr.rmse, mr.mape
                    FROM model_results mr
                    JOIN countries c ON mr.country_id = c.id
                    WHERE c.name = ?
                    ORDER BY mr.rmse
                '''
                return pd.read_sql_query(query, conn, params=(country_name,))
            else:
                query = '''
                    SELECT c.name as country, mr.model_name as model, mr.rmse, mr.mape
                    FROM model_results mr
                    JOIN countries c ON mr.country_id = c.id
                    ORDER BY c.name, mr.rmse
                '''
                return pd.read_sql_query(query, conn)
    
    def cache_forecast(self, country_name: str, model_name: str, energy_type: str, 
                      horizon: int, forecast_data: Dict) -> str:
        """Cache forecast results"""
        cache_key = self._generate_cache_key(country_name, model_name, energy_type, horizon)
        country_id = self.add_country(country_name)
        
        with sqlite3.connect(self.db_path) as conn:
            cursor = conn.cursor()
            
            # Set expiration time (1 hour from now)
            expires_at = datetime.now() + timedelta(hours=1)
            
            cursor.execute('''
                INSERT OR REPLACE INTO forecast_cache 
                (cache_key, country_id, model_name, energy_type, horizon, 
                 forecast_data, expires_at)
                VALUES (?, ?, ?, ?, ?, ?, ?)
            ''', (cache_key, country_id, model_name, energy_type, horizon,
                  json.dumps(forecast_data), expires_at))
            
            conn.commit()
        
        return cache_key
    
    def get_cached_forecast(self, country_name: str, model_name: str, 
                           energy_type: str, horizon: int) -> Optional[Dict]:
        """Get cached forecast if available and not expired"""
        cache_key = self._generate_cache_key(country_name, model_name, energy_type, horizon)
        
        with sqlite3.connect(self.db_path) as conn:
            cursor = conn.cursor()
            cursor.execute('''
                SELECT forecast_data FROM forecast_cache 
                WHERE cache_key = ? AND expires_at > ?
            ''', (cache_key, datetime.now()))
            
            result = cursor.fetchone()
            if result:
                return json.loads(result[0])
        
        return None
    
    def _generate_cache_key(self, country_name: str, model_name: str, 
                           energy_type: str, horizon: int) -> str:
        """Generate a unique cache key"""
        key_string = f"{country_name}_{model_name}_{energy_type}_{horizon}"
        return hashlib.md5(key_string.encode()).hexdigest()
    
    def log_api_usage(self, endpoint: str, country: str = None, 
                     model: str = None, response_time: float = None):
        """Log API usage for analytics"""
        with sqlite3.connect(self.db_path) as conn:
            cursor = conn.cursor()
            cursor.execute('''
                INSERT INTO api_usage (endpoint, country, model, response_time)
                VALUES (?, ?, ?, ?)
            ''', (endpoint, country, model, response_time))
            conn.commit()
    
    def get_usage_stats(self) -> Dict[str, Any]:
        """Get API usage statistics"""
        with sqlite3.connect(self.db_path) as conn:
            cursor = conn.cursor()
            
            # Total requests
            cursor.execute("SELECT COUNT(*) FROM api_usage")
            total_requests = cursor.fetchone()[0]
            
            # Most popular endpoints
            cursor.execute('''
                SELECT endpoint, COUNT(*) as count 
                FROM api_usage 
                GROUP BY endpoint 
                ORDER BY count DESC 
                LIMIT 5
            ''')
            popular_endpoints = dict(cursor.fetchall())
            
            # Most requested countries
            cursor.execute('''
                SELECT country, COUNT(*) as count 
                FROM api_usage 
                WHERE country IS NOT NULL
                GROUP BY country 
                ORDER BY count DESC 
                LIMIT 10
            ''')
            popular_countries = dict(cursor.fetchall())
            
            # Average response time
            cursor.execute("SELECT AVG(response_time) FROM api_usage WHERE response_time IS NOT NULL")
            avg_response_time = cursor.fetchone()[0] or 0
            
            return {
                'total_requests': total_requests,
                'popular_endpoints': popular_endpoints,
                'popular_countries': popular_countries,
                'average_response_time': round(avg_response_time, 3)
            }
    
    def cleanup_expired_cache(self):
        """Remove expired cache entries"""
        with sqlite3.connect(self.db_path) as conn:
            cursor = conn.cursor()
            cursor.execute("DELETE FROM forecast_cache WHERE expires_at < ?", (datetime.now(),))
            conn.commit()
    
    def get_database_stats(self) -> Dict[str, Any]:
        """Get database statistics"""
        with sqlite3.connect(self.db_path) as conn:
            cursor = conn.cursor()
            
            stats = {}
            
            # Count records in each table
            tables = ['countries', 'energy_data', 'model_results', 'forecast_cache', 'api_usage']
            for table in tables:
                cursor.execute(f"SELECT COUNT(*) FROM {table}")
                stats[f'{table}_count'] = cursor.fetchone()[0]
            
            # Database size
            cursor.execute("SELECT page_count * page_size as size FROM pragma_page_count(), pragma_page_size()")
            stats['database_size_bytes'] = cursor.fetchone()[0]
            
            return stats

# Global database instance
db = EnergyForecastDB()

def init_database_from_csv():
    """Initialize database from existing CSV files"""
    import os
    
    # Import energy data
    energy_data_path = os.path.join("datasets", "feature_engineered_energy_data.csv")
    if os.path.exists(energy_data_path):
        print("Importing energy data...")
        df = pd.read_csv(energy_data_path)
        db.import_energy_data(df)
        print(f"Imported {len(df)} energy data records")
    
    # Import model results
    model_results_path = os.path.join("datasets", "model_results_by_country.csv")
    if os.path.exists(model_results_path):
        print("Importing model results...")
        df = pd.read_csv(model_results_path)
        db.import_model_results(df)
        print(f"Imported {len(df)} model result records")

if __name__ == "__main__":
    # Initialize database from CSV files
    init_database_from_csv()
    print("Database initialization complete!")
    
    # Print statistics
    stats = db.get_database_stats()
    print(f"Database statistics: {stats}")
