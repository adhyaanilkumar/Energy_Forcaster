"""
Energy Forecasting Web Application
A Flask-based REST API for energy consumption forecasting and data visualization
"""

import os
import json
import numpy as np
import pandas as pd
from flask import Flask, request, jsonify, render_template
from flask_cors import CORS
import warnings
from datetime import datetime, timedelta
import joblib
from sklearn.ensemble import RandomForestRegressor, GradientBoostingRegressor
from sklearn.metrics import mean_squared_error, mean_absolute_percentage_error
from statsmodels.tsa.arima.model import ARIMA
from statsmodels.tsa.holtwinters import ExponentialSmoothing
import time

# Import database module
try:
    from database import db, init_database_from_csv
    DB_AVAILABLE = True
except ImportError:
    DB_AVAILABLE = False
    print("Database module not available, using CSV-only mode")

# Suppress warnings
warnings.filterwarnings("ignore")

# Initialize Flask app
app = Flask(__name__)
CORS(app)

# Configuration
DATASET_FOLDER = "datasets"
FEATURE_DATA_PATH = os.path.join(DATASET_FOLDER, "feature_engineered_energy_data.csv")
MODEL_RESULTS_PATH = os.path.join(DATASET_FOLDER, "model_results_by_country.csv")
BEST_MODELS_PATH = os.path.join(DATASET_FOLDER, "best_model_per_country.csv")

# Global variables for caching
df_cache = None
model_results_cache = None
best_models_cache = None

# Initialize database if available
if DB_AVAILABLE:
    try:
        init_database_from_csv()
        print("Database initialized successfully")
    except Exception as e:
        print(f"Database initialization failed: {e}")
        DB_AVAILABLE = False

def load_data():
    """Load and cache datasets"""
    global df_cache, model_results_cache, best_models_cache
    
    if df_cache is None:
        if os.path.exists(FEATURE_DATA_PATH):
            df_cache = pd.read_csv(FEATURE_DATA_PATH, low_memory=False)
            df_cache['year'] = pd.to_numeric(df_cache['year'], errors='coerce')
            df_cache = df_cache.sort_values(['country', 'year']).reset_index(drop=True)
        else:
            df_cache = pd.DataFrame()
    
    if model_results_cache is None:
        if os.path.exists(MODEL_RESULTS_PATH):
            model_results_cache = pd.read_csv(MODEL_RESULTS_PATH)
        else:
            model_results_cache = pd.DataFrame()
    
    if best_models_cache is None:
        if os.path.exists(BEST_MODELS_PATH):
            best_models_cache = pd.read_csv(BEST_MODELS_PATH)
        else:
            best_models_cache = pd.DataFrame()
    
    return df_cache, model_results_cache, best_models_cache

def safe_mape(y_true, y_pred, eps=1e-8):
    """Calculate MAPE safely"""
    y_true = np.asarray(y_true, dtype=float)
    y_pred = np.asarray(y_pred, dtype=float)
    denom = np.maximum(np.abs(y_true), eps)
    return np.mean(np.abs((y_true - y_pred) / denom)) * 100.0

def evaluate_model(y_true, y_pred):
    """Evaluate model performance"""
    rmse = float(np.sqrt(mean_squared_error(y_true, y_pred)))
    mape = float(safe_mape(y_true, y_pred))
    return rmse, mape

def make_lag_supervised(series, n_lags=3):
    """Create lagged features for supervised learning"""
    frame = pd.DataFrame({f"lag_{i}": series.shift(i) for i in range(1, n_lags+1)})
    frame['target'] = series
    frame = frame.dropna()
    X = frame.drop(columns=['target'])
    y = frame['target']
    return X, y

def time_split(y_series, test_frac=0.2, min_test=6):
    """Split time series into train/test"""
    n = len(y_series)
    t = max(min_test, int(np.ceil(test_frac * n)))
    if n <= t:
        return None, None
    return y_series.iloc[:-t], y_series.iloc[-t:]

def fit_predict_arima(train, test, order=(2,1,2)):
    """Fit ARIMA model and make predictions"""
    try:
        fit = ARIMA(train, order=order).fit()
        pred = fit.forecast(steps=len(test))
        return np.array(pred)
    except Exception as e:
        raise Exception(f"ARIMA fitting failed: {str(e)}")

def fit_predict_ets(train, test):
    """Fit Exponential Smoothing model and make predictions"""
    try:
        fit = ExponentialSmoothing(train, trend='add', seasonal=None).fit()
        pred = fit.forecast(steps=len(test))
        return np.array(pred)
    except Exception as e:
        raise Exception(f"ETS fitting failed: {str(e)}")

def fit_predict_rf(y, train_len, n_lags=3):
    """Fit Random Forest model and make predictions"""
    try:
        X_all, y_all = make_lag_supervised(y, n_lags=n_lags)
        n_shift = len(y) - len(y_all)
        split_idx = train_len - n_shift
        if split_idx <= 0 or split_idx >= len(y_all):
            raise ValueError("Insufficient data after lagging for RF split.")
        
        X_tr, X_te = X_all.iloc[:split_idx], X_all.iloc[split_idx:]
        y_tr, y_te = y_all.iloc[:split_idx], y_all.iloc[split_idx:]
        
        rf = RandomForestRegressor(n_estimators=300, random_state=42, n_jobs=-1)
        rf.fit(X_tr, y_tr)
        pred = rf.predict(X_te)
        return y_te.values, pred
    except Exception as e:
        raise Exception(f"Random Forest fitting failed: {str(e)}")

def fit_predict_xgb(y, train_len, n_lags=3):
    """Fit XGBoost model and make predictions"""
    try:
        X_all, y_all = make_lag_supervised(y, n_lags=n_lags)
        n_shift = len(y) - len(y_all)
        split_idx = train_len - n_shift
        if split_idx <= 0 or split_idx >= len(y_all):
            raise ValueError("Insufficient data after lagging for XGB split.")
        
        X_tr, X_te = X_all.iloc[:split_idx], X_all.iloc[split_idx:]
        y_tr, y_te = y_all.iloc[:split_idx], y_all.iloc[split_idx:]
        
        try:
            from xgboost import XGBRegressor
            xgb = XGBRegressor(
                n_estimators=500, learning_rate=0.05, max_depth=3,
                subsample=0.8, colsample_bytree=0.8, random_state=42
            )
        except ImportError:
            xgb = GradientBoostingRegressor(
                n_estimators=500, learning_rate=0.05, max_depth=3, random_state=42
            )
        
        xgb.fit(X_tr, y_tr)
        pred = xgb.predict(X_te)
        return y_te.values, pred
    except Exception as e:
        raise Exception(f"XGBoost fitting failed: {str(e)}")

def fit_ets_robust(y_pos):
    """Robust ETS fitting with Box-Cox transformation"""
    if np.nanmin(y_pos) > 0:
        try:
            model = ExponentialSmoothing(
                y_pos, trend="add", seasonal=None,
                initialization_method="estimated",
                use_boxcox="auto"
            )
            return model.fit(optimized=True)
        except (TypeError, Exception):
            pass
    
    model = ExponentialSmoothing(
        y_pos, trend="add", seasonal=None,
        initialization_method="estimated"
    )
    return model.fit(optimized=True)

# API Routes

@app.route('/')
def index():
    """Serve the main dashboard"""
    return render_template('dashboard.html')

@app.route('/forecast')
def forecast():
    """Serve the forecasting page"""
    return render_template('forecast.html')

@app.route('/analytics')
def analytics():
    """Serve the analytics page"""
    return render_template('analytics.html')

@app.route('/models')
def models():
    """Serve the model performance page"""
    return render_template('models.html')

@app.route('/api')
def api_docs():
    """Serve the API documentation page"""
    return render_template('api.html')

@app.route('/api/health')
def health_check():
    """Health check endpoint"""
    return jsonify({
        "status": "healthy",
        "timestamp": datetime.now().isoformat(),
        "version": "1.0.0"
    })

@app.route('/api/countries')
def get_countries():
    """Get list of available countries"""
    start_time = time.time()
    
    # Try database first if available
    if DB_AVAILABLE:
        try:
            countries = db.get_all_countries()
            response_time = time.time() - start_time
            db.log_api_usage('get_countries', response_time=response_time)
            return jsonify({
                "countries": countries,
                "total": len(countries),
                "source": "database"
            })
        except Exception as e:
            print(f"Database error: {e}")
    
    # Fallback to CSV
    df, _, _ = load_data()
    if df.empty:
        return jsonify({"error": "No data available"}), 404
    
    countries = sorted(df['country'].dropna().unique().tolist())
    response_time = time.time() - start_time
    if DB_AVAILABLE:
        db.log_api_usage('get_countries', response_time=response_time)
    
    return jsonify({
        "countries": countries,
        "total": len(countries),
        "source": "csv"
    })

@app.route('/api/countries/<country_name>/data')
def get_country_data(country_name):
    """Get energy consumption data for a specific country"""
    df, _, _ = load_data()
    if df.empty:
        return jsonify({"error": "No data available"}), 404
    
    country_data = df[df['country'] == country_name].copy()
    if country_data.empty:
        return jsonify({"error": f"Country '{country_name}' not found"}), 404
    
    # Get available columns
    energy_columns = [col for col in df.columns if 'energy' in col.lower() and col != 'country']
    other_columns = ['year', 'gdp', 'population', 'average_temperature']
    available_columns = [col for col in other_columns if col in df.columns]
    
    # Prepare response data
    response_data = []
    for _, row in country_data.iterrows():
        data_point = {
            "year": int(row['year']) if pd.notna(row['year']) else None
        }
        
        # Add energy consumption data
        for col in energy_columns:
            if col in row and pd.notna(row[col]):
                data_point[col] = float(row[col])
        
        # Add other metrics
        for col in available_columns:
            if col in row and pd.notna(row[col]):
                data_point[col] = float(row[col])
        
        response_data.append(data_point)
    
    return jsonify({
        "country": country_name,
        "data": response_data,
        "available_metrics": energy_columns + available_columns
    })

@app.route('/api/countries/<country_name>/forecast', methods=['POST'])
def generate_forecast(country_name):
    """Generate energy consumption forecast for a country"""
    df, _, _ = load_data()
    if df.empty:
        return jsonify({"error": "No data available"}), 404
    
    # Get request parameters
    data = request.get_json() or {}
    model_type = data.get('model', 'best')  # 'best', 'arima', 'ets', 'rf', 'xgb'
    horizon = data.get('horizon', 12)  # months to forecast
    energy_type = data.get('energy_type', 'primary_energy_consumption')
    
    # Validate parameters
    if horizon > 60:  # Max 5 years
        horizon = 60
    if horizon < 1:
        horizon = 1
    
    # Get country data
    country_data = df[df['country'] == country_name].copy()
    if country_data.empty:
        return jsonify({"error": f"Country '{country_name}' not found"}), 404
    
    # Prepare time series
    if energy_type not in country_data.columns:
        return jsonify({"error": f"Energy type '{energy_type}' not available"}), 400
    
    ts_data = country_data[['year', energy_type]].dropna().sort_values('year')
    if len(ts_data) < 6:
        return jsonify({"error": "Insufficient data for forecasting (need at least 6 data points)"}), 400
    
    y = pd.to_numeric(ts_data[energy_type], errors='coerce').dropna()
    years = ts_data['year'].astype(int).values
    
    if len(y) < 6:
        return jsonify({"error": "Insufficient valid data points"}), 400
    
    try:
        # Determine best model if requested
        if model_type == 'best':
            _, _, best_models = load_data()
            best_model_row = best_models[best_models['country'] == country_name]
            if not best_model_row.empty:
                model_type = best_model_row.iloc[0]['best_model_by_RMSE'].lower()
                if 'exponential' in model_type:
                    model_type = 'ets'
                elif 'random' in model_type:
                    model_type = 'rf'
                elif 'gradient' in model_type or 'xgb' in model_type:
                    model_type = 'xgb'
            else:
                model_type = 'ets'  # Default fallback
        
        # Generate forecast based on model type
        if model_type == 'arima':
            # Shift data if needed for stability
            ymin = float(y.min())
            y_shift = 1 - ymin if ymin <= 0 else 0.0
            y_pos = y + y_shift
            
            fit = ARIMA(y_pos, order=(2,1,2)).fit()
            forecast = fit.forecast(steps=horizon)
            forecast = np.array(forecast) - y_shift
            
        elif model_type == 'ets':
            # Shift data if needed for stability
            ymin = float(y.min())
            y_shift = 1 - ymin if ymin <= 0 else 0.0
            y_pos = y + y_shift
            
            fit = fit_ets_robust(y_pos)
            forecast = fit.forecast(steps=horizon)
            forecast = np.array(forecast) - y_shift
            
        elif model_type in ['rf', 'xgb']:
            # For tree-based models, we need to retrain with all available data
            train_len = len(y)
            if model_type == 'rf':
                _, pred = fit_predict_rf(y, train_len, n_lags=3)
            else:  # xgb
                _, pred = fit_predict_xgb(y, train_len, n_lags=3)
            
            # For simplicity, repeat the last prediction for the horizon
            last_pred = pred[-1] if len(pred) > 0 else y.iloc[-1]
            forecast = np.full(horizon, last_pred)
            
        else:
            return jsonify({"error": f"Unknown model type: {model_type}"}), 400
        
        # Generate forecast dates
        last_year = int(years.max())
        forecast_years = list(range(last_year + 1, last_year + 1 + horizon))
        
        # Calculate confidence intervals (simplified)
        recent_std = float(y.tail(10).std()) if len(y) >= 10 else float(y.std())
        lower_95 = forecast - 1.96 * recent_std
        upper_95 = forecast + 1.96 * recent_std
        lower_80 = forecast - 1.2816 * recent_std
        upper_80 = forecast + 1.2816 * recent_std
        
        # Prepare response
        forecast_data = []
        for i in range(horizon):
            forecast_data.append({
                "year": forecast_years[i],
                "forecast": float(forecast[i]),
                "lower_80": float(lower_80[i]),
                "upper_80": float(upper_80[i]),
                "lower_95": float(lower_95[i]),
                "upper_95": float(upper_95[i])
            })
        
        return jsonify({
            "country": country_name,
            "model_used": model_type,
            "energy_type": energy_type,
            "forecast": forecast_data,
            "historical_data": {
                "years": years.tolist(),
                "values": y.tolist()
            },
            "metadata": {
                "data_points": len(y),
                "last_year": last_year,
                "forecast_horizon": horizon
            }
        })
        
    except Exception as e:
        return jsonify({"error": f"Forecasting failed: {str(e)}"}), 500

@app.route('/api/models/performance')
def get_model_performance():
    """Get model performance metrics across countries"""
    _, model_results, best_models = load_data()
    
    if model_results.empty:
        return jsonify({"error": "No model results available"}), 404
    
    # Clean the data: filter out extreme MAPE values (>1000%) and NaN/Inf values
    # MAPE > 1000% indicates data quality issues or near-zero actual values
    cleaned_results = model_results.copy()
    cleaned_results['MAPE'] = pd.to_numeric(cleaned_results['MAPE'], errors='coerce')
    cleaned_results['RMSE'] = pd.to_numeric(cleaned_results['RMSE'], errors='coerce')
    
    # Filter out unreasonable MAPE values (keep only MAPE <= 1000%)
    cleaned_results = cleaned_results[
        (cleaned_results['MAPE'] <= 1000) | 
        (cleaned_results['MAPE'].isna())
    ].copy()
    
    # Aggregate performance by model
    model_stats = cleaned_results.groupby('model').agg({
        'RMSE': ['mean', 'median', 'std'],
        'MAPE': ['mean', 'median', 'std']
    }).round(3)
    
    # Flatten column names
    model_stats.columns = ['_'.join(col).strip() for col in model_stats.columns]
    model_stats = model_stats.reset_index()
    
    # Replace any remaining NaN/Inf values with None for JSON serialization
    model_stats = model_stats.replace([np.inf, -np.inf, np.nan], None)
    
    # Get best model summary
    best_model_counts = best_models['best_model_by_RMSE'].value_counts().to_dict()
    
    return jsonify({
        "model_performance": model_stats.to_dict('records'),
        "best_model_counts": best_model_counts,
        "total_countries": len(best_models),
        "note": "MAPE values > 1000% have been filtered out as they indicate data quality issues"
    })

@app.route('/api/models/performance/<country_name>')
def get_country_model_performance(country_name):
    """Get model performance for a specific country"""
    _, model_results, _ = load_data()
    
    if model_results.empty:
        return jsonify({"error": "No model results available"}), 404
    
    country_results = model_results[model_results['country'] == country_name].copy()
    if country_results.empty:
        return jsonify({"error": f"No results found for country '{country_name}'"}), 404
    
    # Clean MAPE values: cap at 1000% for display
    country_results['MAPE'] = pd.to_numeric(country_results['MAPE'], errors='coerce')
    country_results['RMSE'] = pd.to_numeric(country_results['RMSE'], errors='coerce')
    
    # Cap MAPE at 1000% for unreasonable values
    country_results.loc[country_results['MAPE'] > 1000, 'MAPE'] = 1000
    country_results = country_results.replace([np.inf, -np.inf, np.nan], None)
    
    return jsonify({
        "country": country_name,
        "model_performance": country_results.to_dict('records')
    })

@app.route('/api/analytics/correlation')
def get_correlation_analysis():
    """Get correlation analysis between energy consumption and other factors"""
    df, _, _ = load_data()
    if df.empty:
        return jsonify({"error": "No data available"}), 404
    
    # Calculate correlations
    energy_col = 'primary_energy_consumption'
    if energy_col not in df.columns:
        return jsonify({"error": "Primary energy consumption data not available"}), 404
    
    correlation_factors = ['gdp', 'population', 'average_temperature']
    available_factors = [col for col in correlation_factors if col in df.columns]
    
    correlations = {}
    for factor in available_factors:
        # Calculate correlation for each country
        country_correlations = []
        for country in df['country'].unique():
            country_data = df[df['country'] == country]
            if len(country_data) >= 5:  # Minimum data points
                corr = country_data[energy_col].corr(country_data[factor])
                if not pd.isna(corr):
                    country_correlations.append(corr)
        
        if country_correlations:
            correlations[factor] = {
                "mean_correlation": float(np.mean(country_correlations)),
                "median_correlation": float(np.median(country_correlations)),
                "countries_with_data": len(country_correlations)
            }
    
    return jsonify({
        "energy_type": energy_col,
        "correlations": correlations
    })

@app.route('/api/analytics/trends')
def get_global_trends():
    """Get global energy consumption trends"""
    df, _, _ = load_data()
    if df.empty:
        return jsonify({"error": "No data available"}), 404
    
    energy_col = 'primary_energy_consumption'
    if energy_col not in df.columns:
        return jsonify({"error": "Primary energy consumption data not available"}), 404
    
    # Global aggregation by year
    global_trends = df.groupby('year')[energy_col].agg(['sum', 'mean', 'count']).reset_index()
    global_trends = global_trends.dropna()
    
    # Top countries by total consumption
    country_totals = df.groupby('country')[energy_col].sum().sort_values(ascending=False).head(10)
    
    return jsonify({
        "global_trends": global_trends.to_dict('records'),
        "top_consumers": {
            "countries": country_totals.index.tolist(),
            "values": country_totals.values.tolist()
        }
    })

@app.route('/api/stats')
def get_application_stats():
    """Get application and database statistics"""
    stats = {
        "timestamp": datetime.now().isoformat(),
        "database_available": DB_AVAILABLE
    }
    
    if DB_AVAILABLE:
        try:
            db_stats = db.get_database_stats()
            usage_stats = db.get_usage_stats()
            stats.update({
                "database_stats": db_stats,
                "usage_stats": usage_stats
            })
        except Exception as e:
            stats["database_error"] = str(e)
    
    # CSV data stats
    df, model_results, best_models = load_data()
    stats["csv_stats"] = {
        "energy_data_records": len(df),
        "model_results_records": len(model_results),
        "best_models_records": len(best_models),
        "countries_count": len(df['country'].unique()) if not df.empty else 0
    }
    
    return jsonify(stats)

if __name__ == '__main__':
    # Create templates directory if it doesn't exist
    os.makedirs('templates', exist_ok=True)
    os.makedirs('static', exist_ok=True)
    
    # Load data on startup
    load_data()
    
    # Run the app
    app.run(debug=True, host='0.0.0.0', port=5000)
