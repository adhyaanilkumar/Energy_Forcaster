# Energy Forecasting Web Application

A modern, interactive web application for energy consumption forecasting with REST API endpoints and real-time data visualization.

## 🚀 Features

### Backend API
- **RESTful API** built with Flask
- **Real-time forecasting** using multiple ML models (ARIMA, ETS, Random Forest, XGBoost)
- **Country-specific analysis** with 294+ countries supported
- **Model performance metrics** (RMSE, MAPE)
- **Correlation analysis** between energy consumption and economic factors
- **Global trends visualization**

### Frontend Dashboard
- **Interactive web interface** with modern UI/UX
- **Real-time charts** using Plotly.js
- **Country selection** with dynamic forecasting
- **Model comparison** and performance metrics
- **Responsive design** for all devices
- **Confidence intervals** for forecast uncertainty

### Deployment
- **Docker containerization** for easy deployment
- **Docker Compose** for multi-service setup
- **Nginx reverse proxy** for production
- **Health checks** and monitoring

## 📊 API Endpoints

### Core Endpoints
- `GET /` - Main dashboard
- `GET /api/health` - Health check
- `GET /api/countries` - List all countries
- `GET /api/countries/{country}/data` - Get country data
- `POST /api/countries/{country}/forecast` - Generate forecast

### Analytics Endpoints
- `GET /api/models/performance` - Model performance metrics
- `GET /api/models/performance/{country}` - Country-specific performance
- `GET /api/analytics/correlation` - Correlation analysis
- `GET /api/analytics/trends` - Global energy trends

## 🛠️ Installation & Setup

### Prerequisites
- Python 3.9+
- Docker & Docker Compose (optional)
- Git

### Method 1: Direct Python Installation

1. **Clone the repository**
```bash
git clone <repository-url>
cd energy-forcaster
```

2. **Install dependencies**
```bash
pip install -r requirements.txt
```

3. **Run the application**
```bash
python app.py
```

4. **Access the dashboard**
Open your browser and go to: `http://localhost:5000`

### Method 2: Docker Deployment

1. **Clone and navigate to the repository**
```bash
git clone <repository-url>
cd energy-forcaster
```

2. **Build and run with Docker Compose**
```bash
docker-compose up --build
```

3. **Access the application**
- With Nginx: `http://localhost:80`
- Direct Flask: `http://localhost:5000`

## 📈 Usage Examples

### Generate a Forecast via API

```bash
# Generate 5-year forecast for United States using best model
curl -X POST "http://localhost:5000/api/countries/United%20States/forecast" \
  -H "Content-Type: application/json" \
  -d '{
    "model": "best",
    "horizon": 60,
    "energy_type": "primary_energy_consumption"
  }'
```

### Get Country Data

```bash
# Get historical data for Germany
curl "http://localhost:5000/api/countries/Germany/data"
```

### Get Model Performance

```bash
# Get overall model performance metrics
curl "http://localhost:5000/api/models/performance"
```

## 🎯 Forecasting Models

The application supports multiple forecasting models:

1. **ARIMA (2,1,2)** - Time series analysis
2. **Exponential Smoothing** - Trend-based forecasting
3. **Random Forest** - Ensemble learning with lagged features
4. **XGBoost** - Gradient boosting (with fallback to scikit-learn)

### Model Selection
- **Best Model**: Automatically selects the best-performing model for each country
- **Manual Selection**: Choose specific models for comparison
- **Performance Metrics**: RMSE and MAPE for model evaluation

## 📊 Data Sources

The application uses processed datasets including:
- **World Bank** energy, GDP, and population data
- **Kaggle** energy consumption and weather data
- **Feature-engineered** datasets with lagged variables and seasonal indicators

## 🔧 Configuration

### Environment Variables
- `FLASK_ENV`: Set to `production` for production deployment
- `FLASK_APP`: Application entry point (default: `app.py`)

### API Parameters
- **Horizon**: Forecast period (1-60 months)
- **Model**: Forecasting algorithm selection
- **Energy Type**: Type of energy consumption to forecast

## 🚀 Production Deployment

### Using Docker Compose (Recommended)

1. **Production configuration**
```bash
# Set production environment
export FLASK_ENV=production

# Run with Docker Compose
docker-compose up -d
```

2. **Scale the application**
```bash
# Scale to multiple instances
docker-compose up --scale energy-forecaster=3
```

### Manual Production Setup

1. **Install production dependencies**
```bash
pip install gunicorn
```

2. **Run with Gunicorn**
```bash
gunicorn -w 4 -b 0.0.0.0:5000 app:app
```

## 📱 Frontend Features

### Interactive Dashboard
- **Country Selection**: Dropdown with 294+ countries
- **Model Selection**: Choose from 4 different forecasting models
- **Forecast Horizon**: 1-10 year forecasting periods
- **Energy Types**: Primary, fossil fuels, renewables

### Visualizations
- **Time Series Charts**: Historical data with forecast overlays
- **Confidence Intervals**: 80% and 95% prediction intervals
- **Global Trends**: Worldwide energy consumption patterns
- **Correlation Analysis**: Energy vs. economic factors

### Real-time Updates
- **Dynamic Loading**: Real-time data fetching
- **Error Handling**: Graceful error messages
- **Responsive Design**: Works on desktop, tablet, and mobile

## 🔍 API Documentation

### Request/Response Examples

#### Forecast Request
```json
{
  "model": "best",
  "horizon": 60,
  "energy_type": "primary_energy_consumption"
}
```

#### Forecast Response
```json
{
  "country": "United States",
  "model_used": "ets",
  "energy_type": "primary_energy_consumption",
  "forecast": [
    {
      "year": 2024,
      "forecast": 2500.5,
      "lower_80": 2400.2,
      "upper_80": 2600.8,
      "lower_95": 2350.1,
      "upper_95": 2650.9
    }
  ],
  "historical_data": {
    "years": [2000, 2001, 2002],
    "values": [2000.1, 2050.3, 2100.7]
  }
}
```

## 🛡️ Security & Performance

### Security Features
- **CORS Configuration**: Proper cross-origin resource sharing
- **Input Validation**: Parameter validation and sanitization
- **Error Handling**: Secure error messages without sensitive data

### Performance Optimizations
- **Data Caching**: In-memory caching for frequently accessed data
- **Lazy Loading**: Load data only when needed
- **Efficient Queries**: Optimized pandas operations
- **Docker Optimization**: Multi-stage builds and caching

## 🐛 Troubleshooting

### Common Issues

1. **Port Already in Use**
```bash
# Kill process using port 5000
lsof -ti:5000 | xargs kill -9
```

2. **Docker Build Issues**
```bash
# Clean Docker cache
docker system prune -a
docker-compose build --no-cache
```

3. **Missing Dependencies**
```bash
# Reinstall requirements
pip install -r requirements.txt --force-reinstall
```

### Logs and Debugging

```bash
# View application logs
docker-compose logs -f energy-forecaster

# Debug mode
export FLASK_DEBUG=1
python app.py
```

## 📈 Performance Metrics

### Model Accuracy Goals
- **RMSE**: < 10% of mean energy consumption
- **MAPE**: < 15% for all countries
- **Coverage**: 294+ countries supported

### API Performance
- **Response Time**: < 2 seconds for forecasts
- **Concurrent Users**: Supports multiple simultaneous requests
- **Data Processing**: Handles large datasets efficiently

## 🤝 Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests if applicable
5. Submit a pull request

## 📄 License

This project is licensed under the MIT License - see the LICENSE file for details.

## 🆘 Support

For support and questions:
- Create an issue in the repository
- Check the troubleshooting section
- Review the API documentation

---

**Energy Forecasting Web Application** - Making energy consumption prediction accessible and interactive for everyone.
