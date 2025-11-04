# Energy Forecasting System

A professional energy consumption forecasting platform built with Next.js, TypeScript, and Python machine learning models.

## Features

- 📊 **Interactive Dashboard** - Real-time visualization of energy consumption trends
- 🔮 **Forecasting** - Generate accurate forecasts using multiple ML models (ARIMA, ETS, Random Forest, XGBoost)
- 📈 **Analytics** - Deep insights into energy consumption patterns and correlations
- 🤖 **Model Performance** - Compare and analyze model performance across countries
- 🔌 **REST API** - Comprehensive API documentation and endpoints

## Tech Stack

### Frontend
- **Next.js 14** - React framework with App Router
- **TypeScript** - Type-safe development
- **Tailwind CSS** - Modern, responsive styling
- **Plotly.js** - Interactive data visualization
- **Lucide React** - Beautiful icon library

### Backend
- **Python 3.9+** - Machine learning backend
- **Flask** - REST API server
- **scikit-learn** - ML models
- **statsmodels** - Time series models (ARIMA, ETS)
- **pandas** - Data processing
- **SQLite** - Database (optional)

## Getting Started

### Prerequisites

- Node.js 18+ and npm/yarn
- Python 3.9+
- pip

### Installation

1. **Clone the repository**
   ```bash
   git clone <repository-url>
   cd energy-forcaster
   ```

2. **Install Python dependencies**
   ```bash
   pip install -r requirements.txt
   ```

3. **Install Node.js dependencies**
   ```bash
   npm install
   # or
   yarn install
   ```

4. **Set up environment variables**
   ```bash
   cp .env.example .env
   # Edit .env with your configuration
   ```

5. **Start the Python backend**
   ```bash
   python app.py
   ```
   The backend will run on `http://localhost:5000`

6. **Start the Next.js frontend** (in a new terminal)
   ```bash
   npm run dev
   # or
   yarn dev
   ```
   The frontend will run on `http://localhost:3000`

### Development

- **Frontend development**: `npm run dev`
- **Type checking**: `npm run type-check`
- **Linting**: `npm run lint`
- **Build**: `npm run build`
- **Production**: `npm start`

## Project Structure

```
energy-forcaster/
├── app/                    # Next.js App Router
│   ├── api/               # API routes (proxy)
│   ├── forecast/          # Forecast page
│   ├── analytics/         # Analytics page
│   ├── models/            # Models page
│   ├── api/               # API docs page
│   ├── layout.tsx         # Root layout
│   ├── page.tsx           # Dashboard
│   └── globals.css        # Global styles
├── components/            # React components
│   ├── Sidebar.tsx        # Navigation sidebar
│   ├── Dashboard.tsx      # Dashboard component
│   ├── Forecast.tsx       # Forecast component
│   ├── Analytics.tsx      # Analytics component
│   ├── Models.tsx         # Models component
│   └── ApiDocs.tsx        # API docs component
├── lib/                   # Utility functions
│   └── api.ts             # API client
├── app.py                 # Python Flask backend
├── database.py            # Database module
├── datasets/              # Data files
├── templates/             # Old HTML templates (to be removed)
└── static/               # Static files
```

## API Endpoints

The application provides a comprehensive REST API:

### Core Endpoints
- `GET /api/health` - Health check
- `GET /api/countries` - List all countries
- `GET /api/countries/{country}/data` - Get country data
- `POST /api/countries/{country}/forecast` - Generate forecast

### Analytics Endpoints
- `GET /api/models/performance` - Model performance metrics
- `GET /api/models/performance/{country}` - Country-specific performance
- `GET /api/analytics/correlation` - Correlation analysis
- `GET /api/analytics/trends` - Global trends

See the API Documentation page in the application for detailed examples.

## Configuration

### Environment Variables

- `PYTHON_API_URL` - Python backend URL (default: `http://localhost:5000`)
- `NEXT_PUBLIC_API_URL` - Public API URL for browser requests (default: `/api/proxy`)
- `NEXT_PUBLIC_APP_URL` - Application URL (default: `http://localhost:3000`)

## Deployment

### Production Build

1. **Build the Next.js application**
   ```bash
   npm run build
   ```

2. **Start production server**
   ```bash
   npm start
   ```

3. **Deploy Python backend**
   - Use gunicorn or similar WSGI server
   - Configure reverse proxy (nginx) if needed

### Docker (Optional)

The project includes Docker configuration for containerized deployment.

## Data

The application uses energy consumption data from multiple sources:
- Kaggle energy datasets
- World Bank data (GDP, population, renewable energy)
- Weather data

All data is stored in the `datasets/` directory and can be loaded into SQLite for better performance.

## Contributing

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

## License

This project is licensed under the MIT License.

## Support

For issues and questions, please open an issue on GitHub.
