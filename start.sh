#!/bin/bash

# Energy Forecasting Web Application Startup Script

echo "🚀 Starting Energy Forecasting Web Application..."

# Check if Python is installed
if ! command -v python3 &> /dev/null; then
    echo "❌ Python 3 is not installed. Please install Python 3.9+ first."
    exit 1
fi

# Check if pip is installed
if ! command -v pip3 &> /dev/null; then
    echo "❌ pip3 is not installed. Please install pip first."
    exit 1
fi

# Check if datasets exist
if [ ! -d "datasets" ]; then
    echo "⚠️  Datasets directory not found. Creating it..."
    mkdir -p datasets
fi

# Check if required data files exist
if [ ! -f "datasets/feature_engineered_energy_data.csv" ]; then
    echo "⚠️  Feature engineered data not found."
    echo "   Please run the Jupyter notebook first to generate the required datasets."
    echo "   Or ensure the following files exist:"
    echo "   - datasets/feature_engineered_energy_data.csv"
    echo "   - datasets/model_results_by_country.csv"
    echo "   - datasets/best_model_per_country.csv"
    echo ""
    echo "   You can run the notebook with: jupyter notebook script.ipynb"
    exit 1
fi

# Install dependencies if requirements.txt exists
if [ -f "requirements.txt" ]; then
    echo "📦 Installing Python dependencies..."
    pip3 install -r requirements.txt
    if [ $? -ne 0 ]; then
        echo "❌ Failed to install dependencies. Please check your Python environment."
        exit 1
    fi
else
    echo "⚠️  requirements.txt not found. Installing basic dependencies..."
    pip3 install flask flask-cors pandas numpy scikit-learn statsmodels plotly
fi

# Create templates directory if it doesn't exist
if [ ! -d "templates" ]; then
    echo "📁 Creating templates directory..."
    mkdir -p templates
fi

# Create static directory if it doesn't exist
if [ ! -d "static" ]; then
    echo "📁 Creating static directory..."
    mkdir -p static
fi

# Check if app.py exists
if [ ! -f "app.py" ]; then
    echo "❌ app.py not found. Please ensure the Flask application file exists."
    exit 1
fi

# Set environment variables
export FLASK_APP=app.py
export FLASK_ENV=development

echo "✅ All checks passed!"
echo ""
echo "🌐 Starting the web application..."
echo "   Dashboard: http://localhost:5000"
echo "   API Health: http://localhost:5000/api/health"
echo ""
echo "Press Ctrl+C to stop the server"
echo ""

# Start the Flask application
python3 app.py
