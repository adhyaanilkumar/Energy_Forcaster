#!/usr/bin/env python3
"""
Deployment script for Energy Forecasting Web Application
Handles database initialization, dependency installation, and service startup
"""

import os
import sys
import subprocess
import argparse
from pathlib import Path

def run_command(command, description=""):
    """Run a shell command and handle errors"""
    print(f"🔄 {description}")
    try:
        result = subprocess.run(command, shell=True, check=True, capture_output=True, text=True)
        print(f"✅ {description} - Success")
        return True
    except subprocess.CalledProcessError as e:
        print(f"❌ {description} - Failed")
        print(f"Error: {e.stderr}")
        return False

def check_python_version():
    """Check if Python version is compatible"""
    version = sys.version_info
    if version.major < 3 or (version.major == 3 and version.minor < 8):
        print("❌ Python 3.8+ is required")
        return False
    print(f"✅ Python {version.major}.{version.minor}.{version.micro} is compatible")
    return True

def install_dependencies():
    """Install Python dependencies"""
    if not os.path.exists("requirements.txt"):
        print("❌ requirements.txt not found")
        return False
    
    return run_command("pip install -r requirements.txt", "Installing Python dependencies")

def initialize_database():
    """Initialize the database from CSV files"""
    if not os.path.exists("database.py"):
        print("⚠️  Database module not found, skipping database initialization")
        return True
    
    return run_command("python database.py", "Initializing database from CSV files")

def check_data_files():
    """Check if required data files exist"""
    required_files = [
        "datasets/feature_engineered_energy_data.csv",
        "datasets/model_results_by_country.csv",
        "datasets/best_model_per_country.csv"
    ]
    
    missing_files = []
    for file_path in required_files:
        if not os.path.exists(file_path):
            missing_files.append(file_path)
    
    if missing_files:
        print("❌ Missing required data files:")
        for file_path in missing_files:
            print(f"   - {file_path}")
        print("\nPlease run the Jupyter notebook first to generate the required datasets:")
        print("   jupyter notebook script.ipynb")
        return False
    
    print("✅ All required data files found")
    return True

def create_directories():
    """Create necessary directories"""
    directories = ["templates", "static", "datasets", "logs"]
    for directory in directories:
        os.makedirs(directory, exist_ok=True)
    print("✅ Created necessary directories")
    return True

def start_application(mode="development"):
    """Start the Flask application"""
    if mode == "production":
        return run_command("gunicorn -w 4 -b 0.0.0.0:5000 app:app", "Starting production server with Gunicorn")
    else:
        return run_command("python app.py", "Starting development server")

def docker_deploy():
    """Deploy using Docker"""
    if not os.path.exists("Dockerfile"):
        print("❌ Dockerfile not found")
        return False
    
    commands = [
        ("docker-compose down", "Stopping existing containers"),
        ("docker-compose build", "Building Docker images"),
        ("docker-compose up -d", "Starting services")
    ]
    
    for command, description in commands:
        if not run_command(command, description):
            return False
    
    print("✅ Docker deployment completed")
    print("🌐 Application available at: http://localhost:80")
    return True

def main():
    parser = argparse.ArgumentParser(description="Deploy Energy Forecasting Web Application")
    parser.add_argument("--mode", choices=["development", "production", "docker"], 
                       default="development", help="Deployment mode")
    parser.add_argument("--skip-deps", action="store_true", 
                       help="Skip dependency installation")
    parser.add_argument("--skip-db", action="store_true", 
                       help="Skip database initialization")
    parser.add_argument("--skip-data-check", action="store_true", 
                       help="Skip data file validation")
    
    args = parser.parse_args()
    
    print("🚀 Energy Forecasting Web Application Deployment")
    print("=" * 50)
    
    # Check Python version
    if not check_python_version():
        sys.exit(1)
    
    # Create directories
    if not create_directories():
        sys.exit(1)
    
    # Check data files
    if not args.skip_data_check and not check_data_files():
        sys.exit(1)
    
    # Install dependencies
    if not args.skip_deps and not install_dependencies():
        sys.exit(1)
    
    # Initialize database
    if not args.skip_db and not initialize_database():
        print("⚠️  Database initialization failed, continuing with CSV-only mode")
    
    # Deploy based on mode
    if args.mode == "docker":
        if not docker_deploy():
            sys.exit(1)
    else:
        print(f"\n🌐 Starting application in {args.mode} mode...")
        print("   Dashboard: http://localhost:5000")
        print("   API Health: http://localhost:5000/api/health")
        print("   Press Ctrl+C to stop the server")
        print("")
        
        if not start_application(args.mode):
            sys.exit(1)

if __name__ == "__main__":
    main()
