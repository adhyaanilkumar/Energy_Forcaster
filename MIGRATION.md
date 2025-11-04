# Migration Guide: Flask to Next.js

This document outlines the changes made during the migration from Flask to Next.js.

## What Changed

### Frontend
- ✅ **Removed**: All HTML templates (`templates/*.html`)
- ✅ **Removed**: Static CSS/JS files (now using Tailwind CSS)
- ✅ **Added**: Next.js App Router structure (`app/`)
- ✅ **Added**: React components with TypeScript (`components/`)
- ✅ **Added**: Modern UI with Tailwind CSS
- ✅ **Added**: Type-safe API client

### Backend
- ✅ **Kept**: Python Flask backend (`app.py`) - still required for ML models
- ✅ **Added**: Next.js API proxy routes (`app/api/proxy/`)
- ✅ **Kept**: Database module (`database.py`)
- ✅ **Kept**: All Python dependencies (`requirements.txt`)

## Architecture

### Before (Flask)
```
Flask App (app.py)
├── HTML Templates (Jinja2)
├── Static Files (CSS/JS)
└── API Routes
```

### After (Next.js)
```
Next.js Frontend (Port 3000)
├── React Components (TypeScript)
├── Tailwind CSS Styling
└── API Proxy Routes → Python Backend

Python Backend (Port 5000)
├── Flask API Routes
├── ML Models (ARIMA, ETS, RF, XGBoost)
└── Database Module
```

## Running the Application

### Development Mode

1. **Start Python Backend** (Terminal 1)
   ```bash
   python app.py
   ```
   Backend runs on: http://localhost:5000

2. **Start Next.js Frontend** (Terminal 2)
   ```bash
   npm run dev
   ```
   Frontend runs on: http://localhost:3000

### Production Mode

1. **Build Next.js**
   ```bash
   npm run build
   npm start
   ```

2. **Run Python Backend**
   ```bash
   gunicorn app:app
   # or
   python app.py
   ```

## API Communication

The Next.js frontend communicates with the Python backend through:

1. **Proxy API Routes** (`app/api/proxy/[...path]/route.ts`)
   - Browser requests go through Next.js proxy
   - Prevents CORS issues
   - Allows for request/response transformation

2. **Direct API Calls** (Server-side)
   - Server-side rendering uses direct API calls
   - Configured via `PYTHON_API_URL` environment variable

## Environment Variables

Create a `.env` file in the root directory:

```env
# Python Backend API URL
PYTHON_API_URL=http://localhost:5000

# Next.js Public API URL (for browser requests)
NEXT_PUBLIC_API_URL=/api/proxy

# Next.js Configuration
NEXT_PUBLIC_APP_URL=http://localhost:3000
```

## Key Differences

### Routing
- **Before**: Flask routes (`@app.route('/')`)
- **After**: Next.js file-based routing (`app/page.tsx`)

### Templates
- **Before**: Jinja2 templates with server-side rendering
- **After**: React components with client-side rendering

### Styling
- **Before**: Custom CSS in templates
- **After**: Tailwind CSS utility classes

### Type Safety
- **Before**: JavaScript (no types)
- **After**: TypeScript (full type safety)

## Benefits

1. **Better Developer Experience**
   - TypeScript for type safety
   - Hot module replacement
   - Modern React patterns

2. **Improved Performance**
   - Server-side rendering
   - Code splitting
   - Optimized builds

3. **Modern UI**
   - Tailwind CSS for responsive design
   - Lucide React icons
   - Interactive Plotly charts

4. **Maintainability**
   - Component-based architecture
   - Type-safe API calls
   - Clear separation of concerns

## Notes

- The Python backend is still required for ML model execution
- All data processing and ML logic remains in Python
- Frontend is now a separate Next.js application
- Database module (`database.py`) is unchanged

## Troubleshooting

### CORS Issues
- Ensure the Python backend has CORS enabled (already configured in `app.py`)
- Use the Next.js proxy for browser requests

### API Connection Issues
- Check that Python backend is running on port 5000
- Verify `PYTHON_API_URL` environment variable
- Check browser console for errors

### Type Errors
- Run `npm run type-check` to check TypeScript errors
- Ensure all dependencies are installed: `npm install`

