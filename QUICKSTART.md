# Quick Start Guide

## Getting Started in 5 Minutes

### 1. Install Dependencies

```bash
# Install Python dependencies
pip install -r requirements.txt

# Install Node.js dependencies
npm install
```

### 2. Set Up Environment

Create a `.env` file in the root directory:

```env
PYTHON_API_URL=http://localhost:5000
NEXT_PUBLIC_API_URL=/api/proxy
NEXT_PUBLIC_APP_URL=http://localhost:3000
```

### 3. Start the Application

**Terminal 1 - Start Python Backend:**
```bash
python app.py
```

**Terminal 2 - Start Next.js Frontend:**
```bash
npm run dev
```

### 4. Open Your Browser

Visit: http://localhost:3000

## What's Different?

### Before (Flask)
- HTML templates with Jinja2
- Static CSS/JS files
- Server-side rendering only

### After (Next.js)
- React components with TypeScript
- Tailwind CSS for styling
- Client-side rendering with SSR
- Modern, professional UI

## Project Structure

```
energy-forcaster/
├── app/                    # Next.js pages
│   ├── api/               # API proxy routes
│   ├── forecast/          # Forecast page
│   ├── analytics/         # Analytics page
│   ├── models/            # Models page
│   └── page.tsx           # Dashboard
├── components/            # React components
├── lib/                   # Utilities
├── app.py                 # Python backend (kept)
└── database.py            # Database module (kept)
```

## Troubleshooting

### Python Backend Not Running
- Check that port 5000 is available
- Verify Python dependencies are installed
- Check `app.py` for errors

### Next.js Not Starting
- Ensure Node.js 18+ is installed
- Run `npm install` to install dependencies
- Check `.env` file exists

### API Connection Issues
- Verify Python backend is running on port 5000
- Check `PYTHON_API_URL` in `.env`
- Check browser console for errors

## Next Steps

1. **Read the README.md** for detailed documentation
2. **Check MIGRATION.md** for migration details
3. **Explore the components/** to understand the structure
4. **Customize the UI** in `app/globals.css` and components

## Features

✅ Professional Next.js frontend
✅ TypeScript for type safety
✅ Tailwind CSS for modern styling
✅ Interactive Plotly charts
✅ Responsive design
✅ API proxy for backend communication
✅ All original functionality preserved

Enjoy your new Next.js application! 🚀

