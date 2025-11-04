'use client'

import { Code, Link as LinkIcon, BarChart3, Terminal } from 'lucide-react'

const API_BASE = process.env.NEXT_PUBLIC_API_URL || '/api/proxy'

export default function ApiDocs() {
  return (
    <div>
      <div className="page-header">
        <h1 className="flex items-center space-x-2">
          <Code className="h-6 w-6" />
          <span>API Documentation</span>
        </h1>
        <p>REST API endpoints for energy consumption forecasting</p>
      </div>

      {/* API Overview */}
      <div className="card mb-6">
        <div className="card-header">
          <h5 className="flex items-center space-x-2">
            <Code className="h-5 w-5" />
            <span>API Overview</span>
          </h5>
        </div>
        <div className="card-body">
          <p className="mb-4">
            The Energy Forecasting API provides access to energy consumption data, forecasting
            capabilities, and analytics for 294+ countries.
          </p>
          <p>
            <strong>Base URL:</strong>{' '}
            <code className="bg-gray-100 px-2 py-1 rounded">{API_BASE}</code>
          </p>
        </div>
      </div>

      <div className="grid grid-cols-1 md:grid-cols-2 gap-6 mb-6">
        {/* Core Endpoints */}
        <div className="card">
          <div className="card-header">
            <h5 className="flex items-center space-x-2">
              <LinkIcon className="h-5 w-5" />
              <span>Core Endpoints</span>
            </h5>
          </div>
          <div className="card-body space-y-4">
            <div>
              <h6 className="font-semibold mb-1">Health Check</h6>
              <code className="block bg-gray-100 px-3 py-2 rounded text-sm mb-1">
                GET /api/health
              </code>
              <p className="text-sm text-gray-600">Returns application health status</p>
            </div>

            <div>
              <h6 className="font-semibold mb-1">Get Countries</h6>
              <code className="block bg-gray-100 px-3 py-2 rounded text-sm mb-1">
                GET /api/countries
              </code>
              <p className="text-sm text-gray-600">Returns list of all available countries</p>
            </div>

            <div>
              <h6 className="font-semibold mb-1">Get Country Data</h6>
              <code className="block bg-gray-100 px-3 py-2 rounded text-sm mb-1">
                GET /api/countries/&#123;country&#125;/data
              </code>
              <p className="text-sm text-gray-600">
                Returns historical energy data for a specific country
              </p>
            </div>

            <div>
              <h6 className="font-semibold mb-1">Generate Forecast</h6>
              <code className="block bg-gray-100 px-3 py-2 rounded text-sm mb-1">
                POST /api/countries/&#123;country&#125;/forecast
              </code>
              <p className="text-sm text-gray-600 mb-2">Generates energy consumption forecast</p>
              <pre className="bg-gray-100 p-3 rounded text-xs overflow-x-auto">
                {`{
  "model": "best",
  "horizon": 60,
  "energy_type": "primary_energy_consumption"
}`}
              </pre>
            </div>
          </div>
        </div>

        {/* Analytics Endpoints */}
        <div className="card">
          <div className="card-header">
            <h5 className="flex items-center space-x-2">
              <BarChart3 className="h-5 w-5" />
              <span>Analytics Endpoints</span>
            </h5>
          </div>
          <div className="card-body space-y-4">
            <div>
              <h6 className="font-semibold mb-1">Model Performance</h6>
              <code className="block bg-gray-100 px-3 py-2 rounded text-sm mb-1">
                GET /api/models/performance
              </code>
              <p className="text-sm text-gray-600">Returns overall model performance metrics</p>
            </div>

            <div>
              <h6 className="font-semibold mb-1">Country Model Performance</h6>
              <code className="block bg-gray-100 px-3 py-2 rounded text-sm mb-1">
                GET /api/models/performance/&#123;country&#125;
              </code>
              <p className="text-sm text-gray-600">
                Returns model performance for a specific country
              </p>
            </div>

            <div>
              <h6 className="font-semibold mb-1">Correlation Analysis</h6>
              <code className="block bg-gray-100 px-3 py-2 rounded text-sm mb-1">
                GET /api/analytics/correlation
              </code>
              <p className="text-sm text-gray-600">
                Returns correlation analysis between energy and factors
              </p>
            </div>

            <div>
              <h6 className="font-semibold mb-1">Global Trends</h6>
              <code className="block bg-gray-100 px-3 py-2 rounded text-sm mb-1">
                GET /api/analytics/trends
              </code>
              <p className="text-sm text-gray-600">Returns global energy consumption trends</p>
            </div>
          </div>
        </div>
      </div>

      {/* Example Requests */}
      <div className="card">
        <div className="card-header">
          <h5 className="flex items-center space-x-2">
            <Terminal className="h-5 w-5" />
            <span>Example Requests</span>
          </h5>
        </div>
        <div className="card-body space-y-6">
          <div>
            <h6 className="font-semibold mb-2">Using cURL</h6>
            <pre className="bg-gray-100 p-4 rounded text-sm overflow-x-auto">
              {`# Get all countries
curl ${API_BASE}/countries

# Get forecast for United States
curl -X POST ${API_BASE}/countries/United%20States/forecast \\
  -H "Content-Type: application/json" \\
  -d '{
    "model": "best",
    "horizon": 60,
    "energy_type": "primary_energy_consumption"
  }'

# Get model performance
curl ${API_BASE}/models/performance`}
            </pre>
          </div>

          <div>
            <h6 className="font-semibold mb-2">Using JavaScript/Axios</h6>
            <pre className="bg-gray-100 p-4 rounded text-sm overflow-x-auto">
              {`// Get all countries
const response = await axios.get('${API_BASE}/countries');

// Generate forecast
const forecast = await axios.post(
  '${API_BASE}/countries/United States/forecast',
  {
    model: 'best',
    horizon: 60,
    energy_type: 'primary_energy_consumption'
  }
);`}
            </pre>
          </div>
        </div>
      </div>
    </div>
  )
}

