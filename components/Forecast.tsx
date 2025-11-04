'use client'

import { useEffect, useState } from 'react'
import dynamic from 'next/dynamic'
import { Zap, Sliders, TrendingUp, CheckCircle, XCircle } from 'lucide-react'
import axios from 'axios'

const Plot = dynamic(() => import('react-plotly.js'), { ssr: false })

const API_BASE = process.env.NEXT_PUBLIC_API_URL || '/api/proxy'

interface ForecastData {
  country: string
  model_used: string
  energy_type: string
  forecast: Array<{
    year: number
    forecast: number
    lower_80: number
    upper_80: number
    lower_95: number
    upper_95: number
  }>
  historical_data: {
    years: number[]
    values: number[]
  }
  metadata: {
    data_points: number
    last_year: number
    forecast_horizon: number
  }
}

interface ModelPerformance {
  model: string
  RMSE: number
  MAPE: number
}

const PROFESSIONAL_STANDARDS = {
  RMSE_THRESHOLD: 0.10, // 10% of mean
  MAPE_THRESHOLD: 15.0, // 15%
}

export default function Forecast() {
  const [countries, setCountries] = useState<string[]>([])
  const [selectedCountry, setSelectedCountry] = useState('')
  const [selectedModel, setSelectedModel] = useState('best')
  const [selectedHorizon, setSelectedHorizon] = useState('3')
  const [selectedEnergyType, setSelectedEnergyType] = useState('primary_energy_consumption')
  const [forecastData, setForecastData] = useState<ForecastData | null>(null)
  const [modelPerformance, setModelPerformance] = useState<ModelPerformance[]>([])
  const [loading, setLoading] = useState(false)
  const [loadingCountries, setLoadingCountries] = useState(true)

  useEffect(() => {
    loadCountries()
  }, [])

  useEffect(() => {
    if (selectedCountry) {
      loadCountryPerformance(selectedCountry)
    }
  }, [selectedCountry])

  const loadCountries = async () => {
    try {
      const response = await axios.get(`${API_BASE}/countries`)
      setCountries(response.data.countries || [])
      setLoadingCountries(false)
    } catch (error) {
      console.error('Error loading countries:', error)
      setLoadingCountries(false)
    }
  }

  const loadCountryPerformance = async (countryName: string) => {
    try {
      const response = await axios.get(
        `${API_BASE}/models/performance/${encodeURIComponent(countryName)}`
      )
      setModelPerformance(response.data.model_performance || [])
    } catch (error) {
      console.error('Error loading country performance:', error)
    }
  }

  const generateForecast = async () => {
    if (!selectedCountry) {
      alert('Please select a country first')
      return
    }

    setLoading(true)
    try {
      const response = await axios.post(
        `${API_BASE}/countries/${encodeURIComponent(selectedCountry)}/forecast`,
        {
          model: selectedModel,
          horizon: parseInt(selectedHorizon) * 12,
          energy_type: selectedEnergyType,
        }
      )
      setForecastData(response.data)
    } catch (error: any) {
      console.error('Error generating forecast:', error)
      alert(error.response?.data?.error || 'Error generating forecast')
    } finally {
      setLoading(false)
    }
  }

  const bestModel = modelPerformance.length > 0
    ? modelPerformance.reduce((prev, current) =>
        prev.RMSE && (!current.RMSE || prev.RMSE < current.RMSE) ? prev : current
      )
    : null

  return (
    <div>
      <div className="page-header">
        <h1 className="flex items-center space-x-2">
          <Zap className="h-6 w-6" />
          <span>Energy Consumption Forecasting</span>
        </h1>
        <p>Generate forecasts for energy consumption using machine learning models</p>
      </div>

      <div className="grid grid-cols-1 lg:grid-cols-4 gap-6">
        {/* Forecast Controls */}
        <div className="lg:col-span-1 space-y-6">
          <div className="card">
            <div className="card-header">
              <h5 className="flex items-center space-x-2">
                <Sliders className="h-5 w-5" />
                <span>Forecast Parameters</span>
              </h5>
            </div>
            <div className="card-body space-y-4">
              <div>
                <label htmlFor="countrySelect" className="form-label">
                  Country
                </label>
                <select
                  id="countrySelect"
                  className="form-control"
                  value={selectedCountry}
                  onChange={(e) => setSelectedCountry(e.target.value)}
                  disabled={loadingCountries}
                >
                  <option value="">{loadingCountries ? 'Loading...' : 'Select a country...'}</option>
                  {countries.map((country) => (
                    <option key={country} value={country}>
                      {country}
                    </option>
                  ))}
                </select>
              </div>

              <div>
                <label htmlFor="modelSelect" className="form-label">
                  Forecasting Model
                </label>
                <select
                  id="modelSelect"
                  className="form-control"
                  value={selectedModel}
                  onChange={(e) => setSelectedModel(e.target.value)}
                >
                  <option value="best">Best Model (Auto)</option>
                  <option value="ets">Exponential Smoothing</option>
                  <option value="arima">ARIMA</option>
                  <option value="rf">Random Forest</option>
                  <option value="xgb">XGBoost</option>
                </select>
              </div>

              <div>
                <label htmlFor="horizonSelect" className="form-label">
                  Forecast Horizon (Years)
                </label>
                <select
                  id="horizonSelect"
                  className="form-control"
                  value={selectedHorizon}
                  onChange={(e) => setSelectedHorizon(e.target.value)}
                >
                  <option value="1">1 Year</option>
                  <option value="2">2 Years</option>
                  <option value="3">3 Years</option>
                  <option value="5">5 Years</option>
                  <option value="10">10 Years</option>
                </select>
              </div>

              <div>
                <label htmlFor="energyTypeSelect" className="form-label">
                  Energy Type
                </label>
                <select
                  id="energyTypeSelect"
                  className="form-control"
                  value={selectedEnergyType}
                  onChange={(e) => setSelectedEnergyType(e.target.value)}
                >
                  <option value="primary_energy_consumption">Primary Energy</option>
                  <option value="fossil_fuel_consumption">Fossil Fuels</option>
                  <option value="renewables_consumption">Renewables</option>
                </select>
              </div>

              <button
                onClick={generateForecast}
                disabled={loading || !selectedCountry}
                className="btn-primary w-full"
              >
                {loading ? 'Generating...' : 'Generate Forecast'}
              </button>
            </div>
          </div>

          {/* Model Performance Standards */}
          {bestModel && selectedCountry && (
            <div className="card">
              <div className="card-header">
                <h5>Model Performance Standards</h5>
              </div>
              <div className="card-body space-y-3">
                <div>
                  <h6 className="font-semibold mb-2">Best Model: {bestModel.model}</h6>
                </div>
                <div className="space-y-2">
                  <div className="flex items-center justify-between">
                    <span className="text-sm">
                      <strong>RMSE:</strong> {bestModel.RMSE.toFixed(2)}
                    </span>
                    {bestModel.RMSE <= PROFESSIONAL_STANDARDS.RMSE_THRESHOLD ? (
                      <CheckCircle className="h-5 w-5 text-green-500" />
                    ) : (
                      <XCircle className="h-5 w-5 text-red-500" />
                    )}
                  </div>
                  <div className="flex items-center justify-between">
                    <span className="text-sm">
                      <strong>MAPE:</strong> {bestModel.MAPE.toFixed(2)}%
                    </span>
                    {bestModel.MAPE <= PROFESSIONAL_STANDARDS.MAPE_THRESHOLD ? (
                      <CheckCircle className="h-5 w-5 text-green-500" />
                    ) : (
                      <XCircle className="h-5 w-5 text-red-500" />
                    )}
                  </div>
                </div>
              </div>
            </div>
          )}

          {/* Forecast Metrics */}
          {forecastData && (
            <div className="card">
              <div className="card-header">
                <h5>Forecast Metrics</h5>
              </div>
              <div className="card-body space-y-2 text-sm">
                <p>
                  <strong>Model Used:</strong> {forecastData.model_used.toUpperCase()}
                </p>
                <p>
                  <strong>Data Points:</strong> {forecastData.metadata.data_points}
                </p>
                <p>
                  <strong>Last Year:</strong> {forecastData.metadata.last_year}
                </p>
                <p>
                  <strong>Forecast Horizon:</strong> {forecastData.metadata.forecast_horizon} months
                </p>
              </div>
            </div>
          )}
        </div>

        {/* Forecast Visualization */}
        <div className="lg:col-span-3 space-y-6">
          <div className="card">
            <div className="card-header">
              <h5 className="flex items-center space-x-2">
                <TrendingUp className="h-5 w-5" />
                <span>Forecast Visualization</span>
              </h5>
            </div>
            <div className="card-body">
              {forecastData ? (
                <Plot
                  data={[
                    {
                      x: forecastData.historical_data.years,
                      y: forecastData.historical_data.values,
                      type: 'scatter',
                      mode: 'lines+markers',
                      name: 'Historical Data',
                      line: { color: '#0066CC', width: 2 },
                      marker: { size: 6 },
                    },
                    {
                      x: forecastData.forecast.map((f) => f.year),
                      y: forecastData.forecast.map((f) => f.upper_95),
                      type: 'scatter',
                      mode: 'lines',
                      name: '95% Confidence',
                      line: { color: 'rgba(220, 53, 69, 0.2)', width: 0 },
                      showlegend: false,
                    },
                    {
                      x: forecastData.forecast.map((f) => f.year),
                      y: forecastData.forecast.map((f) => f.lower_95),
                      type: 'scatter',
                      mode: 'lines',
                      line: { color: 'rgba(220, 53, 69, 0.2)', width: 0 },
                      fill: 'tonexty',
                      fillcolor: 'rgba(220, 53, 69, 0.1)',
                      name: '95% Confidence Interval',
                      showlegend: false,
                    },
                    {
                      x: forecastData.forecast.map((f) => f.year),
                      y: forecastData.forecast.map((f) => f.forecast),
                      type: 'scatter',
                      mode: 'lines+markers',
                      name: 'Forecast',
                      line: { color: '#DC3545', width: 2, dash: 'dash' },
                      marker: { size: 6 },
                    },
                  ]}
                  layout={{
                    title: `${forecastData.country} - ${forecastData.energy_type
                      .replace(/_/g, ' ')
                      .toUpperCase()}<br><sub>Model: ${forecastData.model_used.toUpperCase()}</sub>`,
                    xaxis: { title: 'Year', gridcolor: '#E9ECEF' },
                    yaxis: {
                      title: forecastData.energy_type.replace(/_/g, ' ').toUpperCase(),
                      gridcolor: '#E9ECEF',
                    },
                    plot_bgcolor: 'rgba(0,0,0,0)',
                    paper_bgcolor: 'rgba(0,0,0,0)',
                    template: 'plotly_white',
                    margin: { t: 80, b: 50, l: 60, r: 20 },
                    legend: { x: 0, y: 1 },
                    height: 600,
                  }}
                  config={{
                    responsive: true,
                    displayModeBar: true,
                    displaylogo: false,
                  }}
                  style={{ width: '100%', height: '600px' }}
                />
              ) : (
                <div className="flex items-center justify-center h-96 text-gray-500">
                  <p>Select a country and click &quot;Generate Forecast&quot; to view predictions.</p>
                </div>
              )}
            </div>
          </div>

          {/* Forecast Table */}
          {forecastData && (
            <div className="card">
              <div className="card-header">
                <h5>Forecast Data</h5>
              </div>
              <div className="card-body">
                <div className="overflow-x-auto">
                  <table className="min-w-full divide-y divide-gray-200">
                    <thead className="bg-gray-50">
                      <tr>
                        <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                          Year
                        </th>
                        <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                          Forecast
                        </th>
                        <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                          Lower 80%
                        </th>
                        <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                          Upper 80%
                        </th>
                        <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                          Lower 95%
                        </th>
                        <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                          Upper 95%
                        </th>
                      </tr>
                    </thead>
                    <tbody className="bg-white divide-y divide-gray-200">
                      {forecastData.forecast.map((f, index) => (
                        <tr key={index} className="hover:bg-gray-50">
                          <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-900">
                            {f.year}
                          </td>
                          <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-900">
                            {f.forecast.toFixed(2)}
                          </td>
                          <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-500">
                            {f.lower_80.toFixed(2)}
                          </td>
                          <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-500">
                            {f.upper_80.toFixed(2)}
                          </td>
                          <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-500">
                            {f.lower_95.toFixed(2)}
                          </td>
                          <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-500">
                            {f.upper_95.toFixed(2)}
                          </td>
                        </tr>
                      ))}
                    </tbody>
                  </table>
                </div>
              </div>
            </div>
          )}
        </div>
      </div>
    </div>
  )
}

