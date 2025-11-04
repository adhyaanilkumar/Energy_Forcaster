'use client'

import { useEffect, useState } from 'react'
import dynamic from 'next/dynamic'
import { Cog, Trophy, Table as TableIcon, Search } from 'lucide-react'
import axios from 'axios'

const Plot = dynamic(() => import('react-plotly.js'), { ssr: false })

const API_BASE = process.env.NEXT_PUBLIC_API_URL || '/api/proxy'

interface ModelPerformance {
  model: string
  RMSE_mean: number
  RMSE_median: number
  MAPE_mean: number
  MAPE_median: number
}

interface CountryModelPerformance {
  model: string
  RMSE: number
  MAPE: number
}

export default function Models() {
  const [countries, setCountries] = useState<string[]>([])
  const [selectedCountry, setSelectedCountry] = useState('')
  const [modelPerformance, setModelPerformance] = useState<ModelPerformance[]>([])
  const [bestModelCounts, setBestModelCounts] = useState<Record<string, number>>({})
  const [countryPerformance, setCountryPerformance] = useState<CountryModelPerformance[]>([])
  const [loading, setLoading] = useState(true)

  useEffect(() => {
    loadModelPerformance()
  }, [])

  const loadModelPerformance = async () => {
    try {
      // Load countries
      const countriesResponse = await axios.get(`${API_BASE}/countries`)
      setCountries(countriesResponse.data.countries || [])

      // Load model performance
      const response = await axios.get(`${API_BASE}/models/performance`)
      setModelPerformance(response.data.model_performance || [])
      setBestModelCounts(response.data.best_model_counts || {})

      setLoading(false)
    } catch (error) {
      console.error('Error loading model performance:', error)
      setLoading(false)
    }
  }

  const loadCountryPerformance = async () => {
    if (!selectedCountry) {
      alert('Please select a country first')
      return
    }

    try {
      const response = await axios.get(
        `${API_BASE}/models/performance/${encodeURIComponent(selectedCountry)}`
      )
      setCountryPerformance(response.data.model_performance || [])
    } catch (error) {
      console.error('Error loading country performance:', error)
    }
  }

  if (loading) {
    return (
      <div className="flex items-center justify-center h-64">
        <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-primary-500"></div>
      </div>
    )
  }

  const bestLabels = Object.keys(bestModelCounts)
  const bestValues = Object.values(bestModelCounts)

  return (
    <div>
      <div className="page-header">
        <h1 className="flex items-center space-x-2">
          <Cog className="h-6 w-6" />
          <span>Model Performance</span>
        </h1>
        <p>Compare and analyze the performance of different forecasting models</p>
      </div>

      {/* Model Comparison */}
      <div className="card mb-6">
        <div className="card-header">
          <h5 className="flex items-center space-x-2">
            <Cog className="h-5 w-5" />
            <span>Model Performance Comparison</span>
          </h5>
        </div>
        <div className="card-body">
          {modelPerformance.length > 0 && (
            <Plot
              data={[
                {
                  x: modelPerformance.map((m) => m.model),
                  y: modelPerformance.map((m) =>
                    m.RMSE_mean != null && !isNaN(m.RMSE_mean) && isFinite(m.RMSE_mean)
                      ? m.RMSE_mean
                      : 0
                  ),
                  type: 'bar',
                  name: 'Average RMSE',
                  marker: { color: '#0066CC' },
                },
                {
                  x: modelPerformance.map((m) => m.model),
                  y: modelPerformance.map((m) =>
                    m.MAPE_mean != null &&
                    !isNaN(m.MAPE_mean) &&
                    isFinite(m.MAPE_mean) &&
                    m.MAPE_mean <= 1000
                      ? m.MAPE_mean
                      : 0
                  ),
                  type: 'bar',
                  name: 'Average MAPE (%)',
                  marker: { color: '#6C757D' },
                  yaxis: 'y2',
                },
              ]}
              layout={{
                title: '',
                xaxis: { title: 'Model', gridcolor: '#E9ECEF' },
                yaxis: { title: 'RMSE', gridcolor: '#E9ECEF' },
                yaxis2: {
                  title: 'MAPE (%)',
                  overlaying: 'y',
                  side: 'right',
                  gridcolor: '#E9ECEF',
                },
                plot_bgcolor: 'rgba(0,0,0,0)',
                paper_bgcolor: 'rgba(0,0,0,0)',
                template: 'plotly_white',
                margin: { t: 20, b: 50, l: 60, r: 60 },
                barmode: 'group',
                height: 500,
              }}
              config={{
                responsive: true,
                displayModeBar: true,
                displaylogo: false,
              }}
              style={{ width: '100%', height: '500px' }}
            />
          )}
        </div>
      </div>

      <div className="grid grid-cols-1 md:grid-cols-2 gap-6 mb-6">
        {/* Best Models by Country */}
        <div className="card">
          <div className="card-header">
            <h5 className="flex items-center space-x-2">
              <Trophy className="h-5 w-5" />
              <span>Best Models by Country</span>
            </h5>
          </div>
          <div className="card-body">
            {bestLabels.length > 0 && (
              <Plot
                data={[
                  {
                    labels: bestLabels,
                    values: bestValues,
                    type: 'pie',
                    marker: { colors: ['#0066CC', '#28A745', '#FFC107', '#DC3545'] },
                  },
                ]}
                layout={{
                  title: '',
                  plot_bgcolor: 'rgba(0,0,0,0)',
                  paper_bgcolor: 'rgba(0,0,0,0)',
                  template: 'plotly_white',
                  margin: { t: 20, b: 20, l: 20, r: 20 },
                  height: 450,
                }}
                config={{
                  responsive: true,
                  displayModeBar: true,
                  displaylogo: false,
                }}
                style={{ width: '100%', height: '450px' }}
              />
            )}
          </div>
        </div>

        {/* Performance Statistics */}
        <div className="card">
          <div className="card-header">
            <h5 className="flex items-center space-x-2">
              <TableIcon className="h-5 w-5" />
              <span>Performance Statistics</span>
            </h5>
          </div>
          <div className="card-body">
            <div className="overflow-x-auto">
              <table className="min-w-full divide-y divide-gray-200">
                <thead className="bg-gray-50">
                  <tr>
                    <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                      Model
                    </th>
                    <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                      Avg RMSE
                    </th>
                    <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                      Median RMSE
                    </th>
                    <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                      Avg MAPE
                    </th>
                    <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                      Median MAPE
                    </th>
                  </tr>
                </thead>
                <tbody className="bg-white divide-y divide-gray-200">
                  {modelPerformance.map((model, index) => (
                    <tr key={index} className="hover:bg-gray-50">
                      <td className="px-6 py-4 whitespace-nowrap text-sm font-medium text-gray-900">
                        {model.model}
                      </td>
                      <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-500">
                        {model.RMSE_mean != null && !isNaN(model.RMSE_mean) && isFinite(model.RMSE_mean)
                          ? model.RMSE_mean.toFixed(2)
                          : '-'}
                      </td>
                      <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-500">
                        {model.RMSE_median != null &&
                        !isNaN(model.RMSE_median) &&
                        isFinite(model.RMSE_median)
                          ? model.RMSE_median.toFixed(2)
                          : '-'}
                      </td>
                      <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-500">
                        {model.MAPE_mean != null &&
                        !isNaN(model.MAPE_mean) &&
                        isFinite(model.MAPE_mean) &&
                        model.MAPE_mean <= 1000
                          ? `${model.MAPE_mean.toFixed(2)}%`
                          : '-'}
                      </td>
                      <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-500">
                        {model.MAPE_median != null &&
                        !isNaN(model.MAPE_median) &&
                        isFinite(model.MAPE_median) &&
                        model.MAPE_median <= 1000
                          ? `${model.MAPE_median.toFixed(2)}%`
                          : '-'}
                      </td>
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>
          </div>
        </div>
      </div>

      {/* Country-Specific Performance */}
      <div className="card">
        <div className="card-header">
          <h5 className="flex items-center space-x-2">
            <Search className="h-5 w-5" />
            <span>Country-Specific Performance</span>
          </h5>
        </div>
        <div className="card-body">
          <div className="grid grid-cols-1 md:grid-cols-4 gap-4 mb-6">
            <div className="md:col-span-3">
              <label htmlFor="countryModelSelect" className="form-label">
                Select Country
              </label>
              <select
                id="countryModelSelect"
                className="form-control"
                value={selectedCountry}
                onChange={(e) => setSelectedCountry(e.target.value)}
              >
                <option value="">Select a country...</option>
                {countries.map((country) => (
                  <option key={country} value={country}>
                    {country}
                  </option>
                ))}
              </select>
            </div>
            <div className="md:col-span-1 flex items-end">
              <button onClick={loadCountryPerformance} className="btn-primary w-full">
                View Performance
              </button>
            </div>
          </div>

          {countryPerformance.length > 0 && (
            <div className="mt-6">
              <Plot
                data={[
                  {
                    x: countryPerformance.map((m) => m.model),
                    y: countryPerformance.map((m) => m.RMSE || 0),
                    type: 'bar',
                    name: 'RMSE',
                    marker: { color: '#0066CC' },
                  },
                  {
                    x: countryPerformance.map((m) => m.model),
                    y: countryPerformance.map((m) => m.MAPE || 0),
                    type: 'bar',
                    name: 'MAPE (%)',
                    marker: { color: '#6C757D' },
                    yaxis: 'y2',
                  },
                ]}
                layout={{
                  title: `Model Performance - ${selectedCountry}`,
                  xaxis: { title: 'Model', gridcolor: '#E9ECEF' },
                  yaxis: { title: 'RMSE', gridcolor: '#E9ECEF' },
                  yaxis2: {
                    title: 'MAPE (%)',
                    overlaying: 'y',
                    side: 'right',
                    gridcolor: '#E9ECEF',
                  },
                  plot_bgcolor: 'rgba(0,0,0,0)',
                  paper_bgcolor: 'rgba(0,0,0,0)',
                  template: 'plotly_white',
                  margin: { t: 60, b: 50, l: 60, r: 60 },
                  barmode: 'group',
                  height: 500,
                }}
                config={{
                  responsive: true,
                  displayModeBar: true,
                  displaylogo: false,
                }}
                style={{ width: '100%', height: '500px' }}
              />
            </div>
          )}
        </div>
      </div>
    </div>
  )
}

