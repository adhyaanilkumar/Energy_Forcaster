'use client'

import { useEffect, useState } from 'react'
import dynamic from 'next/dynamic'
import { Globe, Brain, TrendingUp, Percent, LayoutDashboard, Cog } from 'lucide-react'
import axios from 'axios'

const Plot = dynamic(() => import('react-plotly.js'), { ssr: false })

interface DashboardStats {
  totalCountries: number
  avgRMSE: number
  avgMAPE: number
}

interface GlobalTrends {
  year: number
  sum: number
  mean: number
  count: number
}

interface TopConsumers {
  countries: string[]
  values: number[]
}

interface ModelPerformance {
  model: string
  RMSE_mean: number
  RMSE_median: number
  MAPE_mean: number
  MAPE_median: number
}

const API_BASE = process.env.NEXT_PUBLIC_API_URL || '/api/proxy'

export default function Dashboard() {
  const [stats, setStats] = useState<DashboardStats>({
    totalCountries: 0,
    avgRMSE: 0,
    avgMAPE: 0,
  })
  const [globalTrends, setGlobalTrends] = useState<GlobalTrends[]>([])
  const [topConsumers, setTopConsumers] = useState<TopConsumers>({ countries: [], values: [] })
  const [modelPerformance, setModelPerformance] = useState<ModelPerformance[]>([])
  const [loading, setLoading] = useState(true)

  useEffect(() => {
    loadDashboardData()
  }, [])

  const loadDashboardData = async () => {
    try {
      // Load countries count
      const countriesResponse = await axios.get(`${API_BASE}/countries`)
      const totalCountries = countriesResponse.data.total || 0

      // Load global trends
      const trendsResponse = await axios.get(`${API_BASE}/analytics/trends`)
      const trends = trendsResponse.data.global_trends || []
      const topConsumersData = trendsResponse.data.top_consumers || { countries: [], values: [] }

      // Load model performance
      const modelResponse = await axios.get(`${API_BASE}/models/performance`)
      const modelData = modelResponse.data.model_performance || []

      // Calculate averages
      let totalRMSE = 0
      let totalMAPE = 0
      let countRMSE = 0
      let countMAPE = 0

      modelData.forEach((m: ModelPerformance) => {
        if (m.RMSE_mean != null && !isNaN(m.RMSE_mean) && isFinite(m.RMSE_mean)) {
          totalRMSE += m.RMSE_mean
          countRMSE++
        }
        if (m.MAPE_mean != null && !isNaN(m.MAPE_mean) && isFinite(m.MAPE_mean) && m.MAPE_mean <= 1000) {
          totalMAPE += m.MAPE_mean
          countMAPE++
        }
      })

      setStats({
        totalCountries,
        avgRMSE: countRMSE > 0 ? totalRMSE / countRMSE : 0,
        avgMAPE: countMAPE > 0 ? totalMAPE / countMAPE : 0,
      })
      setGlobalTrends(trends)
      setTopConsumers(topConsumersData)
      setModelPerformance(modelData)
      setLoading(false)
    } catch (error) {
      console.error('Error loading dashboard data:', error)
      setLoading(false)
    }
  }

  if (loading) {
    return (
      <div className="flex items-center justify-center h-64">
        <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-primary-500"></div>
      </div>
    )
  }

  return (
    <div>
      <div className="page-header">
        <h1 className="flex items-center space-x-2">
          <LayoutDashboard className="h-6 w-6" />
          <span>Dashboard</span>
        </h1>
        <p>Overview of energy consumption forecasting system</p>
      </div>

      {/* Statistics Cards */}
      <div className="grid grid-cols-1 md:grid-cols-4 gap-6 mb-8">
        <div className="card">
          <div className="card-body">
            <div className="flex items-center justify-between">
              <div>
                <p className="text-sm text-gray-600 mb-1">Total Countries</p>
                <h3 className="text-2xl font-bold text-gray-900">{stats.totalCountries}</h3>
              </div>
              <Globe className="h-10 w-10 text-primary-500" />
            </div>
          </div>
        </div>

        <div className="card">
          <div className="card-body">
            <div className="flex items-center justify-between">
              <div>
                <p className="text-sm text-gray-600 mb-1">Models Available</p>
                <h3 className="text-2xl font-bold text-gray-900">4</h3>
              </div>
              <Brain className="h-10 w-10 text-primary-500" />
            </div>
          </div>
        </div>

        <div className="card">
          <div className="card-body">
            <div className="flex items-center justify-between">
              <div>
                <p className="text-sm text-gray-600 mb-1">Avg. RMSE</p>
                <h3 className="text-2xl font-bold text-gray-900">
                  {stats.avgRMSE > 0 ? stats.avgRMSE.toFixed(2) : '-'}
                </h3>
              </div>
              <TrendingUp className="h-10 w-10 text-primary-500" />
            </div>
          </div>
        </div>

        <div className="card">
          <div className="card-body">
            <div className="flex items-center justify-between">
              <div>
                <p className="text-sm text-gray-600 mb-1">Avg. MAPE</p>
                <h3 className="text-2xl font-bold text-gray-900">
                  {stats.avgMAPE > 0 ? `${stats.avgMAPE.toFixed(2)}%` : '-'}
                </h3>
              </div>
              <Percent className="h-10 w-10 text-primary-500" />
            </div>
          </div>
        </div>
      </div>

      {/* Global Trends Chart */}
      <div className="card mb-6">
        <div className="card-header">
          <h5 className="flex items-center space-x-2">
            <TrendingUp className="h-5 w-5" />
            <span>Global Energy Consumption Trends</span>
          </h5>
        </div>
        <div className="card-body">
          {globalTrends.length > 0 && (
            <Plot
              data={[
                {
                  x: globalTrends.map((d) => d.year),
                  y: globalTrends.map((d) => d.sum),
                  type: 'scatter',
                  mode: 'lines+markers',
                  name: 'Global Energy Consumption',
                  line: { color: '#0066CC', width: 2 },
                  marker: { size: 5 },
                },
              ]}
              layout={{
                title: '',
                xaxis: { title: 'Year', gridcolor: '#E9ECEF' },
                yaxis: { title: 'Total Energy Consumption (TWh)', gridcolor: '#E9ECEF' },
                plot_bgcolor: 'rgba(0,0,0,0)',
                paper_bgcolor: 'rgba(0,0,0,0)',
                template: 'plotly_white',
                margin: { t: 20, b: 50, l: 60, r: 20 },
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

      {/* Charts Row */}
      <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
        {/* Top Consumers */}
        <div className="card">
          <div className="card-header">
            <h5 className="flex items-center space-x-2">
              <TrendingUp className="h-5 w-5" />
              <span>Top Energy Consumers</span>
            </h5>
          </div>
          <div className="card-body">
            {topConsumers.countries.length > 0 && (
              <Plot
                data={[
                  {
                    x: topConsumers.countries,
                    y: topConsumers.values,
                    type: 'bar',
                    marker: { color: '#0066CC' },
                  },
                ]}
                layout={{
                  title: '',
                  xaxis: { title: '', gridcolor: '#E9ECEF' },
                  yaxis: { title: 'Energy Consumption (TWh)', gridcolor: '#E9ECEF' },
                  plot_bgcolor: 'rgba(0,0,0,0)',
                  paper_bgcolor: 'rgba(0,0,0,0)',
                  template: 'plotly_white',
                  margin: { t: 20, b: 50, l: 60, r: 20 },
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

        {/* Model Performance Summary */}
        <div className="card">
          <div className="card-header">
            <h5 className="flex items-center space-x-2">
              <Cog className="h-5 w-5" />
              <span>Model Performance Summary</span>
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
                    name: 'RMSE',
                    marker: { color: '#0066CC' },
                  },
                  {
                    x: modelPerformance.map((m) => m.model),
                    y: modelPerformance.map((m) =>
                      m.MAPE_mean != null && !isNaN(m.MAPE_mean) && isFinite(m.MAPE_mean) && m.MAPE_mean <= 1000
                        ? m.MAPE_mean
                        : 0
                    ),
                    type: 'bar',
                    name: 'MAPE (%)',
                    marker: { color: '#6C757D' },
                    yaxis: 'y2',
                  },
                ]}
                layout={{
                  title: '',
                  xaxis: { title: 'Model', gridcolor: '#E9ECEF' },
                  yaxis: { title: 'Average RMSE', gridcolor: '#E9ECEF' },
                  yaxis2: {
                    title: 'Average MAPE (%)',
                    overlaying: 'y',
                    side: 'right',
                    gridcolor: '#E9ECEF',
                  },
                  plot_bgcolor: 'rgba(0,0,0,0)',
                  paper_bgcolor: 'rgba(0,0,0,0)',
                  template: 'plotly_white',
                  margin: { t: 20, b: 50, l: 60, r: 60 },
                  height: 450,
                  barmode: 'group',
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
      </div>
    </div>
  )
}


