'use client'

import { useEffect, useState } from 'react'
import dynamic from 'next/dynamic'
import { BarChart3, TrendingUp, Trophy, PieChart } from 'lucide-react'
import axios from 'axios'

const Plot = dynamic(() => import('react-plotly.js'), { ssr: false })

const API_BASE = process.env.NEXT_PUBLIC_API_URL || '/api/proxy'

interface Correlation {
  mean_correlation: number
  median_correlation: number
  countries_with_data: number
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

export default function Analytics() {
  const [correlations, setCorrelations] = useState<Record<string, Correlation>>({})
  const [globalTrends, setGlobalTrends] = useState<GlobalTrends[]>([])
  const [topConsumers, setTopConsumers] = useState<TopConsumers>({ countries: [], values: [] })
  const [loading, setLoading] = useState(true)

  useEffect(() => {
    loadAnalytics()
  }, [])

  const loadAnalytics = async () => {
    try {
      // Load correlation data
      const corrResponse = await axios.get(`${API_BASE}/analytics/correlation`)
      setCorrelations(corrResponse.data.correlations || {})

      // Load global trends
      const trendsResponse = await axios.get(`${API_BASE}/analytics/trends`)
      setGlobalTrends(trendsResponse.data.global_trends || [])
      setTopConsumers(trendsResponse.data.top_consumers || { countries: [], values: [] })

      setLoading(false)
    } catch (error) {
      console.error('Error loading analytics:', error)
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

  const correlationFactors = Object.keys(correlations)
  const correlationValues = correlationFactors.map((factor) => correlations[factor].mean_correlation)

  return (
    <div>
      <div className="page-header">
        <h1 className="flex items-center space-x-2">
          <BarChart3 className="h-6 w-6" />
          <span>Analytics</span>
        </h1>
        <p>Explore energy consumption patterns and correlations</p>
      </div>

      <div className="grid grid-cols-1 md:grid-cols-2 gap-6 mb-6">
        {/* Correlation Analysis */}
        <div className="card">
          <div className="card-header">
            <h5 className="flex items-center space-x-2">
              <BarChart3 className="h-5 w-5" />
              <span>Correlation Analysis</span>
            </h5>
          </div>
          <div className="card-body">
            {correlationFactors.length > 0 && (
              <Plot
                data={[
                  {
                    x: correlationFactors,
                    y: correlationValues,
                    type: 'bar',
                    marker: {
                      color: correlationValues.map((v) => (v > 0 ? '#0066CC' : '#DC3545')),
                      line: { color: 'white', width: 1 },
                    },
                  },
                ]}
                layout={{
                  title: '',
                  xaxis: { title: 'Factors', gridcolor: '#E9ECEF' },
                  yaxis: {
                    title: 'Correlation Coefficient',
                    range: [-1, 1],
                    gridcolor: '#E9ECEF',
                  },
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

        {/* Global Trends Over Time */}
        <div className="card">
          <div className="card-header">
            <h5 className="flex items-center space-x-2">
              <TrendingUp className="h-5 w-5" />
              <span>Global Energy Trends</span>
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
      </div>

      <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
        {/* Top Energy Consumers */}
        <div className="card">
          <div className="card-header">
            <h5 className="flex items-center space-x-2">
              <Trophy className="h-5 w-5" />
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

        {/* Energy Distribution */}
        <div className="card">
          <div className="card-header">
            <h5 className="flex items-center space-x-2">
              <PieChart className="h-5 w-5" />
              <span>Energy Distribution</span>
            </h5>
          </div>
          <div className="card-body">
            <Plot
              data={[
                {
                  labels: ['Fossil Fuels', 'Renewables', 'Nuclear', 'Other'],
                  values: [70, 20, 7, 3],
                  type: 'pie',
                  marker: { colors: ['#DC3545', '#28A745', '#FFC107', '#6C757D'] },
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
          </div>
        </div>
      </div>
    </div>
  )
}

