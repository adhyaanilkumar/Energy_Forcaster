import axios from 'axios'

const API_BASE = process.env.NEXT_PUBLIC_API_URL || '/api/proxy'

// Use proxy in browser, direct API in server
const getApiBase = () => {
  if (typeof window !== 'undefined') {
    // Browser: use proxy
    return API_BASE
  }
  // Server: use direct Python API
  return process.env.PYTHON_API_URL || 'http://localhost:5000/api'
}

export const api = {
  get: async (path: string) => {
    const base = getApiBase()
    const url = path.startsWith('http') ? path : `${base}/${path}`
    return axios.get(url)
  },

  post: async (path: string, data: any) => {
    const base = getApiBase()
    const url = path.startsWith('http') ? path : `${base}/${path}`
    return axios.post(url, data)
  },
}

export default api

