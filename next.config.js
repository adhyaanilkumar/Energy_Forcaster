/** @type {import('next').NextConfig} */
const nextConfig = {
  reactStrictMode: true,
  swcMinify: true,
  images: {
    domains: [],
  },
  env: {
    PYTHON_API_URL: process.env.PYTHON_API_URL || 'http://localhost:5000',
  },
}

module.exports = nextConfig

