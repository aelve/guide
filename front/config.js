const { API_URL, PORT } = process.env

export default {
  apiUrl: API_URL || 'https://staging.guide.aelve.com:4400/',
  port: PORT || 5000
}
