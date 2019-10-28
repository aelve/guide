import config from '../config.js'

const baseUrl = `http://localhost:${config.port}`

export default {
  ...config,
  baseUrl
}
