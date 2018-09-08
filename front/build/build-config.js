const path = require('path')
const moment = require('moment')
const axios = require('axios')

const appName = 'Aelve Guide'
const clientPort = 4000
const ssrPort = 5000
const distPath = rootResolve('./dist')

axios.defaults.baseURL = `http://localhost:${ssrPort}`

const env = {
  'process.env.NODE_ENV': JSON.stringify(process.env.NODE_ENV),
  'process.env.BUILD_TIME': JSON.stringify(moment(new Date()).format('YYYY-MM-DD HH:mm:ss'))
}

module.exports = {
  appName,
  env,
  clientPort,
  ssrPort,
  distPath
}

function rootResolve (filePath) {
  return path.resolve(__dirname, '../', filePath)
}
