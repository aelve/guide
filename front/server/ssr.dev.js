const axios = require('axios')
const fs = require('fs')
const path = require('path')
const MemoryFs = require('memory-fs')
const webpack = require('webpack')
const VueServerRenderer = require('vue-server-renderer')

const serverConfig = require('../build/webpack.server.conf')
const serverCompiler = webpack(serverConfig)
const { clientPort } = require('../build/build-config')
const { templateEnvs } = require('./ssr.config')

const mfs = new MemoryFs()
serverCompiler.outputFileSystem = mfs

let bundle = null
serverCompiler.watch({}, (err, stats) => {
  if (err) {
    throw err
  }
  stats = stats.toJson()
  stats.errors.forEach(err => console.log(err))
  stats.warnings.forEach(warn => console.log(warn))

  const bundlePath = path.join(serverConfig.output.path, 'vue-ssr-server-bundle.json')
  bundle = JSON.parse(mfs.readFileSync(bundlePath, 'utf-8'))
})

const urlsToSkip = [
  '/favicon.ico'
]

module.exports = async function handler (ctx) {
  const url = ctx.url

  // Skip favicon.
  if (urlsToSkip.indexOf(url) > -1) {
    ctx.body = ''
    return
  }

  if (!bundle) {
    ctx.body = 'Please wait...'
    return
  }

  const clientManifestUrl = `http://${ctx.hostname}:${clientPort}/vue-ssr-client-manifest.json`
  let clientManifest = null
  try {
    const { data } = await axios.get(clientManifestUrl)
    clientManifest = data
  } catch (error) {
    console.error(`[Error] Failed to get ${clientManifestUrl}:`, error)
    ctx.body = error.message || 'Failed to get client manifest json'
    return
  }

  const renderer = VueServerRenderer.createBundleRenderer(bundle, {
    clientManifest,
    template: fs.readFileSync(path.resolve(__dirname, './template.html'), 'utf-8'),
    runInNewContext: false
  })

  const context = Object.assign(templateEnvs(), {
    url
  })

  try {
    ctx.body = await renderer.renderToString(context)
  } catch (error) {
    console.error('[Error] SSR render error:', error)
    ctx.body = error.message || 'SSR unknown renderer error'
  }
}

