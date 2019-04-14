import Koa from 'koa'
import bodyparser from 'koa-bodyparser'
import proxy from 'koa-proxy'
import config from './config.js'

const { port, apiUrl } = config
const isProduction = process.env.NODE_ENV === 'production'

async function start () {
  const app = new Koa()

  // TODO replace proxy lib or write own middleware
  app.use(proxy({
    requestOptions: {
      strictSSL: false
    },
    host: apiUrl,
    match: /^\/api\//,
    map: (path: string) => path.replace('/api', '')
  }))
  app.use(bodyparser())

  const setupServer = isProduction
    ? (await import('./build/setupProdServer')).default
    : (await import('./build/setupDevServer')).default

  await setupServer(app)

  app.listen(port, () => {
    console.log(`[Info] Server is on at ${port}.`)
  })
}

start()
