import Koa from 'koa'
import bodyparser from 'koa-bodyparser'
import koaStatic from 'koa-static'
import proxy from 'koa-proxy'
import DefferedPromise from './utils/DeferredPromise'
import config from './config.js'

export default (async () => {
  const promise = new DefferedPromise()
  try {
    const { port, apiUrl } = config
    const isProduction = process.env.NODE_ENV === 'production'

    const app = new Koa()

    // TODO replace proxy lib or write own middleware for log and flexibility
    app.use(proxy({
      requestOptions: {
        strictSSL: false
      },
      host: apiUrl,
      match: /^\/api\//,
      map: (path: string) => path.replace('/api', '')
    }))
    app.use(koaStatic('./static'))
    app.use(bodyparser())

    const setupServer = isProduction
      ? (await import('./build/setupProdServer')).default
      : (await import('./build/setupDevServer')).default

    await setupServer(app)

    app.listen(port, () => {
      console.log(`[Info] Server is on at ${port}.`)
      promise.resolve()
    })

    return promise
  } catch (e) {
    promise.reject(e)
  }
})()
