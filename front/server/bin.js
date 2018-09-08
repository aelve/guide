const path = require('path')
// TODO заменить на express
const Koa = require('koa')
const serve = require('koa-static')
const bodyparser = require('koa-bodyparser')

const { ssrPort } = require('../build/build-config')
const proxy = require('koa-proxy')

const config = require('../config.json')

const app = new Koa()
app.use(proxy({
  host: config.apiUrl,
  match: /^\/api\//,
  map: function (path) {
    return path.replace('/api', '')
  }
}))
app.use(bodyparser())

let ssrRouter = null
switch (process.env.NODE_ENV) {
  case 'production':
    app.use(serve(path.resolve(__dirname, '../dist'), {
      index: 'does-not-exist.html'  // Set to anything but 'index.html' to use ssr.
    }))
    ssrRouter = require('./ssr.prod.js')
    break

  case 'development':
    ssrRouter = require('./ssr.dev.js')
    break
}
app.use(ssrRouter)
app.listen(ssrPort, () => {
  console.log(`[Info] Server is on at ${ssrPort}.`)
})
