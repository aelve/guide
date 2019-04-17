// TODO move imports here and make no dynamic
import path from 'path'
import fs from 'fs'
import { createBundleRenderer } from 'vue-server-renderer'
// TODO move prefix to config
import bundle from '../src/vue-ssr-server-bundle.json'
import clientManifest from '../src/vue-ssr-client-manifest.json'
import serve from 'koa-static'
import koaMount from 'koa-mount'

const fsAsync = fs.promises
export default async function setupProdServer (app) {

  const template = await fsAsync.readFile(path.resolve(__dirname, '../src/index.html'), 'utf-8') as string

  const renderer = createBundleRenderer(bundle, {
    template,
    clientManifest,
    runInNewContext: false
  })

  async function handler (ctx) {
    const context = {
      url: ctx.path
    }

    try {
      ctx.body = await renderer.renderToString(context)
    } catch (error) {
      ctx.body = error
    }
  }

  app.use(koaMount('/src',  serve(path.resolve(__dirname, '../src/'))))
  app.use(handler)
}
