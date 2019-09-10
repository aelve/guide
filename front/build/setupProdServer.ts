import path from 'path'
import fs from 'fs'
import { createBundleRenderer } from 'vue-server-renderer'
// TODO move 'src' prefix to config
// TODO find a way to get rid of ts ignore and still build without errors
// @ts-ignore
import bundle from '../src/vue-ssr-server-bundle.json'
// @ts-ignore
import clientManifest from '../src/vue-ssr-client-manifest.json'
import serve from 'koa-static'
import koaMount from 'koa-mount'
import serverRequestsHandler from './serverRequestsHandler'

const fsAsync = fs.promises
export default async function setupProdServer (app) {

  const template = await fsAsync.readFile(path.resolve(__dirname, '../src/index.html'), 'utf-8') as string

  const renderer = createBundleRenderer(bundle, {
    template,
    clientManifest,
    runInNewContext: false
  })

  const handler = (ctx) => serverRequestsHandler(ctx, renderer)

  app.use(koaMount('/src', serve(path.resolve(__dirname, '../src/'))))
  app.use(handler)
}
