import fs from 'fs'
import path from 'path'
import MemoryFs from 'memory-fs'
import webpack from 'webpack'
import { createBundleRenderer } from 'vue-server-renderer'
import koaWebpack from 'koa-webpack'
import DeferredPromise from '../utils/DeferredPromise'
import serverRequestsHandler from './serverRequestsHandler'

const fsAsync = fs.promises

MemoryFs.prototype.readFileAsync = async function (...args: any) {
  return new Promise((resolve, reject) => {
    this.readFile(...args, (err: any, result: string | Buffer) => {
      if (err) {
        reject(err)
        return
      }
      resolve(result)
    })
  })
}

import serverConfig from '../build/webpack.server.conf'
import clientConfig from '../build/webpack.client.conf'
const serverCompiler = webpack(serverConfig)
const clientCompiler = webpack(clientConfig)

const mfs = new MemoryFs()

const clientManifestFileName = 'vue-ssr-client-manifest.json'
const serverBundleFileName = 'vue-ssr-server-bundle.json'

// TODO add icon and refactor, change favicon serving
const urlsToSkip = [
  '/favicon.ico'
]

let bundle: object = null
let clientManifest: object = null
let renderer = null
let template: string = null

export default async function setupDevServer (app): Promise<void> {
  const promise = new DeferredPromise()

  serverCompiler.outputFileSystem = mfs
  serverCompiler.watch({}, async (err: any, stats: any) => {
    logWebpack(err, stats)
    const bundlePath = path.join(serverConfig.output.path, serverBundleFileName)
    bundle = JSON.parse(await mfs.readFileAsync(bundlePath, 'utf-8'))
    updateRenderer(promise.resolve)
  })

  await Promise.all([setupClientDevMiddlware(app, promise.resolve), setupTemplate()])
  const handler = (ctx) => serverRequestsHandler(ctx, renderer)
  app.use(handler)

  // @ts-ignore
  return promise
}

async function setupClientDevMiddlware (app, resolveSetup) {
  const clientDevMiddleware = await koaWebpack({
    compiler: clientCompiler
  })
  clientCompiler.hooks.done.tap('updateRender', async (stats: any) => {
    logWebpack(undefined, stats)
    const manifestPath = path.join(clientConfig.output.path, clientManifestFileName)
    const fileSystem = clientDevMiddleware.devMiddleware.fileSystem
    clientManifest = JSON.parse(await fileSystem.readFileAsync(manifestPath, 'utf-8'))
    updateRenderer(resolveSetup)
  })
  app.use(clientDevMiddleware)
}

async function setupTemplate () {
  const templatePath = path.resolve(__dirname, '../index.html')
  template = await fsAsync.readFile(templatePath, 'utf-8') as string
}

function logWebpack (err: Error, stats) {
  if (err) {
    throw err
  }
  stats = stats.toJson()
  stats.errors.forEach((err: any) => console.log(err))
  stats.warnings.forEach((warn: any) => console.log(warn))
}

async function updateRenderer (resolveSetup: () => void) {
  if (!bundle || !clientManifest || !template) {
    return
  }

  renderer = createBundleRenderer(bundle, {
    clientManifest,
    template,
    runInNewContext: false
  })

  if (resolveSetup) {
    resolveSetup()
  }
}
