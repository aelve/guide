const path = require('path')

const webpack = require('webpack')
const merge = require('webpack-merge')
const nodeExternals = require('webpack-node-externals')
const MiniCssExtractPlugin = require('mini-css-extract-plugin')
const VueServerPlugin = require('vue-server-renderer/server-plugin')
const axios = require('axios')
const { distPath, env, ssrPort } = require('./build-config')
const baseConfig = require('./webpack.base.conf')

axios.defaults.baseURL = `http://localhost:${ssrPort}`

const webpackConfig = merge(baseConfig, {
  mode: process.env.NODE_ENV,

  target: 'node',

  entry: rootResolve('./client/entry.server.ts'),

  output: {
    path: distPath,
    filename: 'server-build.js',
    libraryTarget: 'commonjs2'
  },

  externals: nodeExternals({
    whitelist: [
      /\.css$/,
      /\.vue$/,
      /babel-polyfill/
    ]
  }),

  plugins: [
    new VueServerPlugin(),

    new webpack.DefinePlugin(Object.assign({}, env, {
      'process.env.VUE_ENV': JSON.stringify('server')
    }))
  ]
})

switch (process.env.NODE_ENV) {
  case 'production':
    webpackConfig.plugins.push(
      new MiniCssExtractPlugin({
        filename: 'static/css/[name].[contenthash].css',
        chunkFilename: 'static/css/[id].[contenthash].css'
      })
    )
    webpackConfig.optimization.splitChunks = false
    break
}

module.exports = webpackConfig

function rootResolve (filePath) {
  return path.resolve(__dirname, '../', filePath)
}
