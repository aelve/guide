const path = require('path')
const merge = require('webpack-merge')
const nodeExternals = require('webpack-node-externals')
const VueSSRServerPlugin = require('vue-server-renderer/server-plugin')
const WebpackBar = require('webpackbar')
const baseConfig = require('./webpack.base.conf')

const isProduction = process.env.NODE_ENV === 'production'

const webpackConfig = merge(baseConfig, {
  mode: isProduction ? 'production' : 'development',
  target: 'node',
  devtool: isProduction ? false : 'source-map',
  entry: path.resolve(__dirname, '../client/entry.server.ts'),
  output: {
    filename: 'server-bundle.js',
    libraryTarget: 'commonjs2'
  },
  externals: nodeExternals({
    whitelist: [
      /\.css$/,
      /\.sass$/,
      /\.scss$/,
      /\.svg$/
    ]
  }),

  plugins: [
    new VueSSRServerPlugin(),
    new WebpackBar({
      name: 'Server',
      color: 'orange'
    })
  ]
})

module.exports = webpackConfig
