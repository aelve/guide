const path = require('path')
const webpack = require('webpack')
const HtmlWebpackPlugin = require('html-webpack-plugin')
const MiniCssExtractPlugin = require('mini-css-extract-plugin')
const merge = require('webpack-merge')
const VueClientPlugin = require('vue-server-renderer/client-plugin')

const { appName, clientPort, distPath, env } = require('./build-config')
const baseConfig = require('./webpack.base.conf')

const isProduction = process.env.NODE_ENV === 'production'

const webpackConfig = merge(baseConfig, {
  entry: {
    app: rootResolve('./client/entry.client.ts')
  },

  output: {
    path: distPath,
    filename: `static/js/[name].${isProduction ? '[hash].' : ''}js`
  },

  plugins: [
    new VueClientPlugin(),

    new webpack.DefinePlugin(Object.assign({}, env, {
      'process.env.VUE_ENV': JSON.stringify('client')
    })),

    new HtmlWebpackPlugin({
      template: rootResolve('./index.html'),
      minify: true,
      inject: true,
      title: appName
    })
  ]
})

if (isProduction) {
  webpackConfig.plugins.push(
    new MiniCssExtractPlugin({
      filename: 'static/css/[name].[contenthash].css',
      chunkFilename: 'static/css/[id].[contenthash].css'
    })
  )
  webpackConfig.optimization.runtimeChunk = true
} else {
  webpackConfig.devtool = 'eval-source-map'
  webpackConfig.plugins.push(
    new webpack.HotModuleReplacementPlugin()
  )
  webpackConfig.devServer = {
    headers: {
      'Access-Control-Allow-Origin': '*'
    },
    hot: true,
    quiet: true,
    compress: false,
    port: clientPort,
    proxy: {},
    historyApiFallback: true
  }
}

module.exports = webpackConfig

function rootResolve (filePath) {
  return path.resolve(__dirname, '../', filePath)
}
