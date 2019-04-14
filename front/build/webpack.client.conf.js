const path = require('path')
const merge = require('webpack-merge')
const VueSSRClientPlugin = require('vue-server-renderer/client-plugin')
const WebpackBar = require('webpackbar')
const ForkTsCheckerWebpackPlugin = require('fork-ts-checker-webpack-plugin')

const baseConfig = require('./webpack.base.conf')

const isProduction = process.env.NODE_ENV === 'production'

const webpackConfig = merge(baseConfig, {
  // Entry should be array cause of webpack-hot-client requirements
  mode: isProduction ? 'production' : 'development',
  entry: [path.resolve(__dirname, '../client/entry.client.ts')],
  devtool: isProduction ? false : 'cheap-module-source-map',
  optimization: {
    splitChunks: {
      name: 'manifest',
      minChunks: Infinity
    }
  },

  plugins: [
    new VueSSRClientPlugin(),
    new WebpackBar({
      name: 'Client'
    })
    // TODO make it work
    /* ,
    new ForkTsCheckerWebpackPlugin({
      vue: true,
      tslint: true,
      formatter: 'codeframe',
      checkSyntacticErrors: false
    }) */
  ]
})

module.exports = webpackConfig
