const webpack = require('webpack')
const path = require('path')
const { VueLoaderPlugin } = require('vue-loader')
const FriendlyErrorsPlugin = require('friendly-errors-webpack-plugin')
const TSLintPlugin = require('tslint-webpack-plugin')
const { cssLoader, stylusLoader } = require('./style-loader.conf')

const { clientPort, ssrPort } = require('./build-config')
const isDev = process.env.NODE_ENV === 'development'

module.exports = {
  mode: isDev ? 'development' : 'production',

  output: {
    publicPath: isDev
      ? `//localhost:${clientPort}/`  // Please bind this hostname to 127.0.0.1 when developing.
      : '/'
  },

  optimization: {},

  resolve: {
    extensions: ['.js', '.ts'],
    alias: {
      client: path.resolve(__dirname, '../client/')
    }
  },

  module: {
    rules: [
      cssLoader, 
      stylusLoader,
      {
        test: /\.js$/,
        exclude: /node_modules/,
        use: {
          loader: 'babel-loader'
        }
      },
      {
        test: /\.tsx?$/,
        use: [
          'babel-loader',
          {
            loader: 'ts-loader',
            options: {
              happyPackMode: true,
              appendTsSuffixTo: [/\.vue$/]
            }
          }
        ],
        exclude: /node_modules/
      },
      {
        test: /\.vue$/,
        loader: 'vue-loader'
      },
      {
        test: /\.(png|jpg|gif|svg|woff|woff2|eot|ttf)$/,
        use: [
          {
            loader: 'url-loader',
            options: {
              limit: 8192,
              name: 'img/[name].[hash:7].[ext]'
            }
          }
        ]
      }
    ]
  },

  plugins: [
    new webpack.ContextReplacementPlugin(
      /moment[\/\\]locale$/,
      /ru-RU/
    ),
    new webpack.DefinePlugin({
      BASE_URL: JSON.stringify(`http://localhost:${ssrPort}`)
    }),
    new FriendlyErrorsPlugin(),
    new VueLoaderPlugin(),

    new TSLintPlugin({
      files: [
        './client/**/*.ts',
        './server/**/*.ts',
        './client/**/*.tsx',
        './server/**/*.tsx'
      ]
    })
  ]
}
