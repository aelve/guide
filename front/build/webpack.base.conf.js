const webpack = require('webpack')
const { VueLoaderPlugin } = require('vue-loader')
const FriendlyErrorsPlugin = require('friendly-errors-webpack-plugin')

const { clientPort } = require('./build-config')
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
      'vue$': 'vue/dist/vue.esm.js'
    }
  },

  module: {
    rules: [
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
        test: /\.(jade|pug)$/,
        oneOf: [
          // For vue Jade / Pug template.
          {
            resourceQuery: /^\?vue/,
            use: ['pug-plain-loader']
          },

          // For Jade / Pug standalone files.
          {
            use: ['pug-loader']
          }
        ]
      },
      {
        test: /\.(png|jpg|gif|svg)$/,
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
      /zh-cn/
    ),

    new FriendlyErrorsPlugin(),

    new VueLoaderPlugin()
  ]
}
