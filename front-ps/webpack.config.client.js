
const path = require('path');
const webpack = require('webpack');
const merge = require('webpack-merge');
const ExtractTextPlugin = require('extract-text-webpack-plugin');
const c = require('./webpack.config.common.js');

module.exports = merge(c.config, {
  entry: {
    "vendor": path.join(__dirname, 'common', 'vendor.scss'),
    "category-overview": path.join(__dirname, 'client', 'Guide', 'Client', 'CategoryOverview-entry.js'),
    "category-detail": path.join(__dirname, 'client', 'Guide', 'Client', 'CategoryDetail-entry.js'),
  },
  output: {
    path: path.join(__dirname, 'public'),
    publicPath: '/',
    filename: '[name].js',
    pathinfo: true
  },
  plugins: [
    new ExtractTextPlugin({
        filename: 'vendor.css'
      })
  ],
  module: {
    loaders: [
      {
        test: /\.s[ac]ss$/,
        loader: ExtractTextPlugin.extract({
          fallback: 'style-loader',
          use: [
            'css-loader?importLoaders=1&minimize=' + c.isProd,
            'sass-loader'
          ]
        })
      }
    ]
  },
  resolveLoader: {
    modules: [
      path.join(__dirname, 'node_modules')
    ]
  }
})
