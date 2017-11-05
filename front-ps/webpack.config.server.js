
const path = require('path');
const webpack = require('webpack');
const merge = require('webpack-merge');
const c = require('./webpack.config.common.js');

module.exports = merge(c.config, {
  target: 'node',
  entry: {
    "server": path.join(__dirname, 'server', 'entry.js')
  },
  output: {
    path: path.join(__dirname, 'dist'),
    publicPath: '/',
    filename: '[name].js',
    pathinfo: true
  },
  resolveLoader: {
    modules: [
      path.join(__dirname, 'node_modules')
    ]
  }
})
