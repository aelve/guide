
const path = require('path');
const webpack = require('webpack');
const c = require('./webpack.config.common.js');

module.exports = {
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
  plugins: c.plugins,
  module: {
    loaders: c.loaders
  },
  resolveLoader: {
    modules: [
      path.join(__dirname, 'node_modules')
    ]
  },
  resolve: c.resolve,
  performance: c.performance,
  stats: c.stats
}
