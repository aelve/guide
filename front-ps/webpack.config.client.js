
const path = require('path');
const webpack = require('webpack');
const c = require('./webpack.config.common.js');

module.exports = {
  context: c.context,
  entry: {
    "category-overview": path.join(__dirname, 'client', 'Guide', 'Client', 'CategoryOverview-entry.js'),
    "category-detail": path.join(__dirname, 'client', 'Guide', 'Client', 'CategoryDetail-entry.js')
  },
  output: {
    path: path.join(__dirname, '/public'),
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
