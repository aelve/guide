const path = require('path')
const webpack = require('webpack')
const nodeExternals = require('webpack-node-externals')
const isProd = process.env.NODE_ENV === 'production'

const entries = [path.join(__dirname, 'support', 'server.entry.js')]
const plugins = [
  new webpack.ProvidePlugin({
    'XMLHttpRequest': 'xhr2'
  }),
  new webpack.DefinePlugin({
    'process.env.NODE_ENV': JSON.stringify(process.env.NODE_ENV)
  })
]

module.exports = {
  entry: entries,
  target: 'node',
  output: {
    path: path.join(__dirname, 'dist'),
    filename: 'server.js',
    publicPath: '/',
    libraryTarget: 'commonjs2'
  },
  module: {
    loaders: [
      {
        test: /\.purs$/,
        loader: 'purs-loader',
        exclude: /node_modules/,
        query: {}
      }
    ],
  },
  plugins: plugins,
  externals: [nodeExternals({
    whitelist: ['XMLHttpRequest', 'webpack/hot/poll?1000'],
  })],
  resolve: {
    alias: {
      'react': 'preact-compat',
      'react-dom': 'preact-compat'
    },
    modules: [
      'node_modules'
    ],
    extensions: ['.js', '.purs']
  },
  performance: { hints: false },
  stats: {
    hash: false,
    timings: false,
    version: false,
    assets: false,
    errors: true,
    colors: false,
    chunks: false,
    children: false,
    cached: false,
    modules: false,
    chunkModules: false
  }
}
