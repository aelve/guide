const appConfig = require('./src/App/Config.js').config
const spawn = require('child_process').spawn
const path = require('path')
const webpack = require('webpack')
const isProd = process.env.NODE_ENV === 'production'

const entries = [path.join(__dirname, 'support/client.entry.js')]

const plugins = [
  new webpack.DefinePlugin({
    'process.env.NODE_ENV': JSON.stringify(process.env.NODE_ENV)
  }),
]

if (isProd) {
  plugins.push(
    new webpack.LoaderOptionsPlugin({
      minimize: true,
      debug: false
    })
  )
} else {
  entries.unshift('webpack-hot-middleware/client?path=http://localhost:8080/__webpack_hmr&reload=true');
  plugins.push(
    new webpack.HotModuleReplacementPlugin(),
    new webpack.NoEmitOnErrorsPlugin()
  )
}

const config = {
  entry: entries,
  context: __dirname,
  target: 'web',
  output: {
    path: path.join(__dirname, 'static', 'dist'),
    filename: 'bundle.js',
    publicPath: appConfig.public_path
  },
  module: {
    loaders: [
      {
        test: /\.purs$/,
        loader: 'purs-loader',
        exclude: /node_modules/,
        query: isProd ? {
          bundle: true,
          bundleOutput: 'static/dist/bundle.js'
        } : {
          psc: 'psa',
          pscIde: true
        }
      }
    ],
  },
  plugins: plugins,
  resolveLoader: {
    modules: [
      path.join(__dirname, 'node_modules')
    ]
  },
  resolve: {
    alias: {
      'react': 'preact-compat',
      'react-dom': 'preact-compat'
    },
    modules: [
      'node_modules',
      'bower_components'
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

// If this file is directly run with node, start the development server
// instead of exporting the webpack config.
if (require.main === module) {
  const compiler = webpack(require('./webpack.config.server.js'))
  const client = webpack(config);
  let server = null;
  const app = require('express')();
  app.use(function(req, res, next) {
    res.header("Access-Control-Allow-Origin", "*");
    res.header("Access-Control-Allow-Headers", "Origin, X-Requested-With, Content-Type, Accept");
    next();
  });
  app.use(require('webpack-dev-middleware')(client, {
    noInfo: true,
    publicPath: config.output.publicPath
  }));
  app.use(require('webpack-hot-middleware')(client));
  app.listen(8080);

  console.log('Compiling...')

  compiler.watch({
    aggregateTimeout: 300,
    poll: undefined
  }, (err, stats) => {
    if (err) return console.error(err)

    if (server && !stats.hasErrors()) {
      server.kill('SIGKILL')
      server = spawn('node', ['./dist/server.js']);
    } else {
      server = spawn('node', ['./dist/server.js']);
    }
  })
} else {
  module.exports = config
}
