var path = require('path');
const spawn = require('child_process').spawn
var webpack = require('webpack');
var express = require('express');
var config = require('./webpack.config.client');

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
