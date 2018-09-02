const MiniCssExtractPlugin = require('mini-css-extract-plugin')

const cssLoader = {
  test: /\.css$/,
  use: [
    'vue-style-loader',
    { loader: 'css-loader', options: { sourceMap: false, importLoaders: 1 } },
    'postcss-loader'
  ]
}

const stylusLoader = {
  test: /\.(styl|stylus)$/,
  use: [
    'vue-style-loader',
    { loader: 'css-loader', options: { sourceMap: false, importLoaders: 1 } },
    'postcss-loader',
    { loader: 'stylus-loader', options: { sourceMap: false } }
  ]
}

if (process.env.NODE_ENV === 'production') {
  cssLoader.use = [MiniCssExtractPlugin.loader].concat(cssLoader.use)
  stylusLoader.use = [MiniCssExtractPlugin.loader].concat(stylusLoader.use)
}

module.exports = {
  cssLoader,
  stylusLoader
}
