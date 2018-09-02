const autoprefixer = require('autoprefixer')

const plugins = [
  autoprefixer({
    browsers: ['last 3 versions', '> 2%', 'ie >= 9', 'Firefox >= 30', 'Chrome >= 30']
  })
]

module.exports = {
  plugins
}
