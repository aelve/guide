module.exports = {
  presets: [
    ['@babel/env', {
      targets: {
        browsers: ['last 3 versions', '> 2%', 'ie >= 10', 'Firefox >= 30', 'Chrome >= 30']
      },
      modules: 'commonjs',
      useBuiltIns: 'usage'
    }
    ],
    '@vue/babel-preset-jsx'
  ],
  plugins: [
    '@babel/plugin-transform-runtime',
    '@babel/plugin-syntax-dynamic-import'
  ]
}
