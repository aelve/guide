const path = require('path')
const { DefinePlugin } = require('webpack')
const { VueLoaderPlugin } = require('vue-loader')
const FriendlyErrorsWebpackPlugin = require('friendly-errors-webpack-plugin')
const VuetifyLoaderPlugin = require('vuetify-loader/lib/plugin')
// TODO workaroun for https://github.com/TypeStrong/ts-loader/issues/653 Remove when fixed
const IgnoreNotFoundExportPlugin = require('ignore-not-found-export-webpack-plugin')

const isProduction = process.env.NODE_ENV === 'production'

const config = {
  mode: isProduction ? 'production' : 'development',
  devtool: isProduction ? false : 'cheap-module-source-map',
  output: {
    path: path.resolve(__dirname, '../dist/src'),
    publicPath: '/src/',
    // filename: '[name].[hash].js'
    filename: '[name].js'
  },
  resolve: {
    extensions: [
      '.mjs',
      '.js',
      '.jsx',
      '.ts',
      '.tsx'
    ],
    alias: {
      '~': path.resolve(__dirname, '../'),
      client: path.resolve(__dirname, '../client/'),
      utils: path.resolve(__dirname, '../utils/')
    },
    modules: ['node_modules']
  },
  stats: {
    modules: false
  },
  module: {
    rules: [
      {
        test: /\.vue$/,
        use: [
          {
            loader: 'vue-loader',
            options: {
              compilerOptions: {
                whitespace: 'condense'
              }
            }
          }
        ]
      },
      {
        test: /\.js$/,
        use: {
          loader: 'babel-loader'
        },
        // exclude: /node_modules\/(?!(vuetify)\/).*/
        exclude: /(node_modules)/
      },
      {
        test: /\.ts$/,
        use: [
          {
            // TODO update ts-loader when fixed https://github.com/TypeStrong/ts-loader/issues/653#issuecomment-390889335
            loader: 'ts-loader',
            options: {
              transpileOnly: true,
              appendTsSuffixTo: [/\.vue$/]
            }
          },
        ]
      },
      {
        test: /\.tsx$/,
        use: [
          {
            loader: 'babel-loader'
          },
          {
            loader: 'ts-loader',
            options: {
              transpileOnly: true,
              appendTsxSuffixTo: [/\.vue$/]
            }
          }
        ]
      },
      {
        test: /\.(png|jpe?g|gif|svg)(\?.*)?$/,
        use: [
          {
            loader: 'url-loader',
            options: {
              limit: 4096,
              fallback: {
                loader: 'file-loader',
                options: {
                  name: 'img/[name].[hash:8].[ext]'
                }
              }
            }
          }
        ]
      },
      {
        test: /\.css$/,
        use: [
          {
            loader: 'vue-style-loader',
            options: {
              sourceMap: false,
              shadowMode: false
            }
          },
          {
            loader: 'css-loader',
            options: {
              sourceMap: false,
              importLoaders: 2
            }
          },
          {
            loader: 'postcss-loader',
            options: {
              sourceMap: false
            }
          }
        ]
      },
      {
        test: /\.s(c|a)ss$/,
        use: [
          'vue-style-loader',
          'css-loader',
          {
            loader: 'sass-loader',
            options: {
              implementation: require('sass'),
              fiber: require('fibers'),
              indentedSyntax: true // optional
            }
          }
        ]
      },
      {
        test: /\.p(ost)?css$/,
        use: [
          {
            loader: 'vue-style-loader',
            options: {
              sourceMap: false,
              shadowMode: false
            }
          },
          {
            loader: 'css-loader',
            options: {
              sourceMap: false,
              importLoaders: 2
            }
          },
          {
            loader: 'postcss-loader',
            options: {
              sourceMap: false
            }
          }
        ]
      }
    ]
  },

  plugins: [
    new IgnoreNotFoundExportPlugin({
      include: [
        /Category/,
      ]
    }),
    new VueLoaderPlugin(),
    new FriendlyErrorsWebpackPlugin(),
    new DefinePlugin({
      'process.env': {
        'NODE_ENV': JSON.stringify(process.env.NODE_ENV),
        'PORT': JSON.stringify(process.env.PORT)
      }
    }),
    new VuetifyLoaderPlugin()
  ]
}

module.exports = config
