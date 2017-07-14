import babel from 'rollup-plugin-babel';
import commonjs from 'rollup-plugin-commonjs';
import nodeResolve from 'rollup-plugin-node-resolve';
import replace from 'rollup-plugin-replace';
import babili from 'rollup-plugin-babili';

const NODE_ENV = process.env.NODE_ENV || 'development';
const IS_PRODUCTION = NODE_ENV === 'production';

let config = {
  entry: './src/index.js',
  moduleName: 'guidejs',
  dest: './dist/bundle.js',
  format: 'iife',
  sourceMap: IS_PRODUCTION,
  plugins: [
    // TODO: For production, replace with production and minify.
    nodeResolve({
      module: true,
      jsnext: true,
      main: true,
    }),
    commonjs(),
    replace({
      'process.env.NODE_ENV': JSON.stringify(NODE_ENV),
    }),
    babel({
      exclude: 'node_modules/**',
      runtimeHelpers: true,
    }),
  ]
};

// Reduces the size by about half.
if (IS_PRODUCTION) {
  console.log('Production build');
  config.plugins.unshift(
    babili({
      comments: false,
      sourceMap: true,
      mangle: true,
      evaluate: true
    })
  );
} else {
  console.log('Development build');
}

export default config;
