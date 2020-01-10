const { series, parallel, src, dest, watch } = require('gulp');
const cssimport = require('gulp-cssimport');
const rollup = require('gulp-better-rollup');
const cssnano = require('cssnano');
const postcss = require('gulp-postcss')
const sucrase = require('@sucrase/gulp-plugin');
const minify = require('gulp-minify');
const rename = require('gulp-rename');
const del = require('del');
const resolve = require('rollup-plugin-node-resolve');
const commonjs = require('rollup-plugin-commonjs');
const rootImport = require('rollup-plugin-root-import');
const globals = require('rollup-plugin-node-globals');
const replace = require('@rollup/plugin-replace');

function css_bundle() {
  return src('src/index.css')
    .pipe(cssimport())
    .pipe(postcss([
      cssnano()
    ]))
    .pipe(dest('dist/'));
}

function transform(input) {
  return src(input)
    .pipe(sucrase({
      transforms: [ 'jsx' ]
    }))
    .pipe(dest('build/'));
}

function jsx_transform() {
  return transform('src/**/*.js');
}

const namedExports = {
  'node_modules/react/index.js': [
    'Component'
  ],
  'node_modules/react-is/index.js': [
    'isValidElementType'
  ],
};

const prodPlugins = [
  replace({ 'process.env.NODE_ENV': 'production' })
];

function importPlugins(exps) {
  return [
    commonjs({ namedExports: exps }),
    rootImport({
      root: `${__dirname}/build/js`,
      useEntry: 'prepend',
      extensions: '.js'
    }),
    globals(),
    resolve()
  ];
}

function importPluginsProd(exps) {
  return prodPlugins.concat(importPlugins(exps));
}

function importer(input, plugins) {
  return function(cb) {
    src(input)
      .pipe(rollup({ plugins }, 'umd'))
      .on('error', function(e){
        console.log(e);
        cb();
      })
      .pipe(dest('dist/'))
      .on('end', cb);
  }
}

function js_imports(cb) {
  importer('build/index.js', importPlugins(namedExports))(cb);
}

function js_imports_prod(cb) {
  importer('build/index.js', importPluginsProd(namedExports))(cb);
}

function minifier(input) {
  return function(cb) {
    src(input)
      .pipe(minify())
      .pipe(dest('dist/'));
    cb();
  }
}

function js_minify(cb) {
  return minifier('dist/index.js')(cb);
}

function clean(cb) {
  del([ 'dist', 'build' ]);
  cb();
}

exports.bundle_dev = parallel(
  css_bundle,
  series(jsx_transform, js_imports),
);

exports.bundle_prod = parallel(
  css_bundle,
  series(jsx_transform, js_imports_prod, js_minify),
);

exports.clean = clean;

exports.default = function() {
  watch('src/**/*.css', css_bundle);
  watch('src/**/*.js', exports.bundle_dev);
}

