const { series, parallel, src, dest, watch } = require('gulp');
const rollup = require('gulp-better-rollup');
const sucrase = require('@sucrase/gulp-plugin');
const minify = require('gulp-minify');
const rename = require('gulp-rename');
const del = require('del');

const resolve = require('rollup-plugin-node-resolve');
const commonjs = require('rollup-plugin-commonjs');
const replace = require('rollup-plugin-replace');
const json = require('rollup-plugin-json');
const builtins = require('@joseph184/rollup-plugin-node-builtins');
const rootImport = require('rollup-plugin-root-import');
const globals = require('rollup-plugin-node-globals');

function transform(input) {
  return src(input)
    .pipe(sucrase({
      transforms: [ 'jsx' ]
    }))
    .pipe(dest('build/'));
}

function tile_jsx_transform() {
  return transform('tile/**/*.js');
}

const namedExportsTile = {
  'node_modules/react/index.js': [
    'Component'
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
    json(),
    globals(),
    builtins(),
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

function tile_js_imports(cb) {
  importer('build/tile.js', importPlugins(namedExportsTile))(cb);
}

function minifier(input) {
  return function(cb) {
    src(input)
      .pipe(minify())
      .pipe(dest('dist/'));
    cb();
  }
}

function tile_js_minify(cb) {
  return minifier('dist/tile.js')(cb);
}

function clean(cb) {
  del([ 'dist', 'build' ]);
  cb();
}

exports.bundle_dev = series(tile_jsx_transform, tile_js_imports);

exports.bundle_prod = series(
  tile_jsx_transform,
  tile_js_imports,
  tile_js_minify
);

exports.clean = clean;

exports.default = function() {
  watch('tile/**/*.js', exports.bundle_dev);
}

