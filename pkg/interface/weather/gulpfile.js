var gulp = require('gulp');
var cssimport = require('gulp-cssimport');
var rollup = require('gulp-better-rollup');
var cssnano = require('cssnano');
var autoprefixer = require('autoprefixer');
var postcss = require('gulp-postcss')
var sucrase = require('@sucrase/gulp-plugin');
var minify = require('gulp-minify');
var exec = require('child_process').exec;
var rename = require('gulp-rename');
var del = require('del');

var resolve = require('rollup-plugin-node-resolve');
var commonjs = require('rollup-plugin-commonjs');
var replace = require('rollup-plugin-replace');
var json = require('rollup-plugin-json');
var builtins = require('@joseph184/rollup-plugin-node-builtins');
var rootImport = require('rollup-plugin-root-import');
var globals = require('rollup-plugin-node-globals');

/***
  Main config options
***/

var urbitrc = require('../urbitrc');

/***
  End main config options
***/

gulp.task('jsx-transform', function(cb) {
  return gulp.src('src/**/*.js')
    .pipe(sucrase({
      transforms: ['jsx']
    }))
    .pipe(gulp.dest('dist'));
});

gulp.task('tile-jsx-transform', function(cb) {
  return gulp.src('tile/**/*.js')
    .pipe(sucrase({
      transforms: ['jsx']
    }))
    .pipe(gulp.dest('dist'));
});


gulp.task('js-imports', function (cb) {
  return gulp.src('dist/index.js')
    .pipe(rollup({
      plugins: [
        commonjs({
          namedExports: {
            'node_modules/react/index.js': ['Component'],
            'node_modules/react-is/index.js': ['isValidElementType'],
          }
        }),
        rootImport({
          root: `${__dirname}/dist/js`,
          useEntry: 'prepend',
          extensions: '.js'
        }),
        json(),
        globals(),
        builtins(),
        resolve()
      ]
    }, 'umd'))
    .on('error', function (e) {
      console.log(e);
      cb();
    })
    .pipe(gulp.dest('../../arvo/app/weather/js/'))
    .on('end', cb);
});

gulp.task('tile-js-imports', function(cb) {
  return gulp.src('dist/tile.js')
    .pipe(rollup({
      plugins: [
        commonjs({
          namedExports: {
            'node_modules/react/index.js': [ 'Component' ],
          }
        }),
        rootImport({
          root: `${__dirname}/dist/js`,
          useEntry: 'prepend',
          extensions: '.js'
        }),
        json(),
        globals(),
        builtins(),
        resolve()
      ]
    }, 'umd'))
    .on('error', function(e){
      console.log(e);
      cb();
    })
    .pipe(gulp.dest('../../arvo/app/weather/js/'))
    .on('end', cb);
});


gulp.task('js-minify', function () {
  return gulp.src('../../arvo/app/weather/js/index.js')
    .pipe(minify())
    .pipe(gulp.dest('../../arvo/app/weather/js/'));
});

gulp.task('tile-js-minify', function () {
  return gulp.src('../../arvo/app/weather/js/tile.js')
    .pipe(minify())
    .pipe(gulp.dest('../../arvo/app/weather/js/'));
});

gulp.task('rename-index-min', function() {
  return gulp.src('../../arvo/app/weather/js/index-min.js')
    .pipe(rename('index.js'))
    .pipe(gulp.dest('../../arvo/weather/chat/js/'));
});

gulp.task('rename-tile-min', function() {
  return gulp.src('../../arvo/app/weather/js/tile-min.js')
    .pipe(rename('tile.js'))
    .pipe(gulp.dest('../../arvo/app/weather/js/'));
});

gulp.task('clean-min', function(){
  return del('../../arvo/app/weather/js/tile-min.js', {force: true})
})

gulp.task('urbit-copy', function () {
  let ret = gulp.src('../../arvo/**/*');

  urbitrc.URBIT_PIERS.forEach(function(pier) {
    ret = ret.pipe(gulp.dest(pier));
  });

  return ret;
});

gulp.task('tile-js-bundle-dev', gulp.series('tile-jsx-transform', 'tile-js-imports'));
gulp.task('tile-js-bundle-prod',
  gulp.series('tile-jsx-transform', 'tile-js-imports', 'tile-js-minify'));

gulp.task('bundle-prod', 
  gulp.series('tile-js-bundle-prod', 'rename-tile-min', 'clean-min', 'urbit-copy'));

gulp.task('default', gulp.series('tile-js-bundle-dev', 'urbit-copy'));
gulp.task('watch', gulp.series('default', function () {
  gulp.watch('tile/**/*.js', gulp.parallel('tile-js-bundle-dev'));
  gulp.watch('../../arvo/**/*', gulp.parallel('urbit-copy'));
}));
