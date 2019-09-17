var gulp = require('gulp');
var cssimport = require('gulp-cssimport');
var rollup = require('gulp-better-rollup');
var cssnano = require('cssnano');
var postcss = require('gulp-postcss');
var sucrase = require('@sucrase/gulp-plugin');
var minify = require('gulp-minify');
var rename = require('gulp-rename');
var del = require('del');

var resolve = require('rollup-plugin-node-resolve');
var commonjs = require('rollup-plugin-commonjs');
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


gulp.task('js-imports', function(cb) {
  return gulp.src('dist/index.js')
    .pipe(rollup({
      plugins: [
        commonjs({
          namedExports: {
            'node_modules/react/index.js': [ 'Component' ],
            'node_modules/react-is/index.js': [ 'isValidElementType' ],
          }
        }),
        rootImport({
          root: `${__dirname}/dist/js`,
          useEntry: 'prepend',
          extensions: '.js'
        }),
        globals(),
        resolve()
      ]
    }, 'umd'))
    .on('error', function(e){
      console.log(e);
      cb();
    })
    .pipe(gulp.dest('../../arvo/lib/lyre/'))
    .on('end', cb);
});


gulp.task('js-minify', function () {
  return gulp.src('../../arvo/lib/lyre/index.js')
    .pipe(minify())
    .pipe(gulp.dest('../../arvo/lib/lyre/'));
});


gulp.task('rename-index-min', function() {
  return gulp.src('../../arvo/lib/lyre/index-min.js')
    .pipe(rename('index.js'))
    .pipe(gulp.dest('../../arvo/lib/lyre/'))
});


gulp.task('clean-min', function() {
  return del(['../../arvo/lib/lyre/index-min.js'], {force: true})
});

gulp.task('urbit-copy', function () {
  let ret = gulp.src('../../arvo/**/*');

  urbitrc.URBIT_PIERS.forEach(function(pier) {
    ret = ret.pipe(gulp.dest(pier));
  });

  return ret;
});

gulp.task('js-bundle-dev', gulp.series('jsx-transform', 'js-imports'));
gulp.task('js-bundle-prod', gulp.series('jsx-transform', 'js-imports', 'js-minify'))

gulp.task('bundle-dev',
  gulp.series(
    gulp.parallel(
      'js-bundle-dev',
    ),
    'urbit-copy'
  )
);

gulp.task('bundle-prod',
  gulp.series(
    gulp.parallel(
      'js-bundle-prod',
    ),
    'rename-index-min',
    'clean-min',
    'urbit-copy'
  )
);

gulp.task('default', gulp.series('bundle-dev'));

gulp.task('watch', gulp.series('default', function() {

  gulp.watch('src/**/*.js', gulp.parallel('js-bundle-dev'));

  gulp.watch('../../arvo/**/*', gulp.parallel('urbit-copy'));
}));
