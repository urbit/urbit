var gulp = require('gulp');
var cssimport = require('gulp-cssimport');
var cssnano = require('gulp-cssnano');
var rollup = require('gulp-better-rollup');
var sucrase = require('@sucrase/gulp-plugin');
var minify = require('gulp-minify');
var exec = require('child_process').exec;

var resolve = require('rollup-plugin-node-resolve');
var commonjs = require('rollup-plugin-commonjs');
var replace = require('rollup-plugin-replace');
var json = require('rollup-plugin-json');
var builtins = require('rollup-plugin-node-builtins');
var rootImport = require('rollup-plugin-root-import');
var globals = require('rollup-plugin-node-globals');

/***
  Main config options
***/

var urbitrc = require('./.urbitrc');

/***
  End main config options
***/

gulp.task('chat-css-bundle', function() {
  return gulp
    .src('apps/chat/src/index.css')
    .pipe(cssimport())
    .pipe(cssnano())
    .pipe(gulp.dest('./apps/chat/urbit/app/chat/css'));
});

gulp.task('chat-jsx-transform', function(cb) {
  return gulp.src('apps/chat/src/**/*.js')
    .pipe(sucrase({
      transforms: ['jsx']
    }))
    .pipe(gulp.dest('apps/chat/dist'));
});

gulp.task('chat-js-imports', function(cb) {
  return gulp.src('apps/chat/dist/index.js')
    .pipe(rollup({
      plugins: [
        commonjs({
          namedExports: {
            'apps/chat/node_modules/react/index.js': [ 'Component' ],
            'apps/chat/node_modules/react-is/index.js': [ 'isValidElementType' ],
          }
        }),
        replace({
          'process.env.NODE_ENV': JSON.stringify('development')
        }),
        rootImport({
          root: `${__dirname}/apps/chat/dist/js`,
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
    .pipe(gulp.dest('./apps/chat/urbit/app/chat/js/'))
    .on('end', cb);
});

gulp.task('chat-js-minify', function () {
  return gulp.src('./apps/chat/urbit/app/chat/js/index.js')
    .pipe(minify())
    .pipe(gulp.dest('./apps/chat/urbit/app/chat/js/'));
});

gulp.task('chat-urbit-copy', function () {
  let ret = gulp.src('apps/chat/urbit/**/*');

  urbitrc.URBIT_PIERS.forEach(function(pier) {
    ret = ret.pipe(gulp.dest(pier));
  });

  return ret;
});

gulp.task('chat-js-bundle-dev', gulp.series('chat-jsx-transform', 'chat-js-imports'));
gulp.task('chat-js-bundle-prod', gulp.series('chat-jsx-transform', 'chat-js-imports', 'chat-js-minify'))

gulp.task('chat-bundle-dev',
  gulp.series(
    gulp.parallel(
      'chat-css-bundle',
      'chat-js-bundle-dev'
    ),
    'chat-urbit-copy'
  )
);

gulp.task('chat-bundle-prod',
  gulp.series(
    gulp.parallel(
      'chat-css-bundle',
      'chat-js-bundle-prod'
    ),
    'chat-urbit-copy'
  )
);

gulp.task('default', gulp.series('chat-bundle-dev'));
gulp.task('chat-watch', gulp.series('default', function() {
  gulp.watch('apps/chat/src/**/*.js', gulp.parallel('chat-js-bundle-dev'));
  gulp.watch('apps/chat/src/**/*.css', gulp.parallel('chat-css-bundle'));

  gulp.watch('apps/chat/urbit/**/*', gulp.parallel('chat-urbit-copy'));
}));
