var gulp = require('gulp');
var cssimport = require('gulp-cssimport');
var rollup = require('gulp-better-rollup');
var cssnano = require('cssnano');
var postcss = require('gulp-postcss')
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

gulp.task('css-bundle', function() {
  let plugins = [
    cssnano()
  ];
  return gulp
    .src('src/index.css')
    .pipe(cssimport())
    .pipe(postcss(plugins))
    .pipe(gulp.dest('../../arvo/app/publish/css'));
});

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

gulp.task('js-imports', function(cb) {
  return gulp.src('dist/index.js')
    .pipe(rollup({
      plugins: [
        commonjs({
          namedExports: {
            'node_modules/react/index.js': ['Component', 'cloneElement', 
            'createContext', 'createElement', 'useState', 'useRef', 
            'useLayoutEffect', 'useMemo', 'useEffect', 'forwardRef', 'useContext', 'Children' ],
            'node_modules/react-is/index.js': [ 'isValidElementType', 'isElement', 'ForwardRef' ],
            'node_modules/react-dom/index.js': [ 'createPortal' ]
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
    .pipe(gulp.dest('../../arvo/app/publish/js/'))
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
        globals(),
        resolve()
      ]
    }, 'umd'))
    .on('error', function(e){
      console.log(e);
      cb();
    })
    .pipe(gulp.dest('../../arvo/app/publish/js/'))
    .on('end', cb);
});

gulp.task('js-imports-prod', function(cb) {
  return gulp.src('dist/index.js')
    .pipe(rollup({
      plugins: [
        rollupReplace({'process.env.NODE_ENV': JSON.stringify('production')}),
        commonjs({
          namedExports: {
            'node_modules/react/index.js': ['Component', 'cloneElement', 
            'createContext', 'createElement', 'useState', 'useRef', 
            'useLayoutEffect', 'useMemo', 'useEffect', 'forwardRef', 'useContext', 'Children' ],
            'node_modules/react-is/index.js': [ 'isValidElementType', 'isElement', 'ForwardRef' ],
            'node_modules/react-dom/index.js': [ 'createPortal' ]
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
    .pipe(gulp.dest('../../arvo/app/publish/js/'))
    .on('end', cb);
});


gulp.task('js-minify', function () {
  return gulp.src('../../arvo/app/publish/js/index.js')
    .pipe(minify())
    .pipe(gulp.dest('../../arvo/app/publish/js/'));
});

gulp.task('tile-js-minify', function () {
  return gulp.src('../../arvo/app/publish/js/tile.js')
    .pipe(minify())
    .pipe(gulp.dest('../../arvo/app/publish/js/'));
});

gulp.task('rename-index-min', function() {
  return gulp.src('../../arvo/app/publish/js/index-min.js')
    .pipe(rename('index.js'))
    .pipe(gulp.dest('../../arvo/app/publish/js/'));
});

gulp.task('rename-tile-min', function() {
  return gulp.src('../../arvo/app/publish/js/tile-min.js')
    .pipe(rename('tile.js'))
    .pipe(gulp.dest('../../arvo/app/publish/js/'));
});

gulp.task('clean-min', function() {
  return del(['../../arvo/app/publish/js/index-min.js', '../../arvo/app/publish/js/tile-min.js'], {force: true})
});

gulp.task('urbit-copy', function () {
  let ret = gulp.src('../../arvo/**/*');

  urbitrc.URBIT_PIERS.forEach(function(pier) {
    ret = ret.pipe(gulp.dest(pier));
  });

  return ret;
});

gulp.task('js-bundle-dev', gulp.series('jsx-transform', 'js-imports'));
gulp.task('tile-js-bundle-dev', gulp.series('tile-jsx-transform', 'tile-js-imports'));
gulp.task('js-bundle-prod', gulp.series('jsx-transform', 'js-imports-prod', 'js-minify'))
gulp.task('tile-js-bundle-prod', 
  gulp.series('tile-jsx-transform', 'tile-js-imports', 'tile-js-minify'));

gulp.task('bundle-dev',
  gulp.series(
    gulp.parallel(
      'css-bundle',
      'js-bundle-dev',
      'tile-js-bundle-dev'
    ),
    'urbit-copy'
  )
);

gulp.task('bundle-prod',
  gulp.series(
    gulp.parallel(
      'css-bundle',
      'js-bundle-prod',
      'tile-js-bundle-prod',
    ),
    'rename-index-min',
    'rename-tile-min',
    'clean-min',
    'urbit-copy'
  )
);

gulp.task('default', gulp.series('bundle-dev'));

gulp.task('watch', gulp.series('default', function() {
  gulp.watch('tile/**/*.js', gulp.parallel('tile-js-bundle-dev'));

  gulp.watch('src/**/*.js', gulp.parallel('js-bundle-dev'));
  gulp.watch('src/**/*.css', gulp.parallel('css-bundle'));

  gulp.watch('../../arvo/**/*', gulp.parallel('urbit-copy'));
}));
