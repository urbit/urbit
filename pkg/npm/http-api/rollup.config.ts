import { nodeResolve } from '@rollup/plugin-node-resolve';
import commonJS from '@rollup/plugin-commonjs';
import { terser } from 'rollup-plugin-terser';
import babel from '@rollup/plugin-babel';
import typescript from 'rollup-plugin-typescript2';

const input = ['src/index.ts'];

// Skip certain warnings
function onwarn(warning) {
  if (warning.code === 'THIS_IS_UNDEFINED') {
    return;
  }

  console.warn(warning.message);
}

export default [
  {
    input,
    onwarn,
    plugins: [
      nodeResolve({
        extensions: ['.js', '.jsx', '.ts', '.tsx'],
      }),
      commonJS(),
      typescript(),
      babel({
        babelHelpers: 'bundled',
        exclude: ['node_modules/**'],
      }),
      terser({
        ecma: 2017,
        compress: true,
        mangle: true,
      }),
    ],
    output: {
      file: `dist/urbit-http-api.min.js`,
      format: 'umd',
      name: 'UrbitHttpApi', // this is the name of the global object
      esModule: false,
      exports: 'named',
      sourcemap: true,
    },
  },
  {
    input,
    onwarn,
    plugins: [
      nodeResolve({
        extensions: ['.js', '.jsx', '.ts', '.tsx'],
      }),
      commonJS(),
      typescript(),
    ],
    output: [
      {
        file: 'dist/esm/index.js',
        format: 'esm',
        exports: 'named',
        sourcemap: true,
      },
      {
        file: 'dist/cjs/index.cjs',
        format: 'cjs',
        exports: 'named',
        sourcemap: true,
      },
    ],
  },
];
