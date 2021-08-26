import { defineConfig } from 'vite';
import analyze from 'rollup-plugin-analyzer';
import { visualizer } from 'rollup-plugin-visualizer';
import reactRefresh from '@vitejs/plugin-react-refresh';
import htmlPlugin from 'vite-plugin-html-config';

const htmlPluginOpt = {
  headScripts: [{ src: '/apps/grid/desk.js' }, { src: '/session.js' }]
};
const SHIP_URL = process.env.SHIP_URL || 'http://localhost:8080';

// https://vitejs.dev/config/
export default defineConfig(({ mode }) => ({
  base: mode === 'mock' ? undefined : '/apps/grid/',
  server:
    mode === 'mock'
      ? undefined
      : {
          proxy: {
            '^/apps/grid/desk.js': {
              target: SHIP_URL
            },
            '^((?!/apps/grid).)*$': {
              target: SHIP_URL
            }
          }
        },
  build:
    mode !== 'profile'
      ? undefined
      : {
          rollupOptions: {
            plugins: [
              analyze({
                limit: 20
              }),
              visualizer()
            ]
          }
        },
  plugins: [htmlPlugin(htmlPluginOpt), reactRefresh()]
}));
