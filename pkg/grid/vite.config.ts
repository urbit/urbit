import { defineConfig } from 'vite';
import analyze from 'rollup-plugin-analyzer';
import { visualizer } from 'rollup-plugin-visualizer';
import reactRefresh from '@vitejs/plugin-react-refresh';

// https://vitejs.dev/config/
export default defineConfig(({ mode }) => ({
  base: mode === 'mock' ? undefined : '/apps/grid/',
  server:
    mode === 'mock'
      ? undefined
      : {
          proxy: {
            '^((?!/apps/grid).)*$': {
              target: 'http://localhost:8083'
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
  plugins: [reactRefresh()]
}));
