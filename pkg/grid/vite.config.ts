import { loadEnv, defineConfig } from 'vite';
import analyze from 'rollup-plugin-analyzer';
import { visualizer } from 'rollup-plugin-visualizer';
import reactRefresh from '@vitejs/plugin-react-refresh';
import { urbitPlugin } from '@urbit/vite-plugin-urbit';
import { execSync } from 'child_process';

// https://vitejs.dev/config/
export default ({ mode }) => {
  if (mode !== 'mock') {
    // using current commit until release
    const GIT_DESC = execSync('git rev-parse --short HEAD', { encoding: 'utf8' }).trim();
    process.env.VITE_SHORTHASH = GIT_DESC;
  } else {
    process.env.VITE_SHORTHASH = '1';
  }

  Object.assign(process.env, loadEnv(mode, process.cwd()));
  const SHIP_URL = process.env.SHIP_URL || process.env.VITE_SHIP_URL || 'http://localhost:8080';
  console.log(SHIP_URL);

  return defineConfig({
    base: mode === 'mock' ? undefined : '/apps/grid/',
    server: mode === 'mock' ? undefined : { https: true },
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
    plugins:
      mode === 'mock'
        ? []
        : [urbitPlugin({ base: 'grid', target: SHIP_URL, secure: false }), reactRefresh()]
  });
};
