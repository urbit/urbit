import { loadEnv, defineConfig } from 'vite';
import reactRefresh from '@vitejs/plugin-react-refresh';
import analyze from 'rollup-plugin-analyzer';
import { visualizer } from 'rollup-plugin-visualizer';
import { urbitPlugin } from '@urbit/vite-plugin-urbit';

export default ({ mode }: { mode: string }) => {
  Object.assign(process.env, loadEnv(mode, process.cwd()));
  const SHIP_URL =
    process.env.SHIP_URL ||
    process.env.VITE_SHIP_URL ||
    'http://localhost:8080';
  console.log(SHIP_URL);

  const plugins = () => {
    return [
      urbitPlugin({ base: 'webterm', target: SHIP_URL, secure: false }),
      reactRefresh()
    ];
  };

  return defineConfig({
    // base: 'webterm',
    build:
      mode !== 'profile'
        ? {}
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
    plugins: plugins()
  });
};
