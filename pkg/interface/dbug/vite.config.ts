import { defineConfig } from 'vite'
import react from '@vitejs/plugin-react'

const rollupOptions = {
  output: {
    assetFileNames: '[name][extname]',
    chunkFileNames: '[name].js',
    entryFileNames: '[name].js',
    hashCharacters: 'base36' as any
  }
};

// https://vitejs.dev/config/
export default defineConfig({
  base: '/~debug',
  build: {rollupOptions},
  plugins: [react()],
})
