import { defineConfig } from 'vite'
import react from '@vitejs/plugin-react-swc'

// basePathを構築
const getBasePath = () => {
  // GitHub Actionsの場合
  if (process.env.GITHUB_REF_NAME) {
    const branch = process.env.GITHUB_REF_NAME === 'main'
      ? ''
      : process.env.GITHUB_REF_NAME
    return `/postgresql-cst-parser/${branch}`
  }
  // ローカル開発の場合
  return '/'
}

// https://vitejs.dev/config/
export default defineConfig({
  plugins: [react()],
  base: getBasePath(),
  build: {
    outDir: '../docs',
  }
})
