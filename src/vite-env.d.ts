/// <reference types="vite/client" />

interface ImportMetaEnv {
  readonly VITE_WEBSITE_URL: string
  readonly VITE_BACKEND_URL: string
  readonly VITE_BACKEND_API_KEY?: string
  readonly VITE_PROVER_URL: string
  readonly VITE_ROLLUP_URL: string
  readonly VITE_ROLLUP_API_KEY?: string
}

interface ImportMeta {
  readonly env: ImportMetaEnv
}
