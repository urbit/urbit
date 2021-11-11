interface ImportMetaEnv extends Readonly<Record<string, string | boolean | undefined>> {
  readonly VITE_LAST_WIPE: string;
  readonly VITE_STORAGE_VERSION: string;
}

interface ImportMeta {
  readonly env: ImportMetaEnv;
}
