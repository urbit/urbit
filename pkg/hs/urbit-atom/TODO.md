- Use atomBytes instead of exportBytes where faster.
  - `exportBytes` is slower, use `atomBytes` where possible.
    - Only works for little-endian.
    - Doesn't work in GHCJS.

- Use bytesAtom instead of importBytes where faster.
  - `importBytes` is always faster. Always use it.
    - (Why slower, tho?)

- Get this working with `integer-simple` (for Android).
