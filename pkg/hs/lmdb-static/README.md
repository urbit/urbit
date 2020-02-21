This is a hack to avoid dynamic depencency on lmdb:

This is a vendoring of `haskell-lmdb` and `lmdb` modified to include
the c-build of `lmdb` statically into `haskell-lmdb`.

```
haskell-lmdb:
  repo: https://github.com/dmbarbour/haskell-lmdb.git
  hash: 1e562429874919d445576c87cf118d7de5112b5b
lmdb:
  repo: https://github.com/LMDB/lmdb.git
  hash: c3e6b4209eed13af4a3670e5f04f42169c08e5c6
```
