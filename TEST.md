# Tests

```
|commit %home
=btc -build-file %/lib/btc/hoon
=b158 -build-file %/lib/bip158/hoon
```

## BTC libs
```
|commit %home
=test -build-file %/tests/lib/btc/hoon
run:test
```

## BIP158
```
|commit %home
=test -build-file %/tests/lib/bip158/hoon
((slog test-all-vectors:test) ~)
```

## btc-wallet-store
```
|commit %home
=test -build-file %/tests/lib/btc-wallet-store/hoon
((slog test-all-vectors:test) ~)
```
