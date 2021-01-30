# Tests

```
|commit %home
=btc -build-file %/lib/btc/hoon
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
run:test
```

## btc-wallet-store
```
|commit %home
=test -build-file %/tests/lib/btc-wallet-store/hoon
((slog test-vectors:test) ~)
```
