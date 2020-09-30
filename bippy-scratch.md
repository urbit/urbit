
## base58
Converts a base58 zpub to hex
```
+bip84 "zpub6rFR7y4Q2AijBEqTUquhVz398htDFrtymD9xYYfG1m4wAcvPhXNfE3EfH1r1ADqtfSdVCToUG868RvUUkgDKf31mGDtKsAYz2oz2AGutZYs"
```

### btc-address
Test xpub parsing
```
=btca -build-file %/lib/btc-address/hoon
=px (parse-xpub:btca "zpub6rFR7y4Q2AijBEqTUquhVz398htDFrtymD9xYYfG1m4wAcvPhXNfE3EfH1r1ADqtfSdVCToUG868RvUUkgDKf31mGDtKsAYz2oz2AGutZYs")
=pubk ?~  pp  ~  pubk.u.pp
(is-point:btca pubk)
```
