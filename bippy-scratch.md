
## base58
Converts a base58 zpub to hex
```
=zpub-atom (de-base58:mimes:html "zpub6rFR7y4Q2AijBEqTUquhVz398htDFrtymD9xYYfG1m4wAcvPhXNfE3EfH1r1ADqtfSdVCToUG868RvUUkgDKf31mGDtKsAYz2oz2AGutZYs")
=zpub-bytes (turn (flop (rip 3 zpub-atom)) |=(a=@ `@ux`a))

+bip84 "zpub6rFR7y4Q2AijBEqTUquhVz398htDFrtymD9xYYfG1m4wAcvPhXNfE3EfH1r1ADqtfSdVCToUG868RvUUkgDKf31mGDtKsAYz2oz2AGutZYs"
```

### decoded hex from base58
Get the public key (start at 45 and take 33 bytes):
```
=as-atom (rap 3 (flop (swag [45 33] zpub-bytes)))
```
decompress-point:secp256k1:secp:crypto
