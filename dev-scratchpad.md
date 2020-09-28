
## base58
Converts a base58 zpub to hex
```
=zpub-atom (de-base58:mimes:html "zpub6rFR7y4Q2AijBEqTUquhVz398htDFrtymD9xYYfG1m4wAcvPhXNfE3EfH1r1ADqtfSdVCToUG868RvUUkgDKf31mGDtKsAYz2oz2AGutZYs")
=zpub-bytes (turn (flop (rip 3 zpub)) |=(a=@ `@ux`a))
```
