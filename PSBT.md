# PSBT Handling

## Sample Unsigned
From BIP174 docs.
Uses `bitcoin-cli decodepsbt` to parse.
txid is `fed6cd1fde4db4e13e7e800317e37f9cbd75ec364389670eeff80da993c7e560`
```
bitcoin-cli -rpcuser=__cookie__ -rpcpassword=9e0ca21c70d6b7307b750c8012d12df04ef77d0c5754abc29a3ae31304d946ce decodepsbt cHNidP8BAKACAAAAAqsJSaCMWvfEm4IS9Bfi8Vqz9cM9zxU4IagTn4d6W3vkAAAAAAD+////qwlJoIxa98SbghL0F+LxWrP1wz3PFTghqBOfh3pbe+QBAAAAAP7///8CYDvqCwAAAAAZdqkUdopAu9dAy+gdmI5x3ipNXHE5ax2IrI4kAAAAAAAAGXapFG9GILVT+glechue4O/p+gOcykWXiKwAAAAAAAEHakcwRAIgR1lmF5fAGwNrJZKJSGhiGDR9iYZLcZ4ff89X0eURZYcCIFMJ6r9Wqk2Ikf/REf3xM286KdqGbX+EhtdVRs7tr5MZASEDXNxh/HupccC1AaZGoqg7ECy0OIEhfKaC3Ibi1z+ogpIAAQEgAOH1BQAAAAAXqRQ1RebjO4MsRwUPJNPuuTycA5SLx4cBBBYAFIXRNTfy4mVAWjTbr6nj3aAfuCMIAAAA
```

## TXID from PSBT
```
=btc -build-file %/lib/btc/hoon
=psbt 'cHNidP8BAKACAAAAAqsJSaCMWvfEm4IS9Bfi8Vqz9cM9zxU4IagTn4d6W3vkAAAAAAD+////qwlJoIxa98SbghL0F+LxWrP1wz3PFTghqBOfh3pbe+QBAAAAAP7///8CYDvqCwAAAAAZdqkUdopAu9dAy+gdmI5x3ipNXHE5ax2IrI4kAAAAAAAAGXapFG9GILVT+glechue4O/p+gOcykWXiKwAAAAAAAEHakcwRAIgR1lmF5fAGwNrJZKJSGhiGDR9iYZLcZ4ff89X0eURZYcCIFMJ6r9Wqk2Ikf/REf3xM286KdqGbX+EhtdVRs7tr5MZASEDXNxh/HupccC1AaZGoqg7ECy0OIEhfKaC3Ibi1z+ogpIAAQEgAOH1BQAAAAAXqRQ1RebjO4MsRwUPJNPuuTycA5SLx4cBBBYAFIXRNTfy4mVAWjTbr6nj3aAfuCMIAAAA'

(get-txid:psbt:btc psbt)
::  gives [wid=%32 dat=0xfed6.cd1f.de4d.b4e1.3e7e.8003.17e3.7f9c.bd75.ec36.4389.670e.eff8.0da9.93c7.e560]
```

## Making Derivation Paths
Below is hex for `m/84'/0'/0'/0/0`
`*80 is the ending for hardened paths*
54.00.00.80 00.00.00.80 00.00.00.80 00.00.00.00 00.00.00.00.00`
