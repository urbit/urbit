# `btc-provider` tests

## Set Credentials, Ping Servers, Check Addresses and TXs
```
:btc-provider|command [%set-credentials api-url='http://localhost:50002']
:btc-provider|action ['blockfee' %ping ~]

:btc-provider|action ['addr0' %address-info [%bech32 'bc1qm7cegwfd0pvv9ypvz5nhstage00xkxevtrpshc']]
:btc-provider|action ['addr1' %address-info [%bech32 'bc1qlwd7mw33uea5m8r2lsnsrkc7gp2qynrxsfxpfm']]
:btc-provider|action ['addr2' %address-info [%bech32 'bc1qglkc9zfcn04vcc88nn0ljtxcpu5uxfznc3829k']]
::  first is an address w balance
::  second has no balance but is used
::  third is unused

:btc-provider|action ['reqid' %raw-tx [%32 0x33f.693f.df99.5a5e.a7fe.5c95.1ab6.858c.7e6a.5fff.c585.7992.2cd4.fc31.9c61.4c5b]]
:btc-provider|action ['reqid' %raw-tx [%32 0x2131.b660.7f25.0d31.d8da.9818.d2d9.2560.c7d6.7fe7.8ca4.0d02.6408.c090.6868.71e6]]
::  first is a 382 byte tx
::  second is a 27.660 byte tx

:btc-provider|action ['reqid' %tx-info [%32 0x9ece.9c56.9ab3.27db.ada5.a51e.2653.7b3f.7e99.5579.c18a.af4f.8620.304b.ce53.16f1]]
::  4 inputs, one output
```

##  Generate a Raw TX from Inputs/Outputs
```
:btc-provider|command [%set-credentials api-url='http://localhost:50002']
=inputs ~[[[%32 0xe86.8771.b3bf.f789.525e.21ba.c735.e280.70d1.eff0.fb4d.f59a.dc98.e914.8e2f.85d4] 0]]
=outputs ~[[[%bech32 'bc1q0ydcskwye4rqky4qankhl4kegajl8nh50plmx0'] 12.500.000]]

:btc-provider|action ['rawtx0' %create-raw-tx inputs outputs]
```
