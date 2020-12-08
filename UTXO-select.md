# utxo selection

Creates dummy inputs and outputs. Builds a TX with them.
```
=btc -build-file %/lib/btc/hoon
=bwsl -build-file %/lib/btc-wallet-store/hoon
=u (utxo:btc [pos=0 (hash256:btc [wid=32 dat=0xc493.f6f1.4668.5f76.b44f.0c77.ca88.120c.b8bc.89f5.34fe.69b6.8288.27b9.74e6.8849]) height=3 value=0 ~])
=val0 200.000
=val1 500.000
=val2 30
=val3 235.000

=eny1 0v3uc.iuebi.5qilc.l8d87.c1k6n.7iksq.nkobs.8s5he.raq40.9ff0b.5tj3u.kjtg7.aq59e.hatv7.oioam.mlsr4.pqqcd.cnbjn.pnpi2.1m5rt.k4scg
=eny2 0v1gt.mc4ca.lfs0m.q1dal.lqobu.mmlbd.2umnp.lj9dr.4pf4s.pvclr.dps96.4a6i8.rt6n9.krp0r.11kqu.ckqe4.1tmat.gr754.463aj.a4b41.jj7qg
=w *walt:bwsl
=w w(bipt %bip84, confs 6)
=inputs ~[[u(value val0) %0 0] [u(value val1) %0 2] [u(value val2) %0 1] [u(value val3) %1 2]]
=outputs ~[[[%bech32 'bc1q59u5epktervh6fxqay2dlph0wxu9hjnx6v8n66'] value=200.100] [[%bech32 'bc1qlwd7mw33uea5m8r2lsnsrkc7gp2qynrxsfxpfm'] value=200.000]]

(~(single-random-draw sut:bwsl [w eny1 923 ~ 100 outputs]) inputs)
(~(single-random-draw sut:bwsl [w eny2 923 ~ 100 outputs]) inputs)
```
Above tests w 2 outputs, total fees with 2 inputs of 27.500. Gives:
1. 500.000 input
2. Inputs 0 and 3
