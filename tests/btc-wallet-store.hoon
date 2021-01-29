/+  *btc-wallet-store, btc
=/  blank-utxo=utxo:btc
  :*  pos=0
     [wid=32 dat=0xc493.f6f1.4668.5f76.b44f.0c77.ca88.120c.b8bc.89f5.34fe.69b6.8288.27b9.74e6.8849]
     height=3
     value=0
     ~
  ==
|%
++  utxo-vals  `(list sats:btc)`~[200.000 500.000 30 235.000]
++  eny1  0v3uc.iuebi.5qilc.l8d87.c1k6n.7iksq.nkobs.8s5he.raq40.9ff0b.5tj3u.kjtg7.aq59e.hatv7.oioam.mlsr4.pqqcd.cnbjn.pnpi2.1m5rt.k4scg
++  eny2  0v1gt.mc4ca.lfs0m.q1dal.lqobu.mmlbd.2umnp.lj9dr.4pf4s.pvclr.dps96.4a6i8.rt6n9.krp0r.11kqu.ckqe4.1tmat.gr754.463aj.a4b41.jj7qg
++  blank-walt
  =|  w=walt
  w(bipt %84, confs 6)
++  sample-utxo-set
  ^-  (list insel)
  :~  [blank-utxo(value (snag 0 utxo-vals)) %0 0]
      [blank-utxo(value (snag 1 utxo-vals)) %0 2]
      [blank-utxo(value (snag 2 utxo-vals)) %0 1]
      [blank-utxo(value (snag 3 utxo-vals)) %1 2]
  ==
::
++  run
  ::  random UTXO selection
  =/  outputs=(list txo)
    :~  [[%bech32 'bc1q59u5epktervh6fxqay2dlph0wxu9hjnx6v8n66'] value=200.100 ~]
        [[%bech32 'bc1qlwd7mw33uea5m8r2lsnsrkc7gp2qynrxsfxpfm'] value=200.000 ~]
    ==
  ?.  =(1 (lent (need (~(single-random-draw sut [blank-walt eny1 923 ~ 100 outputs]) sample-utxo-set))))
    ~|("Didn't select correct number of UTXOs" !!)
  ?.  =(2 (lent (need (~(single-random-draw sut [blank-walt eny2 923 ~ 100 outputs]) sample-utxo-set))))
    ~|("Didn't select correct number of UTXOs" !!)
  "All tests passed"
--
