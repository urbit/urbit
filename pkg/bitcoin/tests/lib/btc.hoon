/-  bc=bitcoin
/+  *test, *btc
|%
+$  wallet-vector
  $:  =xpub:bc
      =chyg
      =idx:bc
      =address:bc
  ==
+$  vector
  $:  =xpub:bc
      eny=@uv
      block=@ud
      feyb=sats
      ins=(list insel)
      outs=(list txo)
      expect=[selected=(unit (list insel)) chng=(unit sats:bc)]
  ==
++  mk-utxo
  |=  value=sats:bc
  ^-  utxo:bc
  :*  pos=0
      [wid=32 dat=0xc493.f6f1.4668.5f76.b44f.0c77.ca88.120c.b8bc.89f5.34fe.69b6.8288.27b9.74e6.8849]
      height=3
      value
      recvd=~
  ==
::
++  fprint  4^0xdead.beef
::
++  wallet-vectors
  ^-  (list wallet-vector)
  :~  :*  'zpub6rFR7y4Q2AijBEqTUquhVz398htDFrtymD9xYYfG1m4wAcvPhXNfE3EfH1r1ADqtfSdVCToUG868RvUUkgDKf31mGDtKsAYz2oz2AGutZYs'
          %0
          0
          [%bech32 'bc1qcr8te4kr609gcawutmrza0j4xv80jy8z306fyu']
      ==
  ==
::
++  vectors
  =|  w=walt
  ^-  (list vector)
  :~  :*  'zpub6rFR7y4Q2AijBEqTUquhVz398htDFrtymD9xYYfG1m4wAcvPhXNfE3EfH1r1ADqtfSdVCToUG868RvUUkgDKf31mGDtKsAYz2oz2AGutZYs'
          0v3uc.iuebi.5qilc.l8d87.c1k6n.7iksq.nkobs.8s5he.raq40.9ff0b.5tj3u.kjtg7.aq59e.hatv7.oioam.mlsr4.pqqcd.cnbjn.pnpi2.1m5rt.k4scg
          999
          10
          :~  [(mk-utxo 200.000) %0 1]
              [(mk-utxo 500.000) %0 2]
              [(mk-utxo 204) %0 3]
              [(mk-utxo 235.000) %1 2]
          ==
          :~  [[%bech32 'bc1q59u5epktervh6fxqay2dlph0wxu9hjnx6v8n66'] 200.100 ~]
              [[%bech32 'bc1qlwd7mw33uea5m8r2lsnsrkc7gp2qynrxsfxpfm'] 200.000 ~]
          ==
          :*  `~[[(mk-utxo 500.000) %0 2]]
              `332.500
          ==
      ==
      ::
      :*  'zpub6rFR7y4Q2AijBEqTUquhVz398htDFrtymD9xYYfG1m4wAcvPhXNfE3EfH1r1ADqtfSdVCToUG868RvUUkgDKf31mGDtKsAYz2oz2AGutZYs'
          0v1gt.mc4ca.lfs0m.q1dal.lqobu.mmlbd.2umnp.lj9dr.4pf4s.pvclr.dps96.4a6i8.rt6n9.krp0r.11kqu.ckqe4.1tmat.gr754.463aj.a4b41.jj7qg
          999
          10
          :~  [(mk-utxo 200.000) %0 1]
              [(mk-utxo 500.000) %0 2]
              [(mk-utxo 204) %0 3]
              [(mk-utxo 235.000) %1 2]
          ==
          :~  [[%bech32 'bc1q59u5epktervh6fxqay2dlph0wxu9hjnx6v8n66'] 200.100 ~]
              [[%bech32 'bc1qlwd7mw33uea5m8r2lsnsrkc7gp2qynrxsfxpfm'] 200.000 ~]
          ==
          :*  `~[[(mk-utxo 235.000) %1 2] [(mk-utxo 200.000) %0 1]]
              `297.500
          ==
      ==
      ::
      :*  'zpub6rFR7y4Q2AijBEqTUquhVz398htDFrtymD9xYYfG1m4wAcvPhXNfE3EfH1r1ADqtfSdVCToUG868RvUUkgDKf31mGDtKsAYz2oz2AGutZYs'
          0v1gt.mc4ca.lfs0m.q1dal.lqobu.mmlbd.2umnp.lj9dr.4pf4s.pvclr.dps96.4a6i8.rt6n9.krp0r.11kqu.ckqe4.1tmat.gr754.463aj.a4b41.jj7qg
          999
          10
          ~[[(mk-utxo 500.000) %0 2]]
          :~  [[%bech32 'bc1q59u5epktervh6fxqay2dlph0wxu9hjnx6v8n66'] 299.797 ~]
              [[%bech32 'bc1qlwd7mw33uea5m8r2lsnsrkc7gp2qynrxsfxpfm'] 200.000 ~]
          ==
          :*  *(unit (list insel))
              *(unit sats:bc)
          ==
      ==
      :*  'zpub6rFR7y4Q2AijBEqTUquhVz398htDFrtymD9xYYfG1m4wAcvPhXNfE3EfH1r1ADqtfSdVCToUG868RvUUkgDKf31mGDtKsAYz2oz2AGutZYs'
          0v1gt.mc4ca.lfs0m.q1dal.lqobu.mmlbd.2umnp.lj9dr.4pf4s.pvclr.dps96.4a6i8.rt6n9.krp0r.11kqu.ckqe4.1tmat.gr754.463aj.a4b41.jj7qg
          999
          10
          ~[[(mk-utxo 500.000) %0 2]]
          :~  [[%bech32 'bc1q59u5epktervh6fxqay2dlph0wxu9hjnx6v8n66'] 298.500 ~]
              [[%bech32 'bc1qlwd7mw33uea5m8r2lsnsrkc7gp2qynrxsfxpfm'] 200.000 ~]
          ==
          :*  `~[[(mk-utxo 500.000) %0 2]]
              *(unit sats:bc)
          ==
      ==
  ==
::
++  dust-output-vectors
  =|  w=walt
  ^-  (list vector)
  :~
      :*  'zpub6rFR7y4Q2AijBEqTUquhVz398htDFrtymD9xYYfG1m4wAcvPhXNfE3EfH1r1ADqtfSdVCToUG868RvUUkgDKf31mGDtKsAYz2oz2AGutZYs'
          0v1gt.mc4ca.lfs0m.q1dal.lqobu.mmlbd.2umnp.lj9dr.4pf4s.pvclr.dps96.4a6i8.rt6n9.krp0r.11kqu.ckqe4.1tmat.gr754.463aj.a4b41.jj7qg
          999
          10
          ~[[(mk-utxo 500.000) %0 2]]
          :~  [[%bech32 'bc1q59u5epktervh6fxqay2dlph0wxu9hjnx6v8n66'] 298.580 ~]
              [[%bech32 'bc1qlwd7mw33uea5m8r2lsnsrkc7gp2qynrxsfxpfm'] 204 ~]
          ==
          :*  `~[[(mk-utxo 500.000) %0 2]]
              *(unit sats:bc)
          ==
      ==
  ==
::
++  test-all-vectors
  ^-  tang
  |^  ;:  weld
          %+  category  "address generation/lookup"
          (zing (turn wallet-vectors address-gen-lookup))
          %+  category  "single-random-draw"
          (zing (turn vectors check-single-random-draw))
          ::
          %+  category  "select with change"
          (zing (turn vectors check-change))
          ::
          %+  category  "don't allow dust outputs"
          (zing (turn dust-output-vectors check-dust-output))
      ==
  ::
  ++  address-gen-lookup
    |=  v=wallet-vector
    =/  w=walt  (from-xpub xpub.v fprint ~ ~ ~)
    =/  =address  (~(mk-address wad w chyg.v) idx.v)
    =.  w  (~(update-address wad w chyg.v) address [%.n %0 0 *(set utxo:bc)]) 
    =/  [w2=walt c=chyg i=idx]  (need (address-coords address ~[w]))
    ;:  weld
      %+  expect-eq
        !>(address)
        !>(address.v)
      %+  expect-eq
        !>([w2 c i])
        !>([w chyg.v idx.v])
    ==
    ::
  ++  check-single-random-draw
    |=  v=vector
    =/  w=walt  (from-xpub xpub.v fprint ~ ~ ~)
    %+  expect-eq
      !>(selected.expect.v)
      !>((~(single-random-draw sut [w eny.v block.v ~ feyb.v outs.v]) ins.v))
  ::
  ++  check-change
    |=  v=vector
    =/  w=walt  (from-xpub xpub.v fprint ~ ~ ~)
    =.  wach.w
      %-  ~(gas by *(map address:bc addi))
      %+  turn  ins.v
      |=  i=insel
      :-  (~(mk-address wad w chyg.i) idx.i)
      [%.y %0 0 (sy ~[utxo.i])]
    %+  expect-eq
      !>(chng.expect.v)
      !>(chng:~(with-change sut [w eny.v block.v ~ feyb.v outs.v]))
  ::
  ++  check-dust-output
    |=  v=vector
    =/  w=walt  (from-xpub xpub.v fprint ~ ~ ~)
    =.  wach.w  (insels-to-wach w ins.v)
    %-  expect-fail
    |.(~(with-change sut [w eny.v block.v ~ feyb.v outs.v]))
  ::
  ++  insels-to-wach
    |=  [w=walt is=(list insel)]
    ^-  wach
    %-  ~(gas by *(map address:bc addi))
    %+  turn  is
    |=  i=insel
    :-  (~(mk-address wad w chyg.i) idx.i)
    [%.y %0 0 (sy ~[utxo.i])]
  --
  ::  if a non-change output is dust, error
  ::  change shouldn't be returned when change is dust
  ::
--
