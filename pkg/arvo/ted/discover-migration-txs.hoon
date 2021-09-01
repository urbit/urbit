/-  spider, claz
/+  *strand, strandio, azio, *ethereum, *azimuth
::
::NOTE  be sure to empty out the /migrations dir prior to re-running export
::
=/  az
  %~  .  azio
  :-  'https://mainnet.infura.io/v3/2599df54929b47099bda360958d75aaf'
  =>  mainnet-contracts
  :*  azimuth
      ecliptic
      linear-star-release
      delegated-sending
  ==
::
=/  ceremony=address
  0x740d.6d74.1711.163d.3fca.cecf.1f11.b867.9a7c.7964
::
=/  deep-safe=address
  0x9f57.c77b.1095.bd5d.b055.8b9c.b9b8.e6fc.6737.5e3c
=/  lockup-safe=$-(@p address)
  |=  g=@p
  ?+  g  ~|([%no-safe-address g] !!)
    %~bec  0x9e7a.1400.efd3.9176.a32e.e6e4.5067.97da.cd73.0bb0
    %~bep  0xdb0b.b145.5c0c.ecd1.5c42.274c.03cf.025c.ae87.10d2
    %~ber  0x44c9.8aa6.c90f.530b.7742.b33a.45d5.2736.21e4.a354
    %~byl  0x8d17.0743.d09b.148d.4fe2.f13d.1824.3b8d.a4f2.8dfa
    %~byr  0x15a7.3ad5.fa95.c3a8.1b8b.b9ae.1e75.797a.fa07.49c1
    %~byt  0xac32.a4c1.719e.2cce.840c.1393.3685.220c.287d.de91
    %~deb  0x16af.eb33.f384.d99a.c273.90ee.6e19.c301.8931.2e2f
    %~dem  0xe105.c07b.ddfd.cc5c.02b1.4a02.7733.21ae.c416.117c
    %~dyt  0xbce9.e7d8.bdfb.8c6b.771b.3d43.4b62.9bdf.2368.30a7
    %~fen  0x20f7.bc4d.4c65.d4c3.fa2f.72b4.820c.72c3.190a.c4e8
    %~fur  0xfab2.6586.7b9d.ad9b.6048.3214.b83a.36e7.ee0f.e9b1
    %~fyl  0x89bd.1be2.fe3c.fd49.6a0b.f13a.b78f.63f8.2f4a.fb56
    %~hul  0xe151.61ff.8bd1.24d1.d38d.7d0c.36ce.8a59.98eb.25bb
    %~hus  0xba4b.373b.3bb5.f54c.9bec.55dc.245e.c26c.8025.ab0d
    %~hut  0x7275.d236.77d7.b300.f955.fc4c.23b9.045f.5c51.5b7c
    %~lec  0x87f6.fcce.1d14.515b.6cb9.1586.05a2.ac78.da54.5d6f
    %~len  0x9afc.8f2c.435c.e23f.ce24.1afb.80d8.ffe0.dd5e.a015
    %~lep  0x7031.a990.45de.0392.e128.9e35.c7fd.637b.1e8e.9fde
    %~ler  0xb558.18c0.5f38.2f39.22b2.6758.ed6d.234b.c011.b858
    %~lev  0xabb5.de65.bb69.ee8e.1389.be0a.490c.8e23.478a.c0f4
    %~luc   0x923.9e16.302c.c0a6.676b.a092.8ac4.4ff9.22a3.1992
    %~lud  0xc9b9.0bcf.2c45.b48d.efca.decd.638c.356a.e949.717f
    %~lyn  0x53e8.2723.01d4.db1a.57d3.604f.8b0c.b150.3d4c.efa1
    %~lyr  0x6d15.2c11.91ef.7238.76ff.5fce.e6c9.b6ac.b861.a73b
    %~lys  0x2d83.946b.530b.99e8.23e9.c989.6e0e.0f05.bd5d.1fce
    %~mel  0x3e7b.d722.9665.d19f.e319.b766.05ed.c0b4.3765.5d7b
    %~mex  0x6b92.5675.a51c.9ff2.2afa.e5f3.c2e0.bbdc.9d16.097c
    %~mug  0xbe2a.ec3d.9f95.1767.034e.8bce.739b.227f.4878.c3c4
    %~mun  0x16dd.55be.903e.0ed4.c6fb.dba7.03c3.c223.b76c.c8cd
    %~mur  0xf29c.1553.a5d8.1514.8ea6.b6df.e29a.bfe1.81c0.19b6
    %~myl  0xc135.363f.9b41.8740.cdfd.10c8.b38c.7ac9.1f21.5d17
    %~ned  0xaef1.0838.d3b0.1355.e253.1d99.2f70.0cfb.8467.ea96
    %~nel  0x52d4.8f2a.8b8b.2353.3a6d.407d.38b8.a19b.12ca.6df3
    %~ner  0x3142.2ace.f5b4.2363.f0e3.b2f9.090a.2136.7b76.63db
    %~nev  0x8a2d.1c9c.8946.b8ef.ebd1.9946.88ed.dd3f.9822.c747
    %~nyd  0xc1fb.4905.6ae1.4bfd.8146.30a2.1cc1.e902.d229.797b
    %~nyl  0x7a0c.7b16.1f70.6f3d.0b80.6e33.606f.623e.14f9.4197
    %~nys  0xd15c.6c28.48c4.e132.c925.d55f.3d6b.f417.99dd.be54
    %~pec   0xca7.f4fa.2931.18f7.f31c.d815.4d52.4232.3c9e.572e
    %~pex   0xa19.4d74.ee18.ffe5.b464.e74f.2ff7.45bf.9b77.90fc
    %~rem  0x3071.91f2.9432.3f6d.8416.8ca8.8d9c.8e69.ead6.38f6
    %~ret  0xb582.4001.cef4.324c.b211.606b.b10d.d69a.6fc3.591e
    %~ryc  0x57a5.0a09.bd03.78b0.154f.27d9.d0ac.87ae.9e8c.9ecc
    %~ryd  0xf37e.79d7.e1bb.7687.5d28.8d27.19af.afa6.4047.9812
    %~ryl  0x157c.68b1.0a3f.9311.ac6d.0186.2b69.203f.fe5e.08d8
    %~rym  0x6229.842f.1c82.43ec.f90d.c271.ec8d.069e.65b4.3e8a
    %~seb  0xd324.69f7.afef.e0c9.e959.2b46.0e5d.3109.8014.4be7
    %~sed  0x860d.04e3.cf48.56f1.697e.09a9.b64f.ab4b.4dfc.5d0b
    %~sen  0x17dc.f09a.e852.5d49.ad92.a319.e603.ce56.4c4c.a002
    %~sug   0x67a.3734.20e6.7e4c.0edb.b547.ed61.33b8.3eb8.c7a4
    %~syl  0x5375.4c51.bd2d.dccd.bb85.c8e6.3827.011d.2fed.646b
    %~tec  0xafa9.6b1a.f942.cd9a.e789.c5e4.d86e.4c71.7450.3efa
    %~teg  0x8ba8.a3e0.ac02.fe8f.91f3.0afc.d9f1.9720.b5ad.04a4
    %~tel    0xb2.e3c4.347e.314a.931b.fff8.99d2.3922.2ec8.cc9f
    %~tes  0xb17c.5cd5.6712.33bc.1697.b50e.8298.d401.110b.90bb
    %~tuc  0xa029.107e.4e56.4fcd.8584.8987.4e12.2117.ce40.c135
    %~tun  0x149b.0c5e.1e91.911c.141c.24db.4386.589b.4e08.ba09
    %~tus  0x71d1.8fd8.f78c.4afd.a917.a629.7f61.1f01.2b05.d1c6
    %~wed   0xc6d.f9ca.3805.b950.9459.6cd6.fb24.d335.b7ad.ca0e
    %~weg  0x8fd1.00c6.8015.9934.68bd.41c7.410e.4604.4111.131f
    %~wer  0xece1.ed6f.a335.350a.242a.ddc9.7493.b781.ce17.f960
    %~bel  0x77aa.1a0f.716e.98be.1fc7.9685.54b2.c655.86cb.0d89
    %~ben  0x7dfe.2213.e2bf.a1fd.d03e.8f39.b8b6.9b70.bc23.ea1e
    %~bet  0xb26a.d384.91a4.5c44.b69b.1322.155b.5dfb.ffc9.8cb0
    %~bex  0x13da.dbed.45d8.57cf.fdaa.0e14.769a.c258.fb67.2250
    %~deg  0xf2dc.50be.b378.f481.4640.1603.b6b7.70de.b359.f6ed
    %~det  0x4c54.1588.62ef.ca24.5de4.2af0.11b3.306a.feb9.c6bb
    %~dus  0x9ca4.3d91.9569.d1e8.6799.220f.a9af.ab71.423b.1697
    %~dut  0xfb6d.f432.f02d.4099.e0c1.7690.2351.d8e7.9120.af7d
    %~dux  0x4aa9.ce24.04aa.b305.a0ca.e4eb.fb74.4589.4a2f.2939
    %~dyl  0xf909.2d4a.e814.64c0.a043.0acb.e1d5.70d0.4ac7.99ac
    %~fel  0x8099.b697.6634.47ed.94ec.028e.0053.2776.5a44.ba70
    %~lyx  0x8ff7.bf90.a90e.5da5.15b4.e9ea.0860.16af.7e84.65c1
    %~meb  0xb4e5.db6a.e290.a899.0b13.1614.5e84.db9f.5599.3748
    %~mec  0x1bac.1284.c4de.3dd0.0304.3691.34ff.f4f5.abea.af5a
    %~med  0x8f24.6593.5699.8ba4.59fe.a898.f738.3622.37ee.d53c
    %~meg  0x82ca.887f.3ce5.f07d.ddb3.f05c.67b8.81de.a9de.7da9
    %~mes  0x976c.3d00.6b1e.3cd7.ef1a.4fbc.a9aa.34b2.5ff0.7f2e
    %~myn  0x89e1.d775.9ff1.c8f0.283d.f0c1.7f06.18ca.f7b6.94f2
    %~myr  0xc1f1.9a94.1bba.a3aa.7b70.e13e.ed95.8b67.44c3.baf6
    %~neb  0x68b9.afc7.79d8.6099.fd37.8fc3.1c03.7e19.c6cc.8440
    %~nub  0xa58b.f3fe.3cae.daa9.738f.ac00.857d.fd91.5096.9075
    %~nux  0x1dde.f81e.9d3a.5aa4.d30f.f740.b5c9.bfa6.6a17.6969
    %~pel  0xab6a.c840.a185.2046.c895.3333.fc4f.869c.bb64.b794
    %~rec  0x6f9a.33c8.aa24.9bfe.63b2.85f3.815d.0841.0675.513c
    %~ren  0x30c0.11f8.b14a.dd6c.009b.210a.ad2e.084c.d152.f2d7
    %~res  0x3e26.190d.aacc.364e.ca37.8e45.91bc.a6eb.6c05.078d
    %~rev  0xaf1f.5c6f.8d80.65ec.d810.6212.b024.1cc2.345c.d7db
    %~rux  0x5988.74bf.b886.5f6d.37bb.df56.346d.ad78.0973.cd6d
    %~ryn  0x6ebc.f2f7.0969.3968.710e.441c.9f70.573d.ba71.8627
    %~seg   0x9a8.ee7b.662c.c0c8.d1ef.faf6.c9fc.5263.83f5.bca1
    %~set  0xa6e9.822b.d49d.d0fa.ec06.6487.0bee.a8d4.8a12.2051
    %~sur  0xdb37.c2d3.825b.7920.3bba.a711.66ff.dea2.5347.b673
    %~syp  0xa3c1.bd39.ba24.420c.d04d.6f97.b78b.9116.98a5.a67d
    %~ted  0x2bfa.25f9.46fb.ef42.3330.98e8.8981.7181.7d1c.3348
    %~ter  0x4cdc.baed.9be1.be2e.59e3.49e6.15b4.a7da.fb93.4a28
    %~tex  0xaafb.f2b1.040d.b63d.8e85.1973.03c3.dc73.977e.2efd
    %~tud  0xac4d.0d17.aead.3ae5.555d.6bf5.b22f.5f94.70e6.7fce
    %~tyn   0xcdb.9333.efe7.e358.f1bd.faf9.980a.4184.6688.db8c
    %~wet   0x2c1.be7c.e8c0.7096.308d.b18b.82fe.446c.12dc.ac5d
    %~wyt  0x7dd0.6d81.4423.8e53.f01d.2268.b676.0ec2.52b8.3520
  ==
=/  shallow-safe=address
  0x5eb0.3d35.9e68.15d6.4077.71ab.69e8.0af5.6441.04b9
=/  proxy-safe=address
  0xb735.32b0.4fb5.98f5.d719.ec40.be68.db02.f798.bcf3
::
=/  gax=(list [gal=@p own=? loc=?])
  :~  [~bus & |]
      [~def & |]
      [~dev & |]
      [~lur & |]
      [~pem & |]
      [~pub & |]
      [~sud & |]
      [~ten & |]
      [~zod & |]
      [~nus & |]
      [~feb & |]
      [~fet & |]
      [~nes & |]
      [~rud & |]
      [~rel & |]
      [~bec & &]
      [~bep & &]
      [~ber & &]
      [~byl & &]
      [~byr & &]
      [~byt & &]
      [~deb & &]
      [~dem & &]
      [~dun & |]
      [~dyt & &]
      [~fen & &]
      [~fur & &]
      [~fyl & &]
      [~hul & &]
      [~hus & &]
      [~hut & &]
      [~lec & &]
      [~len & &]
      [~lep & &]
      [~ler & &]
      [~lev & &]
      [~luc & &]
      [~lud & &]
      [~lyn & &]
      [~lyr & &]
      [~lys & &]
      [~mel & &]
      [~mex & &]
      [~mug & &]
      [~mun & &]
      [~mur & &]
      [~myl & &]
      [~ned & &]
      [~nel & &]
      [~ner & &]
      [~nev & &]
      [~nyd & &]
      [~nyl & &]
      [~nys & &]
      [~pec & &]
      [~pex & &]
      [~rem & &]
      [~ret & &]
      [~ryc & &]
      [~ryd & &]
      [~ryl & &]
      [~rym & &]
      [~seb & &]
      [~sed & &]
      [~sen & &]
      [~sug & &]
      [~syl & &]
      [~tec & &]
      [~teg & &]
      [~tel & &]
      [~tes & &]
      [~tuc & &]
      [~tun & &]
      [~tus | &]
      [~wed | &]
      [~weg | &]
      [~wer & &]
      [~bel & &]
      [~ben | &]
      [~bet | &]
      [~bex & &]
      [~deg & &]
      [~det & &]
      [~dus & &]
      [~dut & &]
      [~dux & &]
      [~dyl & &]
      [~fel & &]
      [~lyx & &]
      [~meb & &]
      [~mec & &]
      [~med & &]
      [~meg & &]
      [~mes & &]
      [~myn & &]
      [~myr & &]
      [~neb & &]
      [~nub & &]
      [~nux & &]
      [~pel & &]
      [~rec & &]
      [~ren & &]
      [~res & &]
      [~rev & &]
      [~rux & &]
      [~ryn & &]
      [~seg & &]
      [~set & &]
      [~sur & &]
      [~syp & &]
      [~ted & &]
      [~ter & &]
      [~tex & &]
      [~tud & &]
      [~tyn & &]
      [~wet & &]
      [~wyt & &]
  ==
::
=/  saz=(list @p)
  :~  ~marzod
      ~binzod
      ~samzod
      ~wanzod
      ~litzod
      ~marnus
    ::
      ::  ceremony address
      ~hocdyt  ::  outgoing transfer to lsr
      ~fitdyt  ::  outgoing transfer to lsr
      ~fipbyt
      ~nimdyt  ::  outgoing transfer to lsr
      ~lardyt  ::  outgoing transfer to lsr
      ~waldyt  ::  outgoing transfer to lsr
      ~mipbyt
      ~rapdyt  ::  outgoing transfer to lsr
    ::
      ::NOTE  ~tonwet owned but is outgoing transfer
  ==
::
=/  known=(list @p)
  (weld saz (turn gax head))
::
~&  [%ecliptic ecliptic:mainnet-contracts]
::
^-  thread:spider
|=  args=vase
=+  !<([~ export=?] args)
~&  ?:(export %will-write-txs-to-disk %just-checking)
=/  m  (strand ,vase)
^-  form:m
=|  trawled=(set address)
=|  out=(jar address batch:claz)
::  handle galaxies and lockups
::
~&  %galaxies
|-
=*  loop-gax  $
?^  gax
  =,  i.gax
  ::  if we don't expect to own it, just no-op check for lockups
  ::
  ?.  own
    ;<  =deed:eth-noun  bind:m
      (rights:azimuth:az (rep 3 gal 1 ~))
    =/  lockup=?
      .=  owner.deed
      linear-star-release:mainnet-contracts
    ~?  &(loc lockup)
      [%need-manual-lockup-discovery gal]
    ~?  &(!loc lockup)
      [%make-sure-we-dont-own-lockup-for gal]
    loop-gax(gax t.gax)
  ::  get the owner address, use it to config & transfer the galaxy
  ::
  ;<  =deed:eth-noun  bind:m
    (rights:azimuth:az gal)
  ~?  !=(0x0 transfer-proxy.deed)
    [%unexpected-transfer-proxy gal transfer-proxy.deed]
  ::  set spawn proxy only if needed, pick safe depending on spawn count
  ::
  ;<  count=@ud  bind:m
    (get-spawn-count:azimuth:az gal)
  =/  spawn-proxy=(unit address)
    =/  remaining  (sub 0xff count)
    ?:  =(0 remaining)  ~
    ?:  (lth remaining 50)
      `shallow-safe
    `deep-safe
  ::
  =.  out
    %+  ~(add ja out)  owner.deed
    ^-  batch:claz
    :-  %more
    =;  txs=(list (unit batch:claz))
      (murn txs same)
    :~  `[%single %set-management-proxy gal shallow-safe]
        `[%single %set-voting-proxy gal proxy-safe]
      ::
        ?~  spawn-proxy  ~
        `[%single %set-spawn-proxy gal u.spawn-proxy]
      ::
        `[%single %transfer-ship gal deep-safe]
    ==
  ::  if it controls a lockup, transfer that too
  ::
  ;<  =batch:linear:az  bind:m
    (batches:linear:az owner.deed)
  =/  transferring=?
    &(!=(0x0 approved.batch) !=(owner.deed approved.batch))
  ?:  =(0 amount.batch)
    ~?  loc  [%missing-lockup gal owner.deed]
    loop-gax(gax t.gax)
  ~?  &(!loc !transferring)  [%unexpected-lockup gal owner.deed amount.batch]
  ~?  &(!loc transferring)   [%unexpected-lockup-still-transfer gal owner.deed amount.batch approved.batch]
  ~?  &(loc transferring)    [%unexpected-lockup-transfer gal owner.deed amount.batch approved.batch]
  ::  only transfer the lockup if we expected it
  ::
  =?  out  loc
    %+  ~(add ja out)  owner.deed
    [%single %approve-batch-transfer (lockup-safe gal)]
  =?  out  loc
    %+  ~(add ja out)  (lockup-safe gal)
    [%single %transfer-batch owner.deed]
  ::
  ?:  (~(has in trawled) owner.deed)
    loop-gax(gax t.gax)
  ::  find other assets owned by this address
  ::
  ;<  others=(list @p)  bind:m
    (get-owned-points:azimuth:az owner.deed)
  =.  others  (skip others |=(=@p ?=(^ (find [p]~ known))))
  ~?  !=(~ others)
    [%has-others gal owner.deed others]
  ::  find other assets this address may transfer
  ::
  ;<  transferrable=(list @p)  bind:m
    (get-transferring-for:azimuth:az owner.deed)
  =.  transferrable  (skip transferrable |=(=@p ?=(^ (find [p]~ known))))
  ~?  !=(~ transferrable)
    [%has-transferrable gal owner.deed transferrable]
  ::
  =.  trawled  (~(put in trawled) owner.deed)
  loop-gax(gax t.gax)
::
::  handle stars
::
~&  %stars
|-
=*  loop-saz  $
?^  saz
  =*  star  i.saz
  ::  get the owner address, use it to config & transfer the star
  ::
  ;<  =deed:eth-noun  bind:m
    (rights:azimuth:az star)
  =/  for-lsr-deposit=?
    =(linear-star-release:mainnet-contracts transfer-proxy.deed)
  ~?  &(!=(0x0 transfer-proxy.deed) !for-lsr-deposit)
    [%unexpected-transfer-proxy star transfer-proxy.deed]
  =.  out
    %+  ~(add ja out)  owner.deed
    ^-  batch:claz
    ?:  for-lsr-deposit
      [%single %transfer-ship star shallow-safe]
    :-  %more
    :~  [%single %set-management-proxy star shallow-safe]
        [%single %set-spawn-proxy star proxy-safe]
        [%single %transfer-ship star deep-safe]
    ==
  ::
  ?:  (~(has in trawled) owner.deed)
    loop-saz(saz t.saz)
  ::  find other assets owned by this address
  ::
  ;<  others=(list @p)  bind:m
    (get-owned-points:azimuth:az owner.deed)
  =.  others  (skip others |=(=@p ?=(^ (find [p]~ known))))
  =/  [planets=(list @p) others=(list @p)]
    (skid others (cury lth 0xffff))
  ~?  !=(~ others)
    [%has-others star owner.deed others]
  ::  if controlling any planets, make sure shallow safe can get them out
  ::
  =?  out  !=(~ planets)
    ~&  [%has-planets star owner.deed (lent planets)]
    ~&  %approving-all-for-shallow-safe
    ::TODO  this might be done multiple times if this address owns multiple
    ::      stars. the solution is that the cache is dumb and we should just
    ::      skip processing any address in it.
    %+  ~(add ja out)  owner.deed
    :+  %custom  ecliptic:mainnet-contracts
    [0 'setApprovalForAll' ~[address+shallow-safe bool+&]]
  ::  find other assets this address may transfer
  ::
  ;<  transferrable=(list @p)  bind:m
    (get-transferring-for:azimuth:az owner.deed)
  =.  transferrable  (skip transferrable |=(=@p ?=(^ (find [p]~ known))))
  ~?  !=(~ transferrable)
    [%has-transferrable star owner.deed transferrable]
  ::
  =.  trawled  (~(put in trawled) owner.deed)
  loop-saz(saz t.saz)
::
::  ceremony address
::
?>  (~(has in trawled) ceremony)
::
=.  out
  %+  ~(add ja out)  ceremony
  ^-  batch:claz
  :-  %more
  =,  mainnet-contracts
  :~  [%custom ecliptic 0 'transferOwnership' [%address deep-safe]~]
      [%custom linear-star-release 0 'transferOwnership' [%address shallow-safe]~]
      [%custom conditional-star-release 0 'transferOwnership' [%address deep-safe]~]
  ==
::
::  exporting
::
?.  export  (pure:m !>(~))
~&  [%generating address-count=~(wyt by out)]
::
=/  outs=(list [=address batches=(list batch:claz)])
  ~(tap by out)
|-
=*  loop-export  $
?~  outs
  ~&  %done
  (pure:m !>(~))
=,  i.outs
::
=/  file=path  /migration/(crip '0x' ((x-co:co 40) address))/eth-txs
::
;<  =bowl:spider  bind:m  get-bowl:strandio
;<  ~  bind:m
  %+  poke-our:strandio  %claz
  :-  %noun
  !>  ^-  command:claz
  ::NOTE  flop because +ja adds to list head
  =-  [%generate - %mainnet address %more (flop batches)]
  [(scot %p our.bowl) %home (scot %da now.bowl) file]
::  we must wait for claz to be done before proceeding to the next one
::
|-
=*  loop-check  $
;<  done=?  bind:m  (scry:strandio ? %cu %home file)
?.  done
  ;<  ~  bind:m  (sleep:strandio ~s1)
  loop-check
loop-export(outs t.outs)
