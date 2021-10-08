::  tests for the bip39 lib
::
::  test vectors from here:
::  https://github.com/trezor/python-mnemonic/blob/master/vectors.json
::  which uses "TREZOR" as the password always
::
/+  *test, bip39
=,  bip39
::
=/  vectors=(list [eny=byts nem=tape sed=@ux])
  :~
    :*
      :-  16
      0x0
      "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about"
      0xc552.57c3.60c0.7c72.029a.ebc1.b53c.05ed.
        0362.ada3.8ead.3e3e.9efa.3708.e534.9553.
        1f09.a698.7599.d182.64c1.e1c9.2f2c.f141.
        630c.7a3c.4ab7.c81b.2f00.1698.e746.3b04
    ==
    :*
      :-  16
      0x7f7f.7f7f.7f7f.7f7f.7f7f.7f7f.7f7f.7f7f
      "legal winner thank year wave sausage worth useful legal winner thank yellow"
      0x2e89.0581.9b87.23fe.2c1d.1618.60e5.ee18.
        3031.8dbf.49a8.3bd4.51cf.b844.0c28.bd6f.
        a457.fe12.9610.6559.a3c8.0937.a1c1.069b.
        e3a3.a5bd.381e.e626.0e8d.9739.fce1.f607
    ==
    :*
      :-  16
      0x8080.8080.8080.8080.8080.8080.8080.8080
      "letter advice cage absurd amount doctor acoustic avoid letter advice cage above"
      0xd71d.e856.f81a.8acc.65e6.fc85.1a38.d4d7.
        ec21.6fd0.796d.0a68.27a3.ad6e.d551.1a30.
        fa28.0f12.eb2e.47ed.2ac0.3b5c.462a.0358.
        d18d.69fe.4f98.5ec8.1778.c1b3.70b6.52a8
    ==
    :*
      :-  16
      0xffff.ffff.ffff.ffff.ffff.ffff.ffff.ffff
      "zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo wrong"
      0xac27.4954.8022.5222.079d.7be1.8158.3751.
        e86f.5710.27b0.497b.5b5d.1121.8e0a.8a13.
        3325.7291.7f0f.8e5a.5896.20c6.f15b.11c6.
        1dee.3276.51a1.4c34.e182.3105.2e48.c069
    ==
    :*
      :-  24
      0x0
      "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon agent"
      0x358.95f2.f481.b1b0.f01f.cf8c.289c.7946.
        60b2.8998.1a78.f810.6447.707f.dd96.66ca.
        06da.5a9a.5651.8159.9b79.f53b.844d.8a71.
        dd9f.439c.52a3.d7b3.e8a7.9c90.6ac8.45fa
    ==
    :*
      :-  24
      0x7f7f.7f7f.7f7f.7f7f.7f7f.7f7f.7f7f.7f7f.
        7f7f.7f7f.7f7f.7f7f
      "legal winner thank year wave sausage worth useful legal winner thank year wave sausage worth useful legal will"
      0xf2b9.4508.732b.cbac.bcc0.20fa.efec.fc89.
        feaf.a664.9a54.91b8.c952.cede.496c.214a.
        0c7b.3c39.2d16.8748.f2d4.a612.bada.0753.
        b52a.1c7a.c53c.1e93.abd5.c632.0b9e.95dd
    ==
    :*
      :-  24
      0x8080.8080.8080.8080.8080.8080.8080.8080.
        8080.8080.8080.8080
      "letter advice cage absurd amount doctor acoustic avoid letter advice cage absurd amount doctor acoustic avoid letter always"
      0x107d.7c02.a5aa.6f38.c580.83ff.74f0.4c60.
        7c2d.2c0e.cc55.501d.add7.2d02.5b75.1bc2.
        7fe9.13ff.b796.f841.c49b.1d33.b610.cf0e.
        91d3.aa23.9027.f5e9.9fe4.ce9e.5088.cd65
    ==
    :*
      :-  24
      0xffff.ffff.ffff.ffff.ffff.ffff.ffff.ffff.
        ffff.ffff.ffff.ffff
      "zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo when"
      0xcd6.e5d8.27bb.62eb.8fc1.e262.2542.2381.
        7fd0.68a7.4b5b.449c.c2f6.67c3.f1f9.85a7.
        6379.b433.48d9.52e2.265b.4cd1.2909.0758.
        b3e3.c2c4.9103.b505.1aac.2eae.b890.a528
    ==
    :*
      :-  32
      0x0
      "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon art"
      0xbda8.5446.c684.1370.7090.a520.22ed.d26a.
        1c94.6229.5029.f2e6.0cd7.c4f2.bbd3.0971.
        70af.7a4d.7324.5caf.a9c3.cca8.d561.a7c3.
        de6f.5d4a.10be.8ed2.a5e6.08d6.8f92.fcc8
    ==
    :*
      :-  32
      0x7f7f.7f7f.7f7f.7f7f.7f7f.7f7f.7f7f.7f7f.
        7f7f.7f7f.7f7f.7f7f.7f7f.7f7f.7f7f.7f7f
      "legal winner thank year wave sausage worth useful legal winner thank year wave sausage worth useful legal winner thank year wave sausage worth title"
      0xbc09.fca1.804f.7e69.da93.c2f2.028e.b238.
        c227.f2e9.dda3.0cd6.3699.2325.7848.0a40.
        21b1.46ad.717f.bb7e.451c.e9eb.835f.4362.
        0bf5.c514.db0f.8add.49f5.d121.449d.3e87
    ==
    :*
      :-  32
      0x8080.8080.8080.8080.8080.8080.8080.8080.
        8080.8080.8080.8080.8080.8080.8080.8080
      "letter advice cage absurd amount doctor acoustic avoid letter advice cage absurd amount doctor acoustic avoid letter advice cage absurd amount doctor acoustic bless"
      0xc0c5.19bd.0e91.a2ed.5435.7d9d.1ebe.f6f5.
        af21.8a15.3624.cf4f.2da9.11a0.ed8f.7a09.
        e2ef.61af.0aca.0070.96df.4300.22f7.a2b6.
        fb91.661a.9589.0970.6972.0d01.5e4e.982f
    ==
    :*
      :-  32
      0xffff.ffff.ffff.ffff.ffff.ffff.ffff.ffff.
        ffff.ffff.ffff.ffff.ffff.ffff.ffff.ffff
      "zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo vote"
      0xdd48.c104.698c.30cf.e2b6.1421.0324.8622.
        fb7b.b0ff.692e.ebb0.0089.b32d.2248.4e16.
        1391.2f0a.5b69.4407.be89.9ffd.31ed.3992.
        c456.cdf6.0f5d.4564.b8ba.3f05.a698.90ad
    ==
    :*
      :-  16
      0x9e88.5d95.2ad3.62ca.eb4e.fe34.a8e9.1bd2
      "ozone drill grab fiber curtain grace pudding thank cruise elder eight picnic"
      0x274d.dc52.5802.f7c8.28d8.ef7d.dbcd.c530.
        4e87.ac35.3591.3611.fbbf.a986.d0c9.e547.
        6c91.689f.9c8a.54fd.55bd.3860.6aa6.a859.
        5ad2.13d4.c9c9.f9ac.a3fb.2170.69a4.1028
    ==
    :*
      :-  24
      0x6610.b259.67cd.cca9.d598.75f5.cb50.b0ea.
        7543.3311.869e.930b
      "gravity machine north sort system female filter attitude volume fold club stay feature office ecology stable narrow fog"
      0x628c.3827.a882.3298.ee68.5db8.4f55.caa3.
        4b5c.c195.a778.e52d.45f5.9bcf.75ab.a68e.
        4d75.90e1.01dc.414b.c1bb.d573.7666.fbbe.
        f35d.1f19.0395.3b66.624f.910f.eef2.45ac
    ==
    :*
      :-  32
      0x68a7.9eac.a232.4873.eacc.50cb.9c6e.ca8c.
        c68e.a5d9.36f9.8787.c60c.7ebc.74e6.ce7c
      "hamster diagram private dutch cause delay private meat slide toddler razor book happy fancy gospel tennis maple dilemma loan word shrug inflict delay length"
      0x64c8.7cde.7e12.ecf6.704a.b95b.b140.8bef.
        047c.22db.4cc7.491c.4271.d170.a1b2.13d2.
        0b38.5bc1.588d.9c7b.38f1.b39d.4156.65b8.
        a903.0c9e.c653.d75e.65f8.47d8.fc1f.c440
    ==
    :*
      :-  16
      0xc0ba.5a8e.9141.1121.0f2b.d131.f3d5.e08d
      "scheme spot photo card baby mountain device kick cradle pact join borrow"
      0xea72.5895.aaae.8d4c.1cf6.82c1.bfd2.d358.
        d52e.d9f0.f059.1131.b559.e272.4bb2.34fc.
        a05a.a9c0.2c57.407e.04ee.9dc3.b454.aa63.
        fbff.483a.8b11.de94.9624.b9f1.831a.9612
    ==
    :*
      :-  24
      0x6d9b.e1ee.6ebd.27a2.5811.5aad.99b7.317b.
        9c8d.28b6.d764.31c3
      "horn tenant knee talent sponsor spell gate clip pulse soap slush warm silver nephew swap uncle crack brave"
      0xfd57.9828.af3d.a1d3.2544.ce4d.b5c7.3d53.
        fc8a.cc4d.db1e.3b25.1a31.179c.db71.e853.
        c56d.2fcb.11ae.d398.98ce.6c34.b10b.5382.
        772d.b879.6e52.837b.5446.8aeb.312c.fc3d
    ==
    :*
      :-  32
      0x9f6a.2878.b252.0799.a44e.f18b.c7df.394e.
        7061.a224.d2c3.3cd0.15b1.57d7.4686.9863
      "panda eyebrow bullet gorilla call smoke muffin taste mesh discover soft ostrich alcohol speed nation flash devote level hobby quick inner drive ghost inside"
      0x72be.8e05.2fc4.919d.2adf.28d5.306b.5474.
        b006.9df3.5b02.303d.e8c1.729c.9538.dbb6.
        fc2d.731d.5f83.2193.cd9f.b6ae.ecbc.4695.
        94a7.0e3d.d508.11b5.067f.3b88.b28c.3e8d
    ==
    :*
      :-  16
      0x23db.8160.a31d.3e0d.ca36.88ed.941a.dbf3
      "cat swing flag economy stadium alone churn speed unique patch report train"
      0xdeb5.f454.49e6.15fe.ff56.40f2.e49f.933f.
        f518.95de.3b43.8183.2b31.3994.1c57.b592.
        05a4.2480.c521.75b6.efcf.faa5.8a25.0388.
        7c1e.8b36.3a70.7256.bdd2.b587.b465.41f5
    ==
    :*
      :-  24
      0x8197.a4a4.7f04.25fa.eaa6.9dee.bc05.ca29.
        c0a5.b5cc.76ce.acc0
      "light rule cinnamon wrap drastic word pride squirrel upgrade then income fatal apart sustain crack supply proud access"
      0x4cbd.ff1c.a2db.800f.d61c.ae72.a574.75fd.
        c6ba.b03e.441f.d63f.96da.bd1f.183e.f5b7.
        8292.5f00.105f.3183.09a7.e9c3.ea69.67c7.
        801e.46c8.a580.8267.4c86.0a37.b93e.da02
    ==
    :*
      :-  32
      0x66d.ca1a.2bb7.e8a1.db28.3214.8ce9.933e.
        ea0f.3ac9.548d.7931.12d9.a95c.9407.efad
      "all hour make first leader extend hole alien behind guard gospel lava path output census museum junior mass reopen famous sing advance salt reform"
      0x26e9.75ec.6444.23f4.a4c4.f421.5ef0.9b4b.
        d7ef.924e.85d1.d17c.4cf3.f136.c286.3cf6.
        df0a.4750.4565.2c57.eb5f.b415.13ca.2a2d.
        6772.2b77.e954.b4b3.fc11.f759.0449.191d
    ==
    :*
      :-  16
      0xf30f.8c1d.a665.478f.49b0.01d9.4c5f.c452
      "vessel ladder alter error federal sibling chat ability sun glass valve picture"
      0x2aaa.9242.daaf.cee6.aa9d.7269.f17d.4efe.
        271e.1b9a.5291.78d7.dc13.9cd1.8747.090b.
        f9d6.0295.d0ce.7430.9a78.852a.9caa.df0a.
        f48a.ae1c.6253.8396.2407.6224.374b.c63f
    ==
    :*
      :-  24
      0xc10e.c20d.c3cd.9f65.2c7f.ac2f.1230.f7a3.
        c828.389a.1439.2f05
      "scissors invite lock maple supreme raw rapid void congress muscle digital elegant little brisk hair mango congress clump"
      0x7b4a.10be.9d98.e6cb.a265.566d.b7f1.3671.
        8e13.98c7.1cb5.81e1.b2f4.64ca.c1ce.edf4.
        f3e2.74dc.2700.03c6.70ad.8d02.c455.8b2f.
        8e39.edea.2775.c9e2.32c7.cb79.8b06.9e88
    ==
    :*
      :-  32
      0xf585.c11a.ec52.0db5.7dd3.53c6.9554.b21a.
        89b2.0fb0.6509.66fa.0a9d.6f74.fd98.9d8f
      "void come effort suffer camp survey warrior heavy shoot primary clutch crush open amazing screen patrol group space point ten exist slush involve unfold"
      0x1f5.bced.59de.c48e.362f.2c45.b5de.68b9.
        fd6c.92c6.634f.44d6.d40a.ab69.0565.06f0.
        e355.24a5.1803.4ddc.1192.e1da.cd32.c1ed.
        3eaa.3c3b.131c.88ed.8e7e.54c4.9a5d.0998
    ==
  ==
|%
++  test-all-vectors
  |-
  ?~  vectors  ~
  =*  v  i.vectors
  =-  ?^(- - $(vectors t.vectors))
  =+  %+  expect-eq
        !>  nem.v
        !>  (from-entropy eny.v)
  ?^  -  -
  %+  expect-eq
    !>  sed.v
    !>  `@ux`(to-seed nem.v "TREZOR")
--
