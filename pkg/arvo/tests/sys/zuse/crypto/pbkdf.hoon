::  tests for the pbkdf family
::
::  test vectors from here:
::  https://stackoverflow.com/a/19898265/1334324
::
::NOTE  most high-iteration test vectors have been commented out to prevent
::      the tests from taking too long during automated testing. this can
::      probably be undone once ++pbkdf has gotten jetted.
::
/+  *test
=,  pbkdf:crypto
::
=/  vecs-hs512=(list [pass=@t salt=@t its=@u out=@u res=@ux])
  :~
    :*
      'passDATAb00AB7YxDTT'
      'saltKEYbcTcXHCBxtjD'
      1
      64
      0xcbe6.088a.d435.9af4.2e60.3c2a.3376.0ef9.d401.7a7b.2aad.
        10af.46f9.92c6.60a0.b461.ecb0.dc2a.79c2.5709.41be.a6a0.
        8d15.d688.7e79.f32b.132e.1c13.4e95.25ee.ddd7.44fa
    ==
    :*
      'passDATAb00AB7YxDTT'
      'saltKEYbcTcXHCBxtjD'
      100.000
      64
      0xaccd.cd87.98ae.5cd8.5804.7390.15ef.2a11.e325.91b7.b7d1.
        6f76.819b.30b0.d49d.80e1.abea.6c98.22b8.0a1f.dfe4.21e2.
        6f56.03ec.a8a4.7a64.c9a0.04fb.5af8.229f.762f.f41f
    ==
    :*
      'passDATAb00AB7YxDTTl'
      'saltKEYbcTcXHCBxtjD2'
      1
      64
      0x8e50.74a9.513c.1f15.12c9.b1df.1d8b.ffa9.d8b4.ef91.05df.
        c166.8122.2839.560f.b632.64be.d6aa.bf76.1f18.0e91.2a66.
        e0b5.3d65.ec88.f6a1.519e.1480.4eba.6dc9.df13.7007
    ==
    :: :*
    ::   'passDATAb00AB7YxDTTl'
    ::   'saltKEYbcTcXHCBxtjD2'
    ::   100.000
    ::   64
    ::   0x5942.56b0.bd4d.6c9f.21a8.7f7b.a577.2a79.1a10.e611.0694.
    ::     f443.65cd.9467.0e57.f1ae.cd79.7ef1.d100.1938.7190.44c7.
    ::     f018.0266.9784.5eb9.ad97.d97d.e36a.b878.6aab.5096
    :: ==
    :*
      'passDATAb00AB7YxDTTlR'
      'saltKEYbcTcXHCBxtjD2P'
      1
      64
      0xa6ac.8c04.8a7d.fd7b.838d.a88f.22c3.fab5.bff1.5d7c.b8d8.
        3a62.c672.1a8f.af69.03ea.b615.2cb7.4210.26e3.6f2f.fef6.
        61eb.4384.dc27.6495.c71b.5cab.72e1.c1a3.8712.e56b
    ==
    :: :*
    ::   'passDATAb00AB7YxDTTlR'
    ::   'saltKEYbcTcXHCBxtjD2P'
    ::   100.000
    ::   64
    ::   0x94ff.c2b1.a390.b7b8.a9e6.a449.22c3.30db.2b19.3adc.f082.
    ::     eecd.0605.7197.f359.31a9.d0ec.0ee5.c660.744b.50b6.1f23.
    ::     119b.847e.658d.179a.9148.07f4.b8ab.8eb9.505a.f065
    :: ==
    :*
      'passDATAb00AB7YxDTTlRH2dqxDx19GDxDV1zFMz7E6QVqKIzwOtMnlxQLttpE5'
      'saltKEYbcTcXHCBxtjD2PnBh44AIQ6XUOCESOhXpEp3HrcGMwbjzQKMSaf63IJe'
      1
      64
      0xe2cc.c782.7f1d.d7c3.3041.a989.06a8.fd7b.ae19.20a5.5fcb.
        8f83.1683.f14f.1c39.7935.1cb8.6871.7e5a.b342.d9a1.1acf.
        0b12.d328.3931.d609.b066.02da.33f8.377d.1f1f.9902
    ==
    :: :*
    ::   'passDATAb00AB7YxDTTlRH2dqxDx19GDxDV1zFMz7E6QVqKIzwOtMnlxQLttpE5'
    ::   'saltKEYbcTcXHCBxtjD2PnBh44AIQ6XUOCESOhXpEp3HrcGMwbjzQKMSaf63IJe'
    ::   100.000
    ::   64
    ::   0x744.7401.c857.66e4.aed5.83de.2e6b.f5a6.75ea.be4f.3618.
    ::     281c.9561.6f4f.c1fd.fe6e.cbc1.c398.2789.d4fd.941d.6584.
    ::     ef53.4a78.bd37.ae02.555d.9455.e8f0.89fd.b4df.b6bb
    :: ==
    :*
      'passDATAb00AB7YxDTTlRH2dqxDx19GDxDV1zFMz7E6QVqKIzwOtMnlxQLttpE57'
      'saltKEYbcTcXHCBxtjD2PnBh44AIQ6XUOCESOhXpEp3HrcGMwbjzQKMSaf63IJem'
      1
      64
      0xb029.a551.117f.f369.77f2.83f5.79dc.7065.b352.266e.a243.
        bdd3.f920.f24d.4d14.1ed8.b6e0.2d96.e2d3.bdfb.76f8.d77b.
        a8f4.bb54.8996.ad85.bb6f.11d0.1a01.5ce5.18f9.a717
    ==
    :: :*
    ::   'passDATAb00AB7YxDTTlRH2dqxDx19GDxDV1zFMz7E6QVqKIzwOtMnlxQLttpE57'
    ::   'saltKEYbcTcXHCBxtjD2PnBh44AIQ6XUOCESOhXpEp3HrcGMwbjzQKMSaf63IJem'
    ::   100.000
    ::   64
    ::   0x31f5.cc83.ed0e.948c.05a1.5735.d818.703a.aa7b.ff3f.09f5.
    ::     169c.af5d.ba66.02a0.5a4d.5cff.5553.d42e.82e4.0516.d6dc.
    ::     157b.8dae.ae61.d3fe.a456.d964.cb2f.7f9a.63bb.bdb5
    :: ==
    :*
      'passDATAb00AB7YxDTTlRH2dqxDx19GDxDV1zFMz7E6QVqKIzwOtMnlxQLttpE57U'
      'saltKEYbcTcXHCBxtjD2PnBh44AIQ6XUOCESOhXpEp3HrcGMwbjzQKMSaf63IJemk'
      1
      64
      0x28b8.a9f6.44d6.8006.1219.7bb7.4df4.6027.2e22.76de.8cc0.
        7ac4.897a.c24d.bc6e.b774.99fc.af97.4152.44d9.a29d.a83f.
        c347.d09a.5dbc.fd6b.d63f.f6e4.1080.3dca.8a90.0ab6
    ==
    :: :*
    ::   'passDATAb00AB7YxDTTlRH2dqxDx19GDxDV1zFMz7E6QVqKIzwOtMnlxQLttpE57U'
    ::   'saltKEYbcTcXHCBxtjD2PnBh44AIQ6XUOCESOhXpEp3HrcGMwbjzQKMSaf63IJemk'
    ::   100.000
    ::   64
    ::   0x56b.c907.2a35.6b7d.4da6.0dd6.6f59.68c2.caa3.75c0.220e.
    ::     da6b.47ef.8e8d.105e.d68b.4418.5fe9.003f.bba4.9e2c.8424.
    ::     0c9e.8fd3.f5b2.f4f6.512f.d936.4502.53db.37d1.0028
    :: ==
    :*
      'passDATAb00AB7YxDTTlRH2dqxDx19GDxDV1zFMz7E6QVqKIzwOtMnlxQLttpE57Un4u12D2YD7oOPpiEvCDYvntXEe4NNPLCnGGeJArbYDEu6xDoCfWH6kbuV6awi0'
      'saltKEYbcTcXHCBxtjD2PnBh44AIQ6XUOCESOhXpEp3HrcGMwbjzQKMSaf63IJemkURWoqHusIeVB8Il91NjiCGQacPUu9qTFaShLbKG0Yj4RCMV56WPj7E14EMpbxy'
      1
      64
      0x1622.6c85.e4f8.d604.5730.08bf.e61c.10b6.947b.5399.0450.
        612d.d4a3.077f.7dee.2116.229e.68ef.d1df.6d73.bd3c.6d07.
        5677.90ee.a1e8.b2ae.9a1b.046b.e593.847d.9441.a1b7
    ==
    :: :*
    ::   'passDATAb00AB7YxDTTlRH2dqxDx19GDxDV1zFMz7E6QVqKIzwOtMnlxQLttpE57Un4u12D2YD7oOPpiEvCDYvntXEe4NNPLCnGGeJArbYDEu6xDoCfWH6kbuV6awi0'
    ::   'saltKEYbcTcXHCBxtjD2PnBh44AIQ6XUOCESOhXpEp3HrcGMwbjzQKMSaf63IJemkURWoqHusIeVB8Il91NjiCGQacPUu9qTFaShLbKG0Yj4RCMV56WPj7E14EMpbxy'
    ::   100.000
    ::   64
    ::   0x70cf.39f1.4c4c.af3c.81fa.288f.b46c.1db5.2d19.f727.22f7.
    ::     bc84.f040.676d.3371.c89c.11c5.0f69.bcfb.c3ac.b0ab.9e92.
    ::     e4ef.6227.27a9.1621.9554.b2fa.121b.edda.97ff.3332
    :: ==
    :*
      'passDATAb00AB7YxDTTlRH2dqxDx19GDxDV1zFMz7E6QVqKIzwOtMnlxQLttpE57Un4u12D2YD7oOPpiEvCDYvntXEe4NNPLCnGGeJArbYDEu6xDoCfWH6kbuV6awi04'
      'saltKEYbcTcXHCBxtjD2PnBh44AIQ6XUOCESOhXpEp3HrcGMwbjzQKMSaf63IJemkURWoqHusIeVB8Il91NjiCGQacPUu9qTFaShLbKG0Yj4RCMV56WPj7E14EMpbxy6'
      1
      64
      0x880c.58c3.16d3.a5b9.f059.77ab.9c60.c10a.beeb.fad5.ce89.
        cae6.2905.c1c4.f80a.0a09.8d82.f953.21a6.220f.8aec.cfb4.
        5ce6.1071.4089.9e8d.6553.06ae.6396.553e.2851.376c
    ==
    :: :*
    ::   'passDATAb00AB7YxDTTlRH2dqxDx19GDxDV1zFMz7E6QVqKIzwOtMnlxQLttpE57Un4u12D2YD7oOPpiEvCDYvntXEe4NNPLCnGGeJArbYDEu6xDoCfWH6kbuV6awi04'
    ::   'saltKEYbcTcXHCBxtjD2PnBh44AIQ6XUOCESOhXpEp3HrcGMwbjzQKMSaf63IJemkURWoqHusIeVB8Il91NjiCGQacPUu9qTFaShLbKG0Yj4RCMV56WPj7E14EMpbxy6'
    ::   100.000
    ::   64
    ::   0x2668.b71b.3ca5.6136.b5e8.7f30.e098.f6b4.371c.b5ed.9553.
    ::     7c7a.073d.ac30.a2d5.be52.756a.df5b.b2f4.320c.b11c.4e16.
    ::     b249.65a9.c790.def0.cbc6.2906.920b.4f2e.b84d.1d4a
    :: ==
    :*
      'passDATAb00AB7YxDTTlRH2dqxDx19GDxDV1zFMz7E6QVqKIzwOtMnlxQLttpE57Un4u12D2YD7oOPpiEvCDYvntXEe4NNPLCnGGeJArbYDEu6xDoCfWH6kbuV6awi04U'
      'saltKEYbcTcXHCBxtjD2PnBh44AIQ6XUOCESOhXpEp3HrcGMwbjzQKMSaf63IJemkURWoqHusIeVB8Il91NjiCGQacPUu9qTFaShLbKG0Yj4RCMV56WPj7E14EMpbxy6P'
      1
      64
      0x93b9.ba82.83cc.17d5.0ef3.b448.2082.8a25.8a99.6de2.5822.
        5d24.fb59.990a.6d0d.e82d.fb3f.e2ac.2019.5210.0e4c.c8f0.
        6d88.3a91.3141.9c0f.6f5a.6ecb.8ec8.2154.5f14.adf1
    ==
    :: :*
    ::   'passDATAb00AB7YxDTTlRH2dqxDx19GDxDV1zFMz7E6QVqKIzwOtMnlxQLttpE57Un4u12D2YD7oOPpiEvCDYvntXEe4NNPLCnGGeJArbYDEu6xDoCfWH6kbuV6awi04U'
    ::   'saltKEYbcTcXHCBxtjD2PnBh44AIQ6XUOCESOhXpEp3HrcGMwbjzQKMSaf63IJemkURWoqHusIeVB8Il91NjiCGQacPUu9qTFaShLbKG0Yj4RCMV56WPj7E14EMpbxy6P'
    ::   100.000
    ::   64
    ::   0x2575.b485.afdf.37c2.60b8.f338.6d33.a60e.d929.993c.9d48.
    ::     ac51.6ec6.6b87.e06b.e54a.de7e.7c8c.b341.7c81.603b.080a.
    ::     8eef.c560.7281.1129.737c.ed96.236b.9364.e22c.e3a5
    :: ==
    :*
      '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'
      1
      64
      0x384b.cd69.1440.7e40.c295.d103.7cf4.f990.e8f0.e720.af43.
        cb70.6683.1770.16d3.6d1a.14b3.a7cf.22b5.df8d.5d7d.44d6.
        9610.b642.51ad.e2e7.ab54.a381.3a89.9355.92e3.91bf
    ==
    :: :*
    ::   'passDATAb00AB7YxDTTlRH2dqxDx19GDxDV1zFMz7E6QVqKIzwOtMnlxQLttpE57Un4u12D2YD7oOPpiEvCDYvntXEe4NNPLCnGGeJArbYDEu6xDoCfWH6kbuV6awi04Uz3ebEAhzZ4ve1A2wg5CnLXdZC5Y7gwfVgbEgZSTmoYQSzC5OW4dfrjqiwApTACO6xoOL1AjWj6X6f6qFfF8TVmOzU9RhOd1N4QtzWI4fP6FYttNz5FuLdtYVXWVXH2Tf7I9fieMeWCHTMkM4VcmQyQHpbcP8MEb5f1g6Ckg5xk3HQr3wMBvQcOHpCPy1K8HCM7a5wkPDhgVA0BVmwNpsRIbDQZRtHK6dT6bGyalp6gbFZBuBHwD86gTzkrFY7HkOVrgc0gJcGJZe65Ce8v4Jn5OzkuVsiU8efm2Pw2RnbpWSAr7SkVdCwXK2XSJDQ5fZ4HBEz9VTFYrG23ELuLjvx5njOLNgDAJuf5JB2tn4nMjjcnl1e8qcYVwZqFzEv2zhLyDWMkV4tzl4asLnvyAxTBkxPRZj2pRABWwb3kEofpsHYxMTAn38YSpZreoXipZWBnu6HDURaruXaIPYFPYHl9Ls9wsuD7rzaGfbOyfVgLIGK5rODphwRA7lm88bGKY8b7tWOtepyEvaLxMI7GZF5ScwpZTYeEDNUKPzvM2Im9zehIaznpguNdNXNMLWnwPu4H6zEvajkw3G3ucSiXKmh6XNe3hkdSANm3vnxzRXm4fcuzAx68IElXE2bkGFElluDLo6EsUDWZ4JIWBVaDwYdJx8uCXbQdoifzCs5kuuClaDaDqIhb5hJ2WR8mxiueFsS0aDGdIYmye5svmNmzQxFmdOkHoF7CfwuU1yy4uEEt9vPSP2wFp1dyaMvJW68vtB4kddLmI6gIgVVcT6ZX1Qm6WsusPrdisPLB2ScodXojCbL3DLj6PKG8QDVMWTrL1TpafT2wslRledWIhsTlv2mI3C066WMcTSwKLXdEDhVvFJ6ShiLKSN7gnRrlE0BnAw'
    ::   'saltKEYbcTcXHCBxtjD2PnBh44AIQ6XUOCESOhXpEp3HrcGMwbjzQKMSaf63IJemkURWoqHusIeVB8Il91NjiCGQacPUu9qTFaShLbKG0Yj4RCMV56WPj7E14EMpbxy6PlBdILBOkKUB6TGTPJXh1tpdOHTG6KuIvcbQp9qWjaf1uxAKgiTtYRIHhxjJI2viVa6fDZ67QOouOaf2RXQhpsWaTtAVnff6PIFcvJhdPDFGV5nvmZWoCZQodj6yXRDHPw9PyF0iLYm9uFtEunlAAxGB5qqea4X5tZvB1OfLVwymY3a3JPjdxTdvHxCHbqqE0zip61JNqdmeWxGtlRBC6CGoCiHO4XxHCntQBRJDcG0zW7joTdgtTBarsQQhlLXBGMNBSNmmTbDf3hFtawUBCJH18IAiRMwyeQJbJ2bERsY3MVRPuYCf4Au7gN72iGh1lRktSQtEFye7pO46kMXRrEjHQWXInMzzy7X2StXUzHVTFF2VdOoKn0WUqFNvB6PF7qIsOlYKj57bi1Psa34s85WxMSbTkhrd7VHdHZkTVaWdraohXYOePdeEvIwObCGEXkETUzqM5P2yzoBOJSdjpIYaa8zzdLD3yrb1TwCZuJVxsrq0XXY6vErU4QntsW0972XmGNyumFNJiPm4ONKh1RLvS1kddY3nm8276S4TUuZfrRQO8QxZRNuSaZI8JRZp5VojB5DktuMxAQkqoPjQ5Vtb6oXeOyY591CB1MEW1fLTCs0NrL321SaNRMqza1ETogAxpEiYwZ6pIgnMmSqNMRdZnCqA4gMWw1lIVATWK83OCeicNRUNOdfzS7A8vbLcmvKPtpOFvhNzwrrUdkvuKvaYJviQgeR7snGetO9JLCwIlHIj52gMCNU18d32SJl7Xomtl3wIe02SMvq1i1BcaX7lXioqWGmgVqBWU3fsUuGwHi6RUKCCQdEOBfNo2WdpFaCflcgnn0O6jVHCqkv8cQk81AqS00rAmHGCNTwyA6Tq5TXoLlDnC8gAQjDUsZp0z'
    ::   100.000
    ::   64
    ::   0xb867.4f6c.0cc9.f8cf.1f18.7453.4fd5.af01.fc15.04d7.6c2b.
    ::     c2aa.0a75.fe4d.d5df.d1da.f60e.a7c8.5f12.2bce.eb87.7265.
    ::     9d60.1231.6077.2699.8eac.3f6a.ab72.eff7.ba34.9f7f
    :: ==
    :*
      'passDATAb00AB7YxDTT'
      'saltKEYbcTcXHCBxtjD'
      1
      63
      0xcb.e608.8ad4.359a.f42e.603c.2a33.760e.f9d4.017a.7b2a.
        ad10.af46.f992.c660.a0b4.61ec.b0dc.2a79.c257.0941.bea6.
        a08d.15d6.887e.79f3.2b13.2e1c.134e.9525.eedd.d744
    ==
    :: :*
    ::   'passDATAb00AB7YxDTT'
    ::   'saltKEYbcTcXHCBxtjD'
    ::   100.000
    ::   63
    ::   0xac.cdcd.8798.ae5c.d858.0473.9015.ef2a.11e3.2591.b7b7.
    ::     d16f.7681.9b30.b0d4.9d80.e1ab.ea6c.9822.b80a.1fdf.e421.
    ::     e26f.5603.eca8.a47a.64c9.a004.fb5a.f822.9f76.2ff4
    :: ==
    :*
      'passDATAb00AB7YxDTTl'
      'saltKEYbcTcXHCBxtjD2'
      1
      63
      0x8e.5074.a951.3c1f.1512.c9b1.df1d.8bff.a9d8.b4ef.9105.
        dfc1.6681.2228.3956.0fb6.3264.bed6.aabf.761f.180e.912a.
        66e0.b53d.65ec.88f6.a151.9e14.804e.ba6d.c9df.1370
    ==
    :: :*
    ::   'passDATAb00AB7YxDTTl'
    ::   'saltKEYbcTcXHCBxtjD2'
    ::   100.000
    ::   63
    ::   0x59.4256.b0bd.4d6c.9f21.a87f.7ba5.772a.791a.10e6.1106.
    ::     94f4.4365.cd94.670e.57f1.aecd.797e.f1d1.0019.3871.9044.
    ::     c7f0.1802.6697.845e.b9ad.97d9.7de3.6ab8.786a.ab50
    :: ==
    :*
      'passDATAb00AB7YxDTTlR'
      'saltKEYbcTcXHCBxtjD2P'
      1
      63
      0xa6.ac8c.048a.7dfd.7b83.8da8.8f22.c3fa.b5bf.f15d.7cb8.
        d83a.62c6.721a.8faf.6903.eab6.152c.b742.1026.e36f.2ffe.
        f661.eb43.84dc.2764.95c7.1b5c.ab72.e1c1.a387.12e5
    ==
    :: :*
    ::   'passDATAb00AB7YxDTTlR'
    ::   'saltKEYbcTcXHCBxtjD2P'
    ::   100.000
    ::   63
    ::   0x94.ffc2.b1a3.90b7.b8a9.e6a4.4922.c330.db2b.193a.dcf0.
    ::     82ee.cd06.0571.97f3.5931.a9d0.ec0e.e5c6.6074.4b50.b61f.
    ::     2311.9b84.7e65.8d17.9a91.4807.f4b8.ab8e.b950.5af0
    :: ==
    :*
      'passDATAb00AB7YxDTTlRH2dqxDx19GDxDV1zFMz7E6QVqKIzwOtMnlxQLttpE5'
      'saltKEYbcTcXHCBxtjD2PnBh44AIQ6XUOCESOhXpEp3HrcGMwbjzQKMSaf63IJe'
      1
      63
      0xe2.ccc7.827f.1dd7.c330.41a9.8906.a8fd.7bae.1920.a55f.
        cb8f.8316.83f1.4f1c.3979.351c.b868.717e.5ab3.42d9.a11a.
        cf0b.12d3.2839.31d6.09b0.6602.da33.f837.7d1f.1f99
    ==
    :: :*
    ::   'passDATAb00AB7YxDTTlRH2dqxDx19GDxDV1zFMz7E6QVqKIzwOtMnlxQLttpE5'
    ::   'saltKEYbcTcXHCBxtjD2PnBh44AIQ6XUOCESOhXpEp3HrcGMwbjzQKMSaf63IJe'
    ::   100.000
    ::   63
    ::   0x7.4474.01c8.5766.e4ae.d583.de2e.6bf5.a675.eabe.4f36.
    ::     1828.1c95.616f.4fc1.fdfe.6ecb.c1c3.9827.89d4.fd94.1d65.
    ::     84ef.534a.78bd.37ae.0255.5d94.55e8.f089.fdb4.dfb6
    :: ==
    :*
      'passDATAb00AB7YxDTTlRH2dqxDx19GDxDV1zFMz7E6QVqKIzwOtMnlxQLttpE57'
      'saltKEYbcTcXHCBxtjD2PnBh44AIQ6XUOCESOhXpEp3HrcGMwbjzQKMSaf63IJem'
      1
      63
      0xb0.29a5.5111.7ff3.6977.f283.f579.dc70.65b3.5226.6ea2.
        43bd.d3f9.20f2.4d4d.141e.d8b6.e02d.96e2.d3bd.fb76.f8d7.
        7ba8.f4bb.5489.96ad.85bb.6f11.d01a.015c.e518.f9a7
    ==
    :: :*
    ::   'passDATAb00AB7YxDTTlRH2dqxDx19GDxDV1zFMz7E6QVqKIzwOtMnlxQLttpE57'
    ::   'saltKEYbcTcXHCBxtjD2PnBh44AIQ6XUOCESOhXpEp3HrcGMwbjzQKMSaf63IJem'
    ::   100.000
    ::   63
    ::   0x31.f5cc.83ed.0e94.8c05.a157.35d8.1870.3aaa.7bff.3f09.
    ::     f516.9caf.5dba.6602.a05a.4d5c.ff55.53d4.2e82.e405.16d6.
    ::     dc15.7b8d.aeae.61d3.fea4.56d9.64cb.2f7f.9a63.bbbd
    :: ==
    :*
      'passDATAb00AB7YxDTTlRH2dqxDx19GDxDV1zFMz7E6QVqKIzwOtMnlxQLttpE57U'
      'saltKEYbcTcXHCBxtjD2PnBh44AIQ6XUOCESOhXpEp3HrcGMwbjzQKMSaf63IJemk'
      1
      63
      0x28.b8a9.f644.d680.0612.197b.b74d.f460.272e.2276.de8c.
        c07a.c489.7ac2.4dbc.6eb7.7499.fcaf.9741.5244.d9a2.9da8.
        3fc3.47d0.9a5d.bcfd.6bd6.3ff6.e410.803d.ca8a.900a
    ==
    :: :*
    ::   'passDATAb00AB7YxDTTlRH2dqxDx19GDxDV1zFMz7E6QVqKIzwOtMnlxQLttpE57U'
    ::   'saltKEYbcTcXHCBxtjD2PnBh44AIQ6XUOCESOhXpEp3HrcGMwbjzQKMSaf63IJemk'
    ::   100.000
    ::   63
    ::   0x5.6bc9.072a.356b.7d4d.a60d.d66f.5968.c2ca.a375.c022.
    ::     0eda.6b47.ef8e.8d10.5ed6.8b44.185f.e900.3fbb.a49e.2c84.
    ::     240c.9e8f.d3f5.b2f4.f651.2fd9.3645.0253.db37.d100
    :: ==
    :*
      'passDATAb00AB7YxDTTlRH2dqxDx19GDxDV1zFMz7E6QVqKIzwOtMnlxQLttpE57Un4u12D2YD7oOPpiEvCDYvntXEe4NNPLCnGGeJArbYDEu6xDoCfWH6kbuV6awi0'
      'saltKEYbcTcXHCBxtjD2PnBh44AIQ6XUOCESOhXpEp3HrcGMwbjzQKMSaf63IJemkURWoqHusIeVB8Il91NjiCGQacPUu9qTFaShLbKG0Yj4RCMV56WPj7E14EMpbxy'
      1
      63
      0x16.226c.85e4.f8d6.0457.3008.bfe6.1c10.b694.7b53.9904.
        5061.2dd4.a307.7f7d.ee21.1622.9e68.efd1.df6d.73bd.3c6d.
        0756.7790.eea1.e8b2.ae9a.1b04.6be5.9384.7d94.41a1
    ==
    :: :*
    ::   'passDATAb00AB7YxDTTlRH2dqxDx19GDxDV1zFMz7E6QVqKIzwOtMnlxQLttpE57Un4u12D2YD7oOPpiEvCDYvntXEe4NNPLCnGGeJArbYDEu6xDoCfWH6kbuV6awi0'
    ::   'saltKEYbcTcXHCBxtjD2PnBh44AIQ6XUOCESOhXpEp3HrcGMwbjzQKMSaf63IJemkURWoqHusIeVB8Il91NjiCGQacPUu9qTFaShLbKG0Yj4RCMV56WPj7E14EMpbxy'
    ::   100.000
    ::   63
    ::   0x70.cf39.f14c.4caf.3c81.fa28.8fb4.6c1d.b52d.19f7.2722.
    ::     f7bc.84f0.4067.6d33.71c8.9c11.c50f.69bc.fbc3.acb0.ab9e.
    ::     92e4.ef62.2727.a916.2195.54b2.fa12.1bed.da97.ff33
    :: ==
    :*
      'passDATAb00AB7YxDTTlRH2dqxDx19GDxDV1zFMz7E6QVqKIzwOtMnlxQLttpE57Un4u12D2YD7oOPpiEvCDYvntXEe4NNPLCnGGeJArbYDEu6xDoCfWH6kbuV6awi04'
      'saltKEYbcTcXHCBxtjD2PnBh44AIQ6XUOCESOhXpEp3HrcGMwbjzQKMSaf63IJemkURWoqHusIeVB8Il91NjiCGQacPUu9qTFaShLbKG0Yj4RCMV56WPj7E14EMpbxy6'
      1
      63
      0x88.0c58.c316.d3a5.b9f0.5977.ab9c.60c1.0abe.ebfa.d5ce.
        89ca.e629.05c1.c4f8.0a0a.098d.82f9.5321.a622.0f8a.eccf.
        b45c.e610.7140.899e.8d65.5306.ae63.9655.3e28.5137
    ==
    :: :*
    ::   'passDATAb00AB7YxDTTlRH2dqxDx19GDxDV1zFMz7E6QVqKIzwOtMnlxQLttpE57Un4u12D2YD7oOPpiEvCDYvntXEe4NNPLCnGGeJArbYDEu6xDoCfWH6kbuV6awi04'
    ::   'saltKEYbcTcXHCBxtjD2PnBh44AIQ6XUOCESOhXpEp3HrcGMwbjzQKMSaf63IJemkURWoqHusIeVB8Il91NjiCGQacPUu9qTFaShLbKG0Yj4RCMV56WPj7E14EMpbxy6'
    ::   100.000
    ::   63
    ::   0x26.68b7.1b3c.a561.36b5.e87f.30e0.98f6.b437.1cb5.ed95.
    ::     537c.7a07.3dac.30a2.d5be.5275.6adf.5bb2.f432.0cb1.1c4e.
    ::     16b2.4965.a9c7.90de.f0cb.c629.0692.0b4f.2eb8.4d1d
    :: ==
    :*
      'passDATAb00AB7YxDTTlRH2dqxDx19GDxDV1zFMz7E6QVqKIzwOtMnlxQLttpE57Un4u12D2YD7oOPpiEvCDYvntXEe4NNPLCnGGeJArbYDEu6xDoCfWH6kbuV6awi04U'
      'saltKEYbcTcXHCBxtjD2PnBh44AIQ6XUOCESOhXpEp3HrcGMwbjzQKMSaf63IJemkURWoqHusIeVB8Il91NjiCGQacPUu9qTFaShLbKG0Yj4RCMV56WPj7E14EMpbxy6P'
      1
      63
      0x93.b9ba.8283.cc17.d50e.f3b4.4820.828a.258a.996d.e258.
        225d.24fb.5999.0a6d.0de8.2dfb.3fe2.ac20.1952.100e.4cc8.
        f06d.883a.9131.419c.0f6f.5a6e.cb8e.c821.545f.14ad
    ==
    :: :*
    ::   'passDATAb00AB7YxDTTlRH2dqxDx19GDxDV1zFMz7E6QVqKIzwOtMnlxQLttpE57Un4u12D2YD7oOPpiEvCDYvntXEe4NNPLCnGGeJArbYDEu6xDoCfWH6kbuV6awi04U'
    ::   'saltKEYbcTcXHCBxtjD2PnBh44AIQ6XUOCESOhXpEp3HrcGMwbjzQKMSaf63IJemkURWoqHusIeVB8Il91NjiCGQacPUu9qTFaShLbKG0Yj4RCMV56WPj7E14EMpbxy6P'
    ::   100.000
    ::   63
    ::   0x25.75b4.85af.df37.c260.b8f3.386d.33a6.0ed9.2999.3c9d.
    ::     48ac.516e.c66b.87e0.6be5.4ade.7e7c.8cb3.417c.8160.3b08.
    ::     0a8e.efc5.6072.8111.2973.7ced.9623.6b93.64e2.2ce3
    :: ==
    :: :*
    ::   'passDATAb00AB7YxDTTlRH2dqxDx19GDxDV1zFMz7E6QVqKIzwOtMnlxQLttpE57Un4u12D2YD7oOPpiEvCDYvntXEe4NNPLCnGGeJArbYDEu6xDoCfWH6kbuV6awi04Uz3ebEAhzZ4ve1A2wg5CnLXdZC5Y7gwfVgbEgZSTmoYQSzC5OW4dfrjqiwApTACO6xoOL1AjWj6X6f6qFfF8TVmOzU9RhOd1N4QtzWI4fP6FYttNz5FuLdtYVXWVXH2Tf7I9fieMeWCHTMkM4VcmQyQHpbcP8MEb5f1g6Ckg5xk3HQr3wMBvQcOHpCPy1K8HCM7a5wkPDhgVA0BVmwNpsRIbDQZRtHK6dT6bGyalp6gbFZBuBHwD86gTzkrFY7HkOVrgc0gJcGJZe65Ce8v4Jn5OzkuVsiU8efm2Pw2RnbpWSAr7SkVdCwXK2XSJDQ5fZ4HBEz9VTFYrG23ELuLjvx5njOLNgDAJuf5JB2tn4nMjjcnl1e8qcYVwZqFzEv2zhLyDWMkV4tzl4asLnvyAxTBkxPRZj2pRABWwb3kEofpsHYxMTAn38YSpZreoXipZWBnu6HDURaruXaIPYFPYHl9Ls9wsuD7rzaGfbOyfVgLIGK5rODphwRA7lm88bGKY8b7tWOtepyEvaLxMI7GZF5ScwpZTYeEDNUKPzvM2Im9zehIaznpguNdNXNMLWnwPu4H6zEvajkw3G3ucSiXKmh6XNe3hkdSANm3vnxzRXm4fcuzAx68IElXE2bkGFElluDLo6EsUDWZ4JIWBVaDwYdJx8uCXbQdoifzCs5kuuClaDaDqIhb5hJ2WR8mxiueFsS0aDGdIYmye5svmNmzQxFmdOkHoF7CfwuU1yy4uEEt9vPSP2wFp1dyaMvJW68vtB4kddLmI6gIgVVcT6ZX1Qm6WsusPrdisPLB2ScodXojCbL3DLj6PKG8QDVMWTrL1TpafT2wslRledWIhsTlv2mI3C066WMcTSwKLXdEDhVvFJ6ShiLKSN7gnRrlE0BnAw'
    ::   'saltKEYbcTcXHCBxtjD2PnBh44AIQ6XUOCESOhXpEp3HrcGMwbjzQKMSaf63IJemkURWoqHusIeVB8Il91NjiCGQacPUu9qTFaShLbKG0Yj4RCMV56WPj7E14EMpbxy6PlBdILBOkKUB6TGTPJXh1tpdOHTG6KuIvcbQp9qWjaf1uxAKgiTtYRIHhxjJI2viVa6fDZ67QOouOaf2RXQhpsWaTtAVnff6PIFcvJhdPDFGV5nvmZWoCZQodj6yXRDHPw9PyF0iLYm9uFtEunlAAxGB5qqea4X5tZvB1OfLVwymY3a3JPjdxTdvHxCHbqqE0zip61JNqdmeWxGtlRBC6CGoCiHO4XxHCntQBRJDcG0zW7joTdgtTBarsQQhlLXBGMNBSNmmTbDf3hFtawUBCJH18IAiRMwyeQJbJ2bERsY3MVRPuYCf4Au7gN72iGh1lRktSQtEFye7pO46kMXRrEjHQWXInMzzy7X2StXUzHVTFF2VdOoKn0WUqFNvB6PF7qIsOlYKj57bi1Psa34s85WxMSbTkhrd7VHdHZkTVaWdraohXYOePdeEvIwObCGEXkETUzqM5P2yzoBOJSdjpIYaa8zzdLD3yrb1TwCZuJVxsrq0XXY6vErU4QntsW0972XmGNyumFNJiPm4ONKh1RLvS1kddY3nm8276S4TUuZfrRQO8QxZRNuSaZI8JRZp5VojB5DktuMxAQkqoPjQ5Vtb6oXeOyY591CB1MEW1fLTCs0NrL321SaNRMqza1ETogAxpEiYwZ6pIgnMmSqNMRdZnCqA4gMWw1lIVATWK83OCeicNRUNOdfzS7A8vbLcmvKPtpOFvhNzwrrUdkvuKvaYJviQgeR7snGetO9JLCwIlHIj52gMCNU18d32SJl7Xomtl3wIe02SMvq1i1BcaX7lXioqWGmgVqBWU3fsUuGwHi6RUKCCQdEOBfNo2WdpFaCflcgnn0O6jVHCqkv8cQk81AqS00rAmHGCNTwyA6Tq5TXoLlDnC8gAQjDUsZp0z'
    ::   100.000
    ::   63
    ::   0xb8.674f.6c0c.c9f8.cf1f.1874.534f.d5af.01fc.1504.d76c.
    ::     2bc2.aa0a.75fe.4dd5.dfd1.daf6.0ea7.c85f.122b.ceeb.8772.
    ::     659d.6012.3160.7726.998e.ac3f.6aab.72ef.f7ba.349f
    :: ==
    :*
      'passDATAb00AB7YxDTT'
      'saltKEYbcTcXHCBxtjD'
      1
      65
      0xcb.e608.8ad4.359a.f42e.603c.2a33.760e.f9d4.017a.7b2a.ad10.
        af46.f992.c660.a0b4.61ec.b0dc.2a79.c257.0941.bea6.a08d.
        15d6.887e.79f3.2b13.2e1c.134e.9525.eedd.d744.fa88
    ==
    :: :*
    ::   'passDATAb00AB7YxDTT'
    ::   'saltKEYbcTcXHCBxtjD'
    ::   100.000
    ::   65
    ::   0xac.cdcd.8798.ae5c.d858.0473.9015.ef2a.11e3.2591.b7b7.d16f.
    ::     7681.9b30.b0d4.9d80.e1ab.ea6c.9822.b80a.1fdf.e421.e26f.
    ::     5603.eca8.a47a.64c9.a004.fb5a.f822.9f76.2ff4.1f7c
    :: ==
    :*
      'passDATAb00AB7YxDTTl'
      'saltKEYbcTcXHCBxtjD2'
      1
      65
      0x8e.5074.a951.3c1f.1512.c9b1.df1d.8bff.a9d8.b4ef.9105.dfc1.
        6681.2228.3956.0fb6.3264.bed6.aabf.761f.180e.912a.66e0.
        b53d.65ec.88f6.a151.9e14.804e.ba6d.c9df.1370.070b
    ==
    :: :*
    ::   'passDATAb00AB7YxDTTl'
    ::   'saltKEYbcTcXHCBxtjD2'
    ::   100.000
    ::   65
    ::   0x59.4256.b0bd.4d6c.9f21.a87f.7ba5.772a.791a.10e6.1106.94f4.
    ::     4365.cd94.670e.57f1.aecd.797e.f1d1.0019.3871.9044.c7f0.
    ::     1802.6697.845e.b9ad.97d9.7de3.6ab8.786a.ab50.96e7
    :: ==
    :*
      'passDATAb00AB7YxDTTlR'
      'saltKEYbcTcXHCBxtjD2P'
      1
      65
      0xa6.ac8c.048a.7dfd.7b83.8da8.8f22.c3fa.b5bf.f15d.7cb8.d83a.
        62c6.721a.8faf.6903.eab6.152c.b742.1026.e36f.2ffe.f661.
        eb43.84dc.2764.95c7.1b5c.ab72.e1c1.a387.12e5.6b93
    ==
    :: :*
    ::   'passDATAb00AB7YxDTTlR'
    ::   'saltKEYbcTcXHCBxtjD2P'
    ::   100.000
    ::   65
    ::   0x94.ffc2.b1a3.90b7.b8a9.e6a4.4922.c330.db2b.193a.dcf0.82ee.
    ::     cd06.0571.97f3.5931.a9d0.ec0e.e5c6.6074.4b50.b61f.2311.
    ::     9b84.7e65.8d17.9a91.4807.f4b8.ab8e.b950.5af0.6526
    :: ==
    :*
      'passDATAb00AB7YxDTTlRH2dqxDx19GDxDV1zFMz7E6QVqKIzwOtMnlxQLttpE5'
      'saltKEYbcTcXHCBxtjD2PnBh44AIQ6XUOCESOhXpEp3HrcGMwbjzQKMSaf63IJe'
      1
      65
      0xe2.ccc7.827f.1dd7.c330.41a9.8906.a8fd.7bae.1920.a55f.cb8f.
        8316.83f1.4f1c.3979.351c.b868.717e.5ab3.42d9.a11a.cf0b.
        12d3.2839.31d6.09b0.6602.da33.f837.7d1f.1f99.02da
    ==
    :: :*
    ::   'passDATAb00AB7YxDTTlRH2dqxDx19GDxDV1zFMz7E6QVqKIzwOtMnlxQLttpE5'
    ::   'saltKEYbcTcXHCBxtjD2PnBh44AIQ6XUOCESOhXpEp3HrcGMwbjzQKMSaf63IJe'
    ::   100.000
    ::   65
    ::   0x7.4474.01c8.5766.e4ae.d583.de2e.6bf5.a675.eabe.4f36.1828.
    ::     1c95.616f.4fc1.fdfe.6ecb.c1c3.9827.89d4.fd94.1d65.84ef.
    ::     534a.78bd.37ae.0255.5d94.55e8.f089.fdb4.dfb6.bb30
    :: ==
    :*
      'passDATAb00AB7YxDTTlRH2dqxDx19GDxDV1zFMz7E6QVqKIzwOtMnlxQLttpE57'
      'saltKEYbcTcXHCBxtjD2PnBh44AIQ6XUOCESOhXpEp3HrcGMwbjzQKMSaf63IJem'
      1
      65
      0xb0.29a5.5111.7ff3.6977.f283.f579.dc70.65b3.5226.6ea2.43bd.
        d3f9.20f2.4d4d.141e.d8b6.e02d.96e2.d3bd.fb76.f8d7.7ba8.
        f4bb.5489.96ad.85bb.6f11.d01a.015c.e518.f9a7.1780
    ==
    :: :*
    ::   'passDATAb00AB7YxDTTlRH2dqxDx19GDxDV1zFMz7E6QVqKIzwOtMnlxQLttpE57'
    ::   'saltKEYbcTcXHCBxtjD2PnBh44AIQ6XUOCESOhXpEp3HrcGMwbjzQKMSaf63IJem'
    ::   100.000
    ::   65
    ::   0x31.f5cc.83ed.0e94.8c05.a157.35d8.1870.3aaa.7bff.3f09.f516.
    ::     9caf.5dba.6602.a05a.4d5c.ff55.53d4.2e82.e405.16d6.dc15.
    ::     7b8d.aeae.61d3.fea4.56d9.64cb.2f7f.9a63.bbbd.b59f
    :: ==
    :*
      'passDATAb00AB7YxDTTlRH2dqxDx19GDxDV1zFMz7E6QVqKIzwOtMnlxQLttpE57U'
      'saltKEYbcTcXHCBxtjD2PnBh44AIQ6XUOCESOhXpEp3HrcGMwbjzQKMSaf63IJemk'
      1
      65
      0x28.b8a9.f644.d680.0612.197b.b74d.f460.272e.2276.de8c.c07a.
        c489.7ac2.4dbc.6eb7.7499.fcaf.9741.5244.d9a2.9da8.3fc3.
        47d0.9a5d.bcfd.6bd6.3ff6.e410.803d.ca8a.900a.b671
    ==
    :: :*
    ::   'passDATAb00AB7YxDTTlRH2dqxDx19GDxDV1zFMz7E6QVqKIzwOtMnlxQLttpE57U'
    ::   'saltKEYbcTcXHCBxtjD2PnBh44AIQ6XUOCESOhXpEp3HrcGMwbjzQKMSaf63IJemk'
    ::   100.000
    ::   65
    ::   0x5.6bc9.072a.356b.7d4d.a60d.d66f.5968.c2ca.a375.c022.0eda.
    ::     6b47.ef8e.8d10.5ed6.8b44.185f.e900.3fbb.a49e.2c84.240c.
    ::     9e8f.d3f5.b2f4.f651.2fd9.3645.0253.db37.d100.2889
    :: ==
    :*
      'passDATAb00AB7YxDTTlRH2dqxDx19GDxDV1zFMz7E6QVqKIzwOtMnlxQLttpE57Un4u12D2YD7oOPpiEvCDYvntXEe4NNPLCnGGeJArbYDEu6xDoCfWH6kbuV6awi0'
      'saltKEYbcTcXHCBxtjD2PnBh44AIQ6XUOCESOhXpEp3HrcGMwbjzQKMSaf63IJemkURWoqHusIeVB8Il91NjiCGQacPUu9qTFaShLbKG0Yj4RCMV56WPj7E14EMpbxy'
      1
      65
      0x16.226c.85e4.f8d6.0457.3008.bfe6.1c10.b694.7b53.9904.5061.
        2dd4.a307.7f7d.ee21.1622.9e68.efd1.df6d.73bd.3c6d.0756.
        7790.eea1.e8b2.ae9a.1b04.6be5.9384.7d94.41a1.b766
    ==
    :: :*
    ::   'passDATAb00AB7YxDTTlRH2dqxDx19GDxDV1zFMz7E6QVqKIzwOtMnlxQLttpE57Un4u12D2YD7oOPpiEvCDYvntXEe4NNPLCnGGeJArbYDEu6xDoCfWH6kbuV6awi0'
    ::   'saltKEYbcTcXHCBxtjD2PnBh44AIQ6XUOCESOhXpEp3HrcGMwbjzQKMSaf63IJemkURWoqHusIeVB8Il91NjiCGQacPUu9qTFaShLbKG0Yj4RCMV56WPj7E14EMpbxy'
    ::   100.000
    ::   65
    ::   0x70.cf39.f14c.4caf.3c81.fa28.8fb4.6c1d.b52d.19f7.2722.f7bc.
    ::     84f0.4067.6d33.71c8.9c11.c50f.69bc.fbc3.acb0.ab9e.92e4.
    ::     ef62.2727.a916.2195.54b2.fa12.1bed.da97.ff33.32ec
    :: ==
    :*
      'passDATAb00AB7YxDTTlRH2dqxDx19GDxDV1zFMz7E6QVqKIzwOtMnlxQLttpE57Un4u12D2YD7oOPpiEvCDYvntXEe4NNPLCnGGeJArbYDEu6xDoCfWH6kbuV6awi04'
      'saltKEYbcTcXHCBxtjD2PnBh44AIQ6XUOCESOhXpEp3HrcGMwbjzQKMSaf63IJemkURWoqHusIeVB8Il91NjiCGQacPUu9qTFaShLbKG0Yj4RCMV56WPj7E14EMpbxy6'
      1
      65
      0x88.0c58.c316.d3a5.b9f0.5977.ab9c.60c1.0abe.ebfa.d5ce.89ca.
        e629.05c1.c4f8.0a0a.098d.82f9.5321.a622.0f8a.eccf.b45c.
        e610.7140.899e.8d65.5306.ae63.9655.3e28.5137.6c57
    ==
    :: :*
    ::   'passDATAb00AB7YxDTTlRH2dqxDx19GDxDV1zFMz7E6QVqKIzwOtMnlxQLttpE57Un4u12D2YD7oOPpiEvCDYvntXEe4NNPLCnGGeJArbYDEu6xDoCfWH6kbuV6awi04'
    ::   'saltKEYbcTcXHCBxtjD2PnBh44AIQ6XUOCESOhXpEp3HrcGMwbjzQKMSaf63IJemkURWoqHusIeVB8Il91NjiCGQacPUu9qTFaShLbKG0Yj4RCMV56WPj7E14EMpbxy6'
    ::   100.000
    ::   65
    ::   0x26.68b7.1b3c.a561.36b5.e87f.30e0.98f6.b437.1cb5.ed95.537c.
    ::     7a07.3dac.30a2.d5be.5275.6adf.5bb2.f432.0cb1.1c4e.16b2.
    ::     4965.a9c7.90de.f0cb.c629.0692.0b4f.2eb8.4d1d.4a30
    :: ==
    :*
      'passDATAb00AB7YxDTTlRH2dqxDx19GDxDV1zFMz7E6QVqKIzwOtMnlxQLttpE57Un4u12D2YD7oOPpiEvCDYvntXEe4NNPLCnGGeJArbYDEu6xDoCfWH6kbuV6awi04U'
      'saltKEYbcTcXHCBxtjD2PnBh44AIQ6XUOCESOhXpEp3HrcGMwbjzQKMSaf63IJemkURWoqHusIeVB8Il91NjiCGQacPUu9qTFaShLbKG0Yj4RCMV56WPj7E14EMpbxy6P'
      1
      65
      0x93.b9ba.8283.cc17.d50e.f3b4.4820.828a.258a.996d.e258.225d.
        24fb.5999.0a6d.0de8.2dfb.3fe2.ac20.1952.100e.4cc8.f06d.
        883a.9131.419c.0f6f.5a6e.cb8e.c821.545f.14ad.f199
    ==
    :*
      'passDATAb00AB7YxDTTlRH2dqxDx19GDxDV1zFMz7E6QVqKIzwOtMnlxQLttpE57Un4u12D2YD7oOPpiEvCDYvntXEe4NNPLCnGGeJArbYDEu6xDoCfWH6kbuV6awi04U'
      'saltKEYbcTcXHCBxtjD2PnBh44AIQ6XUOCESOhXpEp3HrcGMwbjzQKMSaf63IJemkURWoqHusIeVB8Il91NjiCGQacPUu9qTFaShLbKG0Yj4RCMV56WPj7E14EMpbxy6P'
      100.000
      65
      0x25.75b4.85af.df37.c260.b8f3.386d.33a6.0ed9.2999.3c9d.48ac.
        516e.c66b.87e0.6be5.4ade.7e7c.8cb3.417c.8160.3b08.0a8e.
        efc5.6072.8111.2973.7ced.9623.6b93.64e2.2ce3.a542
    ==
    :*
      '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'
      1
      65
      0x38.4bcd.6914.407e.40c2.95d1.037c.f4f9.90e8.f0e7.20af.43cb.
        7066.8317.7016.d36d.1a14.b3a7.cf22.b5df.8d5d.7d44.d696.
        10b6.4251.ade2.e7ab.54a3.813a.8993.5592.e391.bf91
    ==
  ==
::
|%
++  test-all-vectors
  =*  v  vecs-hs512
  |-
  ?~  v  ~
  =-  ?^(- - $(v t.v))
  %+  expect-eq
    !>  `@ux`res.i.v
    !>  `@ux`(hmac-sha512t [pass salt its out]:i.v)
--
