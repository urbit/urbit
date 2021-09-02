/-  spider, claz
/+  *strand, strandio, azio, ethio, *ethereum, *azimuth
::
=/  gas-price-gwei=@ud  70
=/  gas-usage=@ud       21.000
=/  tx-cost-wei=@ud  :(mul gas-price-gwei gas-usage 1.000.000.000)
::
=/  url=@t  'https://mainnet.infura.io/v3/2599df54929b47099bda360958d75aaf'
::
=/  hot-wallet=address
  0x460e.4977.44f4.1e80.bcb0.d143.ee3a.ca56.f25f.5e52
::
=/  involved=(list address)  ::  addresses to pick-pocket
  :~
    0xb2.e3c4.347e.314a.931b.fff8.99d2.3922.2ec8.cc9f
    0x14e.e048.3b4a.51c4.ea74.56b0.af83.36af.2836.3978
    0x182.e15c.6556.3ee6.7f61.2179.8728.ebaa.e896.6778
    0x24f.cf40.61a2.f2d3.5dd8.8644.f7a9.9387.3f2f.a7d9
    0x2c1.be7c.e8c0.7096.308d.b18b.82fe.446c.12dc.ac5d
    0x2dc.54ab.6e22.0ba6.34e5.a44f.dc6e.224d.e2bd.55d6
    0x432.c26f.ce02.17d5.f6ef.988b.53b3.7d12.050e.57d9
    0x67a.3734.20e6.7e4c.0edb.b547.ed61.33b8.3eb8.c7a4
    0x923.9e16.302c.c0a6.676b.a092.8ac4.4ff9.22a3.1992
    0x9a8.ee7b.662c.c0c8.d1ef.faf6.c9fc.5263.83f5.bca1
    0xa19.4d74.ee18.ffe5.b464.e74f.2ff7.45bf.9b77.90fc
    0xca7.f4fa.2931.18f7.f31c.d815.4d52.4232.3c9e.572e
    0xcdb.9333.efe7.e358.f1bd.faf9.980a.4184.6688.db8c
    0xf6b.0d07.b771.0345.0c36.3514.a185.48db.204d.5bd5
    0x13d1.3aa2.7762.e77e.14fd.79a2.6bf0.37ec.4810.813b
    0x13da.dbed.45d8.57cf.fdaa.0e14.769a.c258.fb67.2250
    0x149b.0c5e.1e91.911c.141c.24db.4386.589b.4e08.ba09
    0x157c.68b1.0a3f.9311.ac6d.0186.2b69.203f.fe5e.08d8
    0x15a7.3ad5.fa95.c3a8.1b8b.b9ae.1e75.797a.fa07.49c1
    0x16af.eb33.f384.d99a.c273.90ee.6e19.c301.8931.2e2f
    0x16bf.979c.9122.595e.66fb.2d84.71e4.7134.3743.2f5b
    0x16dd.55be.903e.0ed4.c6fb.dba7.03c3.c223.b76c.c8cd
    0x17dc.f09a.e852.5d49.ad92.a319.e603.ce56.4c4c.a002
    0x1ad6.5f7f.792a.030e.5306.fa52.afbc.61d9.4cdd.fbe9
    0x1af3.3b81.cef2.df13.63f6.2c71.3664.2d2c.dee9.b39b
    0x1bac.1284.c4de.3dd0.0304.3691.34ff.f4f5.abea.af5a
    0x1d45.d367.3143.bd50.17e6.88a9.f51d.fc63.96db.c81d
    0x1dde.f81e.9d3a.5aa4.d30f.f740.b5c9.bfa6.6a17.6969
    0x1fa3.ac3a.7122.0b46.759d.ee37.26fe.82f5.58ed.1dae
    0x1fa8.cbd8.2823.2acd.253e.80f6.c6df.1bdf.dcf9.35c3
    0x20f7.bc4d.4c65.d4c3.fa2f.72b4.820c.72c3.190a.c4e8
    0x220c.751d.752c.c2a1.7c91.ab12.3b6f.43a5.d8be.873d
    0x22c0.24c5.d10a.c583.2d19.7951.c10b.5424.3072.22ff
    0x2354.e4e2.4445.6bf4.b3c1.20d5.756f.a2cf.6f91.1ee1
    0x242f.ab47.40e5.f2e6.462c.1e37.dd80.e235.9aff.f6e5
    0x2556.d556.9df4.1356.c7f6.ede8.2659.078a.c65a.729b
    0x2602.3def.9390.d761.178c.3d74.166d.28a0.9086.a1f4
    0x271c.8e7f.09d8.911a.cae8.cf50.34c3.d643.3a00.c6c2
    0x2771.6e50.2ab7.f557.3ea9.aa44.0143.8e2e.cf7e.9876
    0x28b0.5157.5b67.4d8b.bf95.4beb.86a0.8e5d.386d.cf87
    0x2a13.a29d.358f.8a2a.3024.145c.7919.4e0a.e93e.0fd6
    0x2bd9.bc6e.1eb2.a796.39a3.7864.0807.6a17.12a2.6b47
    0x2bfa.25f9.46fb.ef42.3330.98e8.8981.7181.7d1c.3348
    0x2d10.4757.5c10.147c.2fad.0a6f.8328.cca1.0d0b.8b08
    0x2d83.946b.530b.99e8.23e9.c989.6e0e.0f05.bd5d.1fce
    0x2e0e.2a4f.2b92.94b2.c3a6.d9b1.b0eb.782c.aac0.0090
    0x2e94.e896.5b75.7e42.3520.6aef.8f96.61cf.6292.ed4b
    0x3071.91f2.9432.3f6d.8416.8ca8.8d9c.8e69.ead6.38f6
    0x30c0.11f8.b14a.dd6c.009b.210a.ad2e.084c.d152.f2d7
    0x30ea.4f46.9d5d.374a.36c3.69f6.b872.aca3.74d0.c1f5
    0x3106.f41b.69d6.6d54.ab1f.d275.1eb9.62d5.53f4.a4cd
    0x3114.9079.24b2.f6f8.2b55.e6a1.0a95.0775.606a.1bf0
    0x3142.2ace.f5b4.2363.f0e3.b2f9.090a.2136.7b76.63db
    0x314f.9ae1.c011.a7cb.b6cd.b39f.a3eb.4065.a293.9eb7
    0x3172.bcee.c370.7045.da31.0e90.74e0.1cf4.946a.5cc8
    0x3b17.d097.d9dd.711e.4ef8.517a.bbf1.8b2b.a643.81fe
    0x3b24.9041.37a2.3255.e016.195a.3b56.01df.a68c.83e8
    0x3e26.190d.aacc.364e.ca37.8e45.91bc.a6eb.6c05.078d
    0x3e7b.d722.9665.d19f.e319.b766.05ed.c0b4.3765.5d7b
    0x402a.b960.79bd.5f74.7b19.f76a.814f.1f35.6460.e83b
    0x40f1.8a81.011c.aa90.3bed.3021.62a0.8d05.45ea.c7b1
    0x41d9.da60.9346.69b7.e38c.5cb9.fa1f.4421.d452.d29d
    0x44c9.8aa6.c90f.530b.7742.b33a.45d5.2736.21e4.a354
    0x4932.7109.fbfe.1716.b47b.4d86.52b5.824d.0936.7136
    0x4a09.2f74.0076.9c10.cac2.547c.241c.2af8.7b31.ed9d
    0x4aa9.ce24.04aa.b305.a0ca.e4eb.fb74.4589.4a2f.2939
    0x4b7b.2f74.cdcf.725a.9552.31bc.d32b.27f8.162e.5ad4
    0x4c16.2e91.a6c9.6c20.8b52.c415.a1cd.9db5.b206.22a0
    0x4c54.1588.62ef.ca24.5de4.2af0.11b3.306a.feb9.c6bb
    0x4cdc.baed.9be1.be2e.59e3.49e6.15b4.a7da.fb93.4a28
    0x4e90.86f8.bf62.3a96.0671.25be.2c7f.e0d3.bdc2.ab0f
    0x4efd.8a25.3c1a.3a7d.502c.4266.fa5a.eacc.0047.8b9d
    0x4fb0.7fb0.91fd.1d5e.bf01.0c5e.9601.ab26.2bb2.2c3b
    0x4fea.be9b.0e02.565e.1b70.02cb.6ed2.d748.26fa.1f7e
    0x4fed.42d3.67f4.ca5c.f0d0.356e.c034.77d3.1df3.5cd5
    0x5099.ca9e.cbe4.d569.84f3.33e2.a0a7.3207.fdf2.8e27
    0x52cd.baf7.f02f.8ef3.ae9c.7ebc.0e4a.f477.7aa0.b11e
    0x52d4.8f2a.8b8b.2353.3a6d.407d.38b8.a19b.12ca.6df3
    0x5375.4c51.bd2d.dccd.bb85.c8e6.3827.011d.2fed.646b
    0x53bb.a118.e3cf.e275.9361.1f60.7e56.1f6f.d69d.4802
    0x53e8.2723.01d4.db1a.57d3.604f.8b0c.b150.3d4c.efa1
    0x546f.da89.a02f.95d6.10af.ce87.7eec.5df9.fff2.d10f
    0x5525.4c4a.f39f.6db7.e982.bd0c.609f.e4c4.5724.1914
    0x552c.91a5.96fd.67a2.9cb5.1d6e.cb31.bd3d.41a2.87af
    0x57a5.0a09.bd03.78b0.154f.27d9.d0ac.87ae.9e8c.9ecc
    0x594d.ada2.6d79.4542.0f3f.a8a1.8f66.4b4e.8c0a.4138
    0x5988.74bf.b886.5f6d.37bb.df56.346d.ad78.0973.cd6d
    0x5cc9.1115.2eb2.a3d6.baba.7e49.857b.f7d2.2e71.32f4
    0x6229.842f.1c82.43ec.f90d.c271.ec8d.069e.65b4.3e8a
    0x62cc.eae9.9a4a.c8cc.a197.4639.01e9.c6f5.8e2d.94dc
    0x68ab.bad1.58d6.42dc.d362.1335.013c.40ca.6f53.aaf5
    0x68b9.afc7.79d8.6099.fd37.8fc3.1c03.7e19.c6cc.8440
    0x69d0.3481.7e04.fd97.ea94.9ddd.eeff.9ae3.675c.3aa5
    0x6b92.5675.a51c.9ff2.2afa.e5f3.c2e0.bbdc.9d16.097c
    0x6b9f.3cf1.37c0.6edc.d33b.d77e.c7bf.43f9.3f33.3dd3
    0x6bd0.d475.498a.e88f.4342.d926.f046.e353.c7cf.ea21
    0x6c4c.7331.e6a8.06a5.b411.4202.042f.82c8.a260.ec57
    0x6d15.2c11.91ef.7238.76ff.5fce.e6c9.b6ac.b861.a73b
    0x6ebc.f2f7.0969.3968.710e.441c.9f70.573d.ba71.8627
    0x6f9a.33c8.aa24.9bfe.63b2.85f3.815d.0841.0675.513c
    0x7031.a990.45de.0392.e128.9e35.c7fd.637b.1e8e.9fde
    0x7113.4cfc.e518.c005.5076.946b.7c35.7404.f4d2.b59a
    0x7275.d236.77d7.b300.f955.fc4c.23b9.045f.5c51.5b7c
    0x740d.6d74.1711.163d.3fca.cecf.1f11.b867.9a7c.7964
    0x77aa.1a0f.716e.98be.1fc7.9685.54b2.c655.86cb.0d89
    0x7813.07ee.50e9.897d.7907.9ddb.fa31.6259.0aa7.6e87
    0x7924.c208.3a9f.c25c.7565.0c66.3b62.6c8b.3d8a.7b77
    0x797d.c75b.60cb.6398.4c12.2134.5d0c.7b54.9cb7.f978
    0x7a0c.7b16.1f70.6f3d.0b80.6e33.606f.623e.14f9.4197
    0x7dd0.6d81.4423.8e53.f01d.2268.b676.0ec2.52b8.3520
    0x7eab.7302.3e0c.6e36.ee07.20e6.bde8.9715.af1b.be63
    0x8099.b697.6634.47ed.94ec.028e.0053.2776.5a44.ba70
    0x80f3.a01a.432d.69cd.f579.c27d.7558.4636.73ec.03a4
    0x82ca.887f.3ce5.f07d.ddb3.f05c.67b8.81de.a9de.7da9
    0x860d.04e3.cf48.56f1.697e.09a9.b64f.ab4b.4dfc.5d0b
    0x8616.0fea.b0f5.84be.f8a2.8c92.6712.8268.e2c2.fb81
    0x863d.70e8.abd5.790a.4db6.b72f.3e4d.94da.9cc3.aa2c
    0x87f6.fcce.1d14.515b.6cb9.1586.05a2.ac78.da54.5d6f
    0x895e.d77f.58e2.49e0.edd1.01d0.600c.c410.a8ab.f9de
    0x896e.c456.52ae.b13f.bcb4.031f.0024.5140.d19e.459f
    0x89bd.1be2.fe3c.fd49.6a0b.f13a.b78f.63f8.2f4a.fb56
    0x89e1.d775.9ff1.c8f0.283d.f0c1.7f06.18ca.f7b6.94f2
    0x8a2d.1c9c.8946.b8ef.ebd1.9946.88ed.dd3f.9822.c747
    0x8b3b.a6ee.5482.9e6c.55f1.3bef.2bfe.7f2a.12ad.d83a
    0x8ba8.a3e0.ac02.fe8f.91f3.0afc.d9f1.9720.b5ad.04a4
    0x8d17.0743.d09b.148d.4fe2.f13d.1824.3b8d.a4f2.8dfa
    0x8f24.6593.5699.8ba4.59fe.a898.f738.3622.37ee.d53c
    0x8ff7.bf90.a90e.5da5.15b4.e9ea.0860.16af.7e84.65c1
    0x9200.8d95.d72e.275e.51e3.a927.553d.4f13.ea17.e92f
    0x9488.f191.52dc.9da0.b93d.925b.d672.9201.f941.71d6
    0x9559.f387.7b4e.a265.7511.2127.3416.f565.357d.c0fb
    0x976c.3d00.6b1e.3cd7.ef1a.4fbc.a9aa.34b2.5ff0.7f2e
    0x9afc.8f2c.435c.e23f.ce24.1afb.80d8.ffe0.dd5e.a015
    0x9c01.6009.56c6.1202.3b7c.a000.d252.c3bc.a506.8142
    0x9c1c.b42b.6cb3.ab55.e3e7.c09f.9742.7662.c167.0a89
    0x9ca4.3d91.9569.d1e8.6799.220f.a9af.ab71.423b.1697
    0x9e7a.1400.efd3.9176.a32e.e6e4.5067.97da.cd73.0bb0
    0xa029.107e.4e56.4fcd.8584.8987.4e12.2117.ce40.c135
    0xa139.c19a.704b.f775.e736.49a0.e33e.5add.c97f.c31f
    0xa209.5212.a7fd.08a0.2e1e.1bb8.1bf4.38bf.c3d2.bed8
    0xa3c1.bd39.ba24.420c.d04d.6f97.b78b.9116.98a5.a67d
    0xa407.62cd.d508.cdee.100a.c150.c378.da86.b573.2482
    0xa58b.f3fe.3cae.daa9.738f.ac00.857d.fd91.5096.9075
    0xa6e9.822b.d49d.d0fa.ec06.6487.0bee.a8d4.8a12.2051
    0xa88e.b4e1.fff4.2a55.7019.6986.65f7.6b2d.3018.aad8
    0xa9ec.c1b2.981b.06b4.7532.4c39.0e94.ab19.2db6.d6ea
    0xaa21.cd36.185a.7c5f.d0e6.19cc.6319.d5db.1c46.f24a
    0xaafb.f2b1.040d.b63d.8e85.1973.03c3.dc73.977e.2efd
    0xab08.4715.74ff.734c.5875.129b.3ac4.0be3.aea8.149f
    0xab6a.c840.a185.2046.c895.3333.fc4f.869c.bb64.b794
    0xabb5.de65.bb69.ee8e.1389.be0a.490c.8e23.478a.c0f4
    0xabdd.8050.29c8.f3c4.0138.bdbe.75e0.d6f8.a14f.6d03
    0xac32.a4c1.719e.2cce.840c.1393.3685.220c.287d.de91
    0xac4d.0d17.aead.3ae5.555d.6bf5.b22f.5f94.70e6.7fce
    0xaec1.8db5.7755.6ed6.cb41.3e43.e515.0db0.6cb5.266e
    0xaef1.0838.d3b0.1355.e253.1d99.2f70.0cfb.8467.ea96
    0xaf1f.5c6f.8d80.65ec.d810.6212.b024.1cc2.345c.d7db
    0xafa9.6b1a.f942.cd9a.e789.c5e4.d86e.4c71.7450.3efa
    0xaffa.976b.c953.b9d9.1adf.1a65.c9a1.c72e.425c.e5c6
    0xb17c.5cd5.6712.33bc.1697.b50e.8298.d401.110b.90bb
    0xb1ba.6b9a.6314.3981.e23d.9e6e.a39d.0d90.3764.d6e4
    0xb3f2.f678.3c08.5358.a960.f327.ea5d.4487.50c6.9247
    0xb4e5.db6a.e290.a899.0b13.1614.5e84.db9f.5599.3748
    0xb4f9.5a8c.8061.1b05.7999.ca4b.727e.cbd8.ccc2.7837
    0xb558.18c0.5f38.2f39.22b2.6758.ed6d.234b.c011.b858
    0xb582.4001.cef4.324c.b211.606b.b10d.d69a.6fc3.591e
    0xb8ff.1083.b104.2c23.fef7.2687.cc51.8d23.8437.8ce0
    0xb92a.39d2.3427.6244.2916.7678.3eec.b1a4.9491.5270
    0xba4b.373b.3bb5.f54c.9bec.55dc.245e.c26c.8025.ab0d
    0xbbcb.9e22.fa57.efd9.1c81.3c98.101f.fbd8.402f.e577
    0xbc71.2b39.c093.db7e.16be.71ed.0dbe.5e45.b7a8.f3ea
    0xbce9.e7d8.bdfb.8c6b.771b.3d43.4b62.9bdf.2368.30a7
    0xbe2a.ec3d.9f95.1767.034e.8bce.739b.227f.4878.c3c4
    0xc135.363f.9b41.8740.cdfd.10c8.b38c.7ac9.1f21.5d17
    0xc1ab.ce03.36ee.5e78.3d58.a82c.631a.d111.b905.9069
    0xc1f1.9a94.1bba.a3aa.7b70.e13e.ed95.8b67.44c3.baf6
    0xc1fb.4905.6ae1.4bfd.8146.30a2.1cc1.e902.d229.797b
    0xc538.0fcc.87ec.3040.8587.09c1.1546.4f4a.b790.b185
    0xc60a.6ff6.d50e.daf1.9a2b.27c0.fb69.74bc.c2f9.89e6
    0xc6af.cd40.f37c.1e56.9e4d.1c7e.f96c.2c58.1027.d74e
    0xc9b9.0bcf.2c45.b48d.efca.decd.638c.356a.e949.717f
    0xc9eb.67ad.bd42.9e44.65c6.495a.85d6.0bb2.1696.ea1f
    0xccb2.8bea.e6bd.3e40.94fe.6bcf.be27.1e51.1999.e917
    0xcd94.fb4d.41ba.1049.de0c.bad9.8af3.5e37.92c3.1dd9
    0xcdf8.1268.1695.4740.ddf0.8ce4.4410.d825.e6cf.82e2
    0xce08.56e3.0068.4b4a.ee9f.9411.1fc3.7c24.5c0b.6713
    0xd15c.6c28.48c4.e132.c925.d55f.3d6b.f417.99dd.be54
    0xd17f.939c.c0f3.1863.2f8b.d71f.edb0.1b83.638c.97c4
    0xd2ea.7119.77f3.4ced.520b.12c4.2036.491e.34b1.1d70
    0xd324.69f7.afef.e0c9.e959.2b46.0e5d.3109.8014.4be7
    0xdb0b.b145.5c0c.ecd1.5c42.274c.03cf.025c.ae87.10d2
    0xdb37.c2d3.825b.7920.3bba.a711.66ff.dea2.5347.b673
    0xe105.c07b.ddfd.cc5c.02b1.4a02.7733.21ae.c416.117c
    0xe151.61ff.8bd1.24d1.d38d.7d0c.36ce.8a59.98eb.25bb
    0xe277.847d.732e.3579.347c.66ba.ac81.602b.1966.9463
    0xe347.5c65.b7fc.042f.83dd.5465.fb15.f7ef.978c.dca8
    0xe396.45c9.9d7b.1206.1a95.c4d3.b818.0798.0eeb.b574
    0xece1.ed6f.a335.350a.242a.ddc9.7493.b781.ce17.f960
    0xedee.8277.6c51.2917.139b.efca.2cb6.5af8.d33c.e868
    0xf128.d506.2d9b.079a.aab3.e3c6.62d9.64dd.baea.a374
    0xf219.4d53.36de.c1fd.4e3e.f4d6.01d0.7458.bb1d.94db
    0xf29c.1553.a5d8.1514.8ea6.b6df.e29a.bfe1.81c0.19b6
    0xf2dc.50be.b378.f481.4640.1603.b6b7.70de.b359.f6ed
    0xf2f7.4bba.93f2.5f6e.af9a.fd52.8774.7cc1.9c35.0c69
    0xf31f.4897.fb51.8b7b.0c5c.7c8b.682c.b96e.d8b1.301a
    0xf36f.b509.e35e.2cb4.d713.cebe.4ac7.5a54.a19d.cac8
    0xf37e.79d7.e1bb.7687.5d28.8d27.19af.afa6.4047.9812
    0xf7d8.e5a6.dfbc.6f4c.ef76.efb9.78e6.0c9f.abc3.f7d8
    0xf7e2.a87f.ae52.f789.8770.4ee4.ff5d.126a.e672.0cf0
    0xf909.2d4a.e814.64c0.a043.0acb.e1d5.70d0.4ac7.99ac
    0xfab2.6586.7b9d.ad9b.6048.3214.b83a.36e7.ee0f.e9b1
    0xfb6d.f432.f02d.4099.e0c1.7690.2351.d8e7.9120.af7d
    0xfcce.addc.b37e.1248.11c9.81e0.08c5.9ecc.cb3a.6335
    0xfe1d.afc1.0a45.3ed4.c8c9.c902.f0a7.36f3.2c2f.d7c0
  ==
::
=/  safes=(set address)  ::  get to keep their change
  %-  ~(gas in *(set address))
  ^-  (list address)
  :~
    hot-wallet
  ::
    0x740d.6d74.1711.163d.3fca.cecf.1f11.b867.9a7c.7964
    0x9f57.c77b.1095.bd5d.b055.8b9c.b9b8.e6fc.6737.5e3c
    0x5eb0.3d35.9e68.15d6.4077.71ab.69e8.0af5.6441.04b9
    0xb735.32b0.4fb5.98f5.d719.ec40.be68.db02.f798.bcf3
  ::
    0x9e7a.1400.efd3.9176.a32e.e6e4.5067.97da.cd73.0bb0
    0xdb0b.b145.5c0c.ecd1.5c42.274c.03cf.025c.ae87.10d2
    0x44c9.8aa6.c90f.530b.7742.b33a.45d5.2736.21e4.a354
    0x8d17.0743.d09b.148d.4fe2.f13d.1824.3b8d.a4f2.8dfa
    0x15a7.3ad5.fa95.c3a8.1b8b.b9ae.1e75.797a.fa07.49c1
    0xac32.a4c1.719e.2cce.840c.1393.3685.220c.287d.de91
    0x16af.eb33.f384.d99a.c273.90ee.6e19.c301.8931.2e2f
    0xe105.c07b.ddfd.cc5c.02b1.4a02.7733.21ae.c416.117c
    0xbce9.e7d8.bdfb.8c6b.771b.3d43.4b62.9bdf.2368.30a7
    0x20f7.bc4d.4c65.d4c3.fa2f.72b4.820c.72c3.190a.c4e8
    0xfab2.6586.7b9d.ad9b.6048.3214.b83a.36e7.ee0f.e9b1
    0x89bd.1be2.fe3c.fd49.6a0b.f13a.b78f.63f8.2f4a.fb56
    0xe151.61ff.8bd1.24d1.d38d.7d0c.36ce.8a59.98eb.25bb
    0xba4b.373b.3bb5.f54c.9bec.55dc.245e.c26c.8025.ab0d
    0x7275.d236.77d7.b300.f955.fc4c.23b9.045f.5c51.5b7c
    0x87f6.fcce.1d14.515b.6cb9.1586.05a2.ac78.da54.5d6f
    0x9afc.8f2c.435c.e23f.ce24.1afb.80d8.ffe0.dd5e.a015
    0x7031.a990.45de.0392.e128.9e35.c7fd.637b.1e8e.9fde
    0xb558.18c0.5f38.2f39.22b2.6758.ed6d.234b.c011.b858
    0xabb5.de65.bb69.ee8e.1389.be0a.490c.8e23.478a.c0f4
    0x923.9e16.302c.c0a6.676b.a092.8ac4.4ff9.22a3.1992
    0xc9b9.0bcf.2c45.b48d.efca.decd.638c.356a.e949.717f
    0x53e8.2723.01d4.db1a.57d3.604f.8b0c.b150.3d4c.efa1
    0x6d15.2c11.91ef.7238.76ff.5fce.e6c9.b6ac.b861.a73b
    0x2d83.946b.530b.99e8.23e9.c989.6e0e.0f05.bd5d.1fce
    0x3e7b.d722.9665.d19f.e319.b766.05ed.c0b4.3765.5d7b
    0x6b92.5675.a51c.9ff2.2afa.e5f3.c2e0.bbdc.9d16.097c
    0xbe2a.ec3d.9f95.1767.034e.8bce.739b.227f.4878.c3c4
    0x16dd.55be.903e.0ed4.c6fb.dba7.03c3.c223.b76c.c8cd
    0xf29c.1553.a5d8.1514.8ea6.b6df.e29a.bfe1.81c0.19b6
    0xc135.363f.9b41.8740.cdfd.10c8.b38c.7ac9.1f21.5d17
    0xaef1.0838.d3b0.1355.e253.1d99.2f70.0cfb.8467.ea96
    0x52d4.8f2a.8b8b.2353.3a6d.407d.38b8.a19b.12ca.6df3
    0x3142.2ace.f5b4.2363.f0e3.b2f9.090a.2136.7b76.63db
    0x8a2d.1c9c.8946.b8ef.ebd1.9946.88ed.dd3f.9822.c747
    0xc1fb.4905.6ae1.4bfd.8146.30a2.1cc1.e902.d229.797b
    0x7a0c.7b16.1f70.6f3d.0b80.6e33.606f.623e.14f9.4197
    0xd15c.6c28.48c4.e132.c925.d55f.3d6b.f417.99dd.be54
    0xca7.f4fa.2931.18f7.f31c.d815.4d52.4232.3c9e.572e
    0xa19.4d74.ee18.ffe5.b464.e74f.2ff7.45bf.9b77.90fc
    0x3071.91f2.9432.3f6d.8416.8ca8.8d9c.8e69.ead6.38f6
    0xb582.4001.cef4.324c.b211.606b.b10d.d69a.6fc3.591e
    0x57a5.0a09.bd03.78b0.154f.27d9.d0ac.87ae.9e8c.9ecc
    0xf37e.79d7.e1bb.7687.5d28.8d27.19af.afa6.4047.9812
    0x157c.68b1.0a3f.9311.ac6d.0186.2b69.203f.fe5e.08d8
    0x6229.842f.1c82.43ec.f90d.c271.ec8d.069e.65b4.3e8a
    0xd324.69f7.afef.e0c9.e959.2b46.0e5d.3109.8014.4be7
    0x860d.04e3.cf48.56f1.697e.09a9.b64f.ab4b.4dfc.5d0b
    0x17dc.f09a.e852.5d49.ad92.a319.e603.ce56.4c4c.a002
    0x67a.3734.20e6.7e4c.0edb.b547.ed61.33b8.3eb8.c7a4
    0x5375.4c51.bd2d.dccd.bb85.c8e6.3827.011d.2fed.646b
    0xafa9.6b1a.f942.cd9a.e789.c5e4.d86e.4c71.7450.3efa
    0x8ba8.a3e0.ac02.fe8f.91f3.0afc.d9f1.9720.b5ad.04a4
    0xb2.e3c4.347e.314a.931b.fff8.99d2.3922.2ec8.cc9f
    0xb17c.5cd5.6712.33bc.1697.b50e.8298.d401.110b.90bb
    0xa029.107e.4e56.4fcd.8584.8987.4e12.2117.ce40.c135
    0x149b.0c5e.1e91.911c.141c.24db.4386.589b.4e08.ba09
    0xece1.ed6f.a335.350a.242a.ddc9.7493.b781.ce17.f960
    0x77aa.1a0f.716e.98be.1fc7.9685.54b2.c655.86cb.0d89
    0x13da.dbed.45d8.57cf.fdaa.0e14.769a.c258.fb67.2250
    0xf2dc.50be.b378.f481.4640.1603.b6b7.70de.b359.f6ed
    0x4c54.1588.62ef.ca24.5de4.2af0.11b3.306a.feb9.c6bb
    0x9ca4.3d91.9569.d1e8.6799.220f.a9af.ab71.423b.1697
    0xfb6d.f432.f02d.4099.e0c1.7690.2351.d8e7.9120.af7d
    0x4aa9.ce24.04aa.b305.a0ca.e4eb.fb74.4589.4a2f.2939
    0xf909.2d4a.e814.64c0.a043.0acb.e1d5.70d0.4ac7.99ac
    0x8099.b697.6634.47ed.94ec.028e.0053.2776.5a44.ba70
    0x8ff7.bf90.a90e.5da5.15b4.e9ea.0860.16af.7e84.65c1
    0xb4e5.db6a.e290.a899.0b13.1614.5e84.db9f.5599.3748
    0x1bac.1284.c4de.3dd0.0304.3691.34ff.f4f5.abea.af5a
    0x8f24.6593.5699.8ba4.59fe.a898.f738.3622.37ee.d53c
    0x82ca.887f.3ce5.f07d.ddb3.f05c.67b8.81de.a9de.7da9
    0x976c.3d00.6b1e.3cd7.ef1a.4fbc.a9aa.34b2.5ff0.7f2e
    0x89e1.d775.9ff1.c8f0.283d.f0c1.7f06.18ca.f7b6.94f2
    0xc1f1.9a94.1bba.a3aa.7b70.e13e.ed95.8b67.44c3.baf6
    0x68b9.afc7.79d8.6099.fd37.8fc3.1c03.7e19.c6cc.8440
    0xa58b.f3fe.3cae.daa9.738f.ac00.857d.fd91.5096.9075
    0x1dde.f81e.9d3a.5aa4.d30f.f740.b5c9.bfa6.6a17.6969
    0xab6a.c840.a185.2046.c895.3333.fc4f.869c.bb64.b794
    0x6f9a.33c8.aa24.9bfe.63b2.85f3.815d.0841.0675.513c
    0x30c0.11f8.b14a.dd6c.009b.210a.ad2e.084c.d152.f2d7
    0x3e26.190d.aacc.364e.ca37.8e45.91bc.a6eb.6c05.078d
    0xaf1f.5c6f.8d80.65ec.d810.6212.b024.1cc2.345c.d7db
    0x5988.74bf.b886.5f6d.37bb.df56.346d.ad78.0973.cd6d
    0x6ebc.f2f7.0969.3968.710e.441c.9f70.573d.ba71.8627
    0x9a8.ee7b.662c.c0c8.d1ef.faf6.c9fc.5263.83f5.bca1
    0xa6e9.822b.d49d.d0fa.ec06.6487.0bee.a8d4.8a12.2051
    0xdb37.c2d3.825b.7920.3bba.a711.66ff.dea2.5347.b673
    0xa3c1.bd39.ba24.420c.d04d.6f97.b78b.9116.98a5.a67d
    0x2bfa.25f9.46fb.ef42.3330.98e8.8981.7181.7d1c.3348
    0x4cdc.baed.9be1.be2e.59e3.49e6.15b4.a7da.fb93.4a28
    0xaafb.f2b1.040d.b63d.8e85.1973.03c3.dc73.977e.2efd
    0xac4d.0d17.aead.3ae5.555d.6bf5.b22f.5f94.70e6.7fce
    0xcdb.9333.efe7.e358.f1bd.faf9.980a.4184.6688.db8c
    0x2c1.be7c.e8c0.7096.308d.b18b.82fe.446c.12dc.ac5d
    0x7dd0.6d81.4423.8e53.f01d.2268.b676.0ec2.52b8.3520
    0x71d1.8fd8.f78c.4afd.a917.a629.7f61.1f01.2b05.d1c6
    0xc6d.f9ca.3805.b950.9459.6cd6.fb24.d335.b7ad.ca0e
    0x8fd1.00c6.8015.9934.68bd.41c7.410e.4604.4111.131f
    0xb26a.d384.91a4.5c44.b69b.1322.155b.5dfb.ffc9.8cb0
  ==
:: ::
^-  thread:spider
|=  args=vase
=+  !<([~ export=?] args)
~&  ?:(export %will-write-txs-to-disk %just-checking)
=/  m  (strand ,vase)
^-  form:m
=|  out=(map address batch:claz)
=|  sum=@ud
::
|-
=*  loop-involved  $
?^  involved
  =*  addr  i.involved
  ?:  (~(has in safes) addr)
    loop-involved(involved t.involved)
  ::
  ;<  balance=@ud  bind:m
    (get-balance:ethio url addr)
  ?:  (lth balance tx-cost-wei)
    ~?  !=(0 balance)
      [%dust addr balance=balance]
    loop-involved(involved t.involved)
  ~?  (gth balance 1.000.000.000.000.000.000)
    [%whale addr balance]
  ::
  =/  rescued=@ud  (sub balance tx-cost-wei)
  =.  sum  (add sum rescued)
  =.  out
    %+  ~(put by out)  addr
    [%custom hot-wallet rescued ~]
  ::
  loop-involved(involved t.involved)
::
::  exporting
::
~&  [%total-found sum]
?.  export  (pure:m !>(~))
:: ~&  [%generating address-count=~(wyt by out)]
:: ::
=/  outs=(list [=address =batch:claz])
  ~(tap by out)
|-
=*  loop-export  $
?~  outs
  ~&  %done
  (pure:m !>(~))
=,  i.outs
:: ::
=/  file=path  /eth-sweep/(crip '0x' ((x-co:co 40) address))/eth-txs
::
;<  =bowl:spider  bind:m  get-bowl:strandio
;<  ~  bind:m
  %+  poke-our:strandio  %claz
  :-  %noun
  !>  ^-  command:claz
  ::NOTE  flop because +ja adds to list head
  =-  [%generate - %mainnet address batch]
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
