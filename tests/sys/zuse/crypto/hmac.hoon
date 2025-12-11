::  tests for the hmac construction
::
/+  *test
=,  hmac:crypto
::
|%
+$  vector  [key=@ux in=@ux out=@ux]
::
++  do-test-vectors
  |*  [hmac=_hmac-sha1 ves=(list vector)]
  ^-  tang
  |^  (zing (turn ves case))
  ::
  ++  case
    |=  vector
    %+  expect-eq
      !>  out
      !>  `@ux`(hmac key in)
  --
::
::  HMAC-SHA-1. Test vectors from section 3 of RFC 2202:
::  https://tools.ietf.org/html/rfc2202#section-3
::
++  test-hmac-sha1
  %+  do-test-vectors  hmac-sha1
  :~
  ::
    :+  0xb0b.0b0b.0b0b.0b0b.0b0b.0b0b.0b0b.0b0b.
         0b0b.0b0b
      0x4869.2054.6865.7265
    0xb617.3186.5505.7264.e28b.c0b6.fb37.8c8e.
      f146.be00
  ::
    :+  0x4a65.6665
      0x7768.6174.2064.6f20.7961.2077.616e.7420.
        666f.7220.6e6f.7468.696e.673f
    0xeffc.df6a.e5eb.2fa2.d274.16d5.f184.df9c.
      259a.7c79
  ::
    :+  0xaaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.
          aaaa.aaaa
      0xdddd.dddd.dddd.dddd.dddd.dddd.dddd.dddd.
        dddd.dddd.dddd.dddd.dddd.dddd.dddd.dddd.
        dddd.dddd.dddd.dddd.dddd.dddd.dddd.dddd.
        dddd
    0x125d.7342.b9ac.11cd.91a3.9af4.8aa1.7b4f.
      63f1.75d3
  ::
    :+  0x1.0203.0405.0607.0809.0a0b.0c0d.0e0f.
       1011.1213.1415.1617.1819
      0xcdcd.cdcd.cdcd.cdcd.cdcd.cdcd.cdcd.cdcd.
        cdcd.cdcd.cdcd.cdcd.cdcd.cdcd.cdcd.cdcd.
        cdcd.cdcd.cdcd.cdcd.cdcd.cdcd.cdcd.cdcd.
        cdcd
    0x4c90.07f4.0262.50c6.bc84.14f9.bf50.c86c.
      2d72.35da
  ::
    :+  0xc0c.0c0c.0c0c.0c0c.0c0c.0c0c.0c0c.0c0c.
         0c0c.0c0c
      0x5465.7374.2057.6974.6820.5472.756e.6361.
        7469.6f6e
    0x4c1a.0342.4b55.e07f.e7f2.7be1.d58b.b932.
      4a9a.5a04
  ::
    :+  0xaaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.
          aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.
          aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.
          aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.
          aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa
      0x5465.7374.2055.7369.6e67.204c.6172.6765.
        7220.5468.616e.2042.6c6f.636b.2d53.697a.
        6520.4b65.7920.2d20.4861.7368.204b.6579.
        2046.6972.7374
    0xaa4a.e5e1.5272.d00e.9570.5637.ce8a.3b55.
      ed40.2112
  ::
    :+  0xaaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.
          aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.
          aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.
          aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.
          aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa
      0x5465.7374.2055.7369.6e67.204c.6172.6765.
        7220.5468.616e.2042.6c6f.636b.2d53.697a.
        6520.4b65.7920.2d20.4861.7368.204b.6579.
        2046.6972.7374
    0xaa4a.e5e1.5272.d00e.9570.5637.ce8a.3b55.
      ed40.2112
  ::
    :+  0xaaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.
          aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.
          aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.
          aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.
          aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa
      0x54.6573.7420.5573.696e.6720.4c61.7267.
      6572.2054.6861.6e20.426c.6f63.6b2d.5369.
      7a65.204b.6579.2061.6e64.204c.6172.6765.
      7220.5468.616e.204f.6e65.2042.6c6f.636b.
      2d53.697a.6520.4461.7461
    0xe8e9.9d0f.4523.7d78.6d6b.baa7.965c.7808.
      bbff.1a91
  ==
::
::  HMAC-SHA-256. Test vectors from section 4 of RFC 4231:
::  https://tools.ietf.org/html/rfc4231#section-4
::
++  test-hmac-sha256
  %+  do-test-vectors  hmac-sha256
  :~
    :+  0xb0b.0b0b.0b0b.0b0b.0b0b.0b0b.0b0b.0b0b.
         0b0b.0b0b
      0x4869.2054.6865.7265
    0xb034.4c61.d8db.3853.5ca8.afce.af0b.f12b.
      881d.c200.c983.3da7.26e9.376c.2e32.cff7
  ::
    :+  0x4a65.6665
      0x7768.6174.2064.6f20.7961.2077.616e.7420.
        666f.7220.6e6f.7468.696e.673f
    0x5bdc.c146.bf60.754e.6a04.2426.0895.75c7.
      5a00.3f08.9d27.3983.9dec.58b9.64ec.3843
  ::
    :+  0xaaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.
          aaaa.aaaa
      0xdddd.dddd.dddd.dddd.dddd.dddd.dddd.dddd.
        dddd.dddd.dddd.dddd.dddd.dddd.dddd.dddd.
        dddd.dddd.dddd.dddd.dddd.dddd.dddd.dddd.
        dddd
    0x773e.a91e.3680.0e46.854d.b8eb.d091.81a7.
      2959.098b.3ef8.c122.d963.5514.ced5.65fe
  ::
    :+  0x1.0203.0405.0607.0809.0a0b.0c0d.0e0f.
       1011.1213.1415.1617.1819
      0xcdcd.cdcd.cdcd.cdcd.cdcd.cdcd.cdcd.cdcd.
        cdcd.cdcd.cdcd.cdcd.cdcd.cdcd.cdcd.cdcd.
        cdcd.cdcd.cdcd.cdcd.cdcd.cdcd.cdcd.cdcd.
        cdcd
    0x8255.8a38.9a44.3c0e.a4cc.8198.99f2.083a.
      85f0.faa3.e578.f807.7a2e.3ff4.6729.665b
  ::
    :+  0xaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.
        aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.
        aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.
        aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.
        aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.
        aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.
        aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.
        aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.
        aaaa.aaaa
      0x5465.7374.2055.7369.6e67.204c.6172.6765.
        7220.5468.616e.2042.6c6f.636b.2d53.697a.
        6520.4b65.7920.2d20.4861.7368.204b.6579.
        2046.6972.7374
    0x60e4.3159.1ee0.b67f.0d8a.26aa.cbf5.b77f.
      8e0b.c621.3728.c514.0546.040f.0ee3.7f54
  ::
    :+  0xaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.
        aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.
        aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.
        aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.
        aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.
        aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.
        aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.
        aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.
        aaaa.aaaa
      0x5468.6973.2069.7320.6120.7465.7374.2075.
        7369.6e67.2061.206c.6172.6765.7220.7468.
        616e.2062.6c6f.636b.2d73.697a.6520.6b65.
        7920.616e.6420.6120.6c61.7267.6572.2074.
        6861.6e20.626c.6f63.6b2d.7369.7a65.2064.
        6174.612e.2054.6865.206b.6579.206e.6565.
        6473.2074.6f20.6265.2068.6173.6865.6420.
        6265.666f.7265.2062.6569.6e67.2075.7365.
        6420.6279.2074.6865.2048.4d41.4320.616c.
        676f.7269.7468.6d2e
    0x9b09.ffa7.1b94.2fcb.2763.5fbc.d5b0.e944.
      bfdc.6364.4f07.1393.8a7f.5153.5c3a.35e2
  ==
::
::  HMAC-SHA-512. Test vectors from section 4 of RFC 4231:
::  https://tools.ietf.org/html/rfc4231#section-4
::
++  test-hmac-sha512
  %+  do-test-vectors  hmac-sha512
  :~
  ::
    :+  0xb0b.0b0b.0b0b.0b0b.0b0b.0b0b.0b0b.0b0b.
         0b0b.0b0b
      0x4869.2054.6865.7265
    0x87aa.7cde.a5ef.619d.4ff0.b424.1a1d.6cb0.
      2379.f4e2.ce4e.c278.7ad0.b305.45e1.7cde.
      daa8.33b7.d6b8.a702.038b.274e.aea3.f4e4.
      be9d.914e.eb61.f170.2e69.6c20.3a12.6854
  ::
    :+  0x4a65.6665
      0x7768.6174.2064.6f20.7961.2077.616e.7420.
        666f.7220.6e6f.7468.696e.673f
    0x164b.7a7b.fcf8.19e2.e395.fbe7.3b56.e0a3.
      87bd.6422.2e83.1fd6.1027.0cd7.ea25.0554.
      9758.bf75.c05a.994a.6d03.4f65.f8f0.e6fd.
      caea.b1a3.4d4a.6b4b.636e.070a.38bc.e737
  ::
    :+  0xaaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.
          aaaa.aaaa
      0xdddd.dddd.dddd.dddd.dddd.dddd.dddd.dddd.
        dddd.dddd.dddd.dddd.dddd.dddd.dddd.dddd.
        dddd.dddd.dddd.dddd.dddd.dddd.dddd.dddd.
        dddd
    0xfa73.b008.9d56.a284.efb0.f075.6c89.0be9.
      b1b5.dbdd.8ee8.1a36.55f8.3e33.b227.9d39.
      bf3e.8482.79a7.22c8.06b4.85a4.7e67.c807.
      b946.a337.bee8.9426.7427.8859.e132.92fb
  ::
    :+  0x1.0203.0405.0607.0809.0a0b.0c0d.0e0f.
       1011.1213.1415.1617.1819
      0xcdcd.cdcd.cdcd.cdcd.cdcd.cdcd.cdcd.cdcd.
        cdcd.cdcd.cdcd.cdcd.cdcd.cdcd.cdcd.cdcd.
        cdcd.cdcd.cdcd.cdcd.cdcd.cdcd.cdcd.cdcd.
        cdcd
    0xb0ba.4656.3745.8c69.90e5.a8c5.f61d.4af7.
      e576.d97f.f94b.872d.e76f.8050.361e.e3db.
      a91c.a5c1.1aa2.5eb4.d679.275c.c578.8063.
      a5f1.9741.120c.4f2d.e2ad.ebeb.10a2.98dd
  ::
    :+  0xaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.
        aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.
        aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.
        aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.
        aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.
        aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.
        aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.
        aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.
        aaaa.aaaa
      0x5465.7374.2055.7369.6e67.204c.6172.6765.
        7220.5468.616e.2042.6c6f.636b.2d53.697a.
        6520.4b65.7920.2d20.4861.7368.204b.6579.
        2046.6972.7374
    0x80b2.4263.c7c1.a3eb.b714.93c1.dd7b.e8b4.
      9b46.d1f4.1b4a.eec1.121b.0137.83f8.f352.
      6b56.d037.e05f.2598.bd0f.d221.5d6a.1e52.
      95e6.4f73.f63f.0aec.8b91.5a98.5d78.6598
  ::
    :+  0xaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.
        aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.
        aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.
        aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.
        aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.
        aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.
        aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.
        aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.
        aaaa.aaaa
      0x5468.6973.2069.7320.6120.7465.7374.2075.
        7369.6e67.2061.206c.6172.6765.7220.7468.
        616e.2062.6c6f.636b.2d73.697a.6520.6b65.
        7920.616e.6420.6120.6c61.7267.6572.2074.
        6861.6e20.626c.6f63.6b2d.7369.7a65.2064.
        6174.612e.2054.6865.206b.6579.206e.6565.
        6473.2074.6f20.6265.2068.6173.6865.6420.
        6265.666f.7265.2062.6569.6e67.2075.7365.
        6420.6279.2074.6865.2048.4d41.4320.616c.
        676f.7269.7468.6d2e
    0xe37b.6a77.5dc8.7dba.a4df.a9f9.6e5e.3ffd.
      debd.71f8.8672.8986.5df5.a32d.20cd.c944.
      b602.2cac.3c49.82b1.0d5e.eb55.c3e4.de15.
      1346.76fb.6de0.4460.65c9.7440.fa8c.6a58
  ==
--
