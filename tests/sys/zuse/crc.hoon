/+  *test
|%
++  test-fox
  %+  expect-eq
  !>  0x414f.a339
  !>  (crc32:crc [43 0x67.6f64.2079.7a61.6c20.6568.7420.7265.766f.2073.706d.756a.2078.6f66.206e.776f.7262.206b.6369.7571.2065.6854])
++  test-vector
  %+  expect-eq
  !>  0xc87.7f61
  !>  (crc32:crc [28 0x6d6f.632e.6974.6f6f.6265.6620.6d6f.7266.2072.6f74.6365.7620.7473.6554])
++  test-leading-zeros
  %+  expect-eq
  !>  0xbdf3.417e
  !>  (crc32:crc [3 0x29])
++  test-empty
  %+  expect-eq
  !>  0x0
  !>  (crc32:crc [0 0x0])
--
