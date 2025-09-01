/+  *test
|%
++  test-fox
  %+  expect-eq
  !>  0x414f.a339
  !>  (crc32:crc [43 'The quick brown fox jumps over the lazy dog'])
++  test-vector
  %+  expect-eq
  !>  0xc87.7f61
  !>  (crc32:crc [28 'Test vector from febooti.com'])
++  test-leading-zeros
  %+  expect-eq
  !>  0x2b74.f273
  !>  (crc32:crc [7 0xab.cd00.ef00])
++  test-empty
  %+  expect-eq
  !>  0x0
  !>  (crc32:crc [0 0x0])
--
