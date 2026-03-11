/+  *test
=,  checksum
=,  mimes:html
|%
++  test-fox
  %+  expect-eq
  !>  0x414f.a339
  !>  %-  crc32:crc
      (as-octs 'The quick brown fox jumps over the lazy dog')
++  test-mayflower
  %+  expect-eq
  !>  0xa7db.ef9c
  !>  %-  crc32:crc
      (as-octs 'Let a hundred mayflowers bloom')
++  test-leading-zeros
  %+  expect-eq
  !>  0x2b74.f273
  !>  (crc32:crc [7 0xab.cd00.ef00])
++  test-empty
  %+  expect-eq
  !>  0x0
  !>  (crc32:crc [0 0x0])
--
