/+  *test
=,  checksum:userlib
|%
++  test-empty
  %+  expect-eq
    !>  0x1
    !>  (adler32:adler [0 0])
++  test-fox
  %+  expect-eq
    !>  0x12d6.0e08
    !>  %-  adler32:adler
        %-  as-octs:mimes:html
        'the quick fox jumped over the lazy dog'
++  test-leading-zero
  %+  expect-eq
    !>  0xb72.01c1
    !>  (adler32:adler [8 'zero'])
--
