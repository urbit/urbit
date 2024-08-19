/+  *test
=,  checksum
=,  mimes:html
|%
++  test-empty
  %+  expect-eq
    !>  0x1
    !>  (adler32:adler [0 0])
++  test-fox
  %+  expect-eq
    !>  0x5bdc.0fda
    !>  %-  adler32:adler
        (as-octs 'The quick brown fox jumps over the lazy dog')
++  test-leading-zero
  %+  expect-eq
    !>  0xb72.01c1
    !>  (adler32:adler [8 'zero'])
++  test-long
  ;:  weld
    %+  expect-eq
      !>  0x23e4.07f9
      !>  (adler32:adler [8 (fil 3 8 0xff)])
    %+  expect-eq
      !>  0x37a3.3825
      !>  (adler32:adler [11.105 (fil 3 11.105 0xff)])
  ==
--
