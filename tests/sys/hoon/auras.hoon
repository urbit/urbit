/+  *test
|%
++  test-parse-da
  ;:  weld
    :: In the beginning was the Word
    %+  expect-eq
    !>  ~292277024401-.1.1
    !>  `@da`0x0
    :: the Word was with God
    %+  expect-eq
    !>  ~1.12.25..00.00.00..babe
    !>  `@da`0x7fff.fffe.58e4.0f80.babe.0000.0000.0000
    :: and the Word was God - John 1:1
    %+  expect-eq
    !>  ~33.4.3..15.00.00..3300
    !>  `@da`0x7fff.fffe.93b7.2f70.3300.0000.0000.0000
    ::
    %+  expect-eq
    !>  ~2023.3.24..05.44.15..cafe
    !>  `@da`0x8000.000d.32bb.462f.cafe.0000.0000.0000
    ::
    %+  expect-eq
    !>  ~2023.3.24..05.44.15..0000.cafe
    !>  `@da`0x8000.000d.32bb.462f.0000.cafe.0000.0000
    ::
    %+  expect-eq
    !>  ~2023.3.24..05.44.15..0000.0000.cafe
    !>  `@da`0x8000.000d.32bb.462f.0000.0000.cafe.0000
    ::
    %+  expect-eq
    !>  ~2023.3.24..05.44.15..0000.0000.0000.cafe
    !>  `@da`0x8000.000d.32bb.462f.0000.0000.0000.cafe
    ::
    %+  expect-eq
    !>  ~2023.3.1..14.32.22..adef
    !>  `@da`0x8000.000d.329d.6f76.adef.0000.0000.0000
    ::
    %+  expect-eq
    !>  ~2023.3.30..06.36.56..2d00
    !>  `@da`0x8000.000d.32c3.3b88.2d00.0000.0000.0000
    ::
    %+  expect-eq
    !>  ~2023.3.31..16.46.56..2d00
    !>  `@da`0x8000.000d.32c5.1c00.2d00.0000.0000.0000
    ::
    %+  expect-eq
    !>  ~2027.2.22..07.26.56..2d00
    !>  `@da`0x8000.000d.3a19.f0c0.2d00.0000.0000.0000
    ::
    %+  expect-eq
    !>  ~5924.11.10..10.06.56..2d00
    !>  `@da`0x8000.0029.dd78.fec0.2d00.0000.0000.0000
    ::
    %+  expect-eq
    !>  ~3903639.9.11..12.46.56..2d00
    !>  `@da`0x8000.7008.08c7.aec0.2d00.0000.0000.0000
    ::
    %-  expect-fail  |.((need (slaw %da '~2023--.1.1')))
    ::
    %-  expect-fail  |.((need (slaw %da '~2.023.1.1')))
    ::
    %-  expect-fail  |.((need (slaw %da '~2023.01.1')))
    ::
    %-  expect-fail  |.((need (slaw %da '~2023.1.01')))
    ::
    %-  expect-fail  |.((need (slaw %da '~2023.13.1')))
    ::
    %-  expect-fail  |.((need (slaw %da '~2023.12.32')))
    ::
    %-  expect-fail  |.((need (slaw %da '~2023.2.31')))
    ::
    %-  expect-fail  |.((need (slaw %da '~2023.2.29')))

    %-  expect-fail  |.((need (slaw %da '~2023.3.3..24.00.00')))
    ::
    %-  expect-fail  |.((need (slaw %da '~2023.3.3..24.00.00..ca')))
    ::
    %-  expect-fail  |.((need (slaw %da '~2023.3.3..24.00.00..cAFE')))
    ::
    %-  expect-fail  |.((need (slaw %da '~2023.3.3..24.00.00..cAFE')))
    ::
    %-  expect-fail  |.((need (slaw %da '~2023.3.3..24.00.00..cafe.cafe')))
  ==
::
++  test-render-da
  ;:  weld
    %+  expect-eq
      !>  ~.~292277024401-.1.1
      !>  (scot %da 0x0)
    %+  expect-eq
      !>  ~.~1.12.25..00.00.00..babe
      !>  (scot %da 0x7fff.fffe.58e4.0f80.babe.0000.0000.0000)
    %+  expect-eq
      !>  ~.~33.4.3..15.00.00..3300
      !>  (scot %da 0x7fff.fffe.93b7.2f70.3300.0000.0000.0000)
    ::
    %+  expect-eq
      !>  ~.~2023.3.24..05.44.15..cafe
      !>  (scot %da 0x8000.000d.32bb.462f.cafe.0000.0000.0000)
    ::
    %+  expect-eq
      !>  ~.~2023.3.24..05.44.15..0000.cafe
      !>  (scot %da 0x8000.000d.32bb.462f.0000.cafe.0000.0000)
    ::
    %+  expect-eq
      !>  ~.~2023.3.24..05.44.15..0000.0000.cafe
      !>  (scot %da 0x8000.000d.32bb.462f.0000.0000.cafe.0000)
    ::
    %+  expect-eq
      !>  ~.~2023.3.24..05.44.15..0000.0000.0000.cafe
      !>  (scot %da 0x8000.000d.32bb.462f.0000.0000.0000.cafe)
    ::
    %+  expect-eq
      !>  ~.~2023.3.1..14.32.22..adef
      !>  (scot %da 0x8000.000d.329d.6f76.adef.0000.0000.0000)
    ::
    %+  expect-eq
      !>  ~.~2023.3.30..06.36.56..2d00
      !>  (scot %da 0x8000.000d.32c3.3b88.2d00.0000.0000.0000)
    ::
    %+  expect-eq
      !>  ~.~2023.3.31..16.46.56..2d00
      !>  (scot %da 0x8000.000d.32c5.1c00.2d00.0000.0000.0000)
    ::
    %+  expect-eq
      !>  ~.~2027.2.22..07.26.56..2d00
      !>  (scot %da 0x8000.000d.3a19.f0c0.2d00.0000.0000.0000)
    ::
    %+  expect-eq
      !>  ~.~5924.11.10..10.06.56..2d00
      !>  (scot %da 0x8000.0029.dd78.fec0.2d00.0000.0000.0000)
    ::
    %+  expect-eq
      !>  ~.~3903639.9.11..12.46.56..2d00
      !>  (scot %da 0x8000.7008.08c7.aec0.2d00.0000.0000.0000)
    ::
    %+  expect-eq
      !>  ~.~1990808568848630424650.2.5..23.52.42..d022
      !>  (scot %da 0xcafe.abcd.8000.000d.330a.6fca.d022.0000.0000.0000)
    ::
    %+  expect-eq
      !>  ~.~1990808568848630424650.2.5..23.52.42..d022.0000.cafe
      !>  (scot %da 0xcafe.abcd.8000.000d.330a.6fca.d022.0000.cafe.0000)
    ==
::
++  test-parse-if
  ;:  weld 
  ::
  %+  expect-eq
  !>  .0.0.0.0
  !>  `@if`0x0
  ::
  %+  expect-eq
  !>  .0.0.202.254 
  !>  `@if`0xcafe
  ::
  %+  expect-eq
  !>  .202.254.18.52
  !>  `@if`0xcafe.1234
  ::
  %+  expect-eq
  !>  .18.52.202.254
  !>  `@if`0x1234.cafe
  ::
  %+  expect-eq
  !>  .255.255.255.255
  !>  `@if`0xffff.ffff
  ::
  %-  expect-fail  |.((need (slaw %if '.0.0.0')))
  %-  expect-fail  |.((need (slaw %if '.256.0.0.0')))
  %-  expect-fail  |.((need (slaw %if '.255.0.0.256')))
  ==
::
++  test-parse-ud
  ;:  weld
  %+  expect-eq
  !>  0
  !>  `@ud`0x0
  ::
  %+  expect-eq
  !>  1
  !>  `@ud`0x1
  ::
  %+  expect-eq
  !>  12
  !>  `@ud`0xc
  ::
  %+  expect-eq
  !>  123
  !>  `@ud`0x7b
  ::
  %+  expect-eq
  !>  1.234
  !>  `@ud`0x4d2
  ::
  %+  expect-eq
  !>  12.345
  !>  `@ud`0x3039
  ::
  %+  expect-eq
  !>  123.456
  !>  `@ud`0x1.e240
  ::
  %+  expect-eq
  !>  1.234.567
  !>  `@ud`0x12.d687
  ::
  %+  expect-eq
  !>  12.345.678
  !>  `@ud`0xbc.614e
  ::
  %+  expect-eq
  !>  123.456.789
  !>  `@ud`0x75b.cd15
  ::
  %+  expect-eq
  !>  100.100.100
  !>  `@ud`0x5f7.6804
  ::
  %+  expect-eq
  !>  101.101.101
  !>  `@ud`0x606.ae2d
  ::
  %+  expect-eq
  !>  201.201.201
  !>  `@ud`0xbfe.1631
  ::
  %+  expect-eq
  !>  302.201.100
  !>  `@ud`0x1203.390c
  ::
  %+  expect-eq
  !>  4.294.967.296
  !>  `@ud`(bex 32)
  ::
  %-  expect-fail  |.((need (slaw %ud '01')))
  %-  expect-fail  |.((need (slaw %ud '02')))
  %-  expect-fail  |.((need (slaw %ud '003')))
  %-  expect-fail  |.((need (slaw %ud '1234')))
  %-  expect-fail  |.((need (slaw %ud '1234.5')))
  %-  expect-fail  |.((need (slaw %ud '1234.567.8')))
  %-  expect-fail  |.((need (slaw %ud '1234.56..78')))
  %-  expect-fail  |.((need (slaw %ud '123.45a')))
  %-  expect-fail  |.((need (slaw %ud '.123.456')))
  ==
::
++  test-render-ud
  ;:  weld
  ::
  %+  expect-eq
  !>  ~.0
  !>  (scot %ud 0)
  ::
  %+  expect-eq
  !>  ~.1
  !>  (scot %ud 1)
  ::
  %+  expect-eq
  !>  ~.12
  !>  (scot %ud 12)
  ::
  %+  expect-eq
  !>  ~.123
  !>  (scot %ud 123)
  ::
  %+  expect-eq
  !>  ~.1.234
  !>  (scot %ud 1.234)
  ::
  %+  expect-eq
  !>  ~.12.345
  !>  (scot %ud 12.345)
  ::
  %+  expect-eq
  !>  ~.123.456
  !>  (scot %ud 123.456)
  ::
  %+  expect-eq
  !>  ~.1.234.567
  !>  (scot %ud 1.234.567)
  ::
  %+  expect-eq
  !>  ~.12.345.678
  !>  (scot %ud 12.345.678)
  ::
  %+  expect-eq
  !>  ~.123.456.789
  !>  (scot %ud 123.456.789)
  ::
  %+  expect-eq
  !>  ~.100.000.000
  !>  (scot %ud 100.000.000)
  ::
  %+  expect-eq
  !>  ~.101.101.101
  !>  (scot %ud 101.101.101)
  ::
  %+  expect-eq
  !>  ~.201.201.201
  !>  (scot %ud 201.201.201)
  ::
  %+  expect-eq
  !>  ~.302.201.100
  !>  (scot %ud 302.201.100)
  ::
  %+  expect-eq
  !>  ~.8.589.934.592
  !>  (scot %ud 8.589.934.592)
  ::
  %+  expect-eq
  !>  ~.2.305.843.009.213.693.952
  !>  (scot %ud 2.305.843.009.213.693.952)
  ::
  %+  expect-eq
  !>  ~.18.446.744.073.709.551.615
  !>  (scot %ud 18.446.744.073.709.551.615)
  ::
  %+  expect-eq
  !>  ~.340.282.366.920.938.463.463.374.607.431.768.211.456
  !>  (scot %ud (bex 128))
  ==
::
++  test-parse-ui
  ;:  weld
  ::
  %+  expect-eq
  !>  0i0
  !>  `@ui`0x0
  ::
  %+  expect-eq
  !>  0i0
  !>  `@ui`0
  ::
  %+  expect-eq
  !>  0i1
  !>  `@ui`1
  ::
  %+  expect-eq
  !>  0i12
  !>  `@ui`12
  ::
  %+  expect-eq
  !>  0i123
  !>  `@ui`123
  ::
  %+  expect-eq
  !>  0i1234
  !>  `@ui`1.234
  ::
  %+  expect-eq
  !>  0i12345
  !>  `@ui`12.345
  ::
  %+  expect-eq
  !>  0i123456
  !>  `@ui`123.456
  ::
  %+  expect-eq
  !>  0i1234567
  !>  `@ui`1.234.567
  ::
  %+  expect-eq
  !>  0i12345678
  !>  `@ui`12.345.678
  ::
  %+  expect-eq
  !>  0i123456789
  !>  `@ui`123.456.789
  ::
  %+  expect-eq
  !>  0i100000000
  !>  `@ui`100.000.000
  ::
  %+  expect-eq
  !>  0i101101101
  !>  `@ui`101.101.101
  ::
  %+  expect-eq
  !>  0i201201201
  !>  `@ui`201.201.201
  ::
  %+  expect-eq
  !>  0i302201100
  !>  `@ui`302.201.100
  ::
  %+  expect-eq
  !>  (bex 32)
  !>  `@ui`0i4294967296
  ::
  %+  expect-eq
  !>  (bex 128)
  !>  0i340282366920938463463374607431768211456
  ::
  %-  expect-fail  |.((need (slaw %ui '0i')))
  %-  expect-fail  |.((need (slaw %ui 'i0')))
  %-  expect-fail  |.((need (slaw %ui '0i01')))
  ==
::
++  test-render-ui
  ;:  weld
  ::
  %+  expect-eq
  !>  ~.0i0
  !>  (scot %ui 0)
  ::
  %+  expect-eq
  !>  ~.0i1
  !>  (scot %ui 1)
  ::
  %+  expect-eq
  !>  ~.0i12
  !>  (scot %ui 12)
  ::
  %+  expect-eq
  !>  ~.0i123
  !>  (scot %ui 123)
  ::
  %+  expect-eq
  !>  ~.0i1234
  !>  (scot %ui 1.234)
  ::
  %+  expect-eq
  !>  ~.0i12345
  !>  (scot %ui 12.345)
  ::
  %+  expect-eq
  !>  ~.0i123456
  !>  (scot %ui 123.456)
  ::
  %+  expect-eq
  !>  ~.0i1234567
  !>  (scot %ui 1.234.567)
  ::
  %+  expect-eq
  !>  ~.0i12345678
  !>  (scot %ui 12.345.678)
  ::
  %+  expect-eq
  !>  ~.0i123456789
  !>  (scot %ui 123.456.789)
  ::
  %+  expect-eq
  !>  ~.0i100000000
  !>  (scot %ui 100.000.000)
  ::
  %+  expect-eq
  !>  ~.0i101101101
  !>  (scot %ui 101.101.101)
  ::
  %+  expect-eq
  !>  ~.0i201201201
  !>  (scot %ui 201.201.201)
  ::
  %+  expect-eq
  !>  ~.0i302201100
  !>  (scot %ui 302.201.100)
  ::
  %+  expect-eq
  !>  ~.0i8589934592
  !>  (scot %ui 8.589.934.592)
  ::
  %+  expect-eq
  !>  ~.0i2305843009213693952
  !>  (scot %ui 2.305.843.009.213.693.952)
  ::
  %+  expect-eq
  !>  ~.0i18446744073709551615
  !>  (scot %ui 18.446.744.073.709.551.615)
  ::
  %+  expect-eq
  !>  ~.0i340282366920938463463374607431768211456
  !>  (scot %ui (bex 128))
  ==
::
++  test-parse-ux
  ;:  weld
  ::
  %+  expect-eq
    !>  0x0
    !>  `@ux`0x0
  ::
  %+  expect-eq
    !>  0x0
    !>  `@ux`0x0
  ::
  %+  expect-eq
    !>  0x1
    !>  `@ux`0x1
  ::
  %+  expect-eq
    !>  0x12
    !>  `@ux`0x12
  ::
  %+  expect-eq
    !>  0x1a3
    !>  `@ux`0x1a3
  ::
  %+  expect-eq
    !>  0x123b
    !>  `@ux`0x123b
  ::
  %+  expect-eq
    !>  0x1.234c
    !>  `@ux`0x1.234c
  ::
  %+  expect-eq
    !>  0x12e.3e56
    !>  `@ux`0x12e.3e56
  ::
  %+  expect-eq
    !>  0x123.4e67
    !>  `@ux`0x123.4e67
  ::
  %+  expect-eq
    !>  0x1234.567f
    !>  `@ux`0x1234.567f
  ::
  %+  expect-eq
    !>  0x1.2345.6789
    !>  `@ux`0x1.2345.6789
  ::
  %+  expect-eq
    !>  0x1.0000.0000
    !>  `@ux`0x1.0000.0000
  ::
  %+  expect-eq
    !>  0x1.0110.1101
    !>  `@ux`0x1.0110.1101
  ::
  %+  expect-eq
    !>  0x2.0120.1201
    !>  `@ux`0x2.0120.1201
  ::
  %+  expect-eq
    !>  0x3.0220.1100
    !>  `@ux`0x3.0220.1100
  ::
  %+  expect-eq
    !>  0x1.0000.0000
    !>  `@ux`(bex 32)
  ::
  %-  expect-fail  |.((need (slaw %ux '0x')))
  %-  expect-fail  |.((need (slaw %ux 'x0')))
  %-  expect-fail  |.((need (slaw %ux '0x01')))
  %-  expect-fail  |.((need (slaw %ux '0x12.345')))
  %-  expect-fail  |.((need (slaw %ux '0x12.3456.789')))
  %-  expect-fail  |.((need (slaw %ux '0x1.2.3456.789')))
  ==
::
++  test-render-ux
  ;:  weld
  ::
  %+  expect-eq
    !>  ~.0x0
    !>  (scot %ux 0x0)
  %+  expect-eq
    !>  ~.0x1
    !>  (scot %ux 0x1)
  ::
  %+  expect-eq
    !>  ~.0x12
    !>  (scot %ux 0x12)
  %+  expect-eq
    !>  ~.0x123
    !>  (scot %ux 0x123)
  %+  expect-eq
    !>  ~.0x1234
    !>  (scot %ux 0x1234)
  ::
  %+  expect-eq
    !>  ~.0x1.2345
    !>  (scot %ux 0x1.2345)
  ::
  %+  expect-eq
    !>  ~.0x12.3456
    !>  (scot %ux 0x12.3456)
  ::
  %+  expect-eq
    !>  ~.0x123.4567
    !>  (scot %ux 0x123.4567)
  ::
  %+  expect-eq
    !>  ~.0x1234.5678
    !>  (scot %ux 0x1234.5678)
  ::
  %+  expect-eq
    !>  ~.0x1.2345.6789
    !>  (scot %ux 0x1.2345.6789)
  ::
  %+  expect-eq
    !>  ~.0x1.0000.0000
    !>  (scot %ux 0x1.0000.0000)
  ::
  %+  expect-eq
    !>  ~.0x1.0110.1101
    !>  (scot %ux 0x1.0110.1101)
  ::
  %+  expect-eq
    !>  ~.0x2.0120.1201
    !>  (scot %ux 0x2.0120.1201)
  ::
  %+  expect-eq
    !>  ~.0x3.0220.1100
    !>  (scot %ux 0x3.0220.1100)
  ::
  %+  expect-eq
    !>  ~.0x123.4567.89ab.cdef
    !>  (scot %ux 0x123.4567.89ab.cdef)
  ::
  %+  expect-eq
    !>  ~.0x85.8993.4592
    !>  (scot %ux 0x85.8993.4592)
  ::
  %+  expect-eq
    !>  ~.0x5843.0092.1369.3952
    !>  (scot %ux 0x5843.0092.1369.3952)
  ::
  %+  expect-eq
    !>  ~.0x6744.0737.0955.1615
    !>  (scot %ux 0x6744.0737.0955.1615)
  ::
  ==
::
++  test-parse-uv
  ;:  weld
  %+  expect-eq
    !>  0v0
    !>  `@uv`0x0
  ::
  %+  expect-eq
    !>  0v0
    !>  `@uv`0x0
  ::
  %+  expect-eq
  !>  0v1
  !>  `@uv`0x1
  ::
  %+  expect-eq
  !>  0v12345
  !>  `@uv`0x11.0c85
  ::
  %+  expect-eq
  !>  0v6789a
  !>  `@uv`0x63.a12a
  ::
  %+  expect-eq
  !>  0vbcdef
  !>  `@uv`0xb6.35cf
  ::
  %+  expect-eq
  !>  0vghijk
  !>  `@uv`0x108.ca74
  ::
  %+  expect-eq
  !>  0vlmnop
  !>  `@uv`0x15b.5f19
  ::
  %+  expect-eq
  !>  0vqrstu
  !>  `@uv`0x1ad.f3be
  ::
  %+  expect-eq
  !>  0vabcdv
  !>  `@uv`0xa5.b1bf
  ::
  %+  expect-eq
  !>  0v123.12345
  !>  `@uv`0x8.8611.0c85
  ::
  %+  expect-eq
  !>  0v123.6789a
  !>  `@uv`0x8.8663.a12a
  ::
  %+  expect-eq
  !>  0v123.bcdef
  !>  `@uv`0x8.86b6.35cf
  ::
  %+  expect-eq
  !>  0v123.ghijk
  !>  `@uv`0x8.8708.ca74
  ::
  %+  expect-eq
  !>  0v123.lmnop
  !>  `@uv`0x8.875b.5f19
  ::
  %+  expect-eq
  !>  0v123.qrstu
  !>  `@uv`0x8.87ad.f3be
  ::
  %+  expect-eq
  !>  0v123.v123v
  !>  `@uv`0x8.87f0.887f
  ::
  %+  expect-eq
  !>  0vvv.vvvvv.vvvvv
  !>  `@uv`0xfff.ffff.ffff.ffff
  ::
  %-  expect-fail  |.((need (slaw %uv '0v')))
  %-  expect-fail  |.((need (slaw %uv 'v0')))
  %-  expect-fail  |.((need (slaw %uv '0v01')))
  %-  expect-fail  |.((need (slaw %uv '0v12.345')))
  %-  expect-fail  |.((need (slaw %uv '0v12.f3456.v789')))
  %-  expect-fail  |.((need (slaw %uv '0v1.3456v.v789vv')))
  ==
++  test-render-uv
  ;:  weld
  %+  expect-eq
    !>  ~.0v0
    !>  (scot %uv 0x0)
  ::
  %+  expect-eq
    !>  ~.0v0
    !>  (scot %uv 0x0)
  ::
  %+  expect-eq
    !>  ~.0v1
    !>  (scot %uv 0x1)
  ::
  %+  expect-eq
    !>  ~.0vg
    !>  (scot %uv 0x10)
  ::
  %+  expect-eq
    !>  ~.0vi
    !>  (scot %uv 0x12)
  ::
  %+  expect-eq
    !>  ~.0v93
    !>  (scot %uv 0x123)
  ::
  %+  expect-eq
    !>  ~.0v4hk
    !>  (scot %uv 0x1234)
  ::
  %+  expect-eq
    !>  ~.0v28q5
    !>  (scot %uv 0x1.2345)
  ::
  %+  expect-eq
    !>  ~.0v14d2m
    !>  (scot %uv 0x12.3456)
  ::
  %+  expect-eq
    !>  ~.0vi6hb7
    !>  (scot %uv 0x123.4567)
  ::
  %+  expect-eq
    !>  ~.0v9.38ljo
    !>  (scot %uv 0x1234.5678)
  ::
  %+  expect-eq
    !>  ~.0v4h.kaps9
    !>  (scot %uv 0x1.2345.6789)
  ::
  %+  expect-eq
    !>  ~.0v40.00000
    !>  (scot %uv 0x1.0000.0000)
  ::
  %+  expect-eq
    !>  ~.0v40.h0481
    !>  (scot %uv 0x1.0110.1101)
  ::
  %+  expect-eq
    !>  ~.0v80.i04g1
    !>  (scot %uv 0x2.0120.1201)
  ::
  %+  expect-eq
    !>  ~.0vc1.20480
    !>  (scot %uv 0x3.0220.1100)
  ::
  %+  expect-eq
    !>  ~.0v28.q5cu4.qnjff
    !>  (scot %uv 0x123.4567.89ab.cdef)
  ::
  %+  expect-eq
    !>  ~.0vgm4.p6hci
    !>  (scot %uv 0x85.8993.4592)
  ::
  %+  expect-eq
    !>  ~.0v5gg.o0i89.mieai
    !>  (scot %uv 0x5843.0092.1369.3952)
  ::
  %+  expect-eq
    !>  ~.0v6eh.076s4.la5gl
    !>  (scot %uv 0x6744.0737.0955.1615)
  ::
  %+  expect-eq
    !>  ~.0v8.i6hb7.h6lsr.ro14d.2mf2d.bpnng
    !>  (scot %uv 0x1.1234.5678.9abc.def0.1234.5678.9abc.def0)
  ==
::
++  test-parse-uw
  ;:  weld
  %+  expect-eq
    !>  0w0
    !>  `@uw`0x0
  ::
  %+  expect-eq
    !>  0w1
    !>  `@uw`0x1
  ::
  %+  expect-eq
    !>  0w12345
    !>  `@uw`0x108.3105
  ::
  %+  expect-eq
    !>  0w6789a
    !>  `@uw`0x61c.824a
  ::
  %+  expect-eq
    !>  0wbcdef
    !>  `@uw`0xb30.d38f
  ::
  %+  expect-eq
    !>  0wghijk
    !>  `@uw`0x1045.24d4
  ::
  %+  expect-eq
    !>  0wlmnop
    !>  `@uw`0x1559.7619
  ::
  %+  expect-eq
    !>  0wqrstu
    !>  `@uw`0x1a6d.c75e
  ::
  %+  expect-eq
    !>  0wvwxyz
    !>  `@uw`0x1f82.18a3
  ::
  %+  expect-eq
    !>  0wABCDE
    !>  `@uw`0x2496.69e8
  ::
  %+  expect-eq
    !>  0wFGHIJ
    !>  `@uw`0x29aa.bb2d
  ::
  %+  expect-eq
    !>  0wKLMNO
    !>  `@uw`0x2ebf.0c72
  ::
  %+  expect-eq
    !>  0wPQRST
    !>  `@uw`0x33d3.5db7
  ::
  %+  expect-eq
    !>  0wUVWXY
    !>  `@uw`0x38e7.aefc
  ::
  %+  expect-eq
    !>  0wZ~-9a
    !>  `@uw`0x3dff.e24a
  ::
  %+  expect-eq
    !>  0w-----
    !>  `@uw`0x3efb.efbe
  ::
  %+  expect-eq
    !>  0w~~~~~
    !>  `@uw`0x3fff.ffff
  ::
  %+  expect-eq
    !>  0w-~-~-
    !>  `@uw`0x3eff.effe
  ::
  %+  expect-eq
    !>  0w1234.abcde
    !>  `@uw`0x1.0831.0a2c.c34e
  ::
  %+  expect-eq
    !>   0wfghij.klmno
    !>  `@uw`0x3d0.4524.d455.65d8
  ::
  %+  expect-eq
    !>   0wpqrst.uvwxy
    !>  `@uw`0x65a.6dc7.5e7e.0862
  ::
  %+  expect-eq
    !>   0wzABCD.EFGHI
    !>  `@uw`0x8e4.9669.e8a6.aaec
  ::
  %+  expect-eq
    !>   0wJKLMN.OPQRS
    !>  `@uw`0xb6e.bf0c.72cf.4d76
  ::
  %+  expect-eq
    !>   0wTUVWX.YZ123
    !>  `@uw`0xdf8.e7ae.fcf4.1083
  ::
  %+  expect-eq
    !>   0w45678.9-~--
    !>  `@uw`0x105.1872.09fb.ffbe
  ::
  %+  expect-eq
    !>   0wf.~~~~~.~~~~~
    !>  `@uw`0xffff.ffff.ffff.ffff
  ::
  %+  expect-eq
    !>  0w4i.d5pUC.HPuY1.8QlDy.qLdXM
    !>  `@uw`0x1.1234.5678.9abc.def0.1234.5678.9abc.def0
  ::
  %-  expect-fail  |.((need (slaw %uw 'w0')))
  %-  expect-fail  |.((need (slaw %uw '0w01')))
  %-  expect-fail  |.((need (slaw %uw '0w12.345')))
  %-  expect-fail  |.((need (slaw %uw '0w1~.f3456.-789')))
  %-  expect-fail  |.((need (slaw %uw '0w1.3456-.-789~-')))
  %-  expect-fail  |.((need (slaw %uw '0wwwwww.wwwwww')))
  ==
::
++  test-render-uw
  ;:  weld
  ::
  %+  expect-eq
    !>  '0w0'
    !>  (scot %uw 0x0)
  ::
  %+  expect-eq
    !>  '0w1'
    !>  (scot %uw 0x1)
  ::
  %+  expect-eq
    !>  '0w12345'
    !>  (scot %uw 0x108.3105)
  ::
  %+  expect-eq
    !>  '0w6789a'
    !>  (scot %uw 0x61c.824a)
  ::
  %+  expect-eq
    !>  '0wbcdef'
    !>  (scot %uw 0xb30.d38f)
  ::
  %+  expect-eq
    !>  '0wghijk'
    !>  (scot %uw 0x1045.24d4)
  ::
  %+  expect-eq
    !>  '0wlmnop'
    !>  (scot %uw 0x1559.7619)
  ::
  %+  expect-eq
    !>  '0wqrstu'
    !>  (scot %uw 0x1a6d.c75e)
  ::
  %+  expect-eq
    !>  '0wvwxyz'
    !>  (scot %uw 0x1f82.18a3)
  ::
  %+  expect-eq
    !>  '0wABCDE'
    !>  (scot %uw 0x2496.69e8)
  ::
  %+  expect-eq
    !>  '0wFGHIJ'
    !>  (scot %uw 0x29aa.bb2d)
  ::
  %+  expect-eq
    !>  '0wKLMNO'
    !>  (scot %uw 0x2ebf.0c72)
  ::
  %+  expect-eq
    !>  '0wPQRST'
    !>  (scot %uw 0x33d3.5db7)
  ::
  %+  expect-eq
    !>  '0wUVWXY'
    !>  (scot %uw 0x38e7.aefc)
  ::
  %+  expect-eq
    !>  '0wZ~-9a'
    !>  (scot %uw 0x3dff.e24a)
  ::
  %+  expect-eq
    !>  '0w-----'
    !>  (scot %uw 0x3efb.efbe)
  ::
  %+  expect-eq
    !>  '0w~~~~~'
    !>  (scot %uw 0x3fff.ffff)
  ::
  %+  expect-eq
    !>  '0w-~-~-'
    !>  (scot %uw 0x3eff.effe)
  ::
  %+  expect-eq
    !>  '0w1234.abcde'
    !>  (scot %uw 0x1.0831.0a2c.c34e)
  ::
  %+  expect-eq
    !>   '0wfghij.klmno'
    !>  (scot %uw 0x3d0.4524.d455.65d8)
  ::
  %+  expect-eq
    !>   '0wpqrst.uvwxy'
    !>  (scot %uw 0x65a.6dc7.5e7e.0862)
  ::
  %+  expect-eq
    !>   '0wzABCD.EFGHI'
    !>  (scot %uw 0x8e4.9669.e8a6.aaec)
  ::
  %+  expect-eq
    !>   '0wJKLMN.OPQRS'
    !>  (scot %uw 0xb6e.bf0c.72cf.4d76)
  ::
  %+  expect-eq
    !>   '0wTUVWX.YZ123'
    !>  (scot %uw 0xdf8.e7ae.fcf4.1083)
  ::
  %+  expect-eq
    !>   '0w45678.9-~--'
    !>  (scot %uw 0x105.1872.09fb.ffbe)
  ::
  %+  expect-eq
    !>   '0wf.~~~~~.~~~~~'
    !>  (scot %uw 0xffff.ffff.ffff.ffff)
  ::
  %+  expect-eq
    !>  '0wwwwww.wwwww'
    !>  (scot %uw 0x820.8208.2082.0820)
  ::
  %+  expect-eq
    !>  '0w9.37a8e.elucg.lcgpl.~--38.alllz.-----.~~~~~'
    !>  (scot %uw 0x24.31ca.20e3.9578.c415.3106.55ff.ef83.20a5.5556.3fbe.fbef.bfff.ffff)
  ==
::
++  test-parse-p
  ;:  weld
    %+  expect-eq
      !>  ~zod
      !>  `@p`0x0
    ::
    %+  expect-eq
      !>  ~wes
      !>  `@p`0x3
    ::
    %+  expect-eq
      !>  ~dep
      !>  `@p`0x17
    ::
    %+  expect-eq
      !>  ~led
      !>  `@p`0x29
    ::
    %+  expect-eq
      !>  ~myl
      !>  `@p`0xbf
    ::
    %+  expect-eq
      !>  ~wel
      !>  `@p`0xcf
    ::
    %+  expect-eq
      !>  ~fes
      !>  `@p`0xff
    ::
    %+  expect-eq
      !>  ~marryd
      !>  `@p`0x1cc
    ::
    %+  expect-eq
      !>  ~dalnup
      !>  `@p`0x2513
    ::
    %+  expect-eq
      !>  ~dacwyc
      !>  `@p`0x753b
    ::
    %+  expect-eq
      !>  ~dibwet
      !>  `@p`0xb365
    ::
    %+  expect-eq
      !>  ~rislep
      !>  `@p`0xdcaa
    ::
    %+  expect-eq
      !>  ~fipfes
      !>  `@p`0xffff
    ::
    %+  expect-eq
      !>  ~hocmeb-dapsen
      !>  `@p`0x6d.2030
    ::
    %+  expect-eq
      !>  ~divbud-ladbyn
      !>  `@p`0x108d.eca3
    ::
    %+  expect-eq
      !>  ~mopten-hilfex
      !>  `@p`0x64f4.eace
    ::
    %+  expect-eq
      !>  ~tinbyn-fammun
      !>  `@p`0xa1ae.3130
    ::
    %+  expect-eq
      !>  ~dinnex-sonnum
      !>  `@p`0xb91f.853a
    ::
    %+  expect-eq
      !>  ~dostec-risfen
      !>  `@p`0xffff.ffff
    ::
    %+  expect-eq
      !>  ~sigmyl-bintus-sovpet
      !>  `@p`0x6bf.c3f1.881b
    ::
    %+  expect-eq
      !>  ~novweg-bilnet-radfep
      !>  `@p`0x46f6.e045.8bc7
    ::
    %+  expect-eq
      !>  ~boswyd-lagdut-tobhes
      !>  `@p`0xab36.928a.695b
    ::
    %+  expect-eq
      !>  ~larpub-bacfus-nisbex
      !>  `@p`0xe1a6.70e9.eebd
    ::
    %+  expect-eq
      !>  ~tonbyl-dasryg-bitlen
      !>  `@p`0xf6b0.1478.1344
    ::
    %+  expect-eq
      !>  ~fipfes-dostec-risfen
      !>  `@p`0xffff.ffff.ffff
    ::
    %+  expect-eq
      !>  ~lisnet-rivnys-natdem-donful
      !>  `@p`0x94f.ede6.4d31.f2a0
    ::
    %+  expect-eq
      !>  ~dibryg-bichut-witsev-fanpub
      !>  `@p`0xb396.38ae.3f90.9214
    ::
    %+  expect-eq
      !>  ~nilsul-picpur-nocsem-tasrys
      !>  `@p`0xd226.683f.5a2f.a433
    ::
    %+  expect-eq
      !>  ~fopbyt-worwes-rolput-nodruc
      !>  `@p`0xd5bc.5e03.458e.7790
    ::
    %+  expect-eq
      !>  ~fitwes-hopfep-bitwyd-doswer
      !>  `@p`0xe203.1698.49fc.1124
    ::
    %+  expect-eq
      !>  ~fipfes-fipfes-dostec-risfen
      !>  `@p`0xffff.ffff.ffff.ffff
    ::
    %+  expect-eq
      !>  ~dirrul-radner-dister-ritnus--taddus-digmus-torlun-filnyt
      !>  `@p`0xb46.c9c7.8171.50a2.3781.c15d.2c20.c2f3
    ::
    %+  expect-eq
      !>  ~patdun-hinfel-fadpem-tocnyd--nactyl-tadpel-balrev-tipsym
      !>  `@p`0x9fd9.c878.ceb2.bdd0.db35.376f.6b8b.495f
    ::
    %+  expect-eq
      !>  ~radbec-sipfes-harryt-fitdun--rovheb-haprys-morrel-bostux
      !>  `@p`0xc9ee.5fff.c61c.e2d9.2314.9c40.5dd2.ab6c
    ::
    %+  expect-eq
      !>  ~podmeb-sovsep-silryl-fotrep--pintux-fotfex-wictyp-sivder
      !>  `@p`0x5172.f923.1ede.ddfa.a56c.ddc3.63bd.703d
    ::
    %+  expect-eq
      !>  ~tolfex-watden-ragmer-navren--folseg-lonryc-rispyx-fiptes
      !>  `@p`0x54c3.6542.cc9f.897c.1b85.87e4.dc95.fff1
    ::
    %+  expect-eq
      !>  ~locpes--batweg-toplys-rivren-ripreg--moldyt-hadted-wachut-witnec
      !>  `@p`0x4524.acf6.cfe1.ed7c.9748.43bb.918e.0cae.4c01
    ::
    %+  expect-eq
      !>  ~napsyx-sivtus-nibdys--nibwen-sonnut-ripped-walden--rispur-hollyn-bitmyn-davlys
      !>  `@p`0x5711.709a.8c18.8c2c.a443.9727.e342.dc3f.20e8.b667.69e1
  ::
  %+  expect-eq
  !>  ~doznec--doprut-posfel-tilbyt-riblyr--doprut-posfel-tilbyt-riblyr
  !>  `@p`0x1.1234.5678.9abc.def0.1234.5678.9abc.def0
  %-  expect-fail  |.((need (slaw %p '~')))
  %-  expect-fail  |.((need (slaw %p '~doz')))
  %-  expect-fail  |.((need (slaw %p '~dozzod')))
  %-  expect-fail  |.((need (slaw %p '~doznec')))
  %-  expect-fail  |.((need (slaw %p '~bin-zod')))
  %-  expect-fail  |.((need (slaw %p '~zod-mipryg-rambyn')))
  %-  expect-fail  |.((need (slaw %p '~doz-mipryg-rambyn')))
  %-  expect-fail  |.((need (slaw %p '~dozzod-fipfes-dostec-risfen')))
  %-  expect-fail  |.((need (slaw %p '~fipfes-fipfes-dostec-risfen--')))
  %-  expect-fail  |.((need (slaw %p '~fipfes-fipfes--dostec-risfen')))
  ==
::
++  test-render-p
  ;:  weld
    %+  expect-eq
      !>  ~.~zod
      !>  (scot %p 0x0)
    ::
    %+  expect-eq
      !>  ~.~wes
      !>  (scot %p 0x3)
    ::
    %+  expect-eq
      !>  ~.~dep
      !>  (scot %p 0x17)
    ::
    %+  expect-eq
      !>  ~.~led
      !>  (scot %p 0x29)
    ::
    %+  expect-eq
      !>  ~.~myl
      !>  (scot %p 0xbf)
    ::
    %+  expect-eq
      !>  ~.~wel
      !>  (scot %p 0xcf)
    ::
    %+  expect-eq
      !>  ~.~fes
      !>  (scot %p 0xff)
    ::
    %+  expect-eq
      !>  ~.~marryd
      !>  (scot %p 0x1cc)
    ::
    %+  expect-eq
      !>  ~.~dalnup
      !>  (scot %p 0x2513)
    ::
    %+  expect-eq
      !>  ~.~dacwyc
      !>  (scot %p 0x753b)
    ::
    %+  expect-eq
      !>  ~.~dibwet
      !>  (scot %p 0xb365)
    ::
    %+  expect-eq
      !>  ~.~rislep
      !>  (scot %p 0xdcaa)
    ::
    %+  expect-eq
      !>  ~.~fipfes
      !>  (scot %p 0xffff)
    ::
    %+  expect-eq
      !>  ~.~hocmeb-dapsen
      !>  (scot %p 0x6d.2030)
    ::
    %+  expect-eq
      !>  ~.~divbud-ladbyn
      !>  (scot %p 0x108d.eca3)
    ::
    %+  expect-eq
      !>  ~.~mopten-hilfex
      !>  (scot %p 0x64f4.eace)
    ::
    %+  expect-eq
      !>  ~.~tinbyn-fammun
      !>  (scot %p 0xa1ae.3130)
    ::
    %+  expect-eq
      !>  ~.~dinnex-sonnum
      !>  (scot %p 0xb91f.853a)
    ::
    %+  expect-eq
      !>  ~.~dostec-risfen
      !>  (scot %p 0xffff.ffff)
    ::
    %+  expect-eq
      !>  ~.~sigmyl-bintus-sovpet
      !>  (scot %p 0x6bf.c3f1.881b)
    ::
    %+  expect-eq
      !>  ~.~novweg-bilnet-radfep
      !>  (scot %p 0x46f6.e045.8bc7)
    ::
    %+  expect-eq
      !>  ~.~boswyd-lagdut-tobhes
      !>  (scot %p 0xab36.928a.695b)
    ::
    %+  expect-eq
      !>  ~.~larpub-bacfus-nisbex
      !>  (scot %p 0xe1a6.70e9.eebd)
    ::
    %+  expect-eq
      !>  ~.~tonbyl-dasryg-bitlen
      !>  (scot %p 0xf6b0.1478.1344)
    ::
    %+  expect-eq
      !>  ~.~fipfes-dostec-risfen
      !>  (scot %p 0xffff.ffff.ffff)
    ::
    %+  expect-eq
      !>  ~.~lisnet-rivnys-natdem-donful
      !>  (scot %p 0x94f.ede6.4d31.f2a0)
    ::
    %+  expect-eq
      !>  ~.~dibryg-bichut-witsev-fanpub
      !>  (scot %p 0xb396.38ae.3f90.9214)
    ::
    %+  expect-eq
      !>  ~.~nilsul-picpur-nocsem-tasrys
      !>  (scot %p 0xd226.683f.5a2f.a433)
    ::
    %+  expect-eq
      !>  ~.~fopbyt-worwes-rolput-nodruc
      !>  (scot %p 0xd5bc.5e03.458e.7790)
    ::
    %+  expect-eq
      !>  ~.~fitwes-hopfep-bitwyd-doswer
      !>  (scot %p 0xe203.1698.49fc.1124)
    ::
    %+  expect-eq
      !>  ~.~fipfes-fipfes-dostec-risfen
      !>  (scot %p 0xffff.ffff.ffff.ffff)
    ::
    %+  expect-eq
      !>  ~.~dirrul-radner-dister-ritnus--taddus-digmus-torlun-filnyt
      !>  (scot %p 0xb46.c9c7.8171.50a2.3781.c15d.2c20.c2f3)
    ::
    %+  expect-eq
      !>  ~.~patdun-hinfel-fadpem-tocnyd--nactyl-tadpel-balrev-tipsym
      !>  (scot %p 0x9fd9.c878.ceb2.bdd0.db35.376f.6b8b.495f)
    ::
    %+  expect-eq
      !>  ~.~radbec-sipfes-harryt-fitdun--rovheb-haprys-morrel-bostux
      !>  (scot %p 0xc9ee.5fff.c61c.e2d9.2314.9c40.5dd2.ab6c)
    ::
    %+  expect-eq
      !>  ~.~podmeb-sovsep-silryl-fotrep--pintux-fotfex-wictyp-sivder
      !>  (scot %p 0x5172.f923.1ede.ddfa.a56c.ddc3.63bd.703d)
    ::
    %+  expect-eq
      !>  ~.~tolfex-watden-ragmer-navren--folseg-lonryc-rispyx-fiptes
      !>  (scot %p 0x54c3.6542.cc9f.897c.1b85.87e4.dc95.fff1)
    ::
    %+  expect-eq
      !>  ~.~locpes--batweg-toplys-rivren-ripreg--moldyt-hadted-wachut-witnec
      !>  (scot %p 0x4524.acf6.cfe1.ed7c.9748.43bb.918e.0cae.4c01)
    ::
    %+  expect-eq
      !>  ~.~napsyx-sivtus-nibdys--nibwen-sonnut-ripped-walden--rispur-hollyn-bitmyn-davlys
      !>  (scot %p 0x5711.709a.8c18.8c2c.a443.9727.e342.dc3f.20e8.b667.69e1)
  ==
::
++  test-parse-q
  ;:  weld
    %+  expect-eq
      !>  .~zod
      !>  `@q`0x0
    ::
    %+  expect-eq
      !>  .~marbud
      !>  `@q`0x102
    ::
    %+  expect-eq
      !>  .~nec-marbud
      !>  `@q`0x1.0102
    ::
    %+  expect-eq
      !>  .~marnec-marnec-marnec-marnec-marbud
      !>  `@q`0x101.0101.0101.0101.0102
    ::
  ==
::
++  test-render-q
  ;:  weld
    %+  expect-eq
      !>  '.~zod'
      !>  (scot %q 0x0)
    ::
    %+  expect-eq
      !>  '.~marbud'
      !>  (scot %q 0x102)
    ::
    %+  expect-eq
      !>  '.~nec-marbud'
      !>  (scot %q 0x1.0102)
    ::
    %+  expect-eq
      !>  '.~marnec-marnec-marnec-marnec-marbud'
      !>  (scot %q 0x101.0101.0101.0101.0102)
    ::
  ==
::
++  test-sane
  %-  expect
  !>(((sane %t) '🤔'))
--
