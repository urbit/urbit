/+  *test
|%
++  test-parse-p
  ;:  weld
    %+  expect-eq
      !>  ~zod
      !>  `@p`0
    ::
    %+  expect-eq
      !>  ~lex
      !>  `@p`200
    ::
    %+  expect-eq
      !>  ~binzod
      !>  `@p`512
    ::
    %+  expect-eq
      !>  ~samzod
      !>  `@p`1.024
    ::
    %+  expect-eq
      !>  ~poldec-tonteg
      !>  `@p`9.896.704
    ::
    %+  expect-eq
      !>  ~nidsut-tomdun
      !>  `@p`15.663.360
    ::
    %+  expect-eq
      !>  ~morlyd-mogmev
      !>  `@p`3.108.299.008
    ::
    %+  expect-eq
      !>  ~fipfes-morlyd
      !>  `@p`479.733.505
    ::
    %+  expect-eq
      !>  ~dilwes-fadnel
      !>  `@p`23.554.048
    ::
    %+  expect-eq
      !>  ~fipfes-dilwes
      !>  `@p`529.511.092
    ::
    %+  expect-eq
      !>  ~hossev-roppec
      !>  `@p`1.859.915.444
    ::
    %+  expect-eq
      !>  ~fipfes-hossev
      !>  `@p`145.391.618
    ::
  ==
::
++  test-render-p
  ;:  weld
    %+  expect-eq
      !>  '~zod'
      !>  (scot %p 0)
    ::
    %+  expect-eq
      !>  '~lex'
      !>  (scot %p 200)
    ::
    %+  expect-eq
      !>  '~binzod'
      !>  (scot %p 512)
    ::
    %+  expect-eq
      !>  '~samzod'
      !>  (scot %p 1.024)
    ::
    %+  expect-eq
      !>  '~poldec-tonteg'
      !>  (scot %p 9.896.704)
    ::
    %+  expect-eq
      !>  '~nidsut-tomdun'
      !>  (scot %p 15.663.360)
    ::
    %+  expect-eq
      !>  '~morlyd-mogmev'
      !>  (scot %p 3.108.299.008)
    ::
    %+  expect-eq
      !>  '~fipfes-morlyd'
      !>  (scot %p 479.733.505)
    ::
    %+  expect-eq
      !>  '~dilwes-fadnel'
      !>  (scot %p 23.554.048)
    ::
    %+  expect-eq
      !>  '~fipfes-dilwes'
      !>  (scot %p 529.511.092)
    ::
    %+  expect-eq
      !>  '~hossev-roppec'
      !>  (scot %p 1.859.915.444)
    ::
    %+  expect-eq
      !>  '~fipfes-hossev'
      !>  (scot %p 145.391.618)
    ::
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
--
