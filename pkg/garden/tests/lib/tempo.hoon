/+  *test, tempo
|%
++  utc  `@dau`~2022.2.9..21.32.58..1c5a
++  sta  ~2022.2.9..00.00.01
++  zon  --600
++  loc  `@dal`~2022.2.10..07.32.58..1c5a
++  test-to-utc
  %+  expect-eq  !>(utc)
  !>((~(to-utc tz:tempo zon) loc))
::
++  test-from-utc
  %+  expect-eq  !>(loc)
  !>((~(from-utc tz:tempo zon) utc))
++  test-start-of-day
  %+  expect-eq  !>(sta)
  !>((start-of-day:tempo utc))
--

