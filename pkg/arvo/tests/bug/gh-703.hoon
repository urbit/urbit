::  list addressing bugs: &33:~ bails out
::
/+  *test
|%
::
++  test-addr-no-hang
  %-  expect-fail
    |.  &33:~[[%leaf p="syntax error"] [%leaf p="\{1 11}"]]
::
++  test-addr-no-bail
  %-  expect-fail
    |.  &33:~
--
