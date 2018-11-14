::  list addressing bugs: &33:~ bails out
::
/+  *test
|%
::
++  test-hang
  %-  expect-fail
    |.  &33:~[[%leaf p="syntax error"] [%leaf p="\{1 11}"]]
::
++  test-vere-bail
  %-  expect-fail
    |.  &33:~
--
