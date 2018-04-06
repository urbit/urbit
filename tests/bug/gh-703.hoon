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
  ~&  [`path`% running-unsafe+"bails out with out of loom error"]
  %-  expect-fail
    |.  &33:~
--
