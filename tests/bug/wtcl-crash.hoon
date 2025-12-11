::  miscompilation: crashing conditional expression compiled out
::
/+  *test
|%
::
++  test-wtcl-cond-crash
  %-  expect-fail
  |.  %.  %foo
  |=  sam=$@(?(%foo %bar) [%baz @])
  ^-  [%baz @]
  ?>  ?=(%baz -.sam)
  sam
--
