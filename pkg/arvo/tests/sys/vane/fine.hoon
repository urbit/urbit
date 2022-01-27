/+  *test
/=  fine  /sys/vane/fine
::  construct some test fixtures
::
=/  nec  (fine ~nec)
=,  ^fine
|%
++  test-invertible-encoding
  =/  data   [%foo %bar %baz]
  =/  =path  /cz/~zod/foo/bar/baz
  =/  =song  (encode-response path `noun/`*`data)
  %-  zing
  %-  head
  %+  spin  song  1
  |=  [=purr num=@ud]
  :_  +(num)
  =/  =rawr  (decode-response-packet purr)
  (handle-response:bus


  %+  expect-eq  !>(data)






  

