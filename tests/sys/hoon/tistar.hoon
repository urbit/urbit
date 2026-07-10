/+  *test
|%
++  test-tistar-no-self-reference
  %-  expect-success  |.
  =+  a=1
  =*  a  +(a)
  ?>(=(2 a) ~)
--
