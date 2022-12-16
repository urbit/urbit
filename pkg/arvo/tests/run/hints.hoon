::  Test that these hints do not crash the runtime
::    there is no need to include a test for dynamic %bout
::    since all hoon tests exersize dynamic %bout
|%
::  test that these trace hints
::  are safe to run or ignore
++  test-hilt-hela
  ~>  %hela
  ~
++  test-hint-hela
  ~>  %hela.[1 leaf+"test-hint-hela ~"]
  ~
++  test-hilt-nara
  ~>  %nara
  ~
++  test-hint-nara
  ~>  %nara.[1 leaf+"test-hint-nara ~"]
  ~
::  test that theses bytecode-report hints
::  are safe to run or ignore
++  test-hilt-xray
  ~>  %xray
  ~
++  test-hint-xray
  ~>  %xray.[1 leaf+"test-hint-xray ~"]
  ~
::  test that the hilt bout hint
::  is safe to run or ignore
++  test-hilt-bout
  ~>  %bout
  ~
--

