::  Test that these hints do not crash the runtime
::    we only test the dynamic hints here
::    also, there is no need to include the hints for dynamic %bout
::    since all hoon tests exersize dynamic %bout
|%
::  this tests that the short trace hint
::  is safe to run or ignore
++  test-nara
  ~>  %nara.[1 leaf+"nara trace hint test"]
  ~
::  this tests that the full trace hint
::  is safe to run or ignore
++  test-hela
  ~>  %hela.[1 leaf+"hela trace hint test"]
  ~
--
