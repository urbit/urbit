::  Test that these hints do not crash the runtime
::    there is no need to include the hints for dynamic %bout
::    since all hoon tests exersize dynamic %bout
|%
::  these test that the hilt-trace hints
::  are safe to run or ignore
++  test-hela-hilt
  ~>  %hela
  ~
++  test-nara-hilt
  ~>  %nara
  ~
::  these test that the hint-trace hints
::  are safe to run or ignore
++  test-hela-hint
  ~>  %hela.[1 'test-hela-trace-hint']
  ~
++  test-nara-hint
  ~>  %nara.[1 'test-nara-trace-hint']
  ~
--
