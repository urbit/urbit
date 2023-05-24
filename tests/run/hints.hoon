::  Test that these hints do not crash the runtime
::    there is no need to include a test for dynamic %bout
::    since all hoon tests exersize dynamic %bout
|%
::  test that these trace hints
::  are safe to run or ignore
::
::    XX disabled due to CI noise
::
++  disabled-test-hilt-hela
  ~>  %hela
  ~
++  disabled-test-hint-hela
  ~>  %hela.[1 leaf+"test-hint-hela ~"]
  ~
++  test-hilt-nara
  %-  need  %-  mole  |.
  ~|  %hilt-nara
  ~>  %nara
  ~
++  test-hint-nara
  %-  need  %-  mole  |.
  ~|  %hint-nara
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
::  test that these memory-report hints
::  are safe to run or ignore
++  test-hilt-meme
  ~>  %meme
  ~
++  test-hint-meme
  ~>  %meme.[1 leaf+"test-hint-meme ~"]
  ~
::  test that the hilt bout hint
::  is safe to run or ignore
++  test-hilt-bout
  ~>  %bout
  ~
--

