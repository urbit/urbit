::  Test that these hints do not crash the runtime
::    there is no need to include a test for dynamic %bout
::    since all hoon tests exersize dynamic %bout
|%
::  test that these trace hints
::  are safe to run or ignore
++  test-09-hilt-hela
  ~>  %hela
  ~
++  test-08-hint-hela
  ~>  %hela.[1 leaf+"test-08-hint-hela ~"]
  ~
++  test-07-hilt-nara
  ~>  %nara
  ~
++  test-06-hint-nara
  ~>  %nara.[1 leaf+"test-06-hint-nara ~"]
  ~
::  test that these memory-report hints
::  are safe to run or ignore
++  test-05-hilt-meme
  ~>  %meme
  ~
++  test-04-hint-meme
  ~>  %meme.[1 leaf+"test-04-hint-meme ~"]
  ~
::  test that theses bytecode-report hints
::  are safe to run or ignore
++  test-03-hilt-xray
  ~>  %xray
  ~
++  test-02-hint-xray
  ~>  %xray.[1 leaf+"test-02-hint-xray ~"]
  ~
::  test that the hilt bout hint
::  is safe to run or ignore
++  test-01-hilt-bout
  ~>  %bout
  ~
--
