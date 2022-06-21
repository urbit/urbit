::  Test that these hints do not crash the runtime
::    there is no need to include a test for dynamic %bout
::    since all hoon tests exersize dynamic %bout
|%
::  test that these trace hints
::  are safe to run or ignore
++  test-12-hilt-hela
  ~>  %hela
  ~
++  test-11-hint-hela
  ~>  %hela.[1 leaf+"test-11-hint-hela ~"]
  ~
++  test-10-hilt-nara
  ~>  %nara
  ~
++  test-09-hint-nara
  ~>  %nara.[1 leaf+"test-09-hint-nara ~"]
  ~
::  test that these memory-report hints
::  are safe to run or ignore
++  test-08-hilt-meme
  ~>  %meme
  ~
++  test-07-hint-meme
  ~>  %meme.[1 leaf+"test-07-hint-meme ~"]
  ~
::  test that theses bytecode-report hints
::  are safe to run or ignore
++  test-06-hilt-xray
  ~>  %xray
  ~
++  test-05-hint-xray
  ~>  %xray.[1 leaf+"test-05-hint-xray ~"]
  ~
:::: these tests show, but don't prove, if xray reads all internal bytecode
::++  test-04-hint-xray-add
::  ~>  %xray.[1 leaf+"(add 1 2)"]
::  (add 1 2)
::++  test-03-hint-xray-meme
::  ~>  %xray.[1 leaf+"(meme)"]
::  ~>  %meme
::  ~
::++  test-02-hint-xray-combo
::  ^-  @ud
::  ~>  %xray
::  ~>(%meme (mul 1 2))
::::  test that the hilt bout hint
::::  is safe to run or ignore
++  test-01-hilt-bout
  ~>  %bout
  ~
--
