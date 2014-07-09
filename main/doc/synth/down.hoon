!:
:::::::::   /synth/doc
::
  =>
::::::  models
|%
++  down
  $&  [p=down q=down]
  $%  [%$ p=tape]
      [%code p=tape]
      [%inco p=tape]
      [%head p=@ud q=down]
      [%link p=tape q=tape r=(unit tape)]
      [%lord p=(list down)]
      [%lund p=(list down)]
      [%parg p=down]
      [%quot p=down]
      [%rong p=down]
      [%emph p=down]
      [%hrul ~]
      [%html p=tape]
  ==
--  
::::::  program
::
%-  (fest /synth/doc %)
|=  pic=epic
=+  int=|=(a=cord (slav %ud (need (~(get by qix.pic) a))))
=+  [foo bar]=[(int %foo) (int %bar)]
[%$ "Hello, world - foo plus bar is {<(add foo bar)>}."]
