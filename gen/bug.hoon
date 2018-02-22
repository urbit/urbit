::  "Hello world" sample generator  
::
::::  /hoon/hello/gen
  ::
/?    310
::
::::
  ::
!:
:-  %say
|=  *
=>  ^%
    |%
    ++  one
--
=>  |%
    ++  de
      |=  foo/@ud
      !!
    ++  moo
      $:ankh
    --
:-  %noun
=+  foo=moo
"hello, world"
