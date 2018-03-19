::  "Hello world" sample generator
::
::::  /hoon/hello/gen
  ::
/?    310
::
::::
  ::
:-  %say
|=  {^ {{txt/@tas ~} ~}}
:-  %noun
(crip (weld "hello, " (trip txt)))
