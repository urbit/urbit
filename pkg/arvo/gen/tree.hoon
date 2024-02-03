::  Tree view recursive directory contents
::
::::  /hoon/tree/gen
  ::
/?    310
::
::::
  ::
:-  %say
|=  [[now=@da tick=@ud *] [pax=path ~] ~]
=.  pax  (en-pick now tick pax)
[%tang `tang`(flop (turn .^((list path) %ct pax) smyt))]
