::  Cancel a pending merge in Clay
::
::::  /hoon/cancel/hood/gen
  ::
/?    310
::
::::
  ::
:-  %say
|=  [* a=$@(~ [@tas ~]) *]
[%kiln-cancel ?@(a %foo -.a)]
