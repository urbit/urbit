::
=<  policy
|%
+$  kind  ?(%read %write)
::
+$  policy
  $~  [%black ~ ~]  
  $%  black
      white
  ==
::  $diff: change group policy
+$  diff
  $%  [%black diff:black]
      [%white diff:white]
      [%replace =policy]
  ==
::  $white: allow only some ships
++  white
  =<  white-policy
  |%
  ::
  +$  white-policy
    [%white p=(set ship)]
  ::  $diff: add or remove blacks
  ::
  +$  diff
    $%  [%add p=(set ship)]
        [%del p=(set ship)]
    ==
  --
::  $black: allow all unbanned ships of approriate rank
::
++  black
  =<  black-policy
  |%
  ::
  +$  black-policy
    [%black ranks=(set rank:title) ships=(set ship)]
  :: $diff: ban or allow ranks and ships
  ::
  +$  diff
    $%  [%allow-ranks p=(set rank:title)]
        [%ban-ranks p=(set rank:title)]
        [%ban-ships p=(set ship)]
        [%allow-ships p=(set ship)]
    ==
  --
--
