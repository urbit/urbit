::  LiSt directory subnodes
::
::::  /hoon/ls/gen
  ::
//  /%/subdir
!:
::::
  ::
:-  %say
|=  [^ [arg=path ~] vane=?(%c %g)]
=+  lon=((hard arch) .^((cat 3 vane %y) arg))
tang/[?~(dir.lon leaf/"~" (subdir vane arg dir.lon))]~
