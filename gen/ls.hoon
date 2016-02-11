::  LiSt directory subnodes
::
::::  /hoon/ls/gen
  ::
/?    314
//  /%/subdir
!:
::::
  ::
:-  %say
|=  {^ {arg/path $~} $~}
=+  lon=((hard arch) .^(%cy arg))
tang+[?~(dir.lon leaf+"~" (subdir arg dir.lon))]~
