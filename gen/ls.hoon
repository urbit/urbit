::  LiSt directory subnodes
::
::::  /hoon+ls+gen
  ::
//  /%/subdir
!:
::::
  ::
:-  %say
|=  {^ {arg/path $~} $~}
=+  lon=((hard arch) .^(%cy arg))
tang+[?~(dir.lon leaf+"~" (subdir arg dir.lon))]~
