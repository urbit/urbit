::  LiSt directory subnodes
::
::::  /hoon/ls/gen
  ::
//  /%/subdir
!:
::::
  ::
:-  %cat
|=  [^ [arg=path ~] ~]
=+  lon=((hard arch) .^(%cy arg))
tang/[?~(dir.lon leaf/"~" (subdir arg dir.lon))]~
