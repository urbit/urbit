::  LiSt directory subnodes
::
::::  /hoon/ls/cat
  ::
//  /%/subdir
!:
::::
  ::
|=  [^ [arg=path ~] ~]
=+  lon=((hard arch) .^(%cy arg))
tang/[?~(dir.lon leaf/"~" (subdir arg dir.lon))]~
