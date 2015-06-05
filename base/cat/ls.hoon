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
tang/[?~(r.lon leaf/"~" (subdir arg r.lon))]~
