::  LiSt directory subnodes
::
::::  /hoon/ls/gen
  ::
/?    310
//  /%/subdir
::
::::
  ::
~&  %
:-  %say
|=  {^ {arg/path $~} vane/?($g $c)}
=+  lon=.^(arch (cat 3 vane %y) arg)
tang+[?~(dir.lon leaf+"~" (subdir vane arg dir.lon))]~
