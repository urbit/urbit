::  LiSt directory subnodes
::
::::  /hoon/ls/gen
  ::
/?    310
/+    show-dir
::
::::
  ::
~&  %
:-  %say
|=  $:  [now=@da tick=@ud ^]
        [arg=path ~]
        vane=?(%g %c)
    ==
=/  pax  (en-peck arg now tick)
=+  lon=.^(arch (cat 3 vane %y) pax)
tang+[?~(dir.lon leaf+"~" (show-dir vane pax dir.lon))]~
