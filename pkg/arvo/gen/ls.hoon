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
        [pax=path ~]
        vane=?(%g %c)
    ==
=.  pax  (en-pick now tick pax)
=+  lon=.^(arch (cat 3 vane %y) pax)
tang+[?~(dir.lon leaf+"~" (show-dir vane pax dir.lon))]~
