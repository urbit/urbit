::  Filter Ames debug output by ship
::
/?    310
::
::::
  ::
:-  %say
|=  [^ ships=(list ship) ~]
:-  %helm-ames-sift
ships
