::  Cancel all outstanding remote scry requests
::
:-  %say
|=  [^ [=ship pax=$@(~ [=path ~])] ~]
=/  =path
  :: XX remove default path
  ?~  pax  /c/x/1/kids/sys/kelvin
  ?>  ?=([@ *] path.pax)
  =,  pax
  path
[%helm-pass %a %wham ship path]
