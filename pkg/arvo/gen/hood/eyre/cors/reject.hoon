::  eyre: disallow cors requests from origin
::
:-  %say
|=  [^ [=origin:eyre ~] ~]
[%helm-cors-reject origin]
