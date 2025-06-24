::  Reset congestion control in Ames for specified ships
::
:-  %say
|=  [^ ships=(list ship) ~]
:-  %helm-ames-prod
ships
