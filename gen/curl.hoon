::  Fetch contents at url
::
::::  /hoon/curl/gen
  ::
/?    310
/-  sole
/+  generators, old-zuse
=,  old-zuse
=,  generators
:-  %get  |=  {^ {a/tape ~} ~}
^-  (sole-request:sole (cask httr))
%+  curl  (scan a auri:urlp)
|=  hit/httr
(produce %httr hit)
