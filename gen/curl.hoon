::  Fetch contents at url
::
::::  /hoon/curl/gen
  ::
/?    310
/+  sole, old-zuse
=,  old-zuse
=,  ask:sole
:-  %get  |=  {^ {a/tape ~} ~}
^-  (sole-request:sole (cask httr))
%+  curl  (scan a auri:urlp)
|=  hit/httr
(output %httr hit)
