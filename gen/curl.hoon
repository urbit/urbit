::  Fetch contents at url
::
::::  /hoon/curl/gen
  ::
/?    310
/-  sole
/+  generators
=,  [generators eyre]
:-  %get  |=  {^ {a/tape ~} ~}
^-  (sole-request:sole (cask httr))
%+  curl  (scan a auri:de-purl:html)
|=  hit/httr
(produce %httr hit)
