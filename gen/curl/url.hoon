::  Direct :curl to fetch contents at url
::
::::  /hoon/url/curl/gen
  ::
/?    310
/+  old-zuse
=,  old-zuse
::
:::::
  ::
:-  %say
|=  {^ {arg/tape $~} $~}
purl+(scan arg auri:urlp)
