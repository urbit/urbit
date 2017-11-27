::  Make HTTP request(get only)
::
::::  /hoon/curl-hiss/gen
  ::
/?    310
/-  sole
/+  old-zuse
=,  old-zuse
=,  sole
:-  %get  |=  {^ {a/hiss $~} usr/iden}
^-  (sole-request (cask httr))
?.  ?=($get p.q.a)
  ~|  %only-get-requests-supported-in-generators  :: XX enforced?
  !!
:-  *tang
:^  %|  `usr  `hiss`a
|=(hit/httr (sole-so %httr hit))
