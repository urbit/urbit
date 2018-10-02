::  Make HTTP request(get only)
::
::::  /hoon/curl-hiss/gen
  ::
/?    310
/+  sole, old-zuse
=,  old-zuse
=,  ask:sole
:-  %get  |=  {^ {a/hiss ~} usr/iden}
^-  (sole-request:sole (cask httr))
?.  ?=($get p.q.a)
  ~|  %only-get-requests-supported-in-generators  :: XX enforced?
  !!
:-  *tang
:^  %|  `usr  `hiss`a
|=(hit/httr (output %httr hit))
