::  Make HTTP request(get only)
::
::::  /hoon/curl-hiss/gen
  ::
/?    310
/-  sole
/+  generators, old-zuse
=,  old-zuse
=,  generators
:-  %get  |=  {^ {a/hiss ~} usr/iden}
^-  (sole-request:sole (cask httr))
?.  ?=($get p.q.a)
  ~|  %only-get-requests-supported-in-generators  :: XX enforced?
  !!
:-  *tang
:^  %|  `usr  `hiss`a
|=(hit/httr (produce %httr hit))
