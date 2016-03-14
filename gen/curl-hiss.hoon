::
::::  /hoon/curl/gen
  ::
/?    310
/-  sole
[sole]
:-  %get  |=  {^ {a/hiss $~} $~}
^-  (sole-request (cask httr))
?.  ?=($get p.q.a)
  ~|  %only-get-requests-supported-in-generators  :: XX enforced?
  !!
:-  *tang
:+  %|  `hiss`a
|=(hit/httr (sole-so %httr hit))
