::  Eyre: set web root
::
::::  /hoon/serve/hood/gen
  ::
/?    310
::
::::
  ::
:-  %say
|=  [^ [pax=path syd=desk gen=path ~] ~]
=.  pax  (de-pick pax)
=.  gen  (de-pick pax)
:+  %helm-serve
  `binding:eyre`[~ pax]
`generator:eyre`[syd gen ~]
