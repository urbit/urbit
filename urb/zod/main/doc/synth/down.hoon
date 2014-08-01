!:
::::::   /hoon/down/synth/doc
::
  =>
::::::  models
|%
++  down
  $&  [p=down q=down]
  $%  [%$ p=tape]
      [%code p=tape]
      [%inco p=tape]
      [%head p=@ud q=down]
      [%link p=tape q=tape r=(unit tape)]
      [%lord p=(list down)]
      [%lund p=(list down)]
      [%parg p=down]
      [%quot p=down]
      [%rong p=down]
      [%emph p=down]
      [%hrul ~]
      [%html p=tape]
  ==
--  
::::::  generator
::
~&  [%choon-at %]
%-  (fest /synth/doc %)
|=  pic=epic
=+  unt=|=(a=cord (biff (~(get by qix.pic) a) |=(b=cord (slaw %ud b))))
=+  moo=(both (unt %foo) (unt %bar))
?~  moo  [%$ "Hm, world: usage: url?foo=x&bar=y"]
:*  [%$ "Hm, "]
    [%emph %$ "world"]
    [%$ ": {<-.u.moo>} plus {<+.u.moo>} is {<(add u.moo)>}."]
==
