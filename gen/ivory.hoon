::
::::  /hoon/metal/gen
  ::
/?    310
::
::::
  !:
:-  %say
|=  $:  {now/@da * bec/beak}
        *
    ==
:-  %noun
::
::  sys: root path to boot system, `/~me/[desk]/now/sys`
::
=+  sys=`path`/(scot %p p.bec)/[q.bec]/(scot %da now)/sys
::
::  compiler-source: hoon source file producing compiler, `sys/hoon`
::
=+  compiler-source=.^(@t %cx (welp sys /hoon/hoon))
::
::  compiler-hoon: compiler as hoon expression
::
~&  %ivory-parsing
=+  compiler-hoon=(ream compiler-source)
~&  %ivory-parsed
::
::  arvo-source: hoon source file producing arvo kernel, `sys/arvo`
::
=+  arvo-source=.^(@t %cx (welp sys /arvo/hoon))
::
::  whole-hoon: arvo within compiler
::
=+  whole-hoon=`hoon`[%tsbn compiler-hoon [%tsbn [%$ 7] (ream arvo-source)]]
::
::  compile the whole schmeer
::
~&  %ivory-compiling
=+  whole-formula=q:(~(mint ut %noun) %noun whole-hoon)
~&  %ivory-compiled
::
::  tang: standard library (%zuse) installation event
::
=/  tang
  =/  pax  (weld sys /zuse)
  =/  txt  .^(@ %cx (weld pax /hoon))
  `ovum`[/vane/zuse [%veer %$ pax txt]]
::
::  installed: kernel with tang installed
::
~&  %tang-installing
=/  installed
  .*(0 [%7 whole-formula [%9 2 %10 [6 %1 now tang] %0 1]])
~&  %tang-installed
::
::  produce both whole-formula (for jet registration)
::  and a kernel with the standard library already installed
::
[%7 whole-formula %1 installed]
