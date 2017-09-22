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
::  compiler-twig: compiler as hoon expression
::
~&  %ivory-parsing
=+  compiler-twig=(ream compiler-source)
~&  %ivory-parsed
::
::  arvo-source: hoon source file producing arvo kernel, `sys/arvo`
::
=+  arvo-source=.^(@t %cx (welp sys /arvo/hoon))
::
::  whole-twig: arvo within compiler
::
=+  whole-twig=`twig`[%tsgr compiler-twig [%tsgr [%$ 7] (ream arvo-source)]]
::
::  compile the whole schmeer
::
~&  %ivory-compiling
=+  whole-formula=q:(~(mint ut %noun) %noun whole-twig)
~&  %ivory-compiled
::
whole-formula
