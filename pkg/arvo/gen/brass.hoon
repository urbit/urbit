::  Produce a brass pill
::
::::  /hoon/brass/gen
  ::
/?    310
/+  pill
::
::::
  !:
:-  %say
|=  $:  [now=@da eny=@uvJ bec=beak]
        arg=$@(~ [top=path ~])
        ~
    ==
:-  %boot-pill
^-  pill:pill
::
::  sys: root path to boot system, `/~me/[desk]/now/sys`
::
=/  sys=path
  ?^  arg  top.arg
  /(scot %p p.bec)/[q.bec]/(scot %da now)/sys
::
::  compiler-source: hoon source file producing compiler, `sys/hoon`
::
=+  compiler-source=.^(@t %cx (welp sys /hoon/hoon))
::
::  compiler-twig: compiler as hoon expression
::
~&  %brass-parsing
=+  compiler-twig=(rain /sys/hoon/hoon compiler-source)
~&  %brass-parsed
::
::  compiler-formula: compiler as nock formula
::
~&  %brass-compiling
=+  compiler-formula=q:(~(mint ut %noun) %noun compiler-twig)
~&  %brass-compiled
::
::  arvo-source: hoon source file producing arvo kernel, `sys/arvo`
::
=+  arvo-source=.^(@t %cx (welp sys /arvo/hoon))
::
::  boot-ova: startup events
::
=/  boot-ova=(list)
  :~  aeon:eden:part
      boot:eden:part
      compiler-formula
      compiler-source
      arvo-source
  ==
::  a pill is a 3-tuple of event-lists: [boot kernel userspace]
::
=/  bas=path  (flop (tail (flop sys)))
:+  %pill  %brass
:+  boot-ova
  :~  (boot-ovum:pill compiler-source arvo-source)
      (file-ovum2:pill bas)
  ==
[(file-ovum:pill bas) ~]
