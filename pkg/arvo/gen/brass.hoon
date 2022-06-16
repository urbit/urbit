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
      ::
        ::  arg: desks to build pill from
        ::
        ::    list of desks. defaults to [%base]~.
        ::    the first desk in this list will become the pill's base desk.
        ::    optionally, the first desk may be replaced with a fully
        ::    qualified path to the new boot system (typically in sys).
        ::    the rest of the desks will be installed through kiln.
        ::
        $=  arg
        $@  ~
        $:  base=$@(desk [@ta @ta @ta path])
            rest=(list desk)
        ==
      ::
        ~
    ==
:-  %pill
^-  pill:pill
::  sys: root path to boot system, `/~me/[desk]/now/sys`
::  bas: root path to boot system' desk
::  dez: secondary desks and their root paths
::
=/  sys=path
  ?:  ?=([^ *] arg)
    `path`base.arg
  =/  =desk
    ?~  arg  %base
    ?>(?=(@ base.arg) base.arg)
  /(scot %p p.bec)/[desk]/(scot %da now)/sys
=/  bas=path
  (scag 3 sys)
=/  dez=(list [desk path])
  ?~  arg  ~
  %+  turn  rest.arg
  |=  =desk
  [desk /(scot %p p.bec)/[desk]/(scot %da now)]
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
:+  %pill  %brass
:+  boot-ova
  :~  (boot-ovum:pill compiler-source arvo-source)
      (file-ovum2:pill bas)
  ==
%+  turn
  (snoc dez [%base bas])
file-ovum:pill
