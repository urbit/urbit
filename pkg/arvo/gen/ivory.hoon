::
::::  /hoon/ivory/gen
  ::
/?    310
::
::::
  !:
:-  %say
|=  [[now=@da * bec=beak] *]
:-  %noun
::  sys: root path to boot system, `/~me/[desk]/now/sys`
::
=/  sys=path
  /(scot %p p.bec)/[q.bec]/(scot %da now)/sys
::  compiler-source: hoon source file producing compiler, `sys/hoon`
::
=/  compiler-source
  .^(@t %cx (welp sys /hoon/hoon))
::  compiler-hoon: compiler as hoon expression
::
::    Parsed with a static path for reproducibility.
::
~&  %ivory-parsing
=/  compiler-hoon  (rain /sys/hoon/hoon compiler-source)
~&  %ivory-parsed
::  arvo-source: hoon source file producing arvo kernel, `sys/arvo`
::
=/  arvo-source
  .^(@t %cx (welp sys /arvo/hoon))
::  whole-hoon: arvo within compiler
::
::    Parsed with a static path for reproducibility.
::
=/  whole-hoon=hoon
  :+  %tsbn  compiler-hoon
  :+  %tsld  (rain /sys/arvo/hoon arvo-source)
  [%$ 7]
::  compile the whole schmeer
::
~&  %ivory-compiling
=/  whole-formula
  q:(~(mint ut %noun) %noun whole-hoon)
~&  %ivory-compiled
::  zuse-ovo: standard library installation event
::
::    Arvo parses the %veer card contents with +rain;
::    we include a static path for reproducibility.
::
=/  zuse-ovo=ovum
  :-  /vane/zuse
  [%veer %$ /sys/zuse/hoon .^(@ %cx (weld sys /zuse/hoon))]
::  installed: Arvo gate (formal instance) with %zuse installed
::
::    The :zuse-ovo event occurs at a defaulted date for reproducibility.
::
~&  %zuse-installing
=/  installed
  .*  0
  :+  %7  whole-formula
  [%9 2 %10 [6 %1 *@da zuse-ovo] %0 1]
~&  %zuse-installed
::  our boot-ova is a list containing one massive formula:
::
::    We evaluate :whole-formula (for jet registration),
::    then ignore the result and produces :installed
::
=/  boot-ova=(list)
  [[%7 whole-formula %1 installed] ~]
::  a pill is a 3-tuple of event-lists: [boot kernel userspace]
::
::    Our kernel event-list is ~, as we've already installed them.
::    Our userspace event-list is ~, as this pill must be compact.
::
[boot-ova ~ ~]
