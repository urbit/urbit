::  Compile arvo as a pill noun, without compiler changes.
::  usage
::
::    .urbit/pill +solid
::
::::  /hoon/solid/gen
  ::
/?    310
/+  pill
::
::::
  !:
:-  %say
|=  $:  [now=@da eny=@uvJ bec=beak]
        arg=$@(~ [top=path ~])
        dub=_|
    ==
:-  %pill
^-  pill:pill
::  sys: root path to boot system, `/~me/[desk]/now/sys`
::
=/  sys=path
  ?^  arg  top.arg
  /(scot %p p.bec)/[q.bec]/(scot %da now)/sys
::
=/  compiler-path  (weld sys /hoon)
=/  arvo-path      (weld sys /arvo)
~&  %solid-start
=/  compiler-src  .^(@t %cx (weld compiler-path /hoon))
=/  arvo-src      .^(@t %cx (weld arvo-path /hoon))
=/  arvo-formula
  ~&  %solid-loaded
  =/  compiler-hoon  (rain compiler-path compiler-src)
  ?.  dub
    ::  compile arvo against hoon, with our current compiler
    ::
    =/  whole-hoon=hoon
      [%tsbn compiler-hoon [%tsbn [%$ 7] (rain arvo-path arvo-src)]]
    ~&  %solid-parsed
    =/  whole-formula  q:(~(mint ut %noun) %noun whole-hoon)
    ~&  %solid-arvo
    whole-formula
  ::  compile arvo against hoon, with a freshly compiled hoon (via +ride)
  ::
  ~&  %solid-parsed
  =/  compiler-formula  q:(~(mint ut %noun) %noun compiler-hoon)
  ~&  %solid-compiled
  =/  whole-src
    (rap 3 ['=>  ' compiler-src '=>  +7  ' arvo-src ~])
  ~&  %solid-double-loaded
  =/  whole-formula
    =<  +
    .*  0
    :+  %7
      compiler-formula
    [%9 2 %10 [6 %1 %noun whole-src] [%0 1]]
  ~&  %solid-double-compiled
  whole-formula
::
~&  [%solid-kernel `@ux`(mug arvo-formula)]
::
::  installed: Arvo gate (formal interface) with %zuse and vanes installed
::
=/  installed
  =<  q
  %^    spin
      (module-ova:pill sys)
    .*(0 arvo-formula)
  |=  [ovo=ovum ken=*]
  [~ (slum ken [now ovo])]
::
::  our boot-ova is a list containing one massive formula:
::
::    We evaluate :arvo-formula (for jet registration),
::    then ignore the result and produce :installed
::
=/  boot-ova=(list)
  [[%7 arvo-formula %1 installed] ~]
::
::  a pill is a 3-tuple of event-lists: [boot kernel userspace]
::
::    Our kernel event-list is ~, as we've already installed them.
::    Our userspace event-list is a list containing a full %clay
::    filesystem sync event.
::
:+  boot-ova  ~
=/  bas  (flop (tail (flop sys)))
[(file-ovum:pill bas) ~]
