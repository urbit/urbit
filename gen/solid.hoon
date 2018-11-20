::  Compile arvo as a pill noun, without compiler changes.
::  usage 
::
::    .urbit/pill +solid
::
::::  /hoon/solid/gen
  ::
/?    310
::
::::
  !:
:-  %say
|=  $:  [now=@da eny=@uvJ bec=beak]
        arg=$@(~ [top=path ~])
        dub/_|
    ==
:-  %noun
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
  =;  res
    ?:(?=(%& -.res) p.res (mean (flop p.res)))
  %-  mule  |.
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
::  module-ova: vane load operations.
::
=/  module-ova=(list ovum)
    |^  :~  (vent %$ /zuse)
            (vent %a /vane/ames)
            (vent %b /vane/behn)
            (vent %c /vane/clay)
            (vent %d /vane/dill)
            (vent %e /vane/eyre)
            (vent %f /vane/ford)
            (vent %g /vane/gall)
            (vent %j /vane/jael)
        ==
    ++  vent
      |=  [abr=term den=path]
      ^-  ovum
      =/  pax  (weld sys den)
      =/  txt  .^(@ %cx (weld pax /hoon))
      [[%vane den] [%veer abr pax txt]]
    --
::  arvo with vanes installed
::
=/  installed
  =<  q
  %^    spin
      module-ova
    .*(0 arvo-formula)
  |=  [ovo=ovum ken=*]
  [~ .*(ken [%9 2 %10 [6 %1 now ovo] %0 1])]
::  produce both the arvo-formula (for jet registration)
::  and a kernel with the vanes already installed
::
[arvo-formula .*(installed [%0 7])]
