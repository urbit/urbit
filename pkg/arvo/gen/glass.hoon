::  Produce a glass pill
::
::::  /hoon/glass/gen
  ::
/?    310
::
::::
  ::
:-  %say
|=  $:  {now/@da eny/@uvJ bec/beak}
        *
    ==
=<  [%noun ~]
=>  |%
    ++  wasp                                            ::  arvo effect
      $%  {$wack p/@uvJ}                                ::  add entropy
          {$what p/(list (pair path (pair term noun)))} ::  reset reptile
          {$whom p/@p}                                  ::  id and boot
      ==
    --
::
::  deterministic test
::
=.  eny  0v0
::
::  build arvo with existing compiler
::
|^  ^-  *
    ::
    ::  neo: root path to boot system, `/~me/[desk]/now/neo`
    ::
    =+  neo=`path`/(scot %p p.bec)/[q.bec]/(scot %da now)/neo
    ::
    ::  arvo-source: hoon source file producing arvo kernel, `neo/arvo`
    ::
    =+  arvo-source=.^(@t %cx (welp neo /arvo/hoon))
    ::
    ::  arvo-twig: parsed arvo source
    ::
    ~&  [%parsing `@p`(mug arvo-source)]
    =+  arvo-twig=(ream arvo-source)
    ~&  %parsed
    ::
    ::  arvo-kernel: a living urbit
    ::
    ~&  [%compiling `@p`(mug arvo-twig)]
    =+  arvo-kernel=+:(slap !>(..ream) arvo-twig)
    ~&  %compiled
    ::
    ::  events: list of events to feed our urbit
    ::
    ~&  %loading
    =+  events=main-events
    ~&  [%loaded `@p`(mug events)]
    ::
    ::  execute events
    ::
    =+  number=0
    |-  ^-  *
    ?~  events  arvo-kernel
    ~&  number+number
    %=  $
      events       t.events
      number       +(number)
      arvo-kernel  .*(arvo-kernel(+< i.events) -.arvo-kernel)
    ==
::
++  main-events
  =.  now  ~2017.3.1
  =+  mov=main-moves
  |-  ^-  (list (pair @da ovum))
  ?~  mov  ~
  :-  [now i.mov]
  $(mov t.mov, now (add now (bex 48)))
::
++  main-moves
  ^-  (list ovum)
  :~  [[%$ ~] [%what boot-files]]
      ::  [[%$ ~] [%whom ~zod]]
  ==
++  boot-files
  ^-  (list (pair path (pair term noun)))
  ::
  ::  userspace:
  ::
  ::    /app    %gall applications
  ::    /gen    :dojo generators
  ::    /lib    %ford libraries
  ::    /mar    %ford marks
  ::    /sur    %ford structures
  ::    /ren    %ford renderers
  ::    /web    %eyre web content
  ::    /sys    system files
  ::    /neo    new system files
  ::
  (user-files /neo ~)
::
++  user-files                                      ::  userspace loading
  |=  ::  sal: all spurs to load from
      ::
      sal/(list spur)
  ^-  (list (pair path (pair term noun)))
  ::
  ::  hav: all user files
  ::
  =|  hav/(list (pair path (pair term noun)))
  |-  ^+  hav
  ?~  sal  ~
  =.  hav  $(sal t.sal)
  ::
  ::  tyl: spur
  ::
  =/  tyl  i.sal
  |-  ^+  hav
  ::
  ::  pax: full path at `tyl`
  ::  lon: directory at `tyl`
  ::
  =/  pax  (en-beam:format bec tyl)
  =/  lon  .^(arch %cy pax)
  =?  hav  ?=(^ fil.lon)
      ::
      ::  install only hoon files for now
      ::
      ?.  ?=({$hoon *} tyl)  hav
      :_(hav [(flop `path`t.tyl) [%hoon .^(@t %cx pax)]])
  ::
  =/  all  ~(tap by dir.lon)
  |-  ^+  hav
  ?~  all  hav
  $(all t.all, hav ^$(tyl [p.i.all tyl]))
--
