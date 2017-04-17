::
::::  /hoon/glass/gen
  ::
/?    310
::
::::
  !:
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
|^  ^-  vase
    ::
    ::  sys: root path to boot system, `/~me/[desk]/now/sys`
    ::
    =+  sys=`path`/(scot %p p.bec)/[q.bec]/(scot %da now)/sys
    ::
    ::  arvo-source: hoon source file producing arvo kernel, `sys/parv`
    ::
    =+  arvo-source=.^(@t %cx (welp sys /parv/hoon))
    ::
    ::  arvo-twig: parsed arvo source
    ::
    ~&  [%parsing `@p`(mug arvo-source)]
    =+  arvo-twig=(ream arvo-source)
    ~&  %parsed
    ::
    ::  arvo-vase: a living urbit
    ::
    ~&  [%compiling `@p`(mug arvo-twig)]
    =+  arvo-vase=(slap !>(..ream) arvo-twig)
    ~&  %compiled
    ::
    ::  events: list of events to feed our urbit
    ::
    ~&  %events
    =+  events=main-events
    ~&  [%events `@p`(mug events)]
    ::
    ::  execute events
    ::
    =+  number=0
    |-  ^-  vase
    ?~  events  arvo-vase
    %=  $
      events     t.events
      number     +(number)
      arvo-vase  (slam arvo-vase !>(i.events))
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
      [[%$ ~] [%whom ~zod]]
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
  ::
  (user-files /app /gen /lib /mar /ren /sec /sur /sys /web ~)
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
      :_(hav [(flop `path`tyl) [%hoon .^(@t %cx pax)]])
  ::
  =/  all  (~(tap by dir.lon) ~)
  |-  ^+  hav
  ?~  all  hav
  $(all t.all, hav ^$(tyl [p.i.all tyl]))
--
