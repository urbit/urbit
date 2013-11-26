!:
::  dill (4d), terminal handling
::
|=  pit=vase
^-  vane                                                ::  kernel instrument
=|  $:  dug=(map duct yard)
    ==                                                  ::
|=  [now=@da eny=@ sky=||(* (unit))]                    ::  current invocation
^?                                                      ::  opaque core
|%                                                      ::  poke/peek pattern
++  beat                                                ::  process move
  |=  [wru=(unit writ) tea=wire hen=duct fav=curd]
  =>  .(fav ((hard card) fav))
  ^-  [p=(list move) q=vane]
  ?:  ?=(%flog -.fav)
    :_  ..^$
    %+  turn  (~(tap by dug) *(list ,[p=duct q=yard]))
    |=([a=duct b=yard] [wru [/d a] p.fav])
  =+  ^=  yar  ^-  yard
      =+  yar=(~(get by dug) hen)
      ?^  yar  u.yar
      [& [80 ~ *blot] ~]
  =|  mos=(list move)
  =+  wip=|
  =<  yerk:leap
  |%
  ++  beep  (curb [[%bel ~] ~])                         ::  send beep
  ++  curb                                              ::  send blits
    |=  wab=(list blit)
    ^+  +>
    ?~  wab  +>
    +>(mos [[~ hen [%blit (flop wab)]] mos])
  ::
  ++  edit                                              ::  change the bed
    |=  bed=bead
    ^+  +>
    =.  q.q.yar  [~ bed]
    %-  curb
    |-  ^-  (list blit)
    ?^  hyr.u.q.q.yar
      =+  ris=:(weld "(reverse-i-search)'" (tufa u.hyr.u.q.q.yar) "': ")
      %=  $
        pot.bed        ris
        pol.bed        (lent ris)
        hyr.u.q.q.yar  ~
      ==
    :~  [%hop (add pol.bed bus.bed)]
        :-  %lin
        %+  weld  pot.bed
        ?-  buy.bed
          %none  but.bed
          %text  but.bed
          %pass  `(list ,@)`(runt [(lent but.bed) '*'] ~)
        ==
    ==
  ::
  ++  fume                                              ::  print tank, prefix
    |=  [pef=@tD tac=tank]
    ^+  +>
    =+  wol=(~(win re tac) 2 p.q.yar)
    %-  furl
    %+  turn  wol
    |=  a=tape  ^-  tape
    ?>  ?=([@ @ *] a)
    [pef ' ' t.t.a]
  ::
  ++  furl                                              ::  print wall
    |=  wol=(list tape)
    ^+  +>
    =.  +>
      %-  curb
      %-  flop
      |-  ^-  (list blit)
      ?~  wol  ~
      [[%lin (tuba i.wol)] [%mor ~] $(wol t.wol)]
    ?~  q.q.yar  +>
    (edit(q.q.yar ~) u.q.q.yar)
  ::
  ++  gore                                              ::  move in history
    |=  hup=@ud
    ^+  +>
    =+  but=(goth hup)
    =+  bul=(lent but)
    %-  edit
    %=  u.q.q.yar
      hiz  hup
      hym  %+  ~(put by hym.u.q.q.yar)
             hiz.u.q.q.yar
           but.u.q.q.yar
      bus  bul
      bul  bul
      but  but
    ==
  ::
  ++  goth                                              ::  extract history
    |=  hup=@ud
    =+  byt=(~(get by hym.u.q.q.yar) hup)
    ?^  byt  u.byt
    (tuba (rip 3 (snag hup q.hyt.u.q.q.yar)))
  ::
  ++  kill                                              ::  add to kill ring
    |=  txt=(list ,@c)
    ^+  +>
    =>  ?.  =(16 p.r.q.yar)  .
        .(p.r.q.yar 15, r.r.q.yar (scag 15 r.r.q.yar))
    %=  +>
      p.r.q.yar  +(p.r.q.yar)
      q.r.q.yar  0
      r.r.q.yar  [txt r.r.q.yar]
    ==
  :: 
  ++  look                                              :: search in history
    |=  [hup=@ud txt=(list ,@c)]
    ^+  +>
    =+  ^=  beg
        |=  [a=(list ,@c) b=(list ,@c)]  ^-  ?
        ?~(a & ?~(b | &(=(i.a i.b) $(a t.a, b t.b))))
    =+  ^=  mid
        |=  [a=(list ,@c) b=(list ,@c)]  ^-  ?
        ?~(a & ?~(b | |((beg a b) $(b t.b))))
    ?:  =(hup p.hyt.u.q.q.yar)
      beep
    =+  but=(goth hup)
    ?:  (mid txt but)
      (gore(hyr.u.q.q.yar [~ txt]) hup)
    $(hup +(hup))
  :: 
  ++  leap                                              ::  terminal event
    |-  ^+  +
    ?+    -.fav  +(mos :_(mos [wru hen fav]))
        %noop  +
        %belt                                           ::  terminal input
      ?~  q.q.yar
        beep
      ?^  hyr.u.q.q.yar                                 ::  live search
        ?+    p.fav  $(hiz.u.q.q.yar 0, hyr.u.q.q.yar ~)
            [%bac *]
          ?:  =(~ u.hyr.u.q.q.yar)
            (curb [[%bel ~] ~])
          %-  edit
          %=  u.q.q.yar
            hyr  [~ (scag (dec (lent u.hyr.u.q.q.yar)) u.hyr.u.q.q.yar)]
          ==
        ::
            [%txt *]   (look hiz.u.q.q.yar (weld u.hyr.u.q.q.yar p.p.fav))
            [%ctl %g]  (edit u.q.q.yar(bul 0, bus 0, but ~, hiz 0, hyr ~))
            [%ctl %r]
          ?:  =(p.hyt.u.q.q.yar hiz.u.q.q.yar)
            beep
          (look +(hiz.u.q.q.yar) u.hyr.u.q.q.yar)
        ==
      ?-    -.p.fav
          %aro                                          ::  arrow
        ?-    p.p.fav
            %d                                          ::  down
          ?:  =(0 hiz.u.q.q.yar)
            beep
          (gore (dec hiz.u.q.q.yar))
        ::
            %l                                          ::  left
          ?:  =(0 bus.u.q.q.yar)
            beep
          (edit u.q.q.yar(bus (dec bus.u.q.q.yar)))
        ::
            %r                                          ::  right
          ?:  =(bul.u.q.q.yar bus.u.q.q.yar)
            beep
          (edit u.q.q.yar(bus +(bus.u.q.q.yar)))
        ::
            %u
          =+  hup=+(hiz.u.q.q.yar)
          ?:  =(hup p.hyt.u.q.q.yar)
            beep
          (gore hup)
        ==
      ::
          %bac                                          ::  backspace
        ^+  +.$
        ?:  =(0 bus.u.q.q.yar)
          (curb `(list blit)`[[%bel ~] ~])
        %-  edit
        %=    u.q.q.yar
            bus  (dec bus.u.q.q.yar)
            bul  (dec bul.u.q.q.yar)
            but  
          %+  weld 
            (scag (dec bus.u.q.q.yar) but.u.q.q.yar)
          (slag bus.u.q.q.yar but.u.q.q.yar)
        ==
      ::
          %ctl                                          ::  control
        ?+  p.p.fav
          beep
          %a  (edit u.q.q.yar(bus 0))
          %b  $(fav [%belt %aro %l])
          %d  ?:  ?&  =(0 bul.u.q.q.yar)
                      =(0 bus.u.q.q.yar)
                  ==
                +.$(mos :_(mos [wru [/b /d hen] [%kill ~]]))
              $(fav [%belt %del ~])
          %e  (edit u.q.q.yar(bus bul.u.q.q.yar))
          %f  $(fav [%belt %aro %r])
          %k  ?:  =(bul.u.q.q.yar bus.u.q.q.yar)
                beep
              =>  .(+.$ (kill (slag bus.u.q.q.yar but.u.q.q.yar)))
              %-  edit
              %=  u.q.q.yar
                bul  bus.u.q.q.yar
                but  (scag bus.u.q.q.yar but.u.q.q.yar)
              ==
          %l  +.$(mos :_(mos [~ hen %blit [[%clr ~] ~]])) 
          %n  $(fav [%belt %aro %d])
          %p  $(fav [%belt %aro %u])
          %u  ?:  =(0 bus.u.q.q.yar)
                beep
              =>  .(+.$ (kill (scag bus.u.q.q.yar but.u.q.q.yar)))
              %-  edit
              %=  u.q.q.yar
                bus  0
                bul  (sub bul.u.q.q.yar bus.u.q.q.yar)
                but  (slag bus.u.q.q.yar but.u.q.q.yar)
              ==
          %r  (edit u.q.q.yar(hyr [~ ~]))
          %w  +.$(mos :_(mos [wru [/b /d hen] [%limn ~]]))
          %x  +.$(mos :_(mos [wru [/b /d hen] [%ling ~]]))
          %y  ?:  =(0 p.r.q.yar)
                beep
              $(fav [%belt %txt (snag q.r.q.yar r.r.q.yar)])
        ==
      ::
          %del                                          ::  delete
        ?:  =(bul.u.q.q.yar bus.u.q.q.yar)
          beep
        %-  edit
        %=    u.q.q.yar
            bul  (dec bul.u.q.q.yar)
            but  
          %+  weld 
            (scag bus.u.q.q.yar but.u.q.q.yar)
          (slag +(bus.u.q.q.yar) but.u.q.q.yar)
        ==
      ::
          %met                                          ::  meta
        ?+    p.p.fav
            beep
            %y  
          ?:  =(0 p.r.q.yar)
            beep
          =+  dol=(snag q.r.q.yar r.r.q.yar)
          =+  leo=(lent dol)
          ?.  (gte bus.u.q.q.yar leo)
            beep
          =+  pey=(sub bus.u.q.q.yar leo)
          ?.  =(dol (swag [pey leo] but.u.q.q.yar))
            beep
          =.  q.r.q.yar  ?:(=(p.r.q.yar +(q.r.q.yar)) 0 +(q.r.q.yar))
          =+  ney=(snag q.r.q.yar r.r.q.yar)
          =+  lye=(lent ney)
          %-  edit
          %=  u.q.q.yar
            bus  (sub (add bus.u.q.q.yar lye) leo)
            bul  (sub (add bul.u.q.q.yar lye) leo)
            but  %+  weld
                   (scag pey but.u.q.q.yar)
                 %+  weld
                   `(list ,@c)`ney                      ::  XX weird fuse-loop
                 (slag bus.u.q.q.yar but.u.q.q.yar)
          ==
        ==
      ::
          %ret                                          ::  return
        ?:  =(%none buy.u.q.q.yar)  beep
        =+  jab=(rap 3 (tufa but.u.q.q.yar))
        %=    +.$
            q.q.yar  ~
            r.yar
          ?:  |(=(%$ jab) =(%pass buy.u.q.q.yar))
            r.yar 
          %+  ~(put by r.yar)
            hux.u.q.q.yar  
          [p.hyt.u.q.q.yar [jab ?~(q.hyt.u.q.q.yar ~ +.q.hyt.u.q.q.yar)]]
        ::
            mos
          :*  [~ [/b /d hen] [%hail ~]]
              [~ hen [%bbye ~]]
              [wru [/b /d hen] [%line jab]]
              [~ hen [%blit [[%mor ~] ~]]]
              mos
          ==
        ==
      ::
          %txt                                          ::  text keys
        ?:  =(%none buy.u.q.q.yar)  beep
        =+  let=(lent p.p.fav)
        %-  edit
        %=    u.q.q.yar
            bus  (add let bus.u.q.q.yar)
            bul  (add let bul.u.q.q.yar)
            but  
          ;:  weld
            (scag bus.u.q.q.yar but.u.q.q.yar)
            p.p.fav
            (slag bus.u.q.q.yar but.u.q.q.yar)
          ==
        ==
      ==
    ::
        %blew   +.$(p.q.yar p.p.fav)                    ::  window size
        %boot
      %=    +.$
          mos
        :_(mos [wru [[%b ~] [%d tea] hen] p.fav])
      ==
    ::
        %crud                                           ::  error trace
      =.  q.fav  [[%leaf (trip p.fav)] q.fav]
      |-  ^+  +.^$
      ?~  q.fav  +.^$
      (fume:$(q.fav t.q.fav) '!' i.q.fav)
    ::
        %helo                                           ::  trigger prompt
      %-  edit
      =|  bed=bead
      =+  ^=  hyt  ^-  hist
          =+  hyt=(~(get by r.yar) p.fav)
          ?~(hyt *hist u.hyt)
      ?:  &(?=(^ q.q.yar) =(p.fav hux.u.q.q.yar))
        %=  u.q.q.yar
          hyt  [+(p.hyt) [%$ q.hyt]]
          pot  q.q.fav
          pol  (lent q.q.fav)
          buy  p.q.fav
        ==
      =+  zon=(tuba r.q.fav)
      =+  zow=(lent zon)
      %=    bed
          bul  zow
          bus  zow
          but  zon
          buy  p.q.fav
          hux  p.fav
          hiz  0
          hyt  [+(p.hyt) [%$ q.hyt]]
          pot  q.q.fav
          pol  (lent q.q.fav)
      ==
    ::
        ?(%hail %make %loin %sith)
      +.$(mos :_(mos [wru [/b /d hen] fav]))
    ::
        %note  ?.(p.yar +.$ (fume p.fav q.fav))         ::  debug message
        %save                                           ::  write a file
      %=  +.$
        mos  :_(mos [wru hen `card`[%blit [%sav p.fav q.fav] ~]])
      ==
    ::
        %tell  (furl (turn p.fav |=(a=@t (trip a))))    ::  wall of text
        %text  $(fav [%talk %leaf p.fav])               ::  simple message
        %talk  (furl (~(win re p.fav) 0 p.q.yar))       ::  program output
        %warn  (fume '~' [%leaf p.fav])                 ::  system message
        %wipe  +.$(wip &)                               ::  delete old
    ==
  ::
  ++  yerk                                              ::  complete core
    ^-  [p=(list move) q=_..^$]
    :-  (flop mos)
    ..^$(dug ?.(wip (~(put by dug) hen yar) (~(del by dug) hen)))
  --
::
++  come  
  |=  old=vase 
  ^-  vane
  ~|(%load-nest-dill !!)
::
++  doze
  |=  [now=@da hen=duct]
  ^-  (unit ,@da)
  ~
::
++  flee  stay
++  load
  |=  new=vase
  ^-  vane
  ?.  (~(nest ut -:!>(dug)) & p.new)
    (come new)
  %_  ..^$
    dug  ((map duct yard) q.new)
  ==
::
++  raze  
  ^-  vane
  ..$(dug ~)
::
++  scry
  |=  [our=ship ren=@tas his=ship syd=disc lot=coin tyl=path]
  ^-  (unit)
  ~
::
++  stay  `vase`!>(dug)
--
