!:
::  dill (4d), terminal handling
::
|=  pit=vase
=>  |%
++  gift                                                ::  out result <-$
  card
::
++  kiss                                                ::  in request ->$
  card
::
++  move  ,[p=duct q=(mold note gift)]                  ::  local move
::
++  note                                                ::  out request $->
  card
::
++  sign                                                ::  in result $-<
  card
--
=|  $:  %0                                              ::
        dug=(map duct yard)                             ::
    ==                                                  ::
|=  [now=@da eny=@ ski=sled]                            ::  current invocation
^?                                                      ::  opaque core
|%                                                      ::  poke/peek pattern
++  take                                                ::  process move
  |=  [tea=wire hen=duct hin=(hypo sign)]
  ^-  [p=(list move) q=_..^$]
  ?:  ?=(%flog -.q.hin)
    :_  ..^$
    %+  turn  (~(tap by dug) *(list ,[p=duct q=yard]))
    |=([a=duct b=yard] [a %slip %d p.q.hin])
  ?:  ?=(%soft -.q.hin)
    $(q.hin ((hard card) p.q.hin))
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
    +>(mos [[hen [%give %blit (flop wab)]] mos])
  ::
  ++  wod                                               ::  word forward
    |=  bed=bein
    ^-  @ud
    ?:  =(bul.bed bus.bed)
      bus.bed
    ?:  =(' ' (snag bus.bed but.bed))
      $(bus.bed +(bus.bed))
    |-
    ^-  @ud
    ?:  =(bul.bed bus.bed)
      bus.bed
    ?:  =(' ' (snag bus.bed but.bed))
      bus.bed
    $(bus.bed +(bus.bed))
  ::
  ++  wob                                               ::  word backward
    |=  bed=bein
    ^-  @ud
    ?:  =(0 bus.bed)
      bus.bed
    ?:  =(' ' (snag (dec bus.bed) but.bed))
      $(bus.bed (dec bus.bed))
    |-
    ^-  @ud
    ?:  =(0 bus.bed)
      bus.bed
    ?:  =(' ' (snag (dec bus.bed) but.bed))
      bus.bed
    $(bus.bed (dec bus.bed))
  ::
  ++  edit                                              ::  change the bed
    |=  bed=bein
    ^+  +>
    =.  q.q.yar  [~ bed]
    ?>  ?=(^ q.q.yar)
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
    ?>  ?=(^ q.q.yar)
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
    ?>  ?=(^ q.q.yar)
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
    ?>  ?=(^ q.q.yar)
    ?:  =(hup p.hyt.u.q.q.yar)
      beep
    =+  but=(goth hup)
    ?:  (mid txt but)
      (gore(hyr.u.q.q.yar [~ txt]) hup)
    $(hup +(hup))
  ::
  ++  leap                                              ::  terminal event
    |-  ^+  +
    ?+    -.q.hin  +(mos :_(mos [hen %give q.hin]))
        %noop  +
        %belt                                           ::  terminal input
      ?~  q.q.yar
        beep
      ?^  hyr.u.q.q.yar                                 ::  live search
        ?+    p.q.hin  $(hiz.u.q.q.yar 0, hyr.u.q.q.yar ~)
            [%bac *]
          ?:  =(~ u.hyr.u.q.q.yar)
            (curb [[%bel ~] ~])
          %-  edit
          %=  u.q.q.yar
            hyr  [~ (scag (dec (lent u.hyr.u.q.q.yar)) u.hyr.u.q.q.yar)]
          ==
        ::
            [%txt *]   (look hiz.u.q.q.yar (weld u.hyr.u.q.q.yar p.p.q.hin))
            [%ctl %g]  (edit u.q.q.yar(bul 0, bus 0, but ~, hiz 0, hyr ~))
            [%ctl %r]
          ?:  =(p.hyt.u.q.q.yar hiz.u.q.q.yar)
            beep
          (look +(hiz.u.q.q.yar) u.hyr.u.q.q.yar)
        ==
      ?-    -.p.q.hin
          %aro                                          ::  arrow
        ?-    p.p.q.hin
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
        ?+  p.p.q.hin
          beep
          %a  (edit u.q.q.yar(bus 0))
          %b  $(q.hin [%belt %aro %l])
          %d  ?:  ?&  =(0 bul.u.q.q.yar)
                      =(0 bus.u.q.q.yar)
                  ==
                +.$(mos :_(mos [hen %toss %b ~ [%kill ~]]))
              $(q.hin [%belt %del ~])
          %e  (edit u.q.q.yar(bus bul.u.q.q.yar))
          %f  $(q.hin [%belt %aro %r])
          %k  ?:  =(bul.u.q.q.yar bus.u.q.q.yar)
                beep
              =>  .(+.$ (kill (slag bus.u.q.q.yar but.u.q.q.yar)))
              %-  edit
              ?>  ?=(^ q.q.yar)
              %=  u.q.q.yar
                bul  bus.u.q.q.yar
                but  (scag bus.u.q.q.yar but.u.q.q.yar)
              ==
          %t  ?:  (lth bul.u.q.q.yar 2)
                beep
              =+  ^=  pos
                  ?:  =(bul.u.q.q.yar bus.u.q.q.yar)
                    (sub bus.u.q.q.yar 2)
                  ?:  =(0 bus.u.q.q.yar)
                    bus.u.q.q.yar
                  (dec bus.u.q.q.yar)
              %-  edit
              %=  u.q.q.yar
                bus  (add 2 pos)
                but  %+  weld
                       %+  weld
                         (scag pos but.u.q.q.yar)
                       ^-  (list ,@c)  :+
                         (snag +(pos) but.u.q.q.yar)
                         (snag pos but.u.q.q.yar)
                         ~
                      (slag (add 2 pos) but.u.q.q.yar)
              ==
          %l  +.$(mos :_(mos [hen %give %blit [[%clr ~] ~]]))
          %n  $(q.hin [%belt %aro %d])
          %p  $(q.hin [%belt %aro %u])
          %u  ?:  =(0 bus.u.q.q.yar)
                beep
              =>  .(+.$ (kill (scag bus.u.q.q.yar but.u.q.q.yar)))
              %-  edit
              ?>  ?=(^ q.q.yar)
              %=  u.q.q.yar
                bus  0
                bul  (sub bul.u.q.q.yar bus.u.q.q.yar)
                but  (slag bus.u.q.q.yar but.u.q.q.yar)
              ==
          %r  (edit u.q.q.yar(hyr [~ ~]))
          ::  TODO
          ::  %w  +.$(mos :_(mos [hen %toss %b ~ [%limn ~]]))
          %x  +.$(mos :_(mos [hen %toss %b ~ [%ling ~]]))
          %y  ?:  =(0 p.r.q.yar)
                beep
              $(q.hin [%belt %txt (snag q.r.q.yar r.r.q.yar)])
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
        ?+    p.p.q.hin
            beep
            %f
          ?:  =(bul.u.q.q.yar bus.u.q.q.yar)
            beep
          (edit u.q.q.yar(bus (wod u.q.q.yar)))
          ::
            %b
          ?:  =(0 bus.u.q.q.yar)
            beep
          (edit u.q.q.yar(bus (wob u.q.q.yar)))
          ::
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
          :*  [hen %toss %b ~ [%hail ~]]
              [hen %give [%bbye ~]]
              [hen %toss %b ~ [%line jab]]
              [hen %give [%blit [[%mor ~] ~]]]
              mos
          ==
        ==
      ::
          %txt                                          ::  text keys
        ?:  =(%none buy.u.q.q.yar)  beep
        =+  let=(lent p.p.q.hin)
        %-  edit
        %=    u.q.q.yar
            bus  (add let bus.u.q.q.yar)
            bul  (add let bul.u.q.q.yar)
            but
          ;:  weld
            (scag bus.u.q.q.yar but.u.q.q.yar)
            p.p.q.hin
            (slag bus.u.q.q.yar but.u.q.q.yar)
          ==
        ==
      ==
    ::
        %blew   +.$(p.q.yar p.p.q.hin)                  ::  window size
        %boot
      %=    +.$
          mos
        :_(mos [hen %toss %b tea p.q.hin])
      ==
    ::
        %crud                                           ::  error trace
      =.  q.q.hin  [[%leaf (trip p.q.hin)] q.q.hin]
      |-  ^+  +.^$
      ?~  q.q.hin  +.^$
      (fume:$(q.q.hin t.q.q.hin) '!' i.q.q.hin)
    ::
        %helo                                           ::  trigger prompt
      %-  edit
      =|  bed=bein
      =+  ^=  hyt  ^-  hist
          =+  hyt=(~(get by r.yar) p.q.hin)
          ?~(hyt *hist u.hyt)
      ?:  &(?=(^ q.q.yar) =(p.q.hin hux.u.q.q.yar))
        %=  u.q.q.yar
          hyt  [+(p.hyt) [%$ q.hyt]]
          pot  q.q.q.hin
          pol  (lent q.q.q.hin)
          buy  p.q.q.hin
        ==
      =+  zon=(tuba r.q.q.hin)
      =+  zow=(lent zon)
      %=    bed
          bul  zow
          bus  zow
          but  zon
          buy  p.q.q.hin
          hux  p.q.hin
          hiz  0
          hyt  [+(p.hyt) [%$ q.hyt]]
          pot  q.q.q.hin
          pol  (lent q.q.q.hin)
      ==
    ::
        ?(%hail %make %loin %sith)
      +.$(mos :_(mos [hen %toss %b ~ q.hin]))
    ::
        %note  ?.(p.yar +.$ (fume p.q.hin q.q.hin))         ::  debug message
        %save                                           ::  write a file
      %=  +.$
        mos  :_(mos [hen [%give %blit [%sav p.q.hin q.q.hin] ~]])
      ==
    ::
        %tell  (furl (turn p.q.hin |=(a=@t (trip a))))  ::  wall of text
        %text  $(q.hin [%talk %leaf p.q.hin])           ::  simple message
        %talk  (furl (~(win re p.q.hin) 0 p.q.yar))     ::  program output
        %warn  (fume '~' [%leaf p.q.hin])               ::  system message
        %wipe  +.$(wip &)                               ::  delete old
    ==
  ::
  ++  yerk                                              ::  complete core
    ^-  [p=(list move) q=_..^$]
    :-  (flop mos)
    ..^$(dug ?.(wip (~(put by dug) hen yar) (~(del by dug) hen)))
  --
::
++  call                                                ::  process move
  |=  [hen=duct hic=(hypo kiss)]
  (take ~ hen hic)
::
++  doze
  |=  [now=@da hen=duct]
  ^-  (unit ,@da)
  ~
::
++  load
  |=  old=[%0 dug=(map duct yard)]
  ^+  ..^$
  ..^$(dug dug.old)
::
++  scry
  |=  [fur=(unit (set monk)) ren=@tas his=ship syd=desk lot=coin tyl=path]
  ^-  (unit (unit (pair lode ,*)))
  ~
::
++  stay  [%0 dug]
--
