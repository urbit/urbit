::
::  dill (4d), terminal handling
::
|=  pit=vase
=>  |%                                                  ::  interface tiles
++  gift                                                ::  out result <-$
          $%  [%bbye ~]                                 ::  reset prompt
              [%blit p=(list blit)]                     ::  terminal output
              [%init p=@p]                              ::  report install
              [%logo p=@]                               ::  logout
              [%veer p=@ta q=path r=@t]                 ::  install vane
              [%vega p=path]                            ::  reboot by path
              [%verb ~]                                 ::  by %batz
          ==
++  kiss                                                ::  in request ->$
          $%  [%belt p=belt]                            ::  terminal input
              [%blew p=blew]                            ::  terminal config
              [%boot p=*]                               ::  weird %dill boot
              [%crud p=@tas q=(list tank)]              ::  error with trace
              [%flog p=flog]                            ::  wrapped error
              [%hail ~]                                 ::  terminal refresh
              [%hook ~]                                 ::  this term hung up
              [%harm ~]                                 ::  all terms hung up
              [%noop ~]                                 ::  no operation
              [%talk p=tank]                            ::
              [%text p=tape]                            ::
          ==                                            ::
++  flog                                                ::  sent to %dill
          $%  [%crud p=@tas q=(list tank)]              ::
              [%text p=tape]                            ::
          ==                                            ::
++  move  ,[p=duct q=(mold note gift)]                  ::  local move
++  note                                                ::  out request $->
          $%  $:  %b                                    ::  to %batz
          $%  [%hail ~]                                 ::
              [%harm ~]                                 ::
              [%hook ~]                                 ::
              [%kill p=~]                               ::
              [%line p=@t]                              ::
              [%ling ~]                                 ::
              [%make p=(unit ,@t) q=@ud r=@ s=?]        ::
              [%sith p=@p q=@uw r=?]                    ::
          ==  ==                                        ::
              $:  %d                                    ::  to %dill
          $%  [%crud p=@tas q=(list tank)]              ::
              [%text p=tape]                            ::
          ==  ==  ==                                    ::
++  sign                                                ::  in result $<-
          $?  $:  %b                                    ::  by %batz
          $%  [%hail ~]                                 ::
              [%helo p=path q=prod]                     ::
              [%logo p=@]                               ::
              [%save p=path q=@]                        ::
              [%sage p=path q=*]                        ::
              [%talk p=tank]                            ::
              [%tell p=(list ,@t)]                      ::
              [%text p=tape]                            ::
              [%verb ~]                                 ::
              [%veer p=@ta q=path r=@t]                 ::
              [%vega p=path]                            ::
              [%warn p=tape]                            ::
          ==  ==                                        ::
              $:  @tas                                  ::  by any
          $%  [%crud p=@tas q=(list tank)]              ::
              [%init p=@p]                              ::
              [%note p=@tD q=tank]                      ::
          ==  ==  ==                                    ::
::::::::                                                ::  dill tiles
++  bein                                                ::  terminal control
          $:  $:  bul=@ud                               ::  buffer length
                  bus=@ud                               ::  cursor in buffer
                  but=(list ,@c)                        ::  buffer text
                  buy=prom                              ::  input style
              ==                                        ::
              $:  hiz=@ud                               ::  history depth
                  hux=path                              ::  history path
                  hym=(map ,@ud (list ,@c))             ::  history overlay
                  hyt=hist                              ::  history object
                  hyr=(unit (list ,@c))                 ::  history search
              ==                                        ::
              $:  pol=@ud                               ::  length of prompt
                  pot=tape                              ::  prompt text
              ==                                        ::
          ==                                            ::
++  blew  ,[p=@ud q=@ud]                                ::  columns rows
++  belt                                                ::  raw console input
          $%  [%aro p=?(%d %l %r %u)]                   ::  arrow key
              [%bac ~]                                  ::  true backspace
              [%ctl p=@ud]                              ::  control-key
              [%del ~]                                  ::  true delete
              [%met p=@ud]                              ::  meta-key
              [%ret ~]                                  ::  return
              [%txt p=(list ,@c)]                       ::  utf32 text
          ==                                            ::
++  blit                                                ::  raw console output
          $%  [%bel ~]                                  ::  make a noise
              [%clr ~]                                  ::  clear the screen
              [%hop p=@ud]                              ::  set cursor position
              [%lin p=(list ,@c)]                       ::  set current line
              [%mor ~]                                  ::  newline
              [%sag p=path q=*]                         ::  save to jamfile
              [%sav p=path q=@]                         ::  save to file
          ==                                            ::
++  blot                                                ::  kill ring
          $:  p=@ud                                     ::  length
              q=@ud                                     ::  depth
              r=(list (list ,@c))                       ::  kills
          ==                                            ::
++  blur  ,[p=@ud q=(unit bein) r=blot]                 ::  columns, prompt
++  yard                                                ::  terminal state
          $:  p=?                                       ::  verbose
              q=blur                                    ::  display state
              r=(map path hist)                         ::  history
          ==                                            ::
--  =>
|%
++  dy
  |=  [hen=duct dug=(map duct yard)]
  =+  ^=  yar  ^-  yard
    =+  yur=(~(get by dug) hen)
    ?^  yur  u.yur
    [& [80 ~ *blot] ~]
  =|  mos=(list move)
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
  ++  leap                                              ::  accept response
    |=  [tea=wire sih=sign]  
    ^+  +>
    ?-    -.+.sih
        %crud                                           ::  error trace
      =.  q.+.sih  [[%leaf (trip p.+.sih)] q.+.sih]
      |-  ^+  +>.^$
      ?~  q.+.sih  +>.^$
      (fume:$(q.+.sih t.q.+.sih) '!' `tank`i.q.+.sih)
    ::
        %helo                                           ::  trigger prompt
      %-  edit
      =|  bed=bein
      =+  ^=  hyt  ^-  hist
          =+  hyt=(~(get by r.yar) p.+.sih)
          ?~(hyt *hist u.hyt)
      ?:  &(?=(^ q.q.yar) =(p.+.sih hux.u.q.q.yar))
        %=  u.q.q.yar
          hyt  [+(p.hyt) [%$ q.hyt]]
          pot  q.q.+.sih
          pol  (lent q.q.+.sih)
          buy  p.q.+.sih
        ==
      =+  zon=(tuba r.q.+.sih)
      =+  zow=(lent zon)
      %=    bed
          bul  zow
          bus  zow
          but  zon
          buy  p.q.+.sih
          hux  p.+.sih
          hiz  0
          hyt  [+(p.hyt) [%$ q.hyt]]
          pot  q.q.+.sih
          pol  (lent q.q.+.sih)
      ==
    ::
        ?(%hail %make %sith)
      +>.$(mos :_(mos [hen %pass ~ %b +.sih]))
    ::
        %note  ?.(p.yar +>.$ (fume p.+.sih q.+.sih))    ::  debug message
        %sage                                           ::  write a jamfile
      %=  +>.$
        mos  :_(mos [hen [%give %blit [%sag p.+.sih q.+.sih] ~]])
      ==
    ::
        %save                                           ::  write a file
      %=  +>.$
        mos  :_(mos [hen [%give %blit [%sav p.+.sih q.+.sih] ~]])
      ==
    ::
        %tell  (furl (turn p.+.sih |=(a=@t (trip a))))  ::  wall of text
        %talk  (furl (~(win re p.+.sih) 0 p.q.yar))     ::  program output
        %text  $(+.sih [%talk %leaf p.+.sih])           ::  simple message
        %warn  (fume '~' [%leaf p.+.sih])               ::  system message
        ?(%init %logo %veer %vega %verb)                ::  drop-throughs
      +>(mos :_(mos [hen %give +.sih]))
    ==
  ::
  ++  lear                                              ::  handle request
    |=  kyz=kiss
    ^+  +>
    ?-    -.kyz
        %flog  !!
        %noop  +>
        %belt                                           ::  terminal input
      ?~  q.q.yar
        beep
      ?^  hyr.u.q.q.yar                                 ::  live search
        ?+    p.kyz  $(hiz.u.q.q.yar 0, hyr.u.q.q.yar ~)
            [%bac *]
          ?:  =(~ u.hyr.u.q.q.yar)
            (curb [[%bel ~] ~])
          %-  edit
          %=  u.q.q.yar
            hyr  [~ (scag (dec (lent u.hyr.u.q.q.yar)) u.hyr.u.q.q.yar)]
          ==
        ::
            [%txt *]   (look hiz.u.q.q.yar (weld u.hyr.u.q.q.yar p.p.kyz))
            [%ctl %g]  (edit u.q.q.yar(bul 0, bus 0, but ~, hiz 0, hyr ~))
            [%ctl %r]
          ?:  =(p.hyt.u.q.q.yar hiz.u.q.q.yar)
            beep
          (look +(hiz.u.q.q.yar) u.hyr.u.q.q.yar)
        ==
      ?-    -.p.kyz
          %aro                                          ::  arrow
        ?-    p.p.kyz
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
        ^+  +>.$
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
        ?+  p.p.kyz
          beep
          %a  (edit u.q.q.yar(bus 0))
          %b  $(kyz [%belt %aro %l])
          %d  ?:  ?&  =(0 bul.u.q.q.yar)
                      =(0 bus.u.q.q.yar)
                  ==
                +>.$(mos :_(mos [hen %pass ~ %b [%kill ~]]))
              $(kyz [%belt %del ~])
          %e  (edit u.q.q.yar(bus bul.u.q.q.yar))
          %f  $(kyz [%belt %aro %r])
          %k  ?:  =(bul.u.q.q.yar bus.u.q.q.yar)
                beep
              =>  .(+>.$ (kill (slag bus.u.q.q.yar but.u.q.q.yar)))
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
          %l  +>.$(mos :_(mos [hen %give %blit [[%clr ~] ~]]))
          %n  $(kyz [%belt %aro %d])
          %p  $(kyz [%belt %aro %u])
          %u  ?:  =(0 bus.u.q.q.yar)
                beep
              =>  .(+>.$ (kill (scag bus.u.q.q.yar but.u.q.q.yar)))
              %-  edit
              ?>  ?=(^ q.q.yar)
              %=  u.q.q.yar
                bus  0
                bul  (sub bul.u.q.q.yar bus.u.q.q.yar)
                but  (slag bus.u.q.q.yar but.u.q.q.yar)
              ==
          %r  (edit u.q.q.yar(hyr [~ ~]))
          ::  TODO
          ::  %w  +>.$(mos :_(mos [hen %pass ~ %b [%limn ~]]))
          %x  +>.$(mos :_(mos [hen %pass ~ %b [%ling ~]]))
          %y  ?:  =(0 p.r.q.yar)
                beep
              $(kyz [%belt %txt (snag q.r.q.yar r.r.q.yar)])
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
        ?+    p.p.kyz
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
        %=    +>.$
            q.q.yar  ~
            r.yar
          ?:  |(=(%$ jab) =(%pass buy.u.q.q.yar))
            r.yar
          %+  ~(put by r.yar)
            hux.u.q.q.yar
          [p.hyt.u.q.q.yar [jab ?~(q.hyt.u.q.q.yar ~ +.q.hyt.u.q.q.yar)]]
        ::
            mos
          :*  [hen %pass ~ %b [%hail ~]]
              [hen %give [%bbye ~]]
              [hen %pass ~ %b [%line jab]]
              [hen %give [%blit [[%mor ~] ~]]]
              mos
          ==
        ==
      ::
          %txt                                          ::  text keys
        ?:  =(%none buy.u.q.q.yar)  beep
        =+  let=(lent p.p.kyz)
        %-  edit
        %=    u.q.q.yar
            bus  (add let bus.u.q.q.yar)
            bul  (add let bul.u.q.q.yar)
            but
          ;:  weld
            (scag bus.u.q.q.yar but.u.q.q.yar)
            p.p.kyz
            (slag bus.u.q.q.yar but.u.q.q.yar)
          ==
        ==
      ==
    ::
        %blew   +>.$(p.q.yar p.p.kyz)                   ::  window size
        %boot
      %=    +>.$
          mos
        :_(mos [hen %pass ~ (note %b p.kyz)])
      ==
    ::
        %crud                                           ::  error trace
      =.  q.kyz  [[%leaf (trip p.kyz)] q.kyz]
      |-  ^+  +>.^$
      ?~  q.kyz  +>.^$
      (fume:$(q.kyz t.q.kyz) '!' `tank`i.q.kyz)
    ::
        %hail                                           ::  refresh
      +>.$(mos :_(mos [hen %pass ~ %b kyz]))
    ::
        %harm                                           ::  all terms hung up
      =+  nug=((map duct yard) [[hen (~(get by dug) hen)] ~ ~])
      ^+  +>.$
      %=  +>.$
        dug  nug
        mos  :_(mos [hen %pass ~ %b kyz])
      ==
    ::
        %hook                                           ::  this term hung up
      +>.$(dug (~(del by dug) hen), mos :_(mos [hen %pass ~ %b kyz]))
    ::
        %talk  (furl (~(win re p.kyz) 0 p.q.yar))       ::  program output
        %text  $(kyz [%talk %leaf p.kyz])               ::  simple message
    ==
  ::
  ++  yerk                                              ::  complete core
    ^-  [p=(list move) q=(map duct yard)]
    :-  (flop mos)
    (~(put by dug) hen yar)
  --
--
=|  $:  %0                                              ::
        dug=(map duct yard)                             ::
    ==                                                  ::
|=  [now=@da eny=@ ski=sled]                            ::  current invocation
|%                                                      ::  poke/peek pattern
++  call                                                ::  handle request
  |=  $:  hen=duct
          hic=(hypo (hobo kiss))
      ==
  ^-  [p=(list move) q=_..^$]
  =>  %=    .                                           ::  XX temporary
          q.hic
        ^-  kiss
        ?:  ?=(%soft -.q.hic)
          ::  ~&  [%dill-call-soft (,@tas `*`-.p.q.hic)]
          ((hard kiss) p.q.hic)
        ?:  (~(nest ut -:!>(*kiss)) | p.hic)  q.hic
        ~&  [%dill-call-flub (,@tas `*`-.q.hic)]
        ((hard kiss) q.hic)
      ==
  ?:  ?=(%flog -.q.hic)
    :_  ..^$
    %+  turn  (~(tap by dug) *(list ,[p=duct q=yard]))
    |=([a=duct b=yard] [a %slip %d p.q.hic])
  =^  moz  dug  yerk:(lear:(dy hen dug) q.hic)
  [moz ..^$]
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
  ^-  (unit (unit (pair mark ,*)))
  [~ ~ [%tank >dug<]]
::
++  stay  [%0 dug]
++  take                                                ::  process move
  |=  [tea=wire hen=duct hin=(hypo sign)]
  ^-  [p=(list move) q=_..^$]
  =^  moz  dug  yerk:(leap:(dy hen dug) tea q.hin)
  [moz ..^$]
--
