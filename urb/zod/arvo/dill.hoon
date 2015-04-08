!:
::  dill (4d), terminal handling
::
|=  pit=vase
=>  |%                                                  ::  interface tiles
++  console-action                                      ::  console to app
  $%  [%det console-change]                             ::  edit prompt line
      [%inn ~]                                          ::  enter session
      [%out ~]                                          ::  exit session
      [%ret ~]                                          ::  submit and clear
  ==                                                    :: 
++  console-buffer  (list ,@c)                          ::  command state
++  console-change                                      ::  network change
  $:  ler=console-clock                                 ::  destination clock
      haw=@uvH                                          ::  source hash
      ted=console-edit                                  ::  state change
  ==                                                    ::
++  console-clock  ,[own=@ud his=@ud]                   ::  vector clock
++  console-edit                                        ::  shared state change
  $%  [%del p=@ud]                                      ::  delete one at
      [%ins p=@ud q=@c]                                 ::  insert at
      [%mor p=(list console-edit)]                      ::  combination
      [%nop ~]                                          ::  no-op
      [%set p=console-buffer]                           ::  discontinuity
  ==                                                    ::
++  console-effect                                      ::  app to console
  $%  [%bel ~]                                          ::  beep
      [%blk p=@ud q=@c]                                 ::  blink/match char at
      [%clr ~]                                          ::  clear screen
      [%det console-change]                             ::  edit input
      [%nex ~]                                          ::  save and clear input
      [%tan p=(list tank)]                              ::  classic tank
  ::  [%taq p=tanq]                                     ::  modern tank
      [%txt p=tape]                                     ::  text line
  ==                                                    ::
++  dill-belt                                           ::  console input
  $%  [%aro p=?(%d %l %r %u)]                           ::  arrow key
      [%bac ~]                                          ::  true backspace
      [%cru p=@tas q=(list tank)]                       ::  echo error
      [%ctl p=@ud]                                      ::  control-key
      [%del ~]                                          ::  true delete
      [%met p=@ud]                                      ::  meta-key
      [%ret ~]                                          ::  return
      [%rez p=@ud q=@ud]                                ::  resize, cols, rows
      [%txt p=(list ,@c)]                               ::  utf32 text
      [%yow p=gill]                                     ::  connect to app
  ==                                                    ::
++  dill-blit                                           ::  console output
  $%  [%bel ~]                                          ::  make a noise
      [%clr ~]                                          ::  clear the screen
      [%hop p=@ud]                                      ::  set cursor position
      [%mor p=(list dill-blit)]                         ::  multiple blits
      [%pro p=(list ,@c)]                               ::  show as cursor/line
      [%qit ~]                                          ::  close console
      [%out p=(list ,@c)]                               ::  send output line
      [%sag p=path q=*]                                 ::  save to jamfile
      [%sav p=path q=@]                                 ::  save to file
  ==                                                    ::
++  gill  (pair ship term)                              ::  general contact
--                                                      ::
=>  |%                                                  ::  console protocol
++  axle                                                ::  all dill state
  $:  %2                                                ::
      ore=(unit ship)                                   ::  identity once set
      hey=(unit duct)                                   ::  default duct
      dug=(map duct axon)                               ::  conversations
  ==                                                    ::  
++  axon                                                ::  dill per duct
  $:  ram=term                                          ::  console program
      tem=(unit (list dill-belt))                       ::  pending, reverse
      wid=_80                                           ::  terminal width
      pos=@ud                                           ::  cursor position
      see=(list ,@c)                                    ::  current line
  ==                                                    ::
--  =>                                                  ::
|%                                                      ::  protocol below
++  blew  ,[p=@ud q=@ud]                                ::  columns rows
++  belt                                                ::  raw console input
  $%  [%aro p=?(%d %l %r %u)]                           ::  arrow key
      [%bac ~]                                          ::  true backspace
      [%ctl p=@ud]                                      ::  control-key
      [%del ~]                                          ::  true delete
      [%met p=@ud]                                      ::  meta-key
      [%ret ~]                                          ::  return
      [%txt p=(list ,@c)]                               ::  utf32 text
  ==                                                    ::
++  blit                                                ::  raw console output
  $%  [%bel ~]                                          ::  make a noise
      [%clr ~]                                          ::  clear the screen
      [%hop p=@ud]                                      ::  set cursor position
      [%lin p=(list ,@c)]                               ::  set current line
      [%mor ~]                                          ::  newline
      [%sag p=path q=*]                                 ::  save to jamfile
      [%sav p=path q=@]                                 ::  save to file
  ==                                                    ::
++  flog                                                ::  sent to %dill
  $%  [%crud p=@tas q=(list tank)]                      ::
      [%text p=tape]                                    ::
      [%veer p=@ta q=path r=@t]                         ::  install vane
      [%vega p=path]                                    ::  reboot by path
      [%verb ~]                                         ::  verbose mode
  ==                                                    ::
++  gift                                                ::  out result <-$
  $%  [%bbye ~]                                         ::  reset prompt
      [%blit p=(list blit)]                             ::  terminal output
      [%init p=@p]                                      ::  set owner
      [%logo ~]                                         ::  logout
      [%veer p=@ta q=path r=@t]                         ::  install vane
      [%vega p=path]                                    ::  reboot by path
      [%verb ~]                                         ::  verbose mode
  ==                                                    ::
++  kiss                                                ::  in request ->$
  $%  [%belt p=belt]                                    ::  terminal input
      [%blew p=blew]                                    ::  terminal config
      [%boot p=*]                                       ::  weird %dill boot
      [%crud p=@tas q=(list tank)]                      ::  error with trace
      [%flog p=flog]                                    ::  wrapped error
      [%flow p=@tas q=(list gill)]                      ::  terminal config
      [%hail ~]                                         ::  terminal refresh
      [%hook ~]                                         ::  this term hung up
      [%harm ~]                                         ::  all terms hung up
      [%init p=ship]                                    ::  after gall ready
      [%noop ~]                                         ::  no operation
      [%talk p=tank]                                    ::
      [%text p=tape]                                    ::
      [%veer p=@ta q=path r=@t]                         ::  install vane
      [%vega p=path]                                    ::  reboot by path
      [%verb ~]                                         ::  verbose mode
  ==                                                    ::
--  =>                                                  ::
|%                                                      ::  protocol outward
++  mess                                                ::
  $%  [%dill-belt p=(hypo dill-belt)]                   ::
  ==                                                    ::
++  move  ,[p=duct q=(mold note gift)]                  ::  local move
++  note-ames                                           ::  weird ames move
  $%  [%make p=(unit ,@t) q=@ud r=@ s=?]                ::
      [%sith p=@p q=@uw r=?]                            ::
  ==                                                    ::
++  note-clay                                           ::  wait for clay, hack
  $%  [%warp p=sock q=riff]                             ::
  ==                                                    ::
++  note-dill                                           ::  note to self, odd
  $%  [%crud p=@tas q=(list tank)]                      ::
      [%init p=ship]                                    ::
      [%text p=tape]                                    ::
      [%veer p=@ta q=path r=@t]                         ::  install vane
      [%vega p=path]                                    ::  reboot by path
      [%verb ~]                                         ::  verbose mode
  ==                                                    ::
++  note-gall                                           ::  outbound message
  $%  [%mess p=[ship q=path] q=ship r=mess]             ::
      [%nuke p=[p=ship q=path] q=ship]                  ::
      [%show p=[p=ship q=path] q=ship r=path]           ::
      [%took p=[p=ship q=path] q=ship]                  ::
  ==                                                    ::
++  note                                                ::
  $%  [%a note-ames]                                    ::  out request $->
      [%c note-clay]                                    ::
      [%d note-dill]                                    ::
      [%g note-gall]                                    ::
  ==                                                    ::
++  riff  ,[p=desk q=(unit rave)]                       ::  see %clay
++  sign-ames                                           ::
  $%  [%nice ~]                                         ::
      [%init p=ship]                                    ::
  ==                                                    ::
++  sign-clay                                           ::
  $%  [%note p=@tD q=tank]                              ::
      [%writ p=riot]                                    ::
  ==                                                    ::
++  sign-dill                                           ::
  $%  [%blit p=(list blit)]                             ::
  ==                                                    ::
++  sign-gall                                           ::
  $%  [%crud p=@tas q=(list tank)]                      ::
      [%mean p=ares]                                    ::
      [%nice ~]                                         ::
      [%rush %dill-blit dill-blit]                      ::
  ==                                                    ::
++  sign-time                                           ::
  $%  [%wake ~]                                         ::
  ==                                                    ::
++  sign                                                ::  in result $<-
  $%  [%a sign-ames]                                    ::
      [%c sign-clay]                                    ::
      [%d sign-dill]                                    ::  
      [%g sign-gall]                                    ::
      [%t sign-time]                                    ::
  ==                                                    ::
::::::::                                                ::  dill tiles
--
=|  all=axle
|=  [now=@da eny=@ ski=sled]                            ::  current invocation
=>  |%
    ++  as                                              ::  per cause
      |_  $:  [moz=(list move) hen=duct our=ship]
              axon
          ==
      ++  abet                                          ::  resolve
        ^-  [(list move) axle]
        [(flop moz) all(dug (~(put by dug.all) hen +<+))]
      ::
      ++  call                                          ::  receive input
        |=  kyz=kiss
        ^+  +>
        ?+    -.kyz  ~&  [%strange-kiss -.kyz]  +>
          %flow  +>
          %harm  +>
          %hail  +>
          %belt  (send `dill-belt`p.kyz)
          %text  (from %out (tuba p.kyz))
          %crud  ::  (send `dill-belt`[%cru p.kyz q.kyz])
                 (crud p.kyz q.kyz)
          %blew  (send %rez p.p.kyz q.p.kyz)
          %veer  (dump kyz)
          %vega  (dump kyz)
          %verb  (dump kyz)
        ==
      ::
      ++  crud
        |=  [err=@tas tac=(list tank)]
        =+  ^=  wol  ^-  wall
            :-  (trip err)
            (zing (turn tac |=(a=tank (~(win re a) [0 wid]))))
        |-  ^+  +>.^$
        ?~  wol  +>.^$
        $(wol t.wol, +>.^$ (from %out (tuba i.wol)))
      ::
      ++  dump                                          ::  pass down to hey
        |=  git=gift
        ?>  ?=(^ hey.all)
        +>(moz [[u.hey.all %give git] moz])
      ::
      ++  done                                          ::  return gift
        |=  git=gift
        +>(moz :_(moz [hen %give git])) 
      ::
      ++  from                                          ::  receive belt
        |=  bit=dill-blit
        ^+  +>
        ?:  ?=(%mor -.bit)
          |-  ^+  +>.^$
          ?~  p.bit  +>.^$
          $(p.bit t.p.bit, +>.^$ ^$(bit i.p.bit))
        ?:  ?=(%out -.bit)
          %+  done  %blit
          :~  [%lin p.bit]
              [%mor ~]
              [%lin see]
              [%hop pos]
          ==
        ?:  ?=(%pro -.bit)
          (done(see p.bit) %blit [[%lin p.bit] [%hop pos] ~])
        ?:  ?=(%hop -.bit)
          (done(pos p.bit) %blit [bit ~])
        ?:  ?=(%qit -.bit)
          (dump %logo ~)
        (done %blit [bit ~])
      ::
      ++  init                                          ::  initialize
        ~&  [%dill-init our]
        =+  myt=(flop (need tem))
        =.  tem  ~
        =.  moz  :_(moz [hen %pass ~ %g %show [our [ram ~]] our ~])
        |-  ^+  +>
        ?~  myt  +>
        $(myt t.myt, +> (send i.myt))
      ::
      ++  into                                          ::  preinitialize
        |=  gyl=(list gill)
        %_    +>
            tem  `(turn gyl |=(a=gill [%yow a]))
            moz
          :_  moz
          :*  hen
              %pass 
              / 
              %c 
              [%warp [our our] %main `[%& %y [%ud 1] /]]
          ==
        ==
      ::
      ++  send                                          ::  send action
        |=  bet=dill-belt
        ?^  tem
          +>(tem `[bet u.tem])
        %_    +>
            moz
          :_  moz
          [hen %pass ~ %g %mess [our [ram ~]] our [%dill-belt -:!>(bet) bet]]
        ==
      ::
      ++  took                                          ::  send rush ack
        %_    .
            moz 
          :_(moz [hen %pass ~ %g %took [our [ram ~]] our])
        ==
      ::
      ++  take                                          ::  receive
        |=  sih=sign
        ^+  +>
        ?-    sih
            [%a %nice *]
          ::  ~&  [%take-nice-ames sih]
          +>
        ::
            [%a %init *]
          +>(moz :_(moz [hen %give +.sih]))
        ::
            [%c %note *]
          (from %out (tuba ~(ram re q.+.sih)))
        ::
            [%c %writ *]
          init
        ::
            [%d %blit *]
          (done +.sih)
        ::
            [%g %crud *]
          (crud p.+.sih q.+.sih)
        ::  (send %cru p.+.sih q.+.sih)  
        ::
            [%g %mean *]
          ~&  %dill-take-mean 
          +>
        ::
            [%g %nice *]
          ::  ~&  [%take-nice sih]
          +>
        ::
            [%g %rush %dill-blit *]
          took:(from +>+.sih)
        ::
            [%t %wake *]
          ::  ~&  %dill-wake 
          +>
        ==
      --
    ::
    ++  ax                                              ::  make ++as
      |=  [hen=duct kyz=kiss]                           ::
      ?~  ore.all  ~
      =+  nux=(~(get by dug.all) hen)
      ?^  nux  
        (some ~(. as [~ hen u.ore.all] u.nux))
      ?.  ?=(%flow -.kyz)  ~
      %-  some
      %.  q.kyz
      %~  into  as
      :-  [~ hen u.ore.all]
      :*  p.kyz
          [~ ~]
          80
          0
          (tuba "<{(trip p.kyz)}>")
      ==
    --
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
  ?:  ?=(%boot -.q.hic)
    :_(..^$ [hen %pass ~ (note %a p.q.hic)]~)
  ?:  ?=(%flog -.q.hic)
    ::  ~&  [%dill-flog +.q.hic]
    ?:  ?=([%crud %hax-init [%leaf *] ~] p.q.hic)
      =+  him=(slav %p (crip p.i.q.p.q.hic))
      :_(..^$ ?~(hey.all ~ [u.hey.all %give %init him]~))
    :_(..^$ ?~(hey.all ~ [u.hey.all %slip %d p.q.hic]~))
  =.  hey.all  ?^(hey.all hey.all `hen)
  ?:  ?=(%init -.q.hic)
    ::  ~&  [%call-init hen]
    ?:  =(ore.all `p.q.hic)
      [[hen %give q.hic]~ ..^$]
    =:  ore.all  `p.q.hic
        dug.all   ~
      ==
    =+  ^=  flo  ^-  (list (pair ship term))
        =+  myr=(clan p.q.hic)
        ?:  =(%pawn myr)
          [[p.q.hic %dojo] ~]
        ?:  =(%earl myr)
          =+  fap=(sein p.q.hic)
          [[fap %dojo] [fap %talk] ~]
        [[p.q.hic %dojo] [p.q.hic %talk] ~]
    =^  moz  all  abet:(need (ax (need hey.all) [%flow %sole flo]))
    ?:  |((lth p.q.hic 256) (gte p.q.hic (bex 64)))  [moz ..^$] ::  XX HORRIBLE
    [:_(moz [(need hey.all) %give %init p.q.hic]) ..^$]
  =+  nus=(ax hen q.hic)
  ?~  nus
    ~&  [%dill-no-flow q.hic]
    [~ ..^$]
  =^  moz  all  abet:(call:u.nus q.hic)
  [moz ..^$]
::
++  doze
  |=  [now=@da hen=duct]
  ^-  (unit ,@da)
  ~
::
++  load                                                ::  trivial
  |=  old=axle
  ..^$(all old)
  ::  |=  old=*   ::  diable
  ::  ..^$(ore.all `~zod)
::
++  scry
  |=  [fur=(unit (set monk)) ren=@tas his=ship syd=desk lot=coin tyl=path]
  ^-  (unit (unit (pair mark ,*)))
  [~ ~]
::
++  stay  all 
::
++  take                                                ::  process move
  |=  [tea=wire hen=duct hin=(hypo sign)]
  ^-  [p=(list move) q=_..^$]
  ?:  =(~ ore.all)
    ?:  ?=([%a %init *] q.hin)
      ::  ~&  [%take-init hen]
      =.  hey.all  ?^(hey.all hey.all `hen)
      [[[hen %give +.q.hin] ~] ..^$]
      ::  [~ ..^$]
    ~&  [%take-back q.hin]
    [~ ..^$]
  ?.  (~(has by dug.all) hen)
    ~&  [%take-weird-sign q.hin]
    ~&  [%take-weird-hen hen]
    [~ ..^$]
  =+  our=?>(?=(^ ore.all) u.ore.all)
  =^  moz  all  
    abet:(~(take as [~ hen our] (~(got by dug.all) hen)) q.hin)
  [moz ..^$]
--
