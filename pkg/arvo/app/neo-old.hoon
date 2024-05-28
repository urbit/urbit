/-  neo, sole-sur=sole
/+  default-agent, dbug, verb, shoe, serv=server
/*  txt-hoon-imp    %hoon   /neo/src/std/imp/hoon/hoon
/*  txt-term-imp    %hoon   /neo/src/std/imp/term/hoon
/*  txt-ford-same   %hoon   /neo/src/std/imp/ford-same/hoon
/*  txt-ford-slop   %hoon   /neo/src/std/imp/ford-slop/hoon
/*  txt-ford-slap   %hoon   /neo/src/std/imp/ford-slap/hoon
/*  txt-ford-face   %hoon   /neo/src/std/imp/ford-face/hoon
/*  txt-ford-face   %hoon   /neo/src/std/imp/ford-face/hoon
/*  txt-ford-reef   %hoon   /neo/src/std/imp/ford-reef/hoon
/*  date-now   %js   /web/date-now/js
/*  error-tray   %js   /web/error-tray/js
/*  atom-input   %js   /web/atom-input/js
/*  multiline-input   %js   /web/multiline-input/js
/*  ha-wk   %js   /web/ha-wk/js
/*  s-k-y   %js   /web/s-k-y/js
/*  a-i-r   %js   /web/a-i-r/js
/*  style-css   %css   /web/style/css
|%
::
++  pave  pave:neo
++  ford  ford:neo
++  slug
  |=  a=tang
  ^+  same
  ?~  a  same
  ~_  i.a  $(a t.a)
++  bump-ever
  |=  =ever:neo
  ^-  ever:neo
  [+(-.ever) +(+.ever)]
::
++  trace-card-gall
  |=  =card
  ^-  tank
  ?:  ?=(%give -.card)
    leaf/"give"
  ?>  ?=(%pass -.card)
  leaf/"%pass {(spud p.card)}"
::
++  sole
  |%
  +$  id  sole-id:sole-sur
  +$  action  sole-action:sole-sur
  --
++  show-iota
  |=  i=iota
  ^-  @t
  ?@  i  i  (scot i)
+$  pith   pith:neo
+$  card  card:shoe
+$  race-form  _*eval-form:(pike:neo ,ewer:neo)
+$  race
  $:  rout=term
      form=race-form
      grab=(list item:pike:neo)
      have=(list clot:goon:neo)
  ==
+$  shell
  $:  cwd=name:neo
      race=(unit race)
  ==
+$  hull
  $%  [%clot =clot:goon:neo]
      [%ls ~]
      [%tree depth=@ud]
      [%show ~]
      [%cd =name:neo]
      [%race rout=@tas]
      [%poke p=hoon]
      [%ford ~]
      [%comm ~]
      [%clay ~]
  ==
+$  state-0
  $:  %0
      apex=(axal:neo room:neo)
      :: diary=(axal:neo memo:neo)
      :: dead=(map @uvH (axal:neo room:neo))
      =sound:neo
      foreign=(map tour:neo riot:neo)
      =halt:neo
      =fleet:neo
      $=  fiesta
      $:  by-grab=(jug stud:neo stud:neo)
          by-grow=(jug stud:neo stud:neo)
          con=(map [stud:neo stud:neo] stud:neo)
      ==
      ::
      husks=(jug stud:neo name:neo)
      shells=(map id:sole shell)
      races=(map id:sole race)
      hear=(map name:neo sound:neo)
      $=  unix
      $:  timers=(jug @da pith:neo)
          clay-peers=(map [src=pith hand=pith] [case=@ud =desk =path as=(unit mark)])
          eyre-req=(map @ta path)
          :: iris-req=(map [src=pith hand=pith] 

      ==
      adult=_|
  ==
++  is-parent-p
  |=  [parent=path kid=path]
  ^-  ?
  ?~  parent  &
  ?~  kid     |
  ?.  =(i.parent i.kid)
    |
  $(parent t.parent, kid t.kid)

++  is-parent
  |=  [parent=pith kid=pith]
  ^-  ?
  ?~  parent  &
  ?~  kid     |
  ?.  =(i.parent i.kid)
    |
  $(parent t.parent, kid t.kid)
++  get-ship
  |=  =pith
  ^-  @p
  ?>  ?=([[%p @] *] pith)
  +.i.pith
--
=|  state-0
=*  state  -
=<
  %-  agent:dbug
  %+  verb  |
  %-  (agent:shoe hull)  
  |_  =bowl:gall
  +*  this  .
      run   ~(. +> [bowl ~])
      def   ~(. (default-agent this %|) bowl)
  ++  on-init  
    ^-  (quip card _this)
    =^  cards  state
      abet:init:run
    [cards this]
  ++  on-save  !>(`state-0`state)
  ++  on-load
    |=  =vase
    =+  !<(old=state-0 vase)
    `this(state old)
  ++  on-poke
    |=  [=mark =vase]
    ^-  (quip card _this)
    =^  cards  state
      abet:(poke:run mark vase)
    [cards this]
  ++  on-watch  
    |=  =path
    ^-  (quip card _this)
    =^  cards  state
      abet:(watch:run path)
    [cards this]
  ::
  ++  on-leave
    |=  =path
    ^-  (quip card _this)
    =^  cards  state
      abet:(leave:run path)
    [cards this]
  ::
  ++  on-agent  
    |=  [=wire =sign:agent:gall]
    ^-  (quip card _this)
    =^  cards  state
      abet:(take-agent:run wire sign)
    [cards this]
  ++  on-arvo   
    |=  [=(pole knot) syn=sign-arvo]
    ^-  (quip card _this)
    ?:  ?=([%bind-site ~] pole)
      `this
    ?:  ?=([%next-clay ~] pole)
      =^  cards  state
        abet:(next-clay:run syn)
      [cards this]
    ?.  ?=([%sys rest=*] pole)
      `this
    =^  cards  state
      abet:(take:sys:run rest.pole syn)
    [cards this]
  ++  on-fail   
    |=  [=term =tang]
    ^-  (quip card _this)
    =^  cards  state
      abet:(on-fail:run term tang)
    [cards this]

  ++  on-peek   peek:run
  ++  command-parser 
    |=  =id:sole
    ~(parser walk:run id)
  ++  tab-list
    |=  [=id:sole query=@t]
    (~(tab walk:run id) query)
  ++  on-command
    |=  [=id:sole =hull] 
    =^  cards  state
      abet:(~(do walk:run id) hull)
    [cards this]
  ::
  ++  can-connect
    |=  =id:sole
    =(our src):bowl
  ++  on-connect
    |=  =id:sole
    =^  cards  state
      abet:(conn:run id)
    [cards this]
  ++  on-disconnect
    |=  =id:sole
    =^  cards  state
      abet:~(drop walk:run id)
    [cards this]
  --
|_  [=bowl:gall cards=(list card)]
++  abet  [(flop cards) state]
++  run  .
++  emit  |=(=card run(cards [card cards]))
++  pass  |=([=wire =note:agent:gall] (emit %pass wire note))
++  give  |=(=gift:agent:gall (emit %give gift))
++  fact  |=([pas=(list path) =cage] (give %fact pas cage))
++  emil  |=(caz=(list card) run(cards (welp (flop caz) cards)))
++  def   ~(. (default-agent run %|) bowl)
++  std-warp
  =/  =rave:clay
    [%next %z da/now.bowl /neo]
  (pass /next-clay %arvo %c %warp our.bowl q.byk.bowl `rave)
++  on-fail
  |=  [=term =tang]
  ^+  run
  ?.  =(term %arvo-response)
    +:(on-fail:def term tang)
  =.  run  std-warp
  %-  (slog leaf/"Failed build" (scag 3 tang))
  run

++  poke-our  
  |=([=wire =cage] (pass wire %agent [our dap]:bowl %poke cage))
::
++  poke-move
  |=  =move:neo
  =/  =wire  local/(pout p.move)
  (poke-our wire neo-move+!>(move))
++  poke-neo
  |=([=wire her=ship =cage] (pass wire %agent [her dap.bowl] %poke cage))
++  of-top  ~(. of:neo apex)
++  clay-beak  ^-  path
  /(scot %p our.bowl)/[q.byk.bowl]/(scot %da now.bowl)
++  bump-room-tree
  |=  =room:neo
  ^+  room
  ?.  ?=(%icon -.seat.room)
    room
  room(tree.ever.icon.seat +(tree.ever.icon.seat.room))
++  dorm
  |_  =name:neo
  ++  is-our
    =(our.bowl ship.name)
  ++  h  (hall pith.name)
  ++  j  ~(. jail name)
  ++  r  (reap pith.name)
  ++  pail
    ?:  is-our  pail:h
    pail:j
  ++  vial  (pail:soften pail)
  ++  state  q:pail
  ++  state-stud  p:pail
  ++  ever
    ?:  is-our  ever:h
    ever:j
  ++  slip
    ?:  is-our  slip:h
    slip:j
  ++  cane
    |=  =care:neo
    ?.  is-our  
      (cane:j care)
    =/  =room:neo  (got:of-top pith.name)
    ?:  ~(is-plot husk code.room)
      (cane:r care)
    (cane:h care)
  --
::
++  jail
  |_  =name:neo
  ++  pith  `pith:neo`(en-pith:name:neo name)
  ++  riot
    ^-  riot:neo
    ?^  zed=(~(get by foreign) [%z pith])
      u.zed
    ?^  why=(~(get by foreign) [%y pith])
      u.why
    ?~  exe=(~(get by foreign) [%x pith])
      !!
    u.exe
  ++  pail
    pail.cane:riot
  ++  vial  (pail:soften pail)
  ++  state  q:pail
  ++  state-stud  p:pail
  ++  ever        ever.cane.riot
  ++  slip        slip.riot
  ++  cane
    |=  =care:neo
    cane:(~(got by foreign) [care pith])
  --
  
++  hall
  |=  =pith:neo
  =/  =room:neo  (got:of-top pith)
  ?>  ?=(%icon -.seat.room)
  |%
  ++  pail  `pail:neo`[state-stud state]
  ++  vial  (pail:soften pail)
  ++  state  state.icon.seat.room
  ++  state-stud  `stud:neo`state.room
  ++  ever  ever.icon.seat.room
  ++  firm  ~(firm husk code.room)
  ++  slip  [state poke:firm kids:firm]
  ++  cane
    |=  =care:neo
    |^  ^-  cane:neo
    :*  care
        ever
        pail
        ^-  (map pith:neo [ever:neo pail:neo])
        ?-  care
          %x  *(map pith:neo [ever:neo pail:neo])
          %y  (~(run by (~(kid of:neo apex) pith)) room-to-ever)
          %z  *(map pith:neo [ever:neo pail:neo])
        ==
    ==
    ++  room-to-ever
      |=  =room:neo
      ^-  [ever:neo pail:neo]
      [(get-ever:room:neo room) (to-pail:room:neo room)]
    --
      
  --
++  leave
  |=  =(pole knot)
  ^+  run
  ?.  ?=([%sync %init as=@ car=@ rest=*] pole)
    run
  =+  ;;(=care:neo car.pole)
  =+  ;;(=pulp:neo as.pole)
  =/  pax=pith:neo  (pave:neo rest.pole)
  =/  =tone:neo  [%peer pulp pole]
  =.  sound  (~(del ju sound) [care rest.pole] tone)
  run
::
++  next-clay
  |=  syn=sign-arvo
  ^+  run
  ?>  ?=([%clay %writ *] syn)
  =.  run  std-warp
  ?~  p.syn
    ~&  %weird-clay-next
    run
  (copy-clay ~)
::
++  init
  |^  ^+  run
  =+  .^(neo-vase=vase %ca (welp clay-beak /sur/neo/hoon))
  =/  reef=vase  (slop !>(..zuse) neo-vase(p [%face %neo p.neo-vase]))
  =/  riff=room:neo
    [%ford-riff %ford-out ~ icon/[[1 1] !>(`[cac=(unit vase) ~]`[`!>(ford-riff) ~]) ~ ~ ~]]
  =.  apex  (put:of-top #/out/std/imp/ford-riff riff)
  =.  run  (make-riff #/out/reef reef)
  =.  run  (re-export reef %hoon !,(*hoon @t))
  =.  run  (make-riff #/out/std/pro/ford-out (ford-out reef))
  =.  run  (make-riff #/out/std/pro/ford-in (ford-in reef))
  =.  run  (make-riff #/out/std/pro/term (term reef))
  =.  run  (make-riff-slap #/out/std/imp/hoon reef txt-hoon-imp)
  =.  run  (make-riff-slap #/out/std/imp/term reef txt-term-imp)
  =.  run  (make-riff-slap #/out/std/imp/ford-same reef txt-ford-same)
  =.  run  (make-riff-slap #/out/std/imp/ford-face reef txt-ford-face)
  =.  run  (make-riff-slap #/out/std/imp/ford-slop reef txt-ford-slop)
  =.  run  (make-riff-slap #/out/std/imp/ford-slap reef txt-ford-slap)
  =.  run  (re-export reef %json !,(*hoon json))
  =.  run  (re-export reef %mime !,(*hoon mime))
  =.  run  (poke %noun !>(%clay))
  =.  run  (emit %pass /bind-site %arvo %e %connect [~ dap.bowl ~] dap.bowl)
  std-warp
  ++  make-riff-slap
    |=  [wer=pith:neo reef=vase txt=@t]
    ~|  wer
    =;  =vase
      (make-riff wer vase)
    =+  vaz=(vang & (pout wer))
    %+  slap  reef
    (scan (trip txt) (full (ifix [gay gay] tall:vaz)))
  ::
  ++  ford-riff
    ^-  firm:neo
    |%
    ++  state  %ford-out
    ++  poke   *(set stud:neo)
    ++  kids  ~
    ++  deps  ~
    ++  form
      ^-  form:neo
      |_  [=bowl:neo =ever:neo state-vase=vase *]
      +*  sta  !<([cache=(unit vase) ~] state-vase)
      ++  poke
        |=  =pail:neo  
        ^-  (quip card:neo vase)
        `state-vase
      ::
      ++  init
        |=  old=(unit vase)
        ^-  (quip card:neo vase)
        =+  !<(sta=[ref=(unit vase) ~] (need old))
        `!>(sta)
      --
    --
  ++  re-export
    |=  [reef=vase =stud:neo =hoon]
    ^+  run
    %+  make-riff  ~(pith pro stud)
    (slap reef hoon)
  ::
  ++  term
    |=  reef=vase
    ^-  vase
    %+  slap  reef
    !,  *hoon
    ,term
  ::
  ++  ford-out
    |=  reef=vase
    ^-  vase
    %+  slap  reef
    !,  *hoon
    ,[cache=(unit vase) *]
  ::
  ++  ford-in
    |=  reef=vase
    ^-  vase
    %+  slap  reef
    !,(*hoon ,~)
  ::
  ++  make-riff
    |=  [=pith riff=vase]
    ^+  run
    =.  pith  [p/our.bowl pith]
    (on-card pith %make %ford-riff `!>([`riff ~]) ~)
  --
::
++  sync-room
  |=  [=stud:neo =name:neo]
  ^+  run
  =/  =wire  sync/(pout pith.name) 
  :: =.  run  abet:(~(init xeno name) stud/stud)
  (emit %pass wire %agent [ship.name dap.bowl] %watch [%sync %init (pout pith.name)])
++  our-sys-name  `name:neo`[our.bowl `pith:neo`#/$/sys]
++  our-sys-pith  (en-pith:name:neo our-sys-name)
::
++  peek
  |=  pax=path
  ^-  (unit (unit cage))
  ?.  ?=([%x *] pax)
    [~ ~]
  =/  pax=(pole iota)  (pave:neo t.pax)
  ?+  pax  [~ ~]
    [as=@ car=@ [%p who=@] pith=*]  (run-peek [as car who pith]:pax)
  ==
++  run-peek
  |=  [as=term car=term =name:neo]
  ^-  (unit (unit cage))
  =+  ;;(=care:neo car)
  ?.  =(our.bowl ship.name)
    [~ ~] :: XX: todo
  ?:  ?=([%see *] pith.name)
    (peek:see t.pith.name)
  =/  res  (~(cane dorm name) care)
  ?+    as  [~ ~]
      %noun  ``neo-cane+!>(res)
      %json
    ~&  %json
    ``json+!>((cane:enjs:neo res !<($-(pail:neo json) (all-grow %json))))
  ::
    
      %html
    =*  en-html
      ~(. en-html:neo [#/['~']/scry/[dap.bowl]/html (en-pith:name:neo name)])
    =-  ``hymn+!>(-)
    %+  lift-to-hymn:en-html  pith.name
    (cane:en-html res !<($-(pail:neo manx) (all-grow %node)))
  ==
::
++  take-arvo
  |=  [=pith syn=sign-arvo]
  ^+  run
  ?:  ?=(%remote -.pith)
    !! :: abet:(~(take xeno pith) syn)
  ?:  ?=([%husk @ *] pith)
    !! :: (~(take husk i.t.pith) (pout t.t.pith) syn)
  !!
  :: abet:(take-arvo:(abed:arvo our-sys-pith pith) pith syn)
::
++  forward-poke
  |=  [=name:neo pok=*]
  ^+  run
  =/  =wire  forward/(en-path:name:neo name)
  =/  =dock  [ship.name dap.bowl]
  run :: XX: revive
  :: =-  (emit %pass wire %agent dock %poke -)
  :: noun+!>(`card:neo`[(en-pith:name:neo name) %poke pok])
++  print-dbug
  |=  veb=?
  |^  ^+  same
  %-  %*(. slog pri 1)
  %-  lure
  :+  %rose  [ret "Shrubbery" sep]
  :*  leaf/"Local"
      (local-axal *pith apex)
      leaf/"Remote"
      %+   turn  ~(tap by fleet)
      |=  [=ship =brig:neo]
      (remote-axal [p/ship]~ brig)
  ==
  ++  ret  [',' (^reap 4 ' ')]
  ++  sep  *tape
  ++  remote-kids
    |=  [=pith =(axal:neo cane:neo)]
    ^-  tank
    !!
::    ?:  =(~ kid.axal)
::      leaf/"No children"
::    :+  %rose  [ret "Kids:" sep]
::    %+  murn  ~(tap by kid.axal)
::    |=  [=iota a=(axal:neo cell:neo)]
::    ^-  (unit tank)
::    ?:  &(veb =(pith ~) |(=(iota %src) =(iota %pre)))
::      ~
::    `(remote-axal (snoc pith iota) a)
  ++  remote-axal
    |=  [=pith =(axal:neo cane:neo)]
    ^-  tank
    !!
::    :+  %rose  [ret (en-tape:pith:neo pith) sep]
::    ^-  (list tank)
::    %-  snoc
::    :_  (remote-kids pith axal)
::    ^-  (list tank)
::    ?~  fil.axal
::      ~[leaf/"No data"]
::    =/  =cell:neo  u.fil.axal
::    ?:  =(case.cell 0)
::      ~[leaf/"No data at this path"]
::    :~  leaf/"State"
::        ?:  (lth 10.000 (met 3 (jam q.state.cell)))
::          leaf/"Too large to print"
::        (sell state.cell)
::          
::        leaf/"Case: {(scow %ud case.cell)}"
::      ::
::      ::
::        leaf/"Source: {<p.span.cell>}"
::    ==

  ++  local-kids
    |=  [=pith =(axal:neo room:neo)]
    ^-  tank
    ?:  =(~ kid.axal)
      leaf/"No children"
    :+  %rose  [ret "Kids:" sep]
    %+  murn  ~(tap by kid.axal)
    |=  [=iota a=(axal:neo room:neo)]
    ^-  (unit tank)
    ?:  &(veb =(pith ~) |(=(iota %src) =(iota %pre)))
      ~
    `(local-axal (snoc pith iota) a)
  ++  local-axal
    |=  [=pith =(axal:neo room:neo)]
    ^-  tank
    :+  %rose  [ret (en-tape:pith:neo pith) sep]
    ^-  (list tank)
    %-  snoc
    :_  (local-kids pith axal)
    ^-  (list tank)
    ~
::  ?~  fil.axal
::    ~[leaf/"No data"]
::  =/  =room:neo  u.fil.axal
::  ?:  =(ever.icon.room [0 0])
::    ~[leaf/"No data at this path"]
::  :*  leaf/"State"
::      ?:  (lth 10.000 (met 3 (jam q.state.icon.room)))
::        leaf/"Too large to print"
::      (sell state.icon.room)
::        
::      leaf/"Case: {(scow %ud node.ever.icon.room)}"
::    ::
::    ::
::      leaf/"Source: {<code.room>}"
::      ^-  (list tank)
::      ?:  =(~ conf.room)
::        ~
::      :_  ~
::      :+  %rose  [" " "Dependencies" sep]
::      %+  turn  ~(tap by conf.room)
::      |=  [=term p=^pith]
::      leaf/"{<term>} -> {(en-tape:pith:neo p)}"
::  ==
  --
::
++  poke
  |=  [=mark =vase]
  ^+  run
  ~|  mark/mark
  ?:  =(%handle-http-request mark)
    =-  (~(on-req srv id) req)
    !<([id=@ta req=inbound-request:eyre] vase)
  ?:  =(%neo-raw-poke mark)
    =+  !<(raw=raw-poke:neo vase)
    (on-move (raw-poke:harden raw))
  ?:  =(%neo-move mark)
    =+  !<(=move:neo vase)
    (on-move move)
  ?>  ?=(%noun mark)
  ?:  =(%clay q.vase)
    (copy-clay ~)
  ?:  ?=([%file *] q.vase)
    =+  ;;(=path +.q.vase)
    (copy-clay `path)
  ?:  ?=([%dbug veb=?] q.vase)
    ?>  =(our src):bowl
    %-  (print-dbug veb.q.vase)
    run
  ?:  ?=(%out -.q.vase)
    =+  ;;(=out:neo +.q.vase)
    (do-out out)
  =+  ;;(=card:neo q.vase)
  =/  =name:neo  (de-pith:name:neo p.card) 
  ?.  =(our.bowl ship.name)
    ?>  ?=(%poke -.q.card)
    !! :: XX: fix(forward-poke name val.q.card)
  (on-card card)
++  see
  |%
  ++  peek
    |=  =(pole iota)
    ^-  (unit (unit cage))
    [~ ~]
  --
++  dove
  |_  here=pith:neo
  ++  curr
    ::
    =/  rom  (get:of-top here)
    =-  -(a.g [[%here (en-tape:pith:neo here)] a.g.-])
    ^-  manx
    ?~  rom
      ;div.wf.hf.fc.ac.jc(empty ""): nothing here
    =/  =room:neo  u.rom
    =/  stud  ^-  @tas  ?^(state.room mark.state.room state.room)
    =-  -(a.g [[%stud (trip stud)] a.g.-])
    (val u.rom)
  ::
  ++  children
    ::
    =/  dirs
      ^-  (list iota)
      %~  tap  in
      %-  silt
      %+  turn  ~(tap by (kid:of-top here))
      |=  [=pith:neo *]
      -.pith
    ;div.wf.hf.fc.g1.js.ac
      =here  (en-tape:pith:neo here)
      =slot  "tree"
      ;*
      ?~  (lent dirs)
        ;=
          ;div.wf.hf.fc.ac.jc: no children
        ==
      %+  turn
        :: alphabetical sort
        ^-  (list iota)
        (sort dirs aor)
      |=  =iota
      ;button.p2.br1.b1.hover.wf.fr.js
        =hx-get  (en-tape:pith:neo :(weld /neo/hawk here /[iota]))
        =hx-target  "closest ha-wk"
        =hx-swap  "innerHTML"
        ; {(trip ?@(iota iota (scot iota)))}
      ==
    ==
    ::
  ++  val
    ::
    |=  =room:neo
    ^-  manx
    =+  !<(grow=$-(pail:neo $-(=bowl:neo manx)) (all-grow %htmx))
    ^-  manx
    =/  res=(each $-(=bowl:neo manx) tang)
      (mule |.((grow (to-pail:room:neo room))))
    ?:  ?=(%& -.res)
      =+  man=[u=p.res ~]
      %-  u.man
      =+  b=*bowl.neo  :: manually constructing a bowl. this is ugly
      %=  b
        here  here
        kids
          %-  ~(run by (kid:of-top here))
          |=  =room:neo
          (to-pail:room:neo room)
      ==
    ;div.p3.fc.g3
      ;div.f-error.fc.g2
        ;span: unable to render as %htmx
        ;span.bold
          ;+  ;/
          ?^  state.room
            (trip mark.state.room)
          (trip state.room)
        ==
      ==
      ;code.pre.scroll-x.flex.flex-col
        ;div: {?>(?=(%icon -.seat.room) (text state.icon.seat.room))}
        ;*
        %+  turn  p.res
        |=  tan=tank
        ;div: {~(ram re tan)}
      ==
    ==
    ::
  ++  svg-wrapper
    ::
    |=  [color=tape viewbox=tape body=manx]
    ^-  manx
    ;svg
      =xmlns  "http://www.w3.org/2000/svg"
      =viewBox  viewbox
      =fill  color
      =style  "height: 1em;"
      ;+  body
    ==
    ::
  ++  svg-square
    ::
    |=  color=(unit tape)
    %^  svg-wrapper  (fall color "currentColor")
      "0 0 448 512"
    ;path(d "M0 96C0 60.7 28.7 32 64 32H384c35.3 0 64 28.7 64 64V416c0 35.3-28.7 64-64 64H64c-35.3 0-64-28.7-64-64V96z");
    ::
  ++  favicon
    ::
    =-
      ;link
        =rel  "icon"
        =type  "image/svg+xml"
        =href  -
        ;
      ==
    %+  weld  "data:image/svg+xml;utf8,"
    %-  en-xml:html
    (svg-square `"white")
    ::
  ++  html-enc-js
    ::
    ::  htmx extension which encodes the request
    ::  as the serialized HTML of the calling element
    ::
    %-  trip
    '''
    htmx.defineExtension('html-enc', {
      onEvent: function (name, evt) {
        if (name === "htmx:configRequest") {
          evt.detail.headers['Content-Type'] = "text/html";
        }
      },
      encodeParameters : function(xhr, parameters, elt) {
        xhr.overrideMimeType('text/html');
        let xmls = new XMLSerializer();
        return (xmls.serializeToString(elt));
      }
    });
    '''
    ::
  ++  lift
    ::
    |=  in=manx
    ^-  manx
    ;html
      ;head
        ;meta(charset "UTF-8");
        ;title: s k y
        ;script(src "https://code.jquery.com/jquery-3.7.1.js");
        ;script(src "https://unpkg.com/htmx.org@1.9.11");
        ;script(src "https://unpkg.com/htmx.org@1.9.11/dist/ext/response-targets.js");
        ;script: {html-enc-js}
        ;meta
          =name  "viewport"
          =content
            """
            width=device-width,
            initial-scale=1.0,
            maximum-scale=1.0"
            """
          ;
        ==
        ;meta
          =name  "htmx-config"
          =content  (trip '{"ignoreTitle":"true"}')
          ;
        ==
        ;style
          ;+  ;/  %-  trip
          '''
          @font-face {
            font-family: 'Urbit Sans';
            src: url("https://media.urbit.org/fonts/UrbitSans/UrbitSansVFWeb-Regular.woff2") format("woff2");
            font-style: normal;
            font-weight: 100 700;
          }
          /*
          @font-face {
            font-family: 'Urbit';
            src: url('https://nyc3.digitaloceanspaces.com/drain/hawk/2024.4.10..21.47.28-urbit.ttf') format('truetype');
          }
          */
          '''
        ==
        ;script
          ;+  ;/
          """
          const sharedStyles = new CSSStyleSheet();
          sharedStyles.replaceSync(`{(trip style-css)}`);
          document.adoptedStyleSheets = [sharedStyles];
          window.log=function()\{if(this.console)\{console.log(Array.prototype.slice.call(arguments));}};
          jQuery.fn.log=function (msg)\{console.log(msg, this); return this;};
          """
        ==
        ;script: {(trip date-now)}
        ;script: {(trip atom-input)}
        ;script: {(trip error-tray)}
        ;script: {(trip multiline-input)}
        ;script: {(trip ha-wk)}
        ;script: {(trip s-k-y)}
        ;script: {(trip a-i-r)}
        ;+  favicon
      ==
      ;body
        =hx-ext  "html-enc,response-targets"
        =hx-swap  "innerHTML"
        =hx-boost  "true"
        =hx-history  "false"
        =hx-replace-url  "/neo/sky"
        =hx-target  "closest ha-wk"
        ;+  in
      ==
    ==
  --
::
++  srv
  |_  eyre-id=@ta
  ++  send
    |=  res=simple-payload:http
    ^+  run
    (emil (give-simple-payload:app:serv eyre-id res))

  ::
  ++  err
    |=  =tang
    =.  eyre-req.unix  (~(del by eyre-req.unix) eyre-id)
    (send (error:gen:serv tang))
  ::
  ++  response
    |=  =path
    %-  send
    =/  res=(unit (unit cage))
      =/  res  (mule |.((peek %x path)))
      ?:  ?=(%& -.res)
        p.res
      %-  (slog leaf/"Failed to generate response" p.res)
      [~ ~]
    ?.  ?=([~ ~ *] res)
      not-found:gen:serv
    =*  cag  u.u.res
    ?+    p.cag  invalid-req:gen:serv
        %json  (json-response:gen:serv !<(json q.cag))
        %hymn  (manx-response:gen:serv !<(manx q.cag))
        %mime  (mime-response !<(mime q.cag))
        %noun
      :_  `(as-octs:mimes:html (jam q.q.cag))
      [200 [['content-type' 'application/x-urb-jam'] ~]]
    ==
  ++  mime-response
    |=  =mime
    ^-  simple-payload:http
    :_  `q.mime
    [200 [['content-type' (crip (slag 1 (spud p.mime)))] ~]]
  ::
  ++  on-req
    |=  req=inbound-request:eyre
    ^+  run
    ::  XX: revive when privacy
    ::  ?.  authenticated.req
    ::    (login-redirect:app:serv request.req)
    =/  line=request-line:serv  (parse-request-line:serv url.request.req)
    ?>  &(?=([@ @ *] site.line) =('neo' i.site.line))
    ?.  =('scry' i.t.site.line)
      =/  purl  ::  parsed url with query params
        ::
        ^-  [pax=path pam=(map @t @t)]
        =+  %+  rash  url.request.req
            ;~  plug
                ;~(pfix fas (more fas smeg:de-purl:html))
                yque:de-purl:html
            ==
        [->+.- (molt +.-)]
        ::
      =/  body  :: body parsed to a manx
        ::
        %+  fall
          (de-xml:html q:(fall body.request.req [p=0 q='']))
        *manx
        ::
      =/  here=path  (welp /[(scot %p our.bowl)] pax.purl)
      =/  =pith  (pave:neo pax.purl)
      =*  dov  ~(. dove pith)
      ::
      ?:  =('sky' i.t.site.line)
        ::
        =/  here  (pave:neo /sky)
        ?~  rum=(get:of-top here)
          ::
          ::  create default tree
          =/  bootstrap
            ^-  (list card:neo)
            :~
              [(weld #/[p/our.bowl] here) %make %sky `!>([%system ~ 0]) ~]
              [#/[p/our.bowl]/home/diary %make %diary `!>('') ~]
              [#/[p/our.bowl]/home/tasks %make %task `!>(['' | ~]) ~]
              [#/[p/our.bowl]/home/sail %make %sail `!>(['' 'prose p3' ~]) ~]
              [#/[p/our.bowl]/home/iframes/wiki %make %iframe `!>('https://en.wikipedia.org/wiki/Main_Page') ~]
            ==
            |-
            ?~  bootstrap
              %-  send
              %-  manx-response:gen:serv
              %-  ~(lift dove pax.purl)
              ;div.wf.hf.fc.jc.ac
                =hx-get  "/neo/sky"
                =hx-target  "this"
                =hx-swap  "outerHTML"
                =hx-trigger  "load"
                ; initializing
              ==
            =.  run
              %-  poke-move
              :-  #/[p/our.bowl]/$/eyre/req/[eyre-id]
              i.bootstrap
            $(bootstrap t.bootstrap)
            ::
        %-  send
        %-  manx-response:gen:serv
        %-  ~(lift dove pax.purl)
        =+  !<(grow=$-(pail:neo $-(=bowl:neo manx)) (all-grow %htmx))
        ?>  ?=(%icon -.seat.u.rum)
        ?~  man=(mole |.((grow [%sky state.icon.seat.u.rum])))
          ~&  'could not convert sky to htmx'
          !!
        %-  u.man
        =+  b=*bowl.neo
        %=  b
          here  here
          kids
            %-  ~(run by (kid:of-top here))
            |=  =room:neo
            (to-pail:room:neo room)
        ==
      ?>  =('hawk' i.t.site.line)
        ::
        ?:  =(%'POST' method.request.req)  ::  %poke
          ::
          =/  stud  (@tas (~(got by pam.purl) 'stud'))
          =/  conv  !<($-([@ manx] vase) (all-grab %node))
          =/  vert  (mule |.((conv [stud body])))
          ?-  -.vert
            %.n
              %-  send
              =-  -(status-code.response-header 400)
              %-  manx-response:gen:serv
              ;div.fc.p2.border.br1.scroll-x.scroll-y.wf.pre.mono
                =style  "max-height: 400px;"
                =here  (en-tape:pith:neo pith)
                ;*
                %+  turn  (tang p.vert)
                |=  =tank
                ;div: {(of-wall:format (~(win re tank) 0 55))}
              ==
            %.y
              =/  =pail:neo  [stud p.vert]
              =.  run
                %-  poke-move
                :-  #/[p/our.bowl]/$/eyre/req/[eyre-id]
                [(pave:neo here) %poke pail]
              ::
              =+  !<(grow=$-(pail:neo $-(=bowl:neo manx)) (all-grow %htmx))
              =/  man  (mule |.((grow pail)))
              %-  send
              ?-  -.man
                %.y
                  %-  manx-response:gen:serv
                  %-  p.man
                  =+  b=*bowl.neo
                  %=  b
                    here  pith
                  ==
                %.n
                  =-  -(status-code.response-header 500)
                  %-  manx-response:gen:serv
                  ;div.fc.p2.border.br1.scroll-x.scroll-y.wf.pre.mono
                    =style  "max-height: 400px;"
                    =here  (en-tape:pith:neo (pave:neo pax.purl))
                    ;*
                    %+  turn  (tang p.man)
                    |=  =tank
                    ;div: {(of-wall:format (~(win re tank) 0 55))}
                  ==
                ::
              ==
            ::
          ==
        ?:  =(%'PUT' method.request.req)    ::  %make
          ::
          =/  stud  (@tas (~(got by pam.purl) 'stud'))
          =/  conv  !<($-([@ manx] vase) (all-grab %node))
          ?@  vert=(mole |.((conv [stud body])))
            (send (manx-response:gen:serv ;/("failed to convert")))
          =/  =pail:neo  [stud u.vert]
          =.  run
            %-  poke-move
            :-  #/[p/our.bowl]/$/eyre/req/[eyre-id]
            [(pave:neo here) %make p.pail `q.pail ~]
          ::
          %-  send
          %-  manx-response:gen:serv
          =+  !<(grow=$-(pail:neo $-(=bowl:neo manx)) (all-grow %htmx))
          ?^  man=(mole |.((grow pail)))
            %-  u.man
            =+  b=*bowl.neo  :: manually constructing a bowl. this is ugly
            %=  b
              here  (pave:neo pax.purl)
            ==
          ;div: some sorta error occured
          ::
          ::
        ?:  =(%'DELETE' method.request.req) ::  %tomb
          ::
          !!
          ::
        ::    %'GET'  :: "read"
          ::
          %-  send
          %-  manx-response:gen:serv
          ?:  (~(has by pam.purl) 'tree')
            children:dov
          curr:dov
        ::
    =/  =path  t.t.site.line
    ?:  =(%'POST' method.request.req)
      :: ?>  authenticated.req
      (on-post path (need (de:json:html q:(need body.request.req))))
    ?.  =('GET' method.request.req)
      (send invalid-req:gen:serv)
    (response path)
  ::
  ++  on-post
    |=  [=(pole knot) jon=json]
    ^+  run
    ?>  ?=([as=@ car=@ ship=@ rest=*] pole)
    ?.  =((slav %p ship.pole) our.bowl)
      (send invalid-req:gen:serv)
    =/  conv
      !<($-([@ json] vase) (all-grab %json))
    =/  =pail:neo
      ((pail:dejs:neo conv) jon)
    =/  =move:neo
      :-  #/[p/our.bowl]/$/eyre/req/[eyre-id]
      [(pave:neo [ship rest]:pole) %poke pail]
    =.  eyre-req.unix  (~(put by eyre-req.unix) eyre-id pole)
    =.  run  (poke-move move)
    finish-post :: XX: stale
  ::
  ++  finish-post
    =/  =(pole knot)  (~(got by eyre-req.unix) eyre-id)
    =.  eyre-req.unix  (~(del by eyre-req.unix) eyre-id)
    (response pole)
  --
::
::
++  on-card
  |=  =card:neo
  ^+  run
  (on-move our-sys-pith card)
::
++  on-move
  |=  =move:neo
  =/  =name:neo  (de-pith:name:neo p.q.move)
  ?>  =(our.bowl ship.name)
  =/  src=name:neo  [src.bowl p.move]
  ?.  ?=([%$ *] pith.name)
    abet:(arvo move)
  (on-move:sys p.move q.move(p t.pith.name))
++  sys
  |%  
  ++  clay-peer
    |=  [src=pith:neo hand=pith:neo]
    ^+  run
    =/  [case=@ud =peer:clay:neo]  (~(got by clay-peers.unix) [src hand])
    =/  =wire  (welp /sys/clay/peer (pout (en:drive:neo ~[src hand])))
    =/  =rave:clay  [%sing [%t ud/case path.peer]]
    (emit %pass wire %arvo %c %warp our.bowl desk.peer `rave)
  ++  on-move
    |=  [src=pith =card:neo]
    |^  ^+  run
    ?+  p.card  ~|(bad-sys-move-pith/p.card !!)
      [%behn ~]  (behn q.card)
      [%clay ~]  (clay q.card)
      [%%
    ==
    ++  clay
      |=  =note:neo
      ?>  ?=(%poke -.note)
      ?>  ?=(%clay-req p.pail.note)
      =+  !<(=req:clay:neo q.pail.note)
      ?-    -.req
          %pull
        =.  clay-peers.unix  (~(del by clay-peers.unix) [src pith.req])
        run
      ::
          %peer
        =+  .^(=cass:^clay %cw /(scot %p our.bowl)/[desk.peer.req]/(scot %da now.bowl)/sys/kelvin)
        =/  [case=@ud =peer:clay:neo]  (~(gut by clay-peers.unix) [src pith.req] [0 peer.req])
        =.  case  ud.cass
        =.  clay-peers.unix  (~(put by clay-peers.unix) [src pith.req] [case peer])
        (clay-peer src pith.req)
      ==
    ::
    ++  behn
      |=  =note:neo
      ^+  run
      ?>  ?=(%poke -.note)
      ?>  ?=(%behn-req p.pail.note)
      =+  !<(=req:behn:neo q.pail.note)
      ?-    -.req
          %rest
        =/  =wire  /sys/behn/wait/(scot %da p.req)
        =.  timers.unix  (~(del ju timers.unix) p.req src)
        ?.  =(~ (~(get ju timers.unix) p.req))
          run
        (emit %pass wire %arvo %b %rest p.req)
      ::
          %wait
        =/  =wire  /sys/behn/wait/(scot %da p.req)
        =.  timers.unix  (~(put ju timers.unix) p.req src)
        ?.  =(1 ~(wyt in (~(get ju timers.unix) p.req)))
          run
        (emit %pass wire %arvo %b %wait p.req)
      ==
    --
  ++  take
    |=  [=(pole knot) syn=sign-arvo]
    |^  ^+  run
    ?+  pole  ~|(bad-sys-take/pole !!)
      [%behn %wait date=@da ~]  (behn-wait (slav %da date.pole))
      [%clay %peer res=*]  (clay-writ res.pole)
    ::  [%behn %res date=@da ~]  (behn-res (slav %da date.pole))
    ==
    ++  clay-writ
      ?>  ?=(%writ +<.syn)
      |=  wir=(^pole knot)
      =/  paxs=(^pole pith:neo)
        (de:drive:neo (pave:neo wir))
      ?>  ?=([src=* hand=* ~] paxs)
      =/  src=pith  src.paxs
      =/  hand=pith  hand.paxs
      ?~  cas=(~(get by clay-peers.unix) [src hand])
        run
      =/  [case=@ud =peer:clay:neo]  u.cas
      =.  case  +(case)
      ?~  p.syn
        ~&  empty-clay-res/wir
        run
      =+  !<(kids=(list path) q.r.u.p.syn)
      ~&   syn 
      =/  res=(axal cage)
        %-  ~(gas of *(axal cage))
        %+  turn  kids
        |=  kid=path
        ^-  [path cage]
        :: =?  kid   ?=(^ as.peer)
          ::(snoc (snip kid) u.as.peer)
        :-  kid
        ~&  trying/kid
        :-  (fall as.peer (rear kid))
        %.  .^(vase %cr (welp /(scot %p our.bowl)/[r.p.u.p.syn]/(scot %da now.bowl) kid))
        ^-  $-(vase vase)
        ?~  as.peer  |=(=vase vase)
        .^(tube:clay %cc (welp /(scot %p our.bowl)/[r.p.u.p.syn]/(scot %da now.bowl) /(rear kid)/[u.as.peer]))
      =.  res  (~(dip of res) path.peer)
      =/  =note:neo  [%poke %clay-res !>(`res:clay:neo`[hand case res])]
      =/  =move:neo  [[p/our.bowl #/$/clay] src note]
      =/  =wire      (welp /sys/clay/res wir)
      =.  clay-peers.unix  (~(put by clay-peers.unix) [src hand] [case peer])
      =.  run  (poke-our wire neo-move+!>(move))
      (clay-peer src hand)
    ::
    ++  behn-wait
      |=  =@da
      ?>  ?=(%wake +<.syn)
      =/  requested  ~(tap in (~(get ju timers.unix) da))
      =/  =wire  /sys/behn/res/(scot %da da) 
      =/  =note:neo  [%poke %behn-res !>([%wake ~])]
      |-  ^+  run
      ?~  requested
        run
      =/  =move:neo   [[p/our.bowl #/$/behn] i.requested note]
      =.  run  (poke-our wire neo-move+!>(move))
      $(requested t.requested)
    --
  ++  take-agent
    |=  [=(pole knot) =sign:agent:gall]
    ^+  run
    ?>  ?=(%poke-ack -.sign)
    ?:  ?=([%eyre %req eyre-id=@ ~] pole)
      ?^  p.sign
        (~(err srv eyre-id.pole) u.p.sign)
      ~(finish-post srv eyre-id.pole)
    %.  run
    ?~  p.sign  same
    (slog u.p.sign)
  --
++  all-grab
  |=  grab=stud:neo
  ^-  vase :: of $-([to=stud grab-type] vase)
  =/  in=vase  (need ~(get pro grab))
  =/  fiesta=vase  !>(fiesta)
  %+  slap
    %+  with-faces:ford:neo  get-reef
    :~  in/in
        fiesta/fiesta
        grow/!>(grow)
        apex/!>(apex)
        con/!>(con)
        grab/!>(grab)
    ==
  !,  *hoon
  |=  [to=stud:neo in=in]
  ^-  vase
  =/  =stud:neo  
    ~|  missing-con/[grab to]
    (~(got by con.fiesta) [grab to])
  =/  conv  ~(do con stud)
  (slym run:conv in)
::  
++  all-grow
  |=  grow=stud:neo
  ^-  vase :: of $-(pail grow-type)
  =/  out=vase  (need ~(get pro grow))
  =/  fiesta=vase  !>(fiesta)
  %+  slap
    %+  with-faces:ford:neo  get-reef
    :~  out/out
        fiesta/fiesta
        grow/!>(grow)
        apex/!>(apex)
        con/!>(con)
    ==
  !,  *hoon
  |=  =pail:neo
  ^-  out
  ~!  p.pail
  ~!  grow
  =/  =stud:neo  
    ~|  missing-con/[p.pail grow]
    (~(got by con.fiesta) [p.pail grow])
  =/  conv  ~(do con stud)
  !<(out (slam run:conv q.pail))
::  
++  juice
  |_  =pulp:neo
  ++  cane
    |=  =cane:neo
    ^-  cage
    ?-    pulp
        %noun
      neo-wand+!>((cane:soften cane))
        %json
      json+!>((cane:enjs:neo cane !<($-(pail:neo json) (all-grow %json))))
    ==
  --
::
++  watch
  |=  =(pole knot)
  ^+  run
  ?+    pole  ~|(bad-path/pole !!)
      [%http-response *]  run
  ::
      [%sync rest=*]
    ?+    rest.pole  !!
        [%init as=@ car=@ ship=@p path=*]
      ?>  =(our.bowl (slav %p ship.rest.pole))
      =+  ;;(=pulp:neo as.rest.pole)
      =+  ;;(=care:neo car.rest.pole)
      =/  =tone:neo  [%peer pulp pole]
      =/  =pith:neo  (pave path.rest.pole)
      =.  sound  (~(put ju sound) [care pith] tone)
      =/  =cane:neo  (make-cane:neo care [p/our.bowl pith] (dip:of-top pith))
      (fact ~ (~(cane juice pulp) cane))
    ==
  ==
++  do-out
  |=  =out:neo
  ^+  run
  =;  new=_run
    =.  run  new
    run
  ?-  -.out
    %sync  (grab-tour p.out)
    %stop  (drop-tour p.out)
  ==
++  take-agent
  |=  [=(pole knot) =sign:agent:gall]
  ~&  pole/pole
  |^  ^+  run
  ?+  pole  ~|(on-agent-bad-wire/pole !!)
    [%sys rest=*]   (take-agent:sys rest.pole sign)
    [%test ~]       test-wire 
    [%sync %init as=@ care=@ rest=*]  (sync care.pole (pave rest.pole))
    [%forward rest=*]  (forward (pave rest.pole))
    [%local rest=*]    (take-local-agent (pave rest.pole) sign)
    [%nack ~]          take-nack
  ==
  ++  take-nack
    ?>  ?=(%poke-ack -.sign)
    %.  run
    ?~  p.sign  same
    (slog leaf/"failed poke" u.p.sign)
  ++  test-wire
    ?.  ?=(%poke-ack -.sign)
      !!
    %.  run
    ?~  p.sign
      same
    (slog leaf/"nack on test wire" u.p.sign)

  ++  forward
    |=  =pith
    ?.  ?=(%poke-ack -.sign)
      ~|(weird-forward-sign/-.sign !!)
    %.  run
    ?~  p.sign
      same
    (slog leaf/"failed forward poke {(spud (pout pith))}" u.p.sign)
  ::
  ++  sync
    |=  [=knot =pith]
    =+  ;;(=care:neo knot)
    ?+    -.sign  ~|(weird-sync-sign/-.sign !!)
        %watch-ack
      %.  run
      ?~  p.sign  same
      %+  slog
        leaf/"Failed sync from {(spud (pout pith))}" 
      u.p.sign
    ::  TODO: security vuln, confused deputy
        %fact
      ?+  p.cage.sign  !!
        %neo-wand  (~(cane hear [care pith]) (wand:harden !<(wand:neo q.cage.sign)))
        %neo-twig  (~(stem hear [care pith]) (twig:harden !<(twig:neo q.cage.sign)))
      ==
    ::
        %kick
      ~&  'TODO: resub logic'
      run
    ==
  ++  hear-wand
    |=  [=care:neo =pith:neo =wand:neo]
    ^+  run
    =/  =cane:neo  (wand:harden wand)
    =/  =riot:neo  [cane ~ *slip:neo]
    =.  foreign  (~(put by foreign) [care pith] riot)
    (resolved:stop care pith)
  ++  hear-twig
    |=  [=care:neo =pith:neo =twig:neo]
    =/  rot=(unit riot:neo)  (~(get by foreign) [care pith])
    ?~  rot
      ~&  heard-twig-no-cane/[care pith]
      run
    =/  =stem:neo  (twig:harden twig)
    =.  cane.u.rot  (beat-cane cane.u.rot stem)
    =.  foreign  (~(put by foreign) [care pith] u.rot)
    run
  --
++  is-our
  |=  pax=pith:neo
  ?>  ?=([[%p @] *] pax)
  =(our.bowl +.i.pax)
++  shout
  |=  stems=(map tour:neo stem:neo)
  ^+  run
  =/  stems  ~(tap by stems)
  |-
  =*  loop-stems  $
  ?~  stems
    run
  =/  [=tour:neo =stem:neo]
    i.stems
  =/  tones  ~(tap in (~(get ju sound) tour))
  |-  
  =*  loop-tones  $
  ?~  tones
    loop-stems(stems t.stems)
  =/  =tone:neo  i.tones
  =.  run  (yell tone tour stem)
  loop-tones(tones t.tones)

++  yell
  |=  [=tone:neo =tour:neo =stem:neo]
  |^  ^+  run
  ?-  -.tone
    %peer  (peer +.tone)
    %rely  (rely +.tone)
  ==
  ++  peer
    |=  =peer:neo
    ?>  =(pulp.peer %noun)
    (fact ~[path.peer] %neo-twig !>((stem:soften stem)))
  ++  rely
    |=  [=term pax=pith:neo]
    =/  =rely:neo  [term stem]
    (poke-rely pith.tour pax rely)
  --
::
++  dial
  =|  stems=(map tour:neo stem:neo)
  |=  changes=(map pith:neo mode:neo)
  =/  changes  ~(tap by changes)
  |-  ^+  run
  =*  loop-changes  $
  ?~  changes
    (shout stems)
  =/  [pax=pith:neo =mode:neo]
    i.changes
  =/  tours=(list tour:neo)
    ~(tap in ~(tours yelp pax))
  |-  =*  loop-tours  $
  ?~  tours
    loop-changes(changes t.changes)
  =/  =tour:neo  i.tours
  =/  =stem:neo  (~(gut by stems) tour (make-stem care.tour pax))
  ?:  =(pith.tour pax)
    $(stems (~(put by stems) tour stem), tours t.tours)
  ?.  ?=(?(%y %z) -.q.stem)
    $(stems (~(put by stems) tour stem), tours t.tours)
  =.  kids.q.stem
    =/  sfix=pith:neo  (slag (lent pith.tour) pax)
    =/  =room:neo  (got:of-top pax)
    =/  =pail:neo  (to-pail:room:neo room)
    =/  =ever:neo  (get-ever:room:neo room)
    (~(put by kids.q.stem) sfix [ever mode pail])
  $(stems (~(put by stems) tour stem), tours t.tours)
::
++  wash
  |_  =dish:neo
  ++  is-our
    ?>  ?=([[%p @] *] p.dish)
    =(our.bowl +.i.p.dish)
  ++  local
    ?>  ?=([[%p @] *] p.dish)
    t.p.dish
  ++  peer  
    ?.  is-our
      !! :: XX: revive
    :: =/  tones  ~(tones yelp local)
    ~
  --
    
++  yelp
  |_  =pith:neo
  ++  tours
     (~(uni in (~(uni in check-x) check-y)) check-z)
  ++  check-x
    ^-  (set tour:neo)
    ?:  =(~ (~(get ju sound) [%x pith]))
      ~
    (~(gas in *(set tour:neo)) [%x pith] ~)
  ++  check-y
    ^-  (set tour:neo)
    %-  ~(gas in *(set tour:neo)) 
    ;:  welp
      *(list tour:neo) 
      ?~  par-pith=(parent:of-top pith)
        ~
      =/  parent=room:neo  (got:of-top u.par-pith)
      ?:  =(~ (~(get ju sound) [%y u.par-pith]))
        ~
      [%y u.par-pith]^~
    ::
      ?:  =(~ (~(get ju sound) [%y pith]))
        ~
      [%y pith]^~
    ==

  ++  check-z
    ^-  (set tour:neo)
    %+  roll  ~(tap in (anc:of-top pith))
    |=  [pax=pith:neo out=(set tour:neo)]
    %-  ~(gas in out)
    ?:  =(~ (~(get ju sound) [%z pax]))
      ~
    [%z pax]^~
  --
++  bash-cane
  |=  [then=cane:neo now=cane:neo]
  |^  ^-  stem:neo
  ?>  =(care.then care.now)
  :-  ever.now
  ?-    care.now
    %x  [%x pail.now]
    %y  [%y pail.now get-kids]
    %z  [%z pail.now get-kids]
  ==
  ++  get-kids
    %-  ~(gas by *(map pith [ever:neo mode:neo pail:neo]))
    ^-  (list [pith:neo ever:neo mode:neo pail:neo])
    %-  zing
    :~  ^-  (list [pith:neo ever:neo mode:neo pail:neo])
        %+  turn  ~(tap by get-add)
        |=   [pa=pith:neo e=ever:neo p=pail:neo]
        [pa e %add p]
      ::
        ^-  (list [pith:neo ever:neo mode:neo pail:neo])
        %+  turn  ~(tap by get-dif)
        |=   [pa=pith:neo e=ever:neo p=pail:neo]
        [pa e %dif p]
      ::
        ^-  (list [pith:neo ever:neo mode:neo pail:neo])
        %+  turn  ~(tap by get-dif)
        |=   [pa=pith:neo e=ever:neo p=pail:neo]
        [pa e %del p]
    ==
  ++  get-add
    (~(dif by kids.now) kids.then)
  ++  get-dif
    (~(int by kids.now) kids.then)
  ++  get-del
    (~(dif by kids.then) kids.now)
  --
::
++  beat-cane
  |=  [=cane:neo =stem:neo]
  ^+  cane
  ?>  =(care.cane -.q.stem)
  :*  care.cane
      p.stem
    ::
      ?-    -.q.stem
          %x  [pail.q.stem ~]
      ::
          %y
        :-  pail.q.stem 
        %-  ~(gas by kids.cane)
        %+  turn  ~(tap by kids.q.stem)
        |=  [p=pith:neo e=ever:neo m=mode:neo pa=pail:neo]
        ^-  [pith:neo ever:neo pail:neo]
        [p e pa]
      ::
          %z
        :-  pail.q.stem 
        %-  ~(gas by kids.cane)
        %+  turn  ~(tap by kids.q.stem)
        |=  [p=pith:neo e=ever:neo m=mode:neo pa=pail:neo]
        ^-  [pith:neo ever:neo pail:neo]
        [p e pa]
      ==
  ==
++  stop
  |%
  ++  fresh
    |=  [block=(set tour:neo) =move:neo]
    =/  =flow:neo  [p p.q]:move
    ~&  fresh-stop/flow
    ?.  =(~ (~(get by clog.halt) flow))
      ~|  trying-to-block-on-congested-flow/flow
      !!
    =/  q=(qeu move:neo)  (~(put to *(qeu move:neo)) move)
    =.  clog.halt  (~(put by clog.halt) flow q)
    =/  block=(list tour:neo)  ~(tap in block)
    |-  ^+  run
    ?~  block
      run
    =/  =tour:neo  i.block
    =.  by-tour.halt   (~(put by by-tour.halt) tour flow)
    =.  by-flow.halt   (~(put ju by-flow.halt) flow tour)
    =.  run  (grab-tour tour)
    $(block t.block)
  ++  is-congested
    |=  =move:neo
    =/  =flow:neo  [p p.q]:move
    (~(has by clog.halt) flow)
  ::
  ++  add
    |=  =move:neo
    =/  =flow:neo  [p p.q]:move
    =/  q
      ~|  adding-to-empty-clog/flow
      (~(got by clog.halt) flow)
    =.  q  (~(put to q) move)
    =.  clog.halt  (~(put by clog.halt) flow q)
    run
  ++  resolved
    |=  =tour:neo
    =/  fow=(unit flow:neo)  (~(get by by-tour.halt) tour)
    ?~  fow
      run
    =.  by-tour.halt    (~(del by by-tour.halt) tour)
    =.  by-flow.halt    (~(del ju by-flow.halt) u.fow tour)
    =/  tours=(set tour:neo)
      (~(get ju by-flow.halt) u.fow)
    ?.  =(~ tours)
      run
    =/  q  (~(got by clog.halt) u.fow)
    |-
    ?:  =(~ q)  
      =.  clog.halt  (~(del by clog.halt) u.fow)
      run
    =^  nex=move:neo  q  ~(get to q)
    =.  run  (poke-move nex)
    $
  --
::
++  harden
  |%
  ++  raw-poke
    |=  raw=raw-poke:neo
    ^-  move:neo
    [p.p.raw q.p.raw %poke (vial q.raw)]
  ++  vial
    |=  =vial:neo
    :-  p.vial
    (slym (need ~(get pro p.vial)) q.vial)
  ++  wand
    |=  w=wand:neo
    ^-  cane:neo
    :^  care.w  ever.w  (vial vial.w)
    (~(run by kids.w) |=([e=ever:neo v=vial:neo] [e (vial v)]))
  ::
  ++  twig
    |=  =twig:neo
    ^-  stem:neo
    :-  p.twig 
    ?-    -.q.twig
        %x  [%x (vial vial.q.twig)]
        %y
      :+  %y  (vial vial.q.twig)
      (~(run by kids.q.twig) |=([e=ever:neo m=mode:neo v=vial:neo] [e m (vial v)]))
    ::
        %z
      :+  %z  (vial vial.q.twig)
      %-  ~(run by kids.q.twig)
      |=([e=ever:neo m=mode:neo v=vial:neo] [e m (vial v)])
    ==
  --
++  soften
  |%
  ++  pail
    |=  pal=pail:neo
    ^-  vial:neo
    [p.pal q.q.pal]
  ++  cane
    |=  can=cane:neo
    ^-  wand:neo
    :^  care.can  ever.can  (pail pail.can)
    (~(run by kids.can) |=([e=ever:neo pal=pail:neo] [e (pail pal)]))
  ++  stem
    |=  sem=stem:neo
    ^-  twig:neo
    :-  p.sem
    ?-  -.q.sem
      %x  [%x (pail pail.q.sem)]
      %y  [%y (pail pail.q.sem) (~(run by kids.q.sem) |=([e=ever:neo m=mode:neo p=pail:neo] [e m (pail p)]))]
      %z  [%z (pail pail.q.sem) (~(run by kids.q.sem) |=([e=ever:neo m=mode:neo p=pail:neo] [e m (pail p)]))]
    ==
  --
::
++  con
  |_  =stud:neo
  ++  do  
    =+  !<([vix=(unit vase) *] q:(to-pail:room:neo (got:of-top pith)))
    =/  vax  (need vix)
    |%
    ++  grab  !<(stud:neo (slot 4 vax))
    ++  grow  !<(stud:neo (slot 5 vax))
    ++  run   (slot 3 vax)
    ++  register
      ^+  fiesta
      %_  fiesta
        by-grab  (~(put ju by-grab.fiesta) grab grow)
        by-grow  (~(put ju by-grow.fiesta) grow grab)
        con      (~(put by con.fiesta) [grab grow] stud)
      ==

    ++  vale
      ^-  ?
      =;  rap=(trap ?)
        =/  res  (mule rap)
        ?:  ?=(%& -.res)
          p.res
        %-  (slog leaf/"mark-vale" p.res)
        |
      |.  ^-  ?
      =/  src=vase  (need ~(get pro grab))
      =/  dst=vase  (need ~(get pro grow))
      =/  need=type  
        =<  p
        %+  slap  (with-faces:ford:neo get-reef src/src dst/dst ~)
        !,(*hoon *$-(src dst))
      =/  have=type  -:(slot 3 vax)
      (~(nest ut need) & have)
    --

  ++  pith
    `pith:neo`(pave:neo path)
  ++  path
    ^-  ^path
    :-  %out
    ?@  stud
      /std/con/[stud]
    ?:  =(our.bowl ship.stud)
      /our/[desk.stud]/con/[mark.stud]
    :+  %ext  (scot %p ship.stud)
    /[desk.stud]/con/[mark.stud]
  --
++  get-disks
  ^-  (set disk:neo)
  =/  res  (~(put in *(set disk:neo)) ~)
  res :: XX: fix
++  floppy
  |_  =disk:neo
  ++  pith
    ^-  pith:neo
    :-  %src
    ?@  disk
      #/std
    ?:  =(our.bowl ship.disk)
      [%our term.disk ~]
    [%ext p/ship.disk term.disk ~]
  ++  pro
    ^-  (set stud:neo)
    =/  fat  (dip:of-top (snoc pith %pro))
    %-  ~(gas in *(set stud:neo))
    %+  murn  ~(tap by kid.fat)
    |=  [=iota =(axal:neo room:neo)]
    ^-  (unit stud:neo)
    ?.  ?=(@ iota)
      ~&  weird-pro/[disk iota]
      ~
    ?@  disk
      `iota
    `[iota disk]
  --
++  omen
  |_  pax=pith:neo
  ++  pol  `(pole iota)`pax
  ++  eject
    ^-  [(unit disk:neo) _pax]
    ?.  ?=([@ *] pax)
      `pax
    =/  pol  `(pole iota)`t.pax
    ?+  pol   `pax
      [%std rest=*]                     [`~ rest.pol]
      [%our desk=@ rest=*]              [`[our.bowl desk.pol] rest.pol]
      [%ext [%p ship=@] desk=@ rest=*]  [`[ship.pol desk.pol] rest.pol]
    ==
  ++  post
    ^-  (unit post:neo)
    =^  dis=(unit disk:neo)  pax  eject
    ?~  dis  ~
    ?.  ?=([tack:neo @ ~] pax)
      ~
    =/  =stud:neo  ?~(u.dis i.t.pax [i.t.pax u.dis])
    `[i.pax stud]
  --
::
++  pro
  |_  =stud:neo
  ++  get  grab
  ++  grab
    =/  rom  (got:of-top pith)
    =+  !<([cac=(unit vase) *] q:(to-pail:room:neo rom))
    cac
  ++  built
    (has:of-top pith)
  ++  pith
    `pith:neo`(pave:neo path)
  ++  path
    ^-  ^path
    :-  %out
    ?@  stud
      /std/pro/[stud]
    ?:  =(our.bowl ship.stud)
      /our/[desk.stud]/pro/[mark.stud]
    :+  %ext  (scot %p ship.stud)
    /[desk.stud]/pro/[mark.stud]
  ++  exists
    =/  pax  path
    ?>  ?=(^ pax)
    (exists-file %src t.pax)
  --
++  root
  /(scot %p our.bowl)/[q.byk.bowl]/(scot %da now.bowl)/neo
++  exists-file
  |=  pax=path
  =/  p=path
    (welp root pax)
  =.  p  (snoc p %hoon)
  .^(? %cu p)
++  get-reef
  =+  !<([ref=(unit vase) *] q:(to-pail:room:neo (got:of-top #/out/reef)))
  (need ref)
::
++  copy-clay
  |=  pat=(unit path)
  ~>  %bout.[1 %build]
  |^  ^+  run
  =/  paths=(list path)
    .^((list path) %ct root)
::  =.  paths
::    %+  sort  paths
::    |=  [a=path b=path]
::    =-  ~&  sorting/[a b -]  -
::    ?>  ?=([@ @ *] a)
::    ?>  ?=([@ @ *] b)
::    =/  fall  (lte i.t.a i.t.b)
::    =/  a  (pave:neo t.a)
::    =/  b  (pave:neo t.b)
::    =/  a-pos  ~(post omen a)
::    =/  b-pos  ~(post omen b)
::    ?~  a-pos
::      ?~  b-pos  fall
::      %.n
::    ?~  b-pos
::      %.y
::    ?:  =(%imp p.u.a-pos)
::      ?:  =(%imp p.u.b-pos)
::        (aor (get-stud-name:neo q.u.a-pos) (get-stud-name:neo q.u.b-pos))
::      %.n
::    ?:  =(%imp p.u.b-pos)
::      %.y
::    fall
::  ~&  paths/path
  =.  paths
    %+  turn  paths
    |=  pax=path
    =.  pax  (snip pax)
    ?>  ?=(^ pax)
    t.pax
  =/  is-imp 
    |=  pax=path
    ^-  ?
    =/  pix  (pave:neo pax)
    ?~  pos=~(post omen pix)
      |
    =(%imp u.pos)
  =/  is-ford 
    |=  pax=path
    ^-  ?
    =/  pix  (pave:neo pax)
    ?~  pos=~(post omen pix)
      |
    ?.  ?=(@ q.u.pos)
      |
    =(%ford (end 3^4 q.u.pos))
  =^  main=(list path)  paths
    ::  [(skip paths is-ford) (skim paths is-ford)]
    [(skip paths is-ford) ~]
  =^  imps=(list path)  main
    [(skim main is-imp) (skip main is-imp)]
  |-  ^+  run  =*  loop  $
  ?~  paths
    ?^  main
      loop(paths main, main ~)
    ?^  imps
      loop(imps ~, paths imps)
    run(adult &)
  =^  pat=(unit path)  run
    (read-file i.paths)
  ?~  pat
    loop(paths t.paths)
  loop(paths (snoc t.paths u.pat))
  ++  build-pipe
    =/  disks  ~(tap in get-disks)
    |-  ^+  run
    =*  loop-disk  $
    ?~  disks
      run
    =/  pros  ~(pro floppy i.disks)
    |-  
    =*  loop-pro  $
    ?~  pros
      loop-disk(disks t.disks)
    !!
    :: vale -> [grow
  ::
  ++  lib
    |_  =loc:ford:neo
    ++  path
      ^-  ^path
      %-  welp
      :_  [%lib (pout pith.loc)]
      ^-  ^path
      ?@  disk.loc
        /std
      ?:  =(ship.disk.loc our.bowl)
        /our/[term.disk.loc]
      /her/(scot %p ship.disk.loc)/[term.disk.loc]
    ++  pith
      (pave:neo path)
    ++  built
      (has:of-top %out pith)
    ++  exists
      (exists-file %src path)
    --
  ++  do-make
    |=  [=pith:neo lib=term sta=(unit vase) =conf:neo]
    =/  =name:neo  [our.bowl pith]
    ~|  conf/conf
    ~|  make-name/name
    =.  run  
      (on-card (en-pith:name:neo name) %make lib sta conf)
    ?:  |(=(lib %hoon) =(lib %term))
      run
    ~|  ~(key by ~(tar of:neo apex))
    =/  rom  (got:of-top pith.name)
    =+  !<([cache=(unit vase) *] q:(to-pail:room:neo rom))
    ?~  cache
      ~&  failed-to-build/pith
      run
      :: :: !!
    ?~  pos=~(post omen pith)
      run
    ?.  ?=(%con p.u.pos)
      run
    =*  do-con  ~(do con q.u.pos)
    ?.  vale:do-con
      run
    =.  fiesta  register:do-con
    run
  ++  ford-slap
    |=  [wer=pith sut=pith src=pith]
    %^  do-make  wer  %ford-slap
    `(~(gas by *conf:neo) sut/(ours sut) hoon/(ours src) ~)
  ::
  ++  slop
    |=  [wer=pith a=pith b=pith]
    ~|  %ford-slop
    %^  do-make  wer  %ford-slop
    `(~(gas by *conf:neo) a/(ours a) b/(ours b) ~)
  ++  face
    |=  [wer=pith face=pith sut=pith]
    ~|  %ford-face
    %^  do-make  wer  %ford-face
    `(~(gas by *conf:neo) face/(ours face) sut/(ours sut) ~)
  ++  same
    |=  [wer=pith from=pith]
    ~|  ford-same/[wer from]
    %^  do-make  wer  %ford-same
    `(~(gas by *conf:neo) src/(ours from) ~)
  ++  ours
    |=  p=pith:neo  `pith:neo`[p/our.bowl p]
  ++  make-deps
    =|  idx=@ud
    |=  [pat=pith deps=(list [face=term =pith])]
    ^+  run
    ?~  deps
      ~|  pat
      %+  same  pat
      ?:  =(0 idx)
        #/out/reef
      (snoc pat ud/(dec idx))
    =/  wer=pith  (snoc pat ud/idx)
    =/  fac=pith  (snoc wer %face)
    =/  fav=pith  (snoc fac %term)
    =.  run
      (do-make fav %term `!>(face.i.deps) ~)
    =.  run
      (face fac fav pith.i.deps)
    =/  prev=pith
      ?:  =(idx 0)
        #/out/reef
      (snoc pat ud/(dec idx))
    =.  run
      (slop wer fac prev)
    $(deps t.deps, idx +(idx))
  ++  file-to-deps
    |=  =file:ford
    ^-  (list [term pith])
    %+  welp
      (turn pro.file |=(p=pro:ford [face.p ~(pith pro stud.p)]))
    (turn lib.file |=(l=lib:ford [face.l %out ~(pith lib loc.l)]))
  ++  make-prelude
    |=  [pax=pith =file:ford]
    ^-  [pith _run]
    =/  pre-path=pith  [%pre pax]
    [pre-path (make-deps pre-path (file-to-deps file))]
  ++  write-hoon
    |=  [pax=pith fil=@t]
    (do-make pax %hoon `!>(fil) ~)
  ++  src-to-out
    |=  pax=pith:neo
    ^-  pith:neo
    ?>  ?=([@ *] pax)
    [%out t.pax]
  ++  has-modified
    |=  [txt=@t pax=pith:neo]
    ?.  adult
      &
    ?~  rom=(get:of-top pax)
      &
    !=(txt q.q:(to-pail:room:neo u.rom))
  ++  read-file
    |=  pax=path
    ^-  [(unit path) _run]
    ?:  =((rear pax) %ford-parser)
      `run
    =+  .^(src=@t %cx `path`(snoc `path`(welp root pax) %hoon))
    ?.  (has-modified src (pave:neo pax))
      `run
    ~?  >>>  adult
      [%update pax]
    =/  =file:ford
      ~|  parsing/pax
      (scan (trip src) (rein:ford [our.bowl (pave:neo pax)]))
    =/  has-imports=?
      ?&  (levy pro.file |=(pro:ford ~(exists pro stud)))
          (levy lib.file |=(lib:ford ~(exists lib loc)))
      ==
    ?.  has-imports
      ~|  pro.file
      ~|  lib.file
      ~|  %no-imports
      !!
    =/  built-imports=?
      ?&  (levy pro.file |=(pro:ford ~(built pro stud)))
          (levy lib.file |=(lib:ford ~(built lib loc)))
      ==
    ?.  built-imports
      [`pax run]
    =^  pre=pith  run  
      (make-prelude pax file)
    =/  =conf:neo
      (~(gas by *conf:neo) [%sut (ours pre)] ~)
    =.  run  (write-hoon pax src)
    =/  pit  (src-to-out pax)
    :-  ~
    (ford-slap (src-to-out pax) pre pax)
  --
++  get-tour
  |=  =tour:neo
  ^-  (unit cane:neo)
  =/  =name:neo  (de-pith:name:neo pith.tour)
  (mole |.((~(cane dorm name) care.tour)))
::
++  acquire
  |=  =(pole iota:neo)
  ^-  (unit pail:neo)
  ?>  ?=([[%p ship=@] rest=*] pole)
  ?:  =(our.bowl ship.pole)
    ?~  val=(get:of-top rest.pole)
      ~
    `(to-pail:room:neo u.val)
  ?~  val=(~(get by foreign) [%x pole])
    ~
  `pail.cane.u.val
::  XX: check typing
++  can-inject
  |=  [=pith:neo =quay:neo]
  !=(~ (acquire pith))

++  get-val-at-path
  |=  =pith
  ^-  (unit vase)
  ?~  val=(get:of-top pith)
    ~
  ?.  ?=(%icon -.seat.u.val)
    ~
  `state.icon.seat.u.val
++  get-pail-local
  |=  =pith
  ^-  (unit pail:neo)
  ?~  val=(get:of-top pith)
    ~
  `(to-pail:room:neo u.val)

++  get-vial-local
  |=  =pith
  ^-  (unit vial:neo)
  ?~  val=(get:of-top pith)
    ~
  `(to-vial:room:neo u.val)

::  XX: invert and check typing
++  check-conf
  |=  [=conf:neo =deps:neo]
  ^-  [bad=(set term) block=(set tour:neo)]
  %+  roll  ~(tap by deps)
  |=  [[=term required=? =quay:neo] bad=(set term) block=(set tour:neo)]
  =/  =care:neo  (get-care:quay:neo quay)
  ?:  &(required !(~(has by conf) term))
    :_(block (~(put in bad) term))
  ?:  &(!required !(~(has by conf) term))
    [bad block]
  =/  pit=pith:neo   (~(got by conf) term)
  ?~  can=(get-tour care pit)
    [bad (~(put in block) care pit)]
  [bad block]
::
  
++  hear-watch
  |=  [=care:neo =pith:neo =watch:neo]
  ^+  run
  ?~  watch
    run
  :: =.  run  
    :: abet:(~(hear xeno [src.bowl pith.i.watch]) +.i.watch)
  $(watch t.watch)
++  husk
  |_  =stud:neo
  ++  pith
    ^-  pith:neo
    :-  %out
    %-  pave:neo
    ?@  stud 
      /std/imp/[stud]
    ?:  =(our.bowl ship.stud)
      /our/[desk.stud]/imp/[mark.stud]
    /her/(scot %p ship.stud)/[desk.stud]/imp/[mark.stud]
  ++  vase
    ^-  ^vase
    ~|  firm/pith
    =/  rom=room:neo  (got:of-top pith)
    ?>  ?=(%icon -.seat.rom)
    =+  !<([cac=(unit ^vase) *] state.icon.seat.rom)
    (need cac)
  ++  firm
    ^-  firm:neo
    ?.  is-plot
      !<(firm:neo vase)
    (till:neo (need plot))
  ++  is-plot
    (~(nest ut -:!>(*plot:neo)) | p:vase)
  ++  plot
    ^-  (unit plot:neo)
    ?.  is-plot
      ~
    `!<(plot:neo vase)
  ++  wire
    %+  welp  /husk/stud
    (pout pith)
  --
::    ?>  ?=(@ stud)
::    =/  =riff:clay
::      [q.byk.bowl `[%sing %a da/now.bowl spur]]
::    =/  wir  (snoc wire %build)
::    (emit %pass wir %arvo %c %warp our.bowl riff)
::  ++  take
::    |=  [=(pole knot) syn=sign-arvo]
::    ^+  run
::    ?>  ?=([%build ~] pole)
::    ?>  ?=([%clay %writ *] syn)
::    ?~  p.syn
::      ~&  bad-take-husk/pole
::      =.  husks  (~(del in husks) stud)
::      run
::    ?:  =(~ (~(get ju husks) stud))
::      run
::    watch
::
++  hear
  |_  =tour:neo
  ++  slip
    |=  s=slip:neo
    ^+  run
    =/  =riot:neo  (~(gut by foreign) tour *riot:neo)
    =.  slip.riot  s
    =.  foreign  (~(put by foreign) tour riot)
    run
  ++  cane
    |=  =cane:neo
    =/  =riot:neo  (~(gut by foreign) tour %*(. *riot:neo care.cane care.cane))
    =/  =stem:neo  (bash-cane cane.riot cane)
    =.  cane.riot  cane
    =.  foreign  (~(put by foreign) tour riot)
    (tell stem ~(tap in deps.riot))

  ++  tell
    |=  [=stem:neo deps=(list rave:neo)]
    ~&  stem/stem
    ?~  deps
      (resolved:stop tour)
    =/  =rely:neo  [term.i.deps stem]
    =.  run  (poke-rely-xeno pith.tour pith.i.deps rely)
    $(deps t.deps)
  ::
  ++  stem
    |=  =stem:neo
    =/  =riot:neo  
      ~|  hear-stem-no-riot/tour
      (~(got by foreign) tour)
    =.  cane.riot  (beat-cane cane.riot stem)
    =.  foreign    (~(put by foreign) tour riot)
    (tell stem ~(tap in deps.riot))
  --
::
++  xeno
  |_  =tour:neo
  ++  ship   `@p`?>(?=([%p @] pith.tour) +.i.pith.tour)
  ++  here   ?>(?=([%p @] pith.tour) t.pith.tour)
  ++  brig  (fall (~(get by fleet) ship) *brig:neo)
  ++  got   (~(got of:neo brig) here)
  ++  get   (~(get of:neo brig) here)
  ++  put
    |=  =cane:neo
    =.  fleet
      %+   ~(put by fleet)  ship
      (~(put of:neo brig) here cane)
    run
  ++  wire  `^wire`(welp /xeno/[care.tour] (pout pith.tour))
  ++  dock  `^dock`[ship dap.bowl]
  ++  apply
    |=  =stem:neo
    (put (beat-cane got stem))
  ::
  ++  watch
    =/  =path  [%sync %init (pout pith.tour)]
    =.  run
      (emit %pass wire %agent dock %watch path)
    run
  ++  hear
    |=  [=stud:neo case=@ud =pith =diff:neo]
    ^+  run
    ~|  hear-name/tour
    run
    ::=/  firm  ~(firm-vase husk p.span.cel)
    :: =|  =vase
    ::kk(put cel(case case, state vase))
  --
::
++  take-local-agent
  |=  [=pith =sign:agent:gall]
  ^+  run
  ?+    -.sign  !!
      %poke-ack 
    %.  run
    ?~  p.sign
      same
    %+  slog
      leaf/"Poke-ack failed for shrub {(en-tape:pith:neo pith)}" 
    u.p.sign
  ::
  ==
::
++  give-nack
  |=  [src=name:neo dst=name:neo err=tang]
  ^+  run
  ?:  =(src our-sys-name)
    %-  (slog leaf/"nack on sys" err)
    run
  :: TODO: revisit ordering semantics 
  =/  =wire  /nack
  ::  XX: handle remote case
  %^  poke-neo  wire  ship.src
  :-  %neo-move
  !>  ^-  move:neo
  :+  (en-pith:name:neo src)
    (en-pith:name:neo dst)
  [%poke %ack !>([%err err])]
++  serving-tours
  ^-  (set [pulp:neo tour:neo])
  %-  ~(gas in *(set [pulp:neo tour:neo]))
  %+  murn  ~(tap by sup.bowl)
  |=  [=duct =ship =(pole knot)]
  ^-  (unit [pulp:neo tour:neo])
  ?.  ?=([%sync as=@ %init car=@ ship=@ rest=*] pole)
    ~
  =+  ;;(=pulp:neo as.pole)
  =+  ;;(=care:neo car.pole)
  =/  =pith:neo  (pave:neo ship.pole rest.pole)
  `[pulp care pith]
++  match-tour
  |=  [=pith:neo =tour:neo]
  ?>  ?=([[%p @] *] pith)
  ?>  ?=([[%p @] *] pith.tour)
  ?>  =(our.bowl +.i.pith)
  ^-  ?
  ?-    care.tour
      %x  =(pith pith.tour)
      %y  
    =/  par  (parent:of-top t.pith)
    |(=(pith.tour pith) =([~ pith.tour] par))
      %z
    |(=(pith.tour pith) (~(has in (anc:of-top t.pith)) t.pith.tour))
  ==
::
++  grab-twig
  |=  [=care:neo =pith:neo]
  =/  h  (hall pith)
  =/  =vial:neo  vial:h
  :-  ever:h
  ?-  care
    %x  [%x vial]
    %y  [%y vial ~]
    %z  [%z vial ~]
  ==
++  bi  by
::
++  give-facts
  =|  twigs=(map tour:neo twig:neo)
  |=  changes=(list [=pith:neo =mode:neo])
  ^+  run
  =/  change  changes
  =/  tours  ~(tap in serving-tours)
  |-  =*  loop-tour  $
  ?~  tours
    =/  twigs  ~(tap by twigs)
    |-
    ?~  twigs  run
    =/  [=tour:neo =twig:neo]  i.twigs
    =/  =pith:neo  
      (welp #/sync/init/noun tour)
    =.  run  (fact ~[(pout pith)] neo-twig+!>(twig))
    $(twigs t.twigs)
  =/  [=pulp:neo =tour:neo]  i.tours
  |-  =*  loop-change  $
  ?~  change
    :: ?~  tours  run
    loop-tour(change changes, tours t.tours)
  ?.  (match-tour pith.i.change tour)
    loop-change(change t.change)
  =/  [=pith:neo =mode:neo]  i.change
  ?>  ?=([[%p @] *] pith.tour)
  ?>  ?=([[%p @] *] pith)
  =/  =twig:neo
    (~(gut bi twigs) tour (grab-twig tour(pith t.pith.tour)))
  ?:  =(pith.tour pith)
    =.  twigs  (~(put by twigs) tour twig)
    loop-change(change t.change)
  ?<  ?=(%x -.q.twig)
  =>  .(pith.tour `pith:neo`pith.tour, pith `pith:neo`pith)
  ~&  pith/pith.tour
  ~&  kid/pith
  =/  sfix=pith:neo  (slag (lent pith.tour) pith)
  ?>  ?=([[%p @] *] pith)
  =/  h  (hall t.pith)
  =.  kids.q.twig  (~(put by kids.q.twig) sfix ever:h mode vial:h)
  =.  twigs  (~(put by twigs) tour twig)
  loop-change(change t.change)
::
++  poke-rely
  |=  [from=pith:neo to=pith:neo =rely:neo]
  ~&  rely/[from to rely]
  (poke-move [p/our.bowl from] to %poke %rely !>(rely))
::
++  poke-rely-xeno
  |=  [from=pith:neo to=pith:neo =rely:neo]
  ~&  rely-xeno/[from to rely]
  (poke-move from to %poke %rely !>(rely))
::
++  make-stem
  |=  [=care:neo =pith:neo]
  ^-  stem:neo
  =/  =cane:neo  (~(cane dorm [our.bowl pith]) care)
  :-  ever.cane
  ?-  care
    %x  [%x pail.cane]
    %y  [%y pail.cane ~]
    %z  [%z pail.cane ~]
  ==
  
++  plag
  |=  [want=stud:neo have=pail:neo]
  ^-  (unit pail:neo)
  ~|  plug/[p.have want]
  ?:  =(want %pail)
    `have
  ?:  =(want p.have)
    `have
  ?:  =(want %sig)
    `[%sig *vase]
  ?.  (~(has by con.fiesta) [p.have want])
    ~
    
  =/  conv   run:~(do con (~(got by con.fiesta) [p.have want]))
  `[want (slam conv q.have)]
::
++  scion
  |=  [want=kids:neo =pith:neo =pail:neo]
  ^-  (unit pail:neo)
  ?~  pis=(find:peon:neo pith ~(key by want))
    ~
  =/  =port:neo  (~(got by want) u.pis)
  (plag state.port pail)
::
++  moor
  |=  [want=quay:neo =name:neo]
  ^-  cane:neo
  =/  =care:neo  (get-care:quay:neo want)
  =*  d  ~(. dorm name)
  =/  =cane:neo   (cane:d care)
  =.  pail.cane  (need (plag state.p.want pail.cane))
  =?  kids.cane  ?=(^ q.want)
    %-  ~(gas by *(map pith:neo [ever:neo pail:neo]))
    %+  murn  ~(tap by kids.cane)
    |=  [=pith:neo =ever:neo =pail:neo]
    ^-  (unit [pith:neo ever:neo pail:neo])
    ?~  ion=(scion q.u.q.want pith pail)
      ~
    `[pith ever u.ion]
  cane
::
++  dep-change
  |=  [from=name:neo =term to=name:neo]
  ^+  run
  !!
::  ?>  =(our.bowl ship.to)
::  %+  poke-move  (en-pith:name:neo from)
::  :-  (en-pith:name:neo to)
::
::  !!  :: [%poke %rely !>(

::
++  send-sync
  |=  =name:neo
  ^+  run
  ?.  =(our.bowl ship.name)
    ~&  sync-for-foreign/ship.name
    run
  =/  =room:neo  (got:of-top pith.name)
  =/  =cage
    :-  %neo-twig
    !>  *update:neo
    :: ?>  ?=(%stud -.p.span.room)
    :: [pith.name p.p.span.room [case %init q.state]:icon.room]
  =;  paths=(list path)
    ?:  =(~ paths)
      run
    (give %fact paths cage)
  =/  target=path
    %+  welp  /sync/init
    (pout pith.name)
  =-  ~(tap in -)
  %-  ~(gas in *(set path))
  %+  murn  ~(val by sup.bowl)
  |=   [=ship =path]
  ?.  (is-parent-p target path)
    ~
  `path
++  grab-tour
  |=  =tour:neo
  ?>  ?=([[%p @] *] pith.tour) 
  =/  =path  (welp /sync/init/noun/[`@ta`care.tour] (pout pith.tour))
  =/  =tone:neo
    [%peer %noun path]
  (emit %pass path %agent [+.i.pith.tour %neo] %watch path)
++  drop-tour
  |=  =tour:neo
  ?>  ?=([[%p @] *] pith.tour) 
  =/  =path  (welp /sync/init/noun/[`@ta`care.tour] (pout t.pith.tour))
  (emit %pass path %agent [+.i.pith.tour %neo] %leave ~)

::
++  arvo
  =+  verb=&
  =/  old  apex
  =|  =block:neo
  ::  callstack
  =|  $:  done=(list move:neo)
          down=(list move:neo)
          up=(list move:neo)
          change=(map pith mode:neo)
          changes=(map pith mode:neo)
          gifts=(list [pith:neo gift:neo])
      ==
  |=  =move:neo
  =/  src=name:neo  (de-pith:name:neo p.move)
  =/  init=[src=name:neo dst=name:neo]
    [src (de-pith:name:neo p.q.move)]
  =/  init-move  move
  =/  src=name:neo  src.init
  =/  here  pith.dst.init
  ?>  =(our.bowl ship.dst.init)
  =<
  ?.  (is-congested:stop move)
    (apply move)
  =.  run  (add:stop move)
  arvo
  |%
  ++  abet
    ^+  run
    ?:  =([~ ~] block)  
      =.  cards  (welp cards (turn up deal))
      (dial changes)
        :: %+  turn  ~(tap by change)
        ::  |=([=pith:neo =mode:neo] ^+(+< [[p/our.bowl pith] mode]))
      :: run
    ~&  >>>  %reverting
    ~&  >>>  init
    =.  apex  old :: XX: is apex only state that is touched?
    ?.  =(~ get.block)
      (fresh:stop get.block init-move)
    ?>  ?=(^ err.block)
    %-  (slog u.err.block)
    ?:  ?=([%poke %rely *] q.q.move)
      ~&  >>>  rely-nack/[src dst]:init
      run
    (give-nack src.init dst.init u.err.block)
  ::
  ++  deal
    |=  =move:neo
    ^-  card
    :+  %pass  local/(pout p.move)
    ^-  note:agent:gall
    =/  her=ship
      ~|  p.move
      ?>  ?=([[%p @p] *] p.q.move)
      +.i.p.q.move
    ?:  =(our.bowl her)
      [%agent [her dap.bowl] %poke neo-move+!>(move)]
    =/  raw=raw-poke:neo
      ?>  ?=(%poke -.q.q.move)
      [[p p.q]:move (pail:soften pail.q.q.move)]
    ?>  ?=(%poke -.q.q.move)
    [%agent [her dap.bowl] %poke neo-raw-poke+!>(raw)]
  ++  arvo  .
  ++  emit  |=(=move:neo arvo(down [move down]))
  ++  give
    ^+  arvo
    ?~  gifts
      arvo
    =/  [=pith:neo =gift:neo]  i.gifts
    =>  .(gifts `(list [pith:neo gift:neo])`gifts)
    =.  gifts
      ?>  ?=(^ gifts)
      t.gifts
    =.  here  pith
    =/  =pail:neo
      gift/!>(gift)
    =^  cards=(list card:neo)  arvo
      (soft-site |.(si-abet:(si-poke:site pail)))
    (ingest cards)
  ++  trace-card
    |=  =move:neo
    ^-  tank
    :-  %leaf
    "{(en-tape:pith:neo p.move)} -> {(en-tape:pith:neo p.q.move)}: {<-.q.q.move>}"
  ++  trace
    |=  =tang
    ?.  verb  same
    %.  tang
    %*  .  slog
      pri  2
    ==
  ++  inside  (cury is-parent init)
  ++  echo  arvo  :: TODO walk done
  ++  finalize
    ^+  arvo
    =.  gifts
      =-  ~(tap by -)
      ^-  (map pith:neo gift:neo)
      %+  roll  ~(tap by change)
      |=  [[=pith:neo =mode:neo] out=(map pith:neo gift:neo)]
      ?~  par=(parent:of-top pith)
        out
      =/  parent  (~(gut by out) u.par *gift:neo)
      =.  parent  (~(put by parent) (sub:pith:neo pith u.par) mode)
      (~(put by out) u.par parent)
    =.  changes  (~(uni by changes) change)
    =.  change   *(map pith:neo mode:neo)
    ?~  gifts
      arvo
    give
    :: $(gifts t.gifts)
    
  ++  work
    ^+  arvo
    |-  ^+  arvo
    ?^  err.block
      arvo
    ?~  down
      finalize
    =/  nex=move:neo  i.down
    =/  new-arvo  (apply:arvo(down t.down) nex) :: XX: weird compiler?
    $(arvo new-arvo, done (snoc done nex))
  ++  poke
    |=  =pail:neo
    ^+  arvo
    =/  =room:neo  (got:of-top here)
    ?:  &(=(%rely p.pail) ~(is-plot husk code.room))
      =.  change  (~(put by change) here %dif)
      work
      
    =^  cards=(list card:neo)  arvo
      (soft-site |.(si-abet:(si-poke:site pail)))
    (ingest cards)
  ::  XX: a hack
  ::
  ::    this is implicity recursive, and all external dependencies of
  ::    the children need to be woken up. this also breaks referential 
  ::    transparency
  ++  tomb
    |=  *
    =.  apex  (del:of-top here)
    =.  change  (~(put by change) here %del)
    work
  ::
  ++  apply
    |=  =move:neo
    ^+  arvo
    ?.  =(~ err.block)
      :: skip if we have errored
      arvo
    ~|  apply/[p.move p.q.move]
    =.  src   (de-pith:name:neo p.move)
    =/  =name:neo  (de-pith:name:neo p.q.move)
    =.  here       +:p.q.move
    %-  (trace leaf/"{<-.q.q.move>} {(spud (pout here))}" ~)
    ?-  -.q.q.move
      %make  (make +.q.q:move)
      %poke  (poke +.q.q:move)
      %tomb  (tomb +.q.q:move)
      %link   !!
    ==
  ::
  ++  ingest
    |=  caz=(list card:neo)
    ^+  arvo
    =/  =pith  [p/our.bowl here]
    =.  up
      %+  welp  up
      %+  murn  caz
      |=  =card:neo
      ^-  (unit move:neo)
      ?:  (is-parent pith p.card)
        ~
      `[pith card]

    =.  down
      %-  welp
      :_  down
      %+  murn  caz
      |=  =card:neo
      ^-  (unit move:neo)
      ?.  (is-parent pith p.card)
        ~
      `[pith card]
    work
  ::
  ++  listen-conf
    |=  [=conf:neo =deps:neo]
    ^+  arvo
    =/  conf  ~(tap by conf)
    |-
    ?~  conf
      arvo
    =/  [=term dep=pith:neo]  i.conf
    ?>  ?=([[%p @] *] dep)
    =/  d=(unit [req=? =quay:neo])  (~(get by deps) term)
    ?~  d 
      $(conf t.conf)
    =/  [req=? =quay:neo]  u.d
    =/  =tour:neo  [(get-care:quay:neo quay) dep]
    =/  =pith:neo  [p/our.bowl here]
    ?:  =(our.bowl +.i.dep)
      =/  =tone:neo  [%rely term pith]
      =.  sound  (~(put ju sound) [care.tour t.dep] tone)
      $(conf t.conf)
    ::
    =/  =riot:neo  (~(got by foreign) tour)
    =/  =rave:neo   [term pith]
    =.  deps.riot  (~(put in deps.riot) rave)
    =.  foreign    (~(put by foreign) tour riot)
    $(conf t.conf)
  ::
  ++  validate-kids
    ^-  ?
    ?:  =(1 1)
      &
    ?~  par-pith=(parent:of-top here)
      & :: XX: review
    =/  parent=room:neo  (got:of-top u.par-pith)
    =/  parent-firm=firm:neo  ~(firm husk code.parent)
    =/  sfix  (sub:pith:neo here u.par-pith)
    ?~  mat=(find:peon:neo sfix ~(key by kids:parent-firm))
      ~&  >>>  %kids-no-match
      &
    & :: XX: enforce conformance
  ++  make-plot
    |=  [src=stud:neo =conf:neo]
    =/  =plot:neo  (need ~(plot husk src))
    =/  =deps:neo  deps:plot
    =^  bad=(set term)  get.block
      (check-conf conf deps:plot)
    ?.  =(~ get.block)
      arvo
    =.  arvo  (listen-conf conf deps:plot)
    =/  =soil:neo
      [[[1 1] ~] ~]
    =/  =room:neo
      [src state:plot conf soil/soil]
    =.  apex  (put:of-top here room)
    work
  ::
  ++  make
    |=  [src=stud:neo init=(unit vase) =conf:neo]
    ?:  ~(is-plot husk src)
      ~|  %cant-make-plot-w-init
      ?>  ?=(~ init)
      (make-plot src conf)
    =/  =firm:neo  ~(firm husk src)
    :: =.  run        (~(start husk src) our.bowl pith)
    =/  old  (get:of-top here)
    =/  =form:neo  form:firm
    =/  =icon:neo  
      ?~  old
        [[1 1] *vase ~ ~]
      ?>  ?=(%icon -.seat.u.old)
      [ever.icon.seat.u.old *vase ~ ~]
    =?  init  ?=(^ old)
      ~|  bad-install-over/here
      ?>  =(state:firm state.u.old)
      ?:  =(~ init)
        ?.  ?=(%icon -.seat.u.old)
          init
        `state.icon.seat.u.old
      init
    =/  =deps:neo  deps:firm
    =^  bad=(set term)  get.block
      (check-conf conf deps:firm)
    ?.  validate-kids 
      !!
    ?.  =(~ bad)
      ~|  missing-dependecies/~(tap in bad)
      !!
    ?.  =(~ get.block)
      ~&  get/get.block
      arvo
    =.  arvo  (listen-conf conf deps:firm)
    =/  =room:neo  [src state:firm conf icon/icon]
    =.  apex  (put:of-top here room)
    =^  cards=(list card:neo)  arvo
      (soft-site |.(si-abet:(si-init:site init)))
    (ingest cards)

  ++  soft-site
    |=  tap=(trap (quip card:neo _arvo))
    ^-  (quip card:neo _arvo)
    =/  res=(each (quip card:neo _arvo) tang)
      (mule tap)
    ?:  ?=(%& -.res)
      p.res
    =.  err.block  `p.res
    `arvo
  ::
  ++  site
    =/  =room:neo
      (got:of-top here)
    ?>  ?=(%icon -.seat.room)
    =|  cards=(list card:neo)
    |%  
    ++  site  .
    ++  si-emil  |=(caz=(list card:neo) site(cards (welp cards caz)))
    ++  si-abet
      ::  TODO: bump
      =.  apex  (put:of-top here room)
      [cards arvo]
    ::
    ++  si-resolve-kids   kids:(land here)
    ++  si-resolve-deps   deps:(land here)
      ::  TODO type this w/ port??
    ++  si-bowl    
      :: =/  hare  pith:(de-pith:name:neo here)
      :: ~&  hare/hare
      =/  hare  [p/our.bowl here]
      [src our.bowl hare hare now.bowl si-resolve-deps si-resolve-kids]
    ++  si-form    
      ~(. form:si-firm [si-bowl +.seat.room])
    ++  si-firm    `firm:neo`~(firm husk code.room)
    ++  si-can-poke
      |=  =stud:neo
      ^-  ?
      (~(has in poke:si-firm) stud)
    ++  si-tell
      |=  =mode:neo
      =.  change  (~(put by change) here mode)
      site
    ++  si-grab-watch
      ^-  watch:neo
      *watch:neo :: [here p.p.span.room [case %init q.state]:icon.room]~
    ++  si-sync-paths
      ^-  (list path)
      =-  ~(tap in -)
      %-  ~(gas in *(set path))
      %+  murn  ~(val by sup.bowl)
      |=  [=ship =path]
      ::  TODO: tighten
      ^-  (unit _path)
      ?.  ?=([%sync *] path)  
        ~
      `path
    ++  si-bump
      =.  apex  (~(anc-jab of:neo apex) here bump-room-tree)
      site
      
    ++  si-init   
      |=  old=(unit vase)
      ^+  site
      =^  cards=(list card:neo)  state.icon.seat.room
        (init:si-form old)
      =.  site  si-bump
      =.  site  (si-emil cards)
      (si-tell %add)

    ++  si-poke
      |=  =pail:neo
      ^+  site
      ?.  (si-can-poke p.pail)
        ?:  =(%rely p.pail)
          ~&  >>  si-skip-rely/[src here]
          site
        ?:  =(%gift p.pail)
          site
        ?:  =(%ack p.pail)
          ~&  >>  si-skip-ack/[src here]
          site
        ~|(no-poke-at/[p.pail here] !!)
      =/  old  state.icon.seat.room
      =^  cards  state.icon.seat.room
        (poke:si-form pail)
      =.  site  (si-emil cards)
      ?:  =(old state.icon.seat.room)
        site
      ::  XX: maybe skip if no change?
      =.  ever.icon.seat.room  (bump-ever ever.icon.seat.room)
      =.  site  si-bump
      (si-tell %dif)
    --
  --
++  sock
  |_  for=@tas
  ++  spur
    ^-  path
    /lib/plan/[for]/hoon
  ++  resolve
    ^-  path
    %+  welp
      /(scot %p our.bowl)/[q.byk.bowl]/(scot %da now.bowl)
    spur
  ++  sock-vase
    .^(vase %ca resolve)
  ++  plan  form:(pike:neo pail:neo)
  ++  get  
    !<(plan sock-vase)
  --
++  conn
  |=  =id:sole
  =/  =shell  [[our.bowl ~] ~]
  =.  shells  (~(put by shells) id shell)
  ~(start walk id)
++  tell
  |_  =name:neo
  ++  get
    ^-  (unit room:neo)
    ?.  =(our.bowl ship.name)
      ~
    (get:of-top pith.name)
  ++  kids  (desc 0)
  ++  ford
    =/  rom  (need get)
    ^+  run
    =+  !<([vax=(unit vase) *] q:(to-pail:room:neo rom))
    %.  run
    ?~  vax
      (slog leaf/"no ford build here" ~)
    (slog leaf/"Ford build" (sell u.vax) ~)
  ++  desc
    =/  dip
      ?>  =(our.bowl ship.name)
      (dip:of-top pith.name)
    |=  depth=@ud
    =*  loop  $
    ^-  (list (list dime))
    %-  zing
    %+  turn  ~(tap by kid.dip)
    |=  [seg=iota ax=(axal:neo room:neo)]
    ^-  (list (list dime))
    =.  pith.name  (snoc pith.name seg)
    =/  res  ?:(=(depth 0) ~ loop(dip ax, depth +(depth)))
    ?~  fil.ax  
      :_  res
      ~[t/(spat (pout pith.name)) t/'Directory' ud/0]
    :_  res
    row:item
  ++  item
    =/  rom  get
    |%
    ++  code
      |=  =stud:neo
      ^-  @t
      %-  spat
      ^-  path
      ?@  stud
        /kelvin/(scot %ud zuse)/[stud]
      =,  stud
      /(scot %p ship)/[desk]/[mark]
    ++  row
      ^-  (list dime)
      ?~  rom
        ~
      ?.  ?=(%icon -.seat.u.rom)
        :~  t/(spat (pout pith.name))
            t/(code code.u.rom)
        ==
      :~  t/(spat (pout pith.name))
          t/(code code.u.rom)
          ud/node.ever.icon.seat.u.rom
          ud/tree.ever.icon.seat.u.rom
      ==
    --
  ++  show
    ^-  tang
    =/  rom  get
    %-  lure
    ?~  rom
      leaf/"No data"
    ?.  ?=(%icon -.seat.u.rom)
      (sell q:value:(reap pith.name))
    (sell state.icon.seat.u.rom)
  --
++  land
  |=  here=pith:neo
  =/  =room:neo  (got:of-top here)
  |%
  ++  firm  ~(firm husk code.room)
  ++  kids
    =/  kids  kids:firm
    %-  ~(gas by *(map pith pail:neo))
    %+  murn  ~(tap by (kid:of-top here))
    |=  [=pith:neo =room:neo]
    ^-  (unit [pith:neo pail:neo])
    ?~  ion=(scion kids pith (to-pail:room:neo room))
      ~
    `[pith u.ion]
  ++  deps
    %-  ~(gas by *(map term [pith cane:neo]))
    ^-  (list [term pith cane:neo])
    %+  murn  ~(tap by deps:firm)
    |=  [=term required=? =quay:neo]
    ^-  (unit [^term pith cane:neo])
    =/  dep=(unit pith)  (~(get by conf.room) term)
    ?~  dep
      ~|  invariant-missing-required-conf/term
      ?<  required
      ~
    =/  =name:neo  (de-pith:name:neo u.dep)
    =/  =care:neo  (get-care:quay:neo quay)
    =/  =cane:neo  (moor quay name)
    `[term u.dep cane]
  --
++  reap
  |=  here=pith:neo
  =/  =room:neo  (got:of-top here)
  |%
  ++  deps  deps:(land here)
  ++  kids  kids:(land here)
  ++  bowl    
    :: =/  hare  pith:(de-pith:name:neo here)
    :: ~&  hare/hare
    =/  hare  [p/our.^bowl here]
    [[our.^bowl here] our.^bowl hare hare *time deps kids]
  ++  plot  (need ~(plot husk code.room))
  ++  value
    `pail:neo`[state.room (farm:plot bowl)]
  ++  cane
    |=  =care:neo
    ^-  cane:neo
    ?>  =(%x care)
    :*  care
        *ever:neo :: TODO: fix ever handling
        value
        ~
    ==


  --
::
++  walk
  |_  =id:sole
  ++  start 
    abet:prompt:peel
  ++  drop
    ^+  run
    run(shells (~(del by shells) id))
  ++  peel
    =/  =shell  (~(got by shells) id)
    |%  
    ++  peel  .
    ++  abet  
      run(shells (~(put by shells) id shell))
    ++  tell  ~(. ^tell cwd.shell)
    ++  race   (need race.shell)
    ++  has-race  !=(~ race.shell)
    ++  add-race
      |=  r=^race
      =.  race.shell  `r
      peel
    ++  del-race
      =.  race.shell  ~
      peel
    ++  prompt
      |^
      =;  ef=shoe-effect:shoe
        =.  run  (shoe-ef ef)
        peel
      :-  %sole
      :^  %pro  &  %foo
      ^-  styx
      =,  shell
      :-  [un ~['/' (scot %p ship.cwd)]]
      %-  snoc
      :_  '> '
      ^-  (list @t)
      %-  zing
      ^-  (list (list @t))
      %+  turn  pith.cwd
      |=  =iota:neo
      ^-  (list @t)
      =-  ~['/' -]
      ?@  iota  iota
      (scot iota)
      ++  un
        `styl`[`%un ~ ~]
      --
    ++  cwd
      |%
      ++  get  cwd.shell
      ++  set  |=(n=name:neo =.(cwd.shell n prompt))
      --
    --
  ++  lily
    |*  [naf=@ sab=rule]
    =+  vex=(sab [1 1] (trip naf))
    ?~  q.vex  ~  
    [~ u=p.u.q.vex] 
  ++  default-list
    ^-  (list [@t tank]) 
    :~  'ls'^leaf/"List child shrubs at current path"
        'cd'^leaf/"Change directory"
        '.'^leaf/"Print node at path"
        't'^leaf/"List child shrubs at current path, recursively"
        'p'^leaf/"manual poke (takes [=stud val=*])"
        'r'^leaf/"start form (takes form-name)"
    ==
  ++  tab
    |=  query=@t 
    =/  query  (trip query)
    =+  vex=(parser [1 1] query)
    ?~  q.vex
      default-list
    =/  [[? =hull] =nail]  u.q.vex
    =/  parsed  (scag (sub (lent query) (lent q.nail)) query)
    |^  ^-  (list [@t tank])
    ?+  -.hull  ~
      %cd  (cd name.hull)
    ==
    ++  cd
      |=  =name:neo
      ^-  (list [@t tank])
      =/  dip  (dip:of-top pith.name)
      =/  last
        ?:(=(~ pith.name) %$ (rear pith.name))
      =/  remove-len  (met 3 (show-iota last))
      =?  pith.name  =([~ ~] dip)
        (snip pith.name)
      =?  parsed     =([~ ~] dip)
        (scag (sub (lent parsed) remove-len) parsed)
      =.  dip
        (dip:of-top pith.name)
      ?:  =(~ kid.dip)
        ~
      %+  turn  ~(tap by kid.dip)
      |=  [seg=iota ax=(axal:neo room:neo)]
      ^-  [@t tank]
      :_  *tank
      %+  cat  3
      :-  (crip parsed)
      ?@  seg  seg
      (scot seg)
    --
  ++  parser
    |^  ^+  |~(nail *(like [? hull]))
    %+  stag  |
    ?:  has-race:peel
      %+  stag  %clot
      clot
    ;~  pose
      :: (csym %ls (easy ~))

      (cold ls/~ (jest 'ls'))
      (cold show/~ dot)
      (cold ford/~ (jest 'f'))
      (stag %tree ;~(pfix (jest 't') dem:ag))
      (stag %race ;~(pfix (jest 'r') ace sym))
      (stag %poke ;~(pfix (jest 'p') ace van))
    ::
      cd
      ;~(pfix wut (cold clay/~ (jest 'clay')))
      ;~(pfix hax (cold comm/~ (star prn)))
    ==
    ++  van  tall:(vang & /test)

    ++  cd
      :: ^-  _|~(nail *(like hull))
      :: %+  csym  %cd 
      %+  stag  %cd
      %+  ifix  [(jest 'cd ') (easy ~)]
      ;~  pose
        rule:name:neo
        %+  sear
          |=  [kets=(list *) =pith:neo]
          ^-  (unit name:neo)
          =/  cwd  get:cwd:peel
          =/  up  (lent kets)
          ?:  (gth up (lent pith.cwd))
            ~
          =.  pith.cwd  (scag (sub (lent pith.cwd) up) pith.cwd)
          `cwd(pith (welp pith.cwd pith))
        ;~(plug (star ket) (more fas spot:stip:neo))
      ==
    ++  csym
      |*  [term=* rul=rule]
      (stag term ;~(pfix (jest term) rul))
    ++  clot
      :: ^-  _|~(nail *(like [? gait]))
      =/  race  race:peel
      ^-  $-(nail (like clot:goon:neo))
      ?>  ?=(^ grab.race)
      ?+  scar.i.grab.race  !!
        %cord   (stag %cord (cook crip (star prn)))
        %patud  (stag %patud dem:ag)
        %patp   (stag %patp ;~(pfix sig fed:ag))
      ==
  --
  ++  shoe-ef
    |=  ef=shoe-effect:shoe
    ^+  run
    (emit %shoe ~[id] ef)
  ++  do 
    |=  =hull
    |^  ^+  run
    ?-  -.hull 
      %clot   abet:(clot:hike clot.hull)
      %show   (shoe-ef %sole %tan show:tell:peel)
      %ls     (tree 0)
      %cd     abet:(set:cwd:peel name.hull)
      %tree   (tree depth.hull)
      %ford   ford:tell:peel
      %race   (do-race rout.hull)
      %poke   (do-poke p.hull)
      %comm   run
      %clay   clay
    ==

    ++  do-poke
      |=  =hoon
      =/  vax=vase
        %-  slap
        :_  hoon
        %+  with-faces:ford  !>(..zuse)
        :~  neo/!>(neo)
            eny/!>(eny.bowl)
            now/!>(now.bowl)
            our/!>(our.bowl)
        ==
      =+  !<([=stud:neo rest=*] vax)
      =.  vax  (slot 3 vax)
      (on-card (en-pith:name:neo get:cwd:peel) %poke stud vax)
    ++  clay
      =/  rom  (got:of-top pith:get:cwd:peel)
      =+  !<([cac=(unit vase) *] q:(to-pail:room:neo rom))
      =/  desc=@t 
        ?~  cac
          'No cache'
        'Has cache'
      =/  sho=shoe-effect:shoe
        [%sole %klr ~[desc]]
      (shoe-ef sho)

    ++  do-race
      |=  rout=@tas
      ^+  run
      =/  bad=shoe-effect:shoe
        [%sole %klr ~['No pokes here']]
      =/  cwd  get:cwd:peel
      ?~  rom=(get:of-top pith.cwd)
        (shoe-ef bad)
      =/  =plan:sock  ~(get sock rout)
      =/  =race  [rout plan ~ ~]
      =.  run  abet:(add-race:peel rout plan ~ ~)
      abet:start:hike
        
    ++  tree
      |=  dep=@ud
      %-  shoe-ef  
      :-  %table  
      :+  (limo tas/%path tas/%code tas/%node tas/%tree ~)
        (limo 40 40 6 6 ~)
      (desc:tell:peel dep)
    --
  ++  hike
    =/  =race  race:peel
    |%
    ++  abet   `_run`?~(grab.race abet:del-race:peel abet:(add-race:peel race))
    ++  make-bowl
      `bowl:pike:neo`[our.bowl get:cwd:peel eny.bowl now.bowl]
    ++  hike   .
    ++  start  ^+(hike (take ~))
    ++  clot
      |=  =clot:goon:neo
      ^+  hike
      =.  have.race  (snoc have.race clot)
      =.  grab.race
        ?>  ?=(^ grab.race)
        t.grab.race
      ?^  grab.race
        show-grab
      =/  have  have.race
      =.  have.race  ~
      (take `[%grab have])
    ++  take
      |=  syn=(unit sign:pike:neo)
      |-  ^+  hike
      =/  pike  (pike:neo pail:neo)
      =^  res=eval-result:pike  form.race
        (take:pike form.race [make-bowl syn])
      ?-    -.res
          %done
        =/  =pith:neo  (en-pith:name:neo get:cwd:peel)
        =/  =card:neo  [pith %poke value.res]
        =.  run  (on-card card) 
        =;  ef=shoe-effect:shoe
          =.  run  (shoe-ef ef)
          hike
        :+  %sole  %klr
        =/  =styl  [~ ~ `%g]
        =/  txt=styx  ~['Poke success']
        `styx`~[styl^txt]
      ::
          %fail
        =.  races  (~(del by races) id)
        hike
          %emit  
        ?-    -.car.res
        :: TODO: actually scry
            %peek  $(syn `[%peek addr-info/!>(['New York' 'NY'])])
            %grab
          =.  grab.race  items.car.res
          show-grab
        ==
      ==
    ++  show-grab
      |-  ^+  hike
      ?~  grab.race
        hike
      =/  =item:pike:neo  i.grab.race
      =;  ef=shoe-effect:shoe
        =.  run  (shoe-ef ef)
        hike
      :+  %sole  %klr
      ~[(crip "{(trip lede.item)}: {(trip info.item)}")]
    --

::  ++  start
::    |=  soc=@tas
::    ^+  run
::    =/  =race
::      [soc ~(form sock soc) ~ ~]
::    =.  races  (~(put by races) id race)
::    (take ~)
    :: ?~  
    :: =/  =wire  /race/(scot %p who.id)/[ses.id]
    :: (emit %pass wire %agent [
  --
--
