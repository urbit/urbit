/-  neo
/+  lib=neo-two
/+  default-agent
|%
+$  card  card:agent:gall
+$  state-0
  $:  one=loam:dirt:neo
      two=farm:neo
      dev=_|
  ==
++  mute
  |=  dev=?
  |*  *
  ?:  dev
    !:  +<
  !.  +<
--
=|  state-0
=*  state  -
=<
  !.
  |_  =bowl:gall
  +*  this  .
      run   ~(. +> [bowl ~])
      def   ~(. (default-agent this %|) bowl)
  ++  on-init  
    ^-  (quip card _this)
    `!>(state)
  ++  on-save  !>(state)
  ++  on-load
    |=  vax=vase
    =+  !<(sta=state-0 vax)
    `this(state sta)
  ++  on-poke
    |=  =cage
    ^-  (quip card _this)
    =^  cards  state
      abet:(on-poke:run cage)
    [cards this]
  ++  on-watch
    |=  =path
    ^-  (quip card _this)
    =^  cards  state
      abet:(on-watch:run path)
    [cards this]
  ++  on-leave  on-leave:def
  ++  on-agent
    |=  [=wire =sign:agent:gall]
    ^-  (quip card _this)
    =^  cards  state
      abet:(on-agent:run wire sign)
    [cards this]
  ++  on-arvo
    |=  [=wire syn=sign-arvo]
    ^-  (quip card _this)
    =^  cards  state
      abet:(on-arvo:run wire sign)
    [cards this]
  ++  on-fail  on-fail:def
  ++  on-peek  on-peek:run
  --
%-  (mute dev)
|_  [=bowl:gall cards=(list card)]
::  |aux: auxilliary helpers
+|  %aux
++  abet  [(flop cards) state]
++  run  .
++  emit  |=(=card run(cards [card cards]))
++  pass  |=([=wire =note:agent:gall] `card`[%pass wire note])
++  give  |=(=gift:agent:gall (emit %give gift))
++  fact  |=([pas=(list path) =cage] (give %fact pas cage))
++  emil  |=(caz=(list card) run(cards (welp (flop caz) cards)))
++  def   ~(. (default-agent run %|) bowl)
+|  %constants
++  sys-pith
  ^-  pith:neo
  :-  p/our.bowl
  ~[n/~ %sys]
::  |do: effect creation
+|  %do 
++  do-poke
  |=  [=wire =dock =cage]
  (pass wire %agent dock poke/cage)
++  do-poke-our
  |=  [=wire =dude:gall =cage]
  (do-poke wire [our.bowl dude] cage)
++  do-poke-her
  |=  [=wire her=ship =cage]
  (do-poke wire [her dap.bowl] cage)
++  do-poke-self
  |=  [=wire =cage]
  (do-poke-our wire [our dap]:bowl cage)
++  do-move
  |=  =move:neo
  =/  dst=name:neo  (de-pith:name:neo p.q.move)
  =/  src=name:neo  (de-pith:name:neo p.move)
  ?>  =(ship.src our.bowl)
  =/  =wire  /deal/(pout p.move)
  ?:  =(our.bowl ship.dst)
    (do-poke-self wire neo-move+!>(move))
  (do-poke-her wire neo-raw-poke+!>((move:soften move)))
++  do-ack
  |=  =ack:neo
  ^+  (list card)
  ?:  =(p.p.ack sys-pith)
    %.  ~
    ?~  err
      same
    (slog leaf/"nack on sys" err)
  =/  src=name:neo  (de-pith:name:neo p.p.ack)
  =/  =wire  /nack/(pout p.p.ack)
  (do-poke-her wire ship.src neo-ack+!>(ack))^~
:: ?:  =(p.flow 
::  |on: event handlers
+|  %on
++  on-poke
  |=  [=mark =vase] 
  ^+  run
  ?+  mark  ~|(bad-poke-mark/mark !!)
    %neo-move        =;(f (f !<(_+<.f vase)) on-move)
    %neo-raw-move    (on-move (hard
  ==
++  on-move
  |=  =move:neo
  ^+  run
  =/  src=name:neo  (de-pith:name:neo p.move)
  ?>  =(src.bowl ship.src)
  abet:(arvo move)
++  on-watch
  |=  =(pole knot)
  ^+  run
  ~|  bad-watch-path/pole
  ?>  ?=([%sync rest=*] pole)
  =/  =pith:neo  (pave:neo rest.pole)
  =/  paxs=(list poth:neo)  (de:drive:neo pith)
  ?>  ?=([^ ^ ~] paxs)
  ?+  i.paxs  !!
    [car=@ [%ud since=@] ~]  !!  :: XX: TODO:
  ==
++  on-agent
  |=  [=wire =sign:agent:gall]
  ^+  run
  =/  =puth:neo  (pave:neo wire)
  ?+  puth  +:(on-agent:def wire sign)
    [%deal rest=*]  (on-deal-sign rest.puth sign)
  ==
++  on-deal-sign
  |=  [=puth:neo =sign:agent:gall]
  ^+  run
  ?>  ?=(%poke-ack -.sign)
    run
  !! :: XX: deliver nack
    
++  on-arvo
  |=  [=wire syn=sign-arvo]
  ^+  run
  =.  run   +:(on-agent:def wire sign)
  run
++  on-peek
  |=  =path
  ^-  (unit (unit cage))
  =/  =pith:neo  (pave:neo path)
  [~ ~]
::  |jungle: shurb manipulations
+|  %jungle
::  +stop: helper for blocking semantics
++  stop
  |%
  ::  +fresh: Handle newly blocked flow
  ++  fresh
    |=  [prey=(set hunt:neo) =move:neo]
    =/  =flow:neo  [p p.q]:move
    ~&  fresh-stop/flow
    ?.  =(~ (~(get by clog.halt) flow))
      ~|  trying-to-block-on-congested-flow/flow
      !!
    =/  q=(qeu move:neo)  (~(put to *(qeu move:neo)) move)
    =.  clog.halt  (~(put by clog.halt) flow q)
    =/  prey=(list hunt:neo)  ~(tap in prey)
    |-  ^+  run
    ?~  block
      run
    =/  =hunt:neo  i.block
    =.  by-tour.halt   (~(put by by-tour.halt) hunt flow)
    =.  by-flow.halt   (~(put ju by-flow.halt) flow hunt)
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
    =/  prey=(set hunt:neo)
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

::  +arvo: local callstack
++  arvo
  =+  verb=&
  =/  old  state
  ::  data for blocking semantics
  =|  =block:neo
  ::  callstack
  =|  $:  done=(list move:neo)  :: moves we've completed
          down=(list move:neo)  :: pending moves for children
          up=(list move:neo)    :: pending moves for uncles
          change=(map pith mode:neo)  :: changeset
          changes=(map pith mode:neo) :: also changeset
          gifts=(list [pith:neo gift:neo]) :: return values
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
      =.  run    (emit (do-ack [p q]:init-move err.block))
      =.  cards  (welp cards (turn up do-move))
      (dial changes)
        :: %+  turn  ~(tap by change)
        ::  |=([=pith:neo =mode:neo] ^+(+< [[p/our.bowl pith] mode]))
      :: run
    ~&  >>>  %reverting
    ~&  >>>  init
    =.  state  old :: XX: is apex only state that is touched?
    ?.  =(~ get.block)
      (fresh:stop get.block init-move)
    ?>  ?=(^ err.block)
    ::  %-  (slog u.err.block)
    ?:  ?=([%poke %rely *] q.q.move)
      ~&  >>>  rely-nack/[src dst]:init
      run
    (emit (do-nack [p q]:init-move err.block))
  ::
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
      `arvo :: (soft-site |.(si-abet:(si-poke:site pail)))
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
      `arvo :: (soft-site |.(si-abet:(si-poke:site pail)))
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
      `arvo ::  (soft-site |.(si-abet:(si-init:site init)))
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
  --
::  |util: utilties
+|  %util
++  soften
  |%
  ++  move
    |=  =move:neo
    ^-  raw-poke:neo
    ?>  ?=(%poke -.q.q.move)
    [[p p.q]:move (pail:soften pail.q.q.move)]
  ++  pail
    |=  =pail:neo
    ^-  vial:neo
    [p q.q]:pail
  --
++  harden
  |%
  ++  poke
    |=  raw=raw-poke:neo
    ^-  move:neo
    [p.p.raw q.p.raw %poke (vial q.raw)]
  ++  vial
    |=  =vial:neo
    :-  p.vial
    *vase
    ::  (slym (need ~(get pro p.vial)) q.vial)
  --
--

