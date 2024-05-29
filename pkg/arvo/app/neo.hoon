/-  neo
/+  aux=neo-two
/+  default-agent
/+  dbug
/+  libverb=verb
/+  serv=server
/*  txt-hoon-imp    %hoon   /neo/cod/std/src/imp/hoon/hoon
/*  txt-term-imp    %hoon   /neo/cod/std/src/imp/term/hoon
/*  txt-ford-same   %hoon   /neo/cod/std/src/imp/ford-same/hoon
/*  txt-ford-slop   %hoon   /neo/cod/std/src/imp/ford-slop/hoon
/*  txt-ford-slap   %hoon   /neo/cod/std/src/imp/ford-slap/hoon
/*  txt-ford-face   %hoon   /neo/cod/std/src/imp/ford-face/hoon
/*  txt-ford-face   %hoon   /neo/cod/std/src/imp/ford-face/hoon
/*  txt-ford-reef   %hoon   /neo/cod/std/src/imp/ford-reef/hoon
/*  txt-ford-text   %hoon   /neo/cod/std/src/imp/ford-text/hoon
=>
  |%
  ++  dev  &
  ++  mute
    ?:  dev  same
    |*  *
    !.  +<
  --
%-  mute
|%
+$  card  $+(card card:agent:gall)
+$  state-0
  $+  state-0
  $:  =loam:dirt:neo  :: layer 1 
      =farm:neo       :: layer 2
    ::
      =town:neo       :: subscription
      =city:neo
    ::
      =riot:neo       :: dependencies
    ::
      =tide:neo       :: concrete
      =dive:neo       :: build
    ::
      =gang:neo       :: overlay
      =lads:neo       :: virtual
    ::
      =mate:neo       :: peers
    ::
      =unix:neo
    ::
      =halt:neo
    ::
      ripe=_|
    ::
      dev=_|
      run-nonce=@uvJ

  ==
::
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

--
=|  state-0
=*  state  -
=<
  %-  mute
  %+  libverb  &
  %-  agent:dbug
  ^-  agent:gall
  |_  =bowl:gall
  +*  this  .
      run   ~(. +> [bowl ~])
      def   ~(. (default-agent this %|) bowl)
  ++  on-init  
    ^-  (quip card _this)
    =^  cards  state
      abet:boot:run
    [cards this]
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
      abet:(on-peer:run path |)
    [cards this]
  ++  on-leave
    |=  =path
    ^-  (quip card _this)
    =^  cards  state
      abet:(on-peer:run path &)
    [cards this]
  ::
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
      abet:(on-arvo:run wire syn)
    [cards this]
  ++  on-fail
    |=  [=term =tang]
    =^  cards  state
      abet:(on-fail:run term tang)
    [cards this]
  ++  on-peek  on-peek:run
  --
:: %-  mute
|_  [=bowl:gall cards=(list card)]
::  |aux: auxilliary helpers
+|  %aux
++  abet  [(flop cards) state]
++  run  .
++  our  our.bowl
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
++  do-watch
  |=  [=wire =dock =path]
  (pass wire %agent dock watch/path)
++  do-watch-her
  |=  [=wire her=ship =path]
  (do-watch wire [her dap.bowl] path)
++  do-leave
  |=  [=wire =dock]
  (pass wire %agent dock leave/~)
++  do-leave-her
  |=  [=wire her=ship]
  (do-leave wire her dap.bowl)
::
++  do-poke
  |=  [=wire =dock =cage]
  ^-  card
  (pass wire %agent dock poke/cage)
++  do-poke-our
  |=  [=wire =dude:gall =cage]
  ^-  card
  (do-poke wire [our.bowl dude] cage)
++  do-poke-her
  |=  [=wire her=ship =cage]
  ^-  card
  (do-poke wire [her dap.bowl] cage)
++  do-poke-self
  |=  [=wire =cage]
  ^-  card
  (do-poke-our wire dap:bowl cage)
++  do-move
  |=  =move:neo
  ^-  card
  =/  dst=name:neo  (de-pith:name:neo p.q.move)
  =/  src=name:neo  (de-pith:name:neo p.move)
  ?>  =(ship.src our.bowl)
  =/  =wire  deal/(pout p.move)
  ?:  =(our.bowl ship.dst)
    (do-poke-self wire neo-move+!>(move))
  (do-poke-her wire ship.dst neo-raw-poke+!>((move:soften move)))
++  do-card
  |=  =card:neo
  (do-move sys-pith card)
::
++  do-ack
  |=  =ack:neo
  ^-  (list card)
  ?:  =(p.p.ack sys-pith)
    %.  *(list card)
    ?~  q.ack
      same
    ?-  -.u.q.ack
      %goof  (mean leaf/"goof on sys" tang.u.q.ack)
      %gone  (mean leaf/"no dependency {<term.u.q.ack>}" ~)
    ==
  =/  src=name:neo  (de-pith:name:neo p.p.ack)
  =/  =wire  nack/(pout p.p.ack)
  (do-poke-her wire ship.src neo-ack+!>(ack))^~
++  do-grow
  |=  [=pith:neo =pail:neo]
  ^-  card:dirt:neo
  [pith %grow pail ~ *oath:neo]
++  do-grow-our
  |=  [=pith:neo =pail:neo]
  ^-  card:dirt:neo
  (do-grow [p/our.bowl pith] pail)
++  do-std-warp
  =/  =rave:clay
    [%next %z da/now.bowl /neo]
  (pass /next-clay %arvo %c %warp our.bowl q.byk.bowl `rave)
::
++  do-fetch-fine
  |=  =pith:neo
  ^-  card
  =/  =wire:neo  fetch/(pout pith)
  =/  =name:neo  (de-pith:name:neo pith)
  =/  nonce  (scot %uv run-nonce:(~(got by mate) ship.name))
  =/  =spar:ames  [ship.name [nonce (pout pith)]]
  ~&  fetching/spar
  !! :: (pass wire %keen spar)
++  do-gall-grow
  |=  [=pith:neo sag=(unit saga:neo)]
  ^-  card
  =/  =wire   gall-grow/(pout pith)
  =/  =page  
    ?~  sag  none/~
    neo-feat/(saga:soften u.sag)
  (pass wire %grow (pout pith) page)
::
:: ?:  =(p.flow 
::  |on: event handlers
+|  %on
::
++  on-poke
  |=  [=mark =vase] 
  ^+  run
  ?+  mark  ~|(bad-poke-mark/mark !!)
    %neo-move        =;(f (f !<(_+<.f vase)) on-move)
    %neo-card        =;(f (f !<(_+<.f vase)) on-card)
    %neo-dirt-card   =;(f (f !<(_+<.f vase)) on-dirt-card)
    %neo-sync        =;(f (f !<(_+<.f vase)) on-sync)
    %neo-ack         =;(f (f !<(_+<.f vase)) on-ack)
  ::
    %noun            (on-noun q.vase)
    %neo-raw-poke    (on-move (poke:harden !<(raw-poke:neo vase)))
    %handle-http-request  (handle-http-request:sttp !<([@ta inbound-request:eyre] vase))
  ==
++  on-noun
  |=  non=*
  ^+  run
  ?+  non  ~|(bad-noun-poke/non !!)
    %dbug   ((slog (print-dbug ~)) run)
    [%dbug pfix=*]  ((slog (print-dbug ;;(pith:neo pfix.non))) run)
    [%dbug-all pfix=*]  ((slog (print-dbug-all ;;(pith:neo pfix.non))) run)
  ==
++  on-card
  |=  =card:neo
  (on-move sys-pith card)
::
++  on-move
  |=  =move:neo
  ^+  run
  =/  src=name:neo  (de-pith:name:neo p.move)
  =/  dst=name:neo  (de-pith:name:neo p.q.move)
  ?>  =(src.bowl ship.src)
  ?.  ?=([%$ *] pith.dst)
    abet:(arvo move)
  (on-move:sys p.move q.move(p t.pith.dst))
++  on-ack
  |=  =ack:neo
  %.  run
  ?~  q.ack
    same
  ?-  -.u.q.ack
    %gone  (slog leaf/"Missing dep: {<term.u.q.ack>}" ~)
    %goof  (slog leaf/"nacked on flow {<p.ack>}" tang.u.q.ack)
  ==
::
++  on-dirt-card
  |=  =card:dirt:neo
  ^+  run
  +:(take-dirt-card card)
++  on-sync
  |=  =sync:neo
  ^+  run
  ?-    r.sync
    %start   abet:(~(start sale p.sync) [+ -]:q.sync)
    %stop    abet:(~(stop sale p.sync) [+ -]:q.sync)
  ==
++  on-sync-start
  |=  [src=pith:neo =hunt:neo]
  ^+  run
  !!
++  on-sync-stop
  |=  [src=pith:neo =hunt:neo]
  ^+  run
  !!
::
++  on-peer
  |=  [=(pole knot) stop=?]
  ^+  run
  ?+  pole   ~|  bad-watch-path/pole  !!
    [%sync rest=*]  (on-peer-sync (pave:neo rest.pole) stop)
    [%fetch rest=*]  ?:(stop run (on-peer-fetch (pave:neo rest.pole)))
    [%http-response *]   run
  ==
::
++  on-peer-fetch
  |=  =pith:neo
  ^+  run
  =/  sag  (need (peek-x:till pith))
  =/  =feat:neo
    ?~  sag  [*aeon:neo sig/~]
    (saga:soften u.sag)
  =.  run  (emit %give %fact ~ neo-feat+!>(feat))
  (emit %give %kick ~ ~)
::
++  on-peer-sync
  |=  [=pith:neo stop=?]
  ^+  run
  =/  paxs=(list road:neo)  (de:drive:neo pith)
  ?>  ?=([^ ^ ~] paxs)
  ?>  ?=([[%p ship=@] rest=*] i.t.paxs)
  ?>  =(our.bowl ship.i.t.paxs)
  ?+    i.paxs  ~|(bad-watch-sync/paxs !!)
      [car=@ [%f meet=?] [%ud since=@] ~]
    =*  ren  ~(. rent rest.i.t.paxs)
    =+  ;;(=care:neo car.i.paxs)
    =/  =path  sync/(pout pith)
    =<  abet
    ?:  stop
      (stop:ren care path)
    (push:ren meet.i.paxs care path)
  ==
++  on-agent
  |=  [=wire =sign:agent:gall]
  ^+  run
  =/  =road:neo  (pave:neo wire)
  ?+  road  +:(on-agent:def wire sign)
    [%deal rest=*]  (on-deal-sign rest.road sign)
    [%sale %sync rest=*]   abet:(~(on-sync-sign sale rest.road) sign)
    [%sale %fetch rest=*]  abet:(~(on-fetch-sign sale rest.road) sign)
  ==
++  on-deal-sign
  |=  [=road:neo =sign:agent:gall]
  ^+  run
  ?>  ?=(%poke-ack -.sign)
  :: run
  %.  run
  ?~  p.sign
    same
  (slog leaf/"neo: bad deal: {(en-tape:pith:neo road)}" u.p.sign)
::
++  on-arvo
  |=  [=(pole knot) syn=sign-arvo]
  ^+  run
  ?+  pole  +:(on-arvo:def pole syn)
    [%next-clay ~]  (take-next-clay:sys syn)
    [%sys rest=*]  (take-arvo:sys rest.pole syn)
    [%fetch rest=*]  abet:(~(take-fetch sale (pave:neo rest.pole)) syn)
  ==
++  on-peek
  |=  =path
  ^-  (unit (unit cage))
  ?>  ?=(^ path)
  =/  car  i.path
  |^
  =/  =road:neo  (pave:neo t.path)
  ?+  road  [~ ~]
    [%loam [%ud cas=@] rest=*]  (sing (~(scry plow:aux loam) [cas rest]:road))
  ==
  ::
  ++  raise
    |*  a=mold
    (lift (lift a))
  ::
  ++  sing
    %-  raise
    |=  =poem:neo
    neo-poem+!>(poem)
  ::
  ++  tell 
    %-  raise
    |=  =myth:neo
    neo-myth+!>(myth)
  --
++  on-fail
  |=  [=term =tang]
  ~&  fail/term
  %-  (slog tang)
  (emit do-std-warp)
::  |jungle: shurb manipulations
+|  %jungle
::  +crop: build (possibly virtual value)
::
::    TODO: does not work as advertised
++  till  ~(. till:aux [loam farm])
++  tell
  |=  [=pith:neo =epic:neo]
  ^+  run
  =/  [gis=(list gift:dirt:neo) lom=loam:dirt:neo fam=farm:neo]
    (tell:till epic)
  =.  loam  lom  
  =.  farm  fam
  =.  run   (lazarus gis)
  =.  run   (take:rage gis)
  =.  run   (collect-rent gis)
  ~&  gifs/gis
  run
::
++  plow  ~(. plow:aux loam)
++  crop
  |=  =pith:neo
  ^-  [(unit (unit saga:neo)) _run]
  :_  run
  =/  res  (~(peek till:aux [loam farm]) %x pith)
  ?:  ?=($@(~ [~ ~]) res)
    res
  ``(~(got of:neo u.u.res) /)
::
++  look
  |=  =hunt:neo
  ^-  (unit (unit epic:neo))
  (~(peek till:aux [loam farm]) hunt)

::  +sale: synchronisation
++  sale
  |_  =pith:neo
  ++  abet  run
  ++  sale  .
  ++  scry   ~
  ++  get-mall  (~(gut of:neo town) pith *mall:neo)
  ++  get-ship
    =/  =name:neo  (de-pith:name:neo pith)
    ship.name
  ::
  ++  put-mall  |=(=mall:neo sale(town (~(put of:neo town) pith mall)))
  ++  wire
    |=  kind=?(%fetch %sync)
    ^-  ^wire
    [%sale kind (pout pith)]
  ++  care
    ^-  care:neo
    %+  roll  ~(tap in mart:get-mall)
    |=  [=hunt:neo =care:neo]
    ?:  ?=(?(%z %c) care.hunt)  %z
    ?.  =(?(%y %b) care.hunt)  %z
    %x
  ++  peer-path
    %-  pout
    (welp #/sync (en:drive:neo #/[care]/[f/|]/[ud/0] pith ~))
  ++  fetch-path
    %-  pout
    (welp #/fetch pith)

  ++  stop
    |=  [src=pith:neo =care:neo]
    ^+  sale
    !!
  ++  start
    |=  [src=pith:neo =care:neo]
    ^+  sale
    =/  mal  (~(get of:neo town) pith)
    ?^  mal
      =.  mart.u.mal  (~(put in mart.u.mal) [care src])
      =.  town  (~(put of:neo town) pith u.mal)
      sale
    ::  XX: search upwards for 
    =|  =mall:neo
    =.  mart.mall  (~(put in mart.mall) [care src])
    ?.  =(~ find-deli)
      (put-mall mall)
    =|  =deli:neo
    =.  desc.deli  !=(%x care)
    =.  del.mall  `deli
    watch-sync:(put-mall mall)
  ::  XX: cancel freshly redundant
    ::  TODO: cancel subscriptions benear
  ++  resign
    ^-  (unit pith:neo)
    =/  ton  (~(dip of:neo town) pith)
    =|  yoof=(list [pith:neo town:neo])
    =|  here=pith:neo
    =/  kids=(list [pith:neo town:neo])
      (turn ~(tap by kid.ton) |=([=iota =town:neo] [~[iota] town]))
    |-
    ?~  kids
      ?:  =(~ yoof)
        ~
      $(kids yoof, yoof ~)
    =/  [pit=pith:neo tin=town:neo]  i.kids
    =.  yoof
      %+  welp  yoof
      %+  turn  ~(tap by kid.tin)
      |=  [iot=iota =town:neo]
      ^-  [pith:neo town:neo]
      [(welp pit ~[iot]) town]
    ?~  fil.tin
      $(kids t.kids)
    `(welp pith pit)
  ++  leave
    =/  =name:neo  (de-pith:name:neo pith)
    =.  run  (emit (do-leave-her (wire %sync) ship.name))
    sale
  ++  take-fetch
    |=  syn=sign-arvo
    ^+  sale
    ~&  got-fetch/pith
    ?>  ?=([%ames %tune *] syn)
    ?~  roar.syn
      ~&  missing-roar/pith
      sale
    =/  [=path dat=(unit page)]  dat.u.roar.syn
    ?~  dat  
      ~&  missing-page/pith
      sale
    %-  on-saga
    ?>  =(p.u.dat %neo-feat)
    (feat:harden ;;(=feat:neo q.u.dat))

  ++  on-saga
    |=  res=saga:neo
    =/  =mall:neo  get-mall
    =?  shop.mall  ?=(^ shop.mall)
      ~?  !=(p.exe.p.p.res p.exe.p.u.shop.mall)
        mismatch-saga-sale/[exe.p.u.shop.mall exe.p.p.res]
      ~
    =.  sale  (put-mall mall)
    =/  del  
      ~|  town/town
      ~|  mall/mall
      ~|  pith/pith
      (need find-deli)
    ~&  del/del
    =/  kid  (dif:pith:neo pith del)
    ~&  kid/kid
    abet:(fetched:~(meat sale del) (dif:pith:neo pith del) res)
  ::  XX: possibly check that 
  ++  find-deli
    =|  res=(unit pith:neo)
    =/  at=pith:neo  pith
    ?.  =(~ del:get-mall)
      `pith
    =.  pith  ~
    |-  ^+  res
    =/  nex  (dif:pith:neo at pith)
    ?~  nex
      ~?  =(~ nex)
        missing-deli/at
      res
    =/  =mall:neo  get-mall
    =?  res  &(?=(^ del.mall) desc.u.del.mall)
      `pith
    $(pith (snoc pith i.nex))
  ++  meat
    =/  =mall:neo  get-mall
    =/  =deli:neo  (need del.mall)
    |%
    ++  abet  =.(del.mall `deli (put-mall mall))
    ++  meat  .
    ++  new
      |=  =yuga:neo
      =.  yuga.deli  yuga
      ~&  new-yuga/yuga
      meat
    ++  fetched
      |=  [kid=pith:neo =saga:neo]
      =.  yuga.deli  (~(del of:neo yuga.deli) kid)
      =.  epic.deli  (~(put of:neo epic.deli) kid saga)
      ~&  fetched/deli
      ?.  =(~ ~(tap of:neo yuga.deli))
        meat
      =/  =epic:neo  epic.deli
      ~&  finalizing/[pith epic]
      =.  epic.deli  *epic:neo
      =.  run  (tell pith (~(rep of:neo *epic:neo) pith epic))
      meat
    --
  ::
  ++  gone
    |=  sub=hunt:neo
    ^+  sale
    =/  ton  (~(dip of:neo town) pith)
    ?~  fil.ton
      ~&  %gone-no-sub
      sale
    =.  mart.u.fil.ton  (~(del in mart.u.fil.ton) sub)
    ?~  del.u.fil.ton
      sale
    =/  =deli:neo  u.del.u.fil.ton
    =/  resig  resign
    ?~  resig
      ~&  last-standing-ending-sub/pith
      leave
    !!
  ::
  ++  on-sync-sign
    |=  =sign:agent:gall
    ~&  town/town
    ^+  sale
    ?+    -.sign  ~|(bad-sign/-.sign !!)
        %watch-ack
      %.  sale
      ?~  p.sign
        same
      (slog u.p.sign)
    ::
        %fact
      ?.  =(%neo-yuga p.cage.sign)
        ~&  weird-mall-fact/p.cage.sign
        sale
      (on-yuga !<(yuga:neo q.cage.sign))
    ::
        %kick
      ~&  'todo: kick handling'
      sale
    ==
  ::
  ++  on-fetch-sign
    |=  =sign:agent:gall
    ^+  sale
    ?+    -.sign  ~|(bad-sign/-.sign !!)
        %watch-ack
      %.  sale
      ?~  p.sign
        same
      (slog u.p.sign)
    ::
        %fact
      ?.  =(%neo-feat p.cage.sign)
        ~&  weird-fetch-fact/p.cage.sign
        sale
      (on-saga (feat:harden !<(feat:neo q.cage.sign)))
    ::
        %kick
      =/  =mall:neo  get-mall
      ?~  shop.mall
        sale
      watch-fetch
    ==
  ::
  ++  on-yuga
    |=  =yuga:neo
    ^+  sale
    :: =.  sale  abet:(new:meat yuga)
    =/  lis  ~(tap of:neo yuga)
    |-  
    ?~  lis
      ~&  done-yuga-town/town
      abet:(new:meat yuga)
    =/  [kid=pith:neo =aeon:neo]  i.lis
    =/  pit  (welp pith kid)
    =^  res=(unit (unit saga:neo))  run
      (crop pit)
    ?~  res
      =.  run  abet:(~(fresh sale pit) aeon)
      ~&  nothing/pit
      $(lis t.lis)
    ?~  u.res
      ~&  dead/pit
      :: XX: what means??
      $(lis t.lis)
    ~&  alive/pit
    ?:  =(p.u.u.res aeon)
      ~&  clone/pit
      $(lis t.lis, yuga (~(del of:neo yuga) kid))
    ~&  fresh/pit
    =.  run  abet:(~(fresh sale pit) aeon)
    $(lis t.lis)
  ::
  ++  watch-fetch
    =/  wir  (wire %fetch)
    =.  run  (emit (do-watch-her wir get-ship fetch-path))
    sale
  ++  watch-sync 
    =/  wir  (wire %sync)
    =.  run        (emit (do-watch-her (wire %sync) get-ship peer-path))
    sale
  ::
  ++  fresh
    |=  =aeon:neo
    ^+  sale
    =/  =mall:neo  get-mall
    =.  shop.mall  `aeon
    =.  sale  (put-mall mall)
    watch-fetch
  --
++  collect-rent
  |=  gis=(list gift:dirt:neo)
  ^+  run
  ?~  gis
    run
  =/  [=pith:neo =loot:neo]  i.gis
  =/  =name:neo  (de-pith:name:neo pith)
  ?.  =(our.bowl ship.name)
    $(gis t.gis)
  =.  run  abet:(vend:rent pith.name loot)
  $(gis t.gis)
++  rent
  |_  =pith:neo
  ++  abet  run
  ++  rent  .
  ++  get-ward  (~(gut of:neo city) pith *ward:neo)
  ++  put-ward  |=(=ward:neo rent(city (~(put of:neo city) pith ward)))
  ++  fact
    |=  [=care:neo paxs=(set pith:neo)]
    ?:  =(~ paxs)
      rent
    =.  run  (emit %give %fact (turn ~(tap in paxs) pout) (item care))
    rent
  ::  +serve: first sale
  ++  item
    |=  =care:neo
    ^-  cage
    :-  %neo-yuga
    !>  ^-  yuga:neo
    (yuga care)
  ::
  ++  yuga
    |=  =care:neo
    ^-  yuga:neo
    ?~  pic=(need (look care p/our.bowl pith))
      *yuga:neo
    ~&  epic/u.pic
    (epic-to-yuga u.pic)
  ::
  ++  stop
    |=  [=care:neo =path]
    ^+  rent
    =/  =ward:neo  get-ward
    =.  ward
      ?+  care  !!
        %x  ward(exe (~(del in exe.ward) path))
        %y  ward(why (~(del in why.ward) path))
        %z  ward(zed (~(del in zed.ward) path))
      ==
    (put-ward ward)

  ::
  ++  push
    |=  [meet=? =care:neo =path]
    ^+  rent
    =/  =ward:neo  get-ward
    =.  ward
      ?+  care  !!
        %x  ward(exe (~(put in exe.ward) path))
        %y  ward(why (~(put in why.ward) path))
        %z  ward(zed (~(put in zed.ward) path))
      ==
    :: =?  run  meet
      :: (emit %give %fact ~ neo-meet+!>(`meet:neo`[our.bowl run-nonce ~]))
    =.  run  (emit %give %fact ~ (item care))
    (put-ward ward)
  +$  loc  ?(%self %par %anc)
  ++  get-loc
    |=  until=pith:neo
    ?:  =(until pith)
      %self
    =/  left  (dif:pith:neo pith until)
    ?:  (~(has by (~(kid of:neo tide) pith)) left) 
      %par
    %anc
  ::
  ++  vend
    =|  loc=?(%self %par %anc)
    |=  [until=pith:neo =loot:neo]
    ^+  rent
    =.  loc  (get-loc until)
    =/  war  get-ward
    =?  rent  ?=(%self loc)
      (fact %x exe.war)
    =?  rent  ?=(?(%self %par) loc)
      (fact %y why.war)
    =.  rent  (fact %z zed.war)
    ?~  nex=(dif:pith:neo pith until)
      rent
    $(pith (snoc pith i.nex))
  --
++  rage
  |% 
  ++  stalk
    |=  [=hunt:neo =howl:neo]
    ^+  run
    =/  rav  (fall (~(get of:neo riot) pith.hunt) *rave:neo)
    =.  rav  (fume-add rav care.hunt howl)
    =.  riot  (~(put of:neo riot) pith.hunt rav)
    run
  ++  fury
    |=  gis=(list gift:dirt:neo)
    %-  gas-leaf
    %+  turn  gis
    |=  [=pith:neo case=@ud =mode:neo]
    [pith mode]
  ::
  ++  spaz
    |=  [ton=(set howl:neo) =hunt:neo]
    =/  =leaf:neo  (get-leaf:till hunt)
    =/  ton  ~(tap in ton)
    |-
    ?~  ton
      run
    =.  run  (yelp hunt i.ton leaf)
    $(ton t.ton)
  ::
  ++  sweep
    =|  here=pith:neo
    |=  [change=pith:neo =loot:neo]
    =/  =rave:neo  (~(gut of:neo riot) here *rave:neo)
    =?  run  =(here change)
      (spaz exe.rave %x change)
    =?  run   =(here (~(parent of:neo tide) change))
      (spaz why.rave %y change)
    =.  run
      (spaz zed.rave %z change)
    ?~  nex=(dif:pith:neo here change)
      run
    $(here (snoc here i.nex))
  ::
  ++  take
    |=  gis=(list gift:dirt:neo)
    =/  laf  (fury gis)
    =*  loop-gift  $
    ^+  run
    ?~  gis
      run
    =/  [=pith:neo =loot:neo]  i.gis
    =.  run  (sweep i.gis)
    $(gis t.gis)
  ::
  ++  fume-add
    |=  [=rave:neo =care:neo =howl:neo]
    ^+  rave
    ?+  care  !!
      %x  rave(exe (~(put in exe.rave) howl))
      %y  rave(why (~(put in why.rave) howl))
      %z  rave(zed (~(put in zed.rave) howl))
    ==

  ++  fume-del
    |=  [=rave:neo =care:neo =howl:neo]
    ^+  rave
    ?+  care  !!
      %x  rave(exe (~(del in exe.rave) howl))
      %y  rave(why (~(del in why.rave) howl))
      %z  rave(zed (~(del in zed.rave) howl))
    ==
  ::
  ++  free
    |=  =hunt:neo
    ^+  run
    ::  XX: weird shadowing, be careful
    =/  =rave:neo  (~(gut of:neo riot) pith.hunt *rave:neo)
    =.  rave  
      (fume-del rave care.hunt halt/~)
    =.  riot  (~(put of:neo riot) pith.hunt rave)
    (resolved:stop hunt)
  ::
  ++  yelp
    |=  [from=hunt:neo with=howl:neo =leaf:neo]
    ?:  ?=(%halt -.with)
      (free from)
    ?>  ?=(%rely -.with)
    =/  [=term =pith:neo]  +.with
    =/  =rely:neo  [term leaf]
    =/  =move:neo
      [pith.from [p/our.bowl pith] %poke %rely !>(rely)]
    abet:(arvo move)
  --
:: 
++  lazarus
  |=  git=grit:neo
  ^+  run
  ?~  git
    run
  =/  [=pith:neo =loot:neo]  i.git
  =/  =name:neo  (de-pith:name:neo pith)
  ?.  =(our.bowl ship.name)
    $(git t.git) :: XX: turn on for caching??
  =/  res  (need (look-x:till case.loot pith))
  ?:  &(?=(^ res) =(%vase p.q.u.res))
    $(git t.git)
  =.  run
    (emit (do-gall-grow pith (need (look-x:till case.loot pith))))
  $(git t.git)

++  take-dirt-card
  |=  =card:dirt:neo
  ^-  (quip gift:dirt:neo _run)
  =^  gifts=(list gift:dirt:neo)  loam
    (~(call plow:aux loam) card)
  =.  farm  (~(take till:aux [loam farm]) gifts)
  =.  run   (lazarus gifts)
  =.  run   (take:rage gifts)
  =.  run   (collect-rent gifts)
  [gifts run]

::  +stop: helper for blocking semantics
++  stop
  |%
  ::  +fresh: Handle newly blocked flow
  ++  fresh
    |=  [prey=(set hunt:neo) =move:neo]
    =/  =flow:neo  [p p.q]:move
    ~&  fresh-stop/[flow prey]
    ?.  =(~ (~(get by clog.halt) flow))
      ~|  trying-to-block-on-congested-flow/flow
      !!
    =/  q=(qeu move:neo)  (~(put to *(qeu move:neo)) move)
    =.  clog.halt  (~(put by clog.halt) flow q)
    =/  prey=(list hunt:neo)  ~(tap in prey)
    |-  ^+  run
    ?~  prey
      run
    =/  =hunt:neo  i.prey
    =.  by-hunt.halt   (~(put by by-hunt.halt) hunt flow)
    =.  by-flow.halt   (~(put ju by-flow.halt) flow hunt)
    =.  run  abet:(~(start sale pith.hunt) p.q.move care.hunt)
    =.  run  (stalk:rage hunt halt/~)
    $(prey t.prey)
  ::
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
    |=  =hunt:neo
    ~&  resolved/hunt
    =/  fow=(unit flow:neo)  (~(get by by-hunt.halt) hunt)
    ?~  fow
      run
    =.  by-hunt.halt    (~(del by by-hunt.halt) hunt)
    =.  by-flow.halt    (~(del ju by-flow.halt) u.fow hunt)
    =/  prey=(set hunt:neo)
      (~(get ju by-flow.halt) u.fow)
    ?.  =(~ prey)
      run
    =/  q  (~(got by clog.halt) u.fow)
    |-
    ?:  =(~ q)  
      =.  clog.halt  (~(del by clog.halt) u.fow)
      run
    =^  nex=move:neo  q  ~(get to q)
    =.  run  (emit (do-move nex))
    $
  --
::
++  dial
  |=  *
  ^+  run
  run
::
++  husk
  |_  =stud:neo
  ++  dock
    ^-  dock:neo
    [state poke kids]:kook
  ::
  ++  pith
    ^-  pith:neo
    :-  p/our.bowl  
    (~(pith press imp/stud) %out)
  ++  vase
    ^-  ^vase
    ~|  husk/stud
    q:(need (~(peek plow:aux loam) pith))
  ++  is-bunted
    (~(nest ut -:!>(~)) | p:vase)
  ++  default-kook
    ^-  kook:neo
    |%
    ++  state  pro/stud
    ++  poke   (sy stud ~)
    ++  kids   *kids:neo
    ++  deps   *deps:neo
    ++  form
      ^-  form:neo
      |_  [=bowl:neo =saga:neo]
      ++  poke
        |=  [s=stud:neo vax=^vase]
        ^-  (quip card:neo pail:neo)
        `q.saga
      ++  init
        |=  pal=(unit pail:neo)
        ^-  (quip card:neo pail:neo)
        `(need pal)
      --
    --
  ::
  ++  kook
    ^-  kook:neo
    ~|  kook/pith
    ~|  ~(key by ~(tar of:neo loam))
    =/  vax  vase
    ?:  is-bunted
      default-kook
    !<(kook:neo vax)
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
++  lib
  |_  =stud:ford:neo
  ++  pith  (~(pith press lib/stud) %out)
  ++  path  (pout pith)
  ++  built
    !=(~ (~(peek plow:aux loam) p/our.bowl pith))
  ++  exists
    =/  pax  path
    (exists-file (pout (~(pith press lib/stud) %src)))
  --
++  all-grab
  |=  grab=stud:neo
  ^-  vase :: of $-([to=stud grab-type] vase)
  =/  in=vase  ~(get pro grab)
  =/  dive=vase  !>(dive)
  %+  slap
    %+  with-faces:ford:neo  get-reef
    :~  in/in
        dive/dive
        loam/!>(loam)
        farm/!>(farm)
        con/!>(con)
        grab/!>(grab)
    ==
  !,  *hoon
  |=  [to=stud:neo in=in]
  ^-  vase
  =/  =stud:neo  
    ~|  missing-con/[grab to]
    (~(got by con.dive) [grab %$ to])
  =/  conv  ~(do con stud)
  (slym run:conv in)
::  
++  all-grow
  |=  grow=stud:neo
  ^-  vase :: of $-(pail grow-type)
  =/  out=vase  ~(get pro grow)
  %+  slap
    %+  with-faces:ford:neo  get-reef
    :~  out/out
        dive/!>(dive)
        grow/!>(grow)
        loam/!>(loam)
        farm/!>(farm)
        con/!>(con)
    ==
  !,  *hoon
  |=  =pail:neo
  ^-  out
  ~!  p.pail
  ~!  grow
  =/  =stud:neo  
    ~|  missing-con/[p.pail grow]
    (~(got by con.dive) [p.pail %$ grow])
  =/  conv  ~(do con stud)
  !<(out (slam run:conv q.pail))
::  
::
++  con
  |_  =stud:neo
  ++  do  
    =/  vax=vase  
      q.q:(need fil:(need (need (~(peek till:aux [loam farm]) %x [p/our.bowl pith]))))
    ~|  con-pith/pith
    |%
    ++  grab  !<(stud:neo (slot 4 vax))
    ++  thru  ~|  pith  !<(stud:neo (slot 10 vax))
    ++  grow  !<(stud:neo (slot 11 vax))
    ++  run   (slot 3 vax)
    ++  sink
      ^+  dive
      %_  dive
        by-grab  (~(put ju by-grab.dive) grab [thru grow])
        by-grow  (~(put ju by-grow.dive) grow [thru grab])
        con      (~(put by con.dive) [grab thru grow] stud)
      ==
    ::
    ++  vale
      ^-  ?
      =;  rap=(trap ?)
        =/  res  (mule rap)
        ?:  ?=(%& -.res)
          p.res
        %-  (slog leaf/"mark-vale" p.res)
        |
      |.  ^-  ?
      =/  src=vase  ~(get pro grab)
      =/  dst=vase  ~(get pro grow)
      =/  need=type  
        =<  p
        %+  slap  (with-faces:ford:neo get-reef src/src dst/dst ~)
        !,(*hoon *$-(src dst))
      =/  have=type  -:(slot 3 vax)
      (~(nest ut need) & have)
    --
  ++  pith  (~(pith press con/stud) %out)
  ++  path  (pout pith)
  --

::
++  pro
  |_  =stud:neo
  ++  get  grab
  ++  grab
    ~|  pro-grab/stud
    q:(need (~(peek plow:aux loam) p/our.bowl pith))
  ++  built
    !=(~ (~(peek plow:aux loam) p/our.bowl pith))
  ++  pith  (~(pith press pro/stud) %out)
  ++  exists  (exists-file (~(path press pro/stud) %src))
  -- 
::
++  press
  |_  =post:neo
  ++  disk  ^-  disk:neo  ?@(q.post ~ +.q.post)
  ++  stud   q.post
  ++  eject
    |=  =pith:neo
    ^-  [kind:ford:neo post:neo pith:neo]
    ~|  ejecting/pith
    =^  =disk:neo  pith  
      ?>  ?=([%cod *] pith)
      (eject:floppy t.pith)
    ?>  ?=([kind:ford:neo tack:neo @ *] pith)
    =/  =kind:ford:neo  i.pith
    =/  =tack:neo  i.t.pith
    :+  kind  [tack ?@(disk i.t.t.pith [i.t.t.pith ship.disk term.disk])]
    t.t.t.pith
    
  ++  slip
    |=  [=kind:ford:neo pax=pith:neo]
    =/  [@ p=post:neo =pith:neo]
      (eject pax)
    (~(pith press p) kind)
  ++  path
    |=  =kind:ford:neo
    (pout (pith kind))
  ::
  ++  pith
    |=  =kind:ford:neo
    :-  %cod
    %+  welp  ~(pith floppy disk)
    :-  kind
    :-  p.post
    =-  ~[-]
    ?@  q.post  q.post
    mark.q.post
  --
::
++  floppy
  |_  =disk:neo
  ++  eject
    |=  =pith:neo
    ^-  [disk:neo pith:neo]
    ?:  ?=([%std *] pith)
      [~ t.pith]
    ?>  ?=([[%p @] @ *] pith)
    [[+.i.pith i.t.pith] t.t.pith]
  ++  pith
    ^-  pith:neo
    ?@  disk
      #/std
    [p/ship.disk term.disk ~]
  --
::
++  root
  /(scot %p our.bowl)/[q.byk.bowl]/(scot %da now.bowl)/neo
++  exists-file
  |=  pax=path
  =/  p=path
    (welp root pax)
  =.  p  (snoc p %hoon)
  .^(? %cu p)
++  get-reef
  q:(need (~(peek plow:aux loam) #/[p/our.bowl]/out/reef))
::
++  copy-clay
  !:
  ~>  %bout.[1 %build]
  |^  ^+  run
  =/  paths=(list path)
    .^((list path) %ct root)
  ~&  paths/paths
  =.  paths
    %+  turn  paths
    |=  pax=path
    ?>  ?=(^ pax)
    t.pax
  |-
  ?~  paths
    finalize
  =.  run  (read-file i.paths)
  $(paths t.paths)
  ::  +finalize: register conversion
  ++  finalize
    =.  ripe  &
    =/  base=pith:neo  /cod/std/out/con
    =/  cons  
      ~(tap by ~(tar of:neo ~(snip of:neo (~(dip of:neo tide) base))))
    |- 
    ?~  cons
      =.  run  gen-grab
      gen-grow
    =/  [p=pith:neo *]  i.cons
    =/  =stud:neo
      ?>  ?&(?=(^ p) ?=(@ i.p))
      i.p
    =.  dive  sink:~(do con stud)
    $(cons t.cons)
  ::
  ++  gen-grab
    =/  grabs  ~(tap in ~(key by by-grab.dive))
    ~&  genning/grabs
    |-  
    ?~  grabs
      run
    =/  =vase  (all-grab i.grabs)
    =.  run  (make-riff (welp #/cod/grab (stud-to-pith:neo i.grabs)) vase)
    $(grabs t.grabs)
  ::
  ++  gen-grow
    =/  grows  ~(tap in ~(key by by-grow.dive))
    ~&  genning-grows/grows
    |-  
    ?~  grows
      run
    =/  =vase  (all-grow i.grows)
    =.  run  (make-riff (welp #/cod/grow (stud-to-pith:neo i.grows)) vase)
    $(grows t.grows)
  ::
  ++  has-modified
    |=  [txt=@t pax=pith:neo]
    ?.  ripe
      &
    ?~  pal=(~(peek plow:aux loam) [p/our.bowl pax])
      &
    !=(txt q.q.u.pal)
  ++  read-txt
    |=  pax=path
    ~&  reading-txt/pax
    =+  .^(src=@t %cx `path`(welp root pax))
    =.  pax  (snip pax)
    =.  run  (write-txt pax src)
    =.  run  (ford-text (slip:press %out pax) pax)
    run
  ::
  ++  read-file
    |=  pax=path
    ^+  run
    ~&  reading/pax
    ?.  =((rear pax) %hoon)
      (read-txt pax)
    =+  .^(src=@t %cx `path`(welp root pax))
    ?.  (has-modified src (pave:neo (snip pax)))
      run
    ~?  >>>  ripe
      [%update pax]
    =/  =file:ford:neo
      ~|  parsing/pax
      (scan (trip src) (rein:ford:neo [our.bowl (pave:neo (snip pax))]))
    ~&  [lib=lib pro=pro]:file
    =/  has-imports=?
      ?&  (levy pro.file |=(pro:ford:neo ~(exists pro stud)))
          (levy lib.file |=(lib:ford:neo ~(exists lib stud)))
      ==
    ?.  has-imports
      ~|  pro.file
      ~|  lib.file
      ~|  %no-imports
      !!
    =.  run  (build-pros (turn pro.file tail))
    =.  run  (build-libs (turn lib.file tail))
    =.  run  (build-fils (turn fil.file tail))
    =.  run  (build-fars (turn far.file tail))
    =.  run  (build-fals (turn fal.file tail))
    ::  =.  run  (build-fils (turn lib.file tail))
    =/  built-imports=?
      ?&  (levy pro.file |=(pro:ford:neo ~(built pro stud)))
          (levy lib.file |=(lib:ford:neo ~(built lib stud)))
      ==
    ~|  ~(key by ~(tar of:neo loam))
    ~|  imports/file(hoon *hoon)
    ?>  built-imports
    =^  pre=pith  run  
      (make-prelude (snip pax) file)
    =/  =conf:neo
      (~(gas by *conf:neo) [%sut (ours pre)] ~)
    =.  run  (write-hoon (snip pax) src)
    =/  pit  (src-to-out (snip pax))
    (ford-slap (src-to-out pax) pre (snip pax))
  ++  build-fils
    |=  pos=(list stud:neo)
    ^+  run
    ?~  pos
      run
    =/  pat  
      (~(path press fil/i.pos) %src)
    ?:  ~(built pro i.pos)
      $(pos t.pos)
    =+  .^(=arch %cy (welp root pat))
    ~|  pat/pat
    =/  ext  (snag 0 ~(tap in ~(key by dir.arch)))
    =.  run  (read-txt (snoc pat ext))
    $(pos t.pos)
  ::
  ++  build-pros
    |=  pos=(list stud:neo)
    ^+  run
    ?~  pos
      run
    =/  pat  
      (~(path press pro/i.pos) %src)
    ?:  ~(built pro i.pos)
      $(pos t.pos)
    =.  run  (read-file (snoc pat %hoon))
    $(pos t.pos)
  ::
  ++  build-fals
    |=  pos=(list stud:neo)
    ^+  run
    ?~  pos
      run
    =/  pat  
      (welp #/cod/grab (stud-to-pith:neo i.pos))
    ?:  !=(~ (~(peek plow:aux loam) p/our.bowl pat))
      $(pos t.pos)
    =.  run  (on-dirt-card (do-grow-our pat vase/=>(..zuse !>(|=(~ *vase)))))
    $(pos t.pos)
  ::
  ++  build-fars
    |=  pos=(list stud:neo)
    ^+  run
    ?~  pos
      run
    =/  pat  
      (welp #/cod/grow (stud-to-pith:neo i.pos))
    ?:  !=(~ (~(peek plow:aux loam) p/our.bowl pat))
      $(pos t.pos)
    =?  run   !~(built pro i.pos)
      (build-pros ~[i.pos])
    =/  grow=vase  ~(get pro i.pos)
    =/  sut  (with-faces:ford:neo get-reef grow/grow ~)
    =.  run  (on-dirt-card (do-grow-our pat vase/(slap sut !,(*hoon |=(* *grow)))))
    $(pos t.pos)
  ::
  ++  build-libs
    |=  lis=(list stud:ford:neo)
    ^+  run
    ?~  lis
      run
    =/  pat  
      (~(path press lib/i.lis) %src)
    ?:  ~(built lib i.lis)
      $(lis t.lis)
    =.  run  (read-file (snoc pat %hoon))
    $(lis t.lis)
  ++  do-make
    |=  [=pith:neo lib=term sta=(unit pail:neo) =conf:neo]
    =/  =name:neo  [our.bowl pith]
    ~|  conf/conf
    ~|  make-name/name
    (on-card (en-pith:name:neo name) %make lib sta conf)
  ::
  ++  ford-slap
    |=  [wer=pith sut=pith src=pith]
    %^  do-make  wer  %ford-slap
    `(~(gas by *conf:neo) sut/(ours sut) hoon/(ours src) ~)
  ::
  ++  ford-text
    |=  [wer=pith txt=pith]
    %^  do-make  wer  %ford-text
    `(~(gas by *conf:neo) txt/(ours txt) ~)
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
      (do-make fav %term `term/!>(face.i.deps) ~)
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
    |=  =file:ford:neo
    ^-  (list [term pith])
    %-  zing
    :~  (turn pro.file |=(p=pro:ford:neo [face.p ~(pith pro stud.p)]))
        (turn fil.file |=(f=fil:ford:neo [face.f (~(pith press fil/stud.f) %out)]))
        (turn lib.file |=(l=lib:ford:neo [face.l (~(pith press lib/stud.l) %out)]))
        (turn far.file |=(f=far:ford:neo [face.f (welp #/cod/grow (stud-to-pith:neo stud.f))]))
        (turn fal.file |=(f=fal:ford:neo [face.f (welp #/cod/grab (stud-to-pith:neo stud.f))]))
    ==
  ++  make-prelude
    |=  [pax=pith =file:ford:neo]
    ^-  [pith _run]
    =/  pre-path=pith  
      (slip:press %pre pax)
    [pre-path (make-deps pre-path (file-to-deps file))]
  ++  write-hoon
    |=  [pax=pith fil=@t]
    (do-make pax %hoon `hoon/!>(fil) ~)
  ++  write-txt
    |=  [pax=pith fil=@t]
    (do-make pax %txt `txt/!>(fil) ~)

  ++  src-to-out
    |=  pax=pith:neo
    ^-  pith:neo
    (slip:press %out pax)
  --
::
++  boot
  |^  ^+  run
  =.  run-nonce  eny.bowl
  =+  .^(neo-vase=vase %ca (welp clay-beak /sur/neo/hoon))
  =/  reef=vase  (slop !>(..zuse) neo-vase(p [%face %neo p.neo-vase]))
  =/  riff=pail:neo  [%vase !>(riff-kook)]
  =.  run  (on-dirt-card (do-grow-our (pess imp/%ford-riff) riff))
  =.  run  (on-dirt-card (do-grow-our (pess imp/%txt) vase/!>(~)))
  =.  run  (make-riff #/out/reef reef)
  =.  run  (re-export reef %hoon !,(*hoon @t))
  =.  run  (re-export reef %txt !,(*hoon @t))
  =.  run  (re-export reef %desk !,(*hoon desk))
  =.  run  (make-riff (pess pro/%vase) (vase-pro reef))
  =.  run  (make-riff (pess pro/%ford-in) (ford-in reef))
  =.  run  (make-riff (pess pro/%term) (term reef))
  =.  run  (make-riff-slap (pess imp/%hoon) reef txt-hoon-imp)
  =.  run  (make-riff-slap (pess imp/%term) reef txt-term-imp)
  =.  run  (make-riff-slap (pess imp/%ford-same) reef txt-ford-same)
  =.  run  (make-riff-slap (pess imp/%ford-face) reef txt-ford-face)
  =.  run  (make-riff-slap (pess imp/%ford-slop) reef txt-ford-slop)
  =.  run  (make-riff-slap (pess imp/%ford-text) reef txt-ford-text)
  =.  run  (make-riff-slap (pess imp/%ford-slap) reef txt-ford-slap)
  =.  run  (re-export reef %json !,(*hoon json))
  =.  run  (re-export reef %mime !,(*hoon mime))
  =.  run  copy-clay
  ::  =.  run  (emit %pass /bind-site %arvo %e %connect [~ dap.bowl ~] dap.bowl)
  =.  run  (emit do-std-warp)
  =.  run  
    (emit (do-card #/[p/our.bowl]/sky %make %sky `sky/!>([%system [~[%home] ~] 1]) ~))
  =.  run  
    (emit (do-card #/[p/our.bowl]/srv/hawk %make %hawk-eyre ~ ~))
  =.  run  
    (emit (do-card #/[p/our.bowl]/srv/sky %make %sky-eyre ~ ~))
  run
  ++  pess  |=(=post:neo (~(pith press post) %out))
  ++  clay-beak  ^-  path
    /(scot %p our.bowl)/[q.byk.bowl]/(scot %da now.bowl)
  ++  ford-slip
    ^-  dock:neo
    [pro/%vase ~ ~]
  ++  make-riff-slap
    |=  [wer=pith:neo reef=vase txt=@t]
    ~|  wer
    =;  =vase
      (make-riff wer vase)
    =+  vaz=(vang & (pout wer))
    %+  slap  reef
    (scan (trip txt) (full (ifix [gay gay] tall:vaz)))
  ::
  ++  riff-kook
    ^-  kook:neo
    |%
    ++  state  pro/%vase
    ++  poke   *(set stud:neo)
    ++  kids  ~
    ++  deps  ~
    ++  form
      ^-  form:neo
      |_  [=bowl:neo =aeon:neo =pail:neo]
      ++  poke
        |=  pok=pail:neo  
        ^-  (quip card:neo pail:neo)
        `pail
      ::
      ++  init
        |=  old=(unit pail:neo)
        ^-  (quip card:neo pail:neo)
        `(need old)
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
  ++  vase-pro
    |=  reef=vase
    ^-  vase
    %+  slap  reef
    !,  *hoon
    ,vase
  ::
  ++  ford-in
    |=  reef=vase
    ^-  vase
    %+  slap  reef
    !,(*hoon ,~)
  ::
  --
++  make-riff
  |=  [=pith riff=vase]
  ^+  run
  =.  pith  [p/our.bowl pith]
  (on-card pith %make %ford-riff `vase/riff ~)

++  seize
  |=  [par=pith:neo child=pith:neo car=?(%y %z)]
  ^-  ?
  ?:  =(%y car)
    =(par (~(parent of:neo tide) child))
  !=(~ (dif:pith:neo par child))
::
:: +abduct: check capture
++  abduct
  |=  [par=pith:neo child=pith:neo]
  ^-  ?
  ?~  wav=(~(get of:neo tide) par)
    |
  ?~  kids.dock.u.wav
    |
  (seize par child p.u.kids.dock.u.wav)
::  +adopt: produce all capturing parents
::
++  adopt
  =|  here=pith:neo
  =|  res=(set pith:neo)
  |=  =pith:neo
  |-  ^+  res
  =?  res  (abduct here pith)
    (~(put in res) here)
  =/  nex  (dif:pith:neo here pith)
  ?~  nex
    res
  $(here (snoc here i.nex))
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
          smut=(list dust:neo)  :: total changelist
          grit=(list dust:neo)  :: changelist not gifted
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
      =.  run    (emil `(list card)`(do-ack [p p.q]:init-move err.block))
      =.  run    (emil (turn up do-move))
      (dial smut)
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
    (emil (do-ack [p p.q]:init-move err.block))
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
    =^  cards=(list card:neo)  arvo
      (soft-surf |.(su-abet:(su-give:surf gift)))
    (ingest cards)
  ::
  ++  plunder
    ^+  arvo
    =/  by-parent=(jug pith:neo dust:neo)
      %+  roll  grit
      |=  [=dust:neo by-parent=(jug pith:neo dust:neo)]
      %-  ~(gas ju by-parent)
      (turn ~(tap in (adopt pith.dust)) |=(=pith:neo [pith [(dif:pith:neo pith pith.dust) +.dust]]))
    :: XX: assert gifts empty
    =.  gifts
      %+  turn  (sort ~(tap in ~(key by by-parent)) sort:pith:neo)
      |=  =pith:neo
      ^-  [pith:neo gift:neo]
      [pith (gas-gift ~(tap in (~(get ju by-parent) pith)))]
    =.  smut  (welp smut grit)
    =.  grit   ~
    give
  ::
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
  ++  grow
    |=  =pail:neo
    ^+  arvo
    =^  git=grit:neo  run
      (take-dirt-card [p/our.bowl here] %grow pail ~ *oath:neo)
    =.  grit  (welp grit git)
    arvo
  ++  cull
    ^+  arvo
    =^  git=grit:neo  run
      (take-dirt-card [p/our.bowl here] %cull ~)
    =.  grit  (welp grit git)
    work
  ::
  ++  work
    ^+  arvo
    |-  ^+  arvo
    ?^  err.block
      arvo
    ?~  down
      plunder
    =/  nex=move:neo  i.down
    =/  new-arvo  (apply:arvo(down t.down) nex) :: XX: weird compiler?
    $(arvo new-arvo, done (snoc done nex))
  ++  poke
    |=  =pail:neo
    ^+  arvo ::    
    =^  cards=(list card:neo)  arvo
      (soft-surf |.(su-abet:(su-poke:surf pail)))
    (ingest cards)
  ::
  ::  XX: a hack
  ::
  ::    this is implicity recursive, and all external dependencies of
  ::    the children need to be woken up. this also breaks referential 
  ::    transparency
  ++  tomb
    |=  *
    ::  =.  apex  (del:of-top here)
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
      %cull  cull
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
  ++  jazz
    |=  [=conf:neo =deps:neo]
    ^-  [bad=(set term) block=(set tour:neo)]
    %+  roll  ~(tap by deps)
    |=  [[=term required=? =quay:neo] bad=(set term) block=(set hunt:neo)]
    =/  =care:neo  (get-care:quay:neo quay)
    ?:  &(required !(~(has by conf) term))
      :_(block (~(put in bad) term))
    ?:  &(!required !(~(has by conf) term))
      [bad block]
    =/  pit=pith:neo   (~(got by conf) term)
    =/  res  (look care pit)
    =/  nam=name:neo  (de-pith:name:neo pit)
    ?~  res  
      ?:  =(our.bowl ship.nam)
        ?.  required
          [bad block]
        :_(block (~(put in bad) term))
      [bad (~(put in block) care pit)]
    ?~  u.res
      :_(block (~(put in bad) term))
    [bad block] ::
  ::
  ++  dance
    |=  [=crew:neo =band:neo]
    ^+  arvo
    =/  cew  ~(tap by crew)
    |-
    ?~  cew
      arvo
    =/  [=term =pith:neo]  i.cew
    =/  d=(unit [req=? =quay:neo])  (~(get by band) term)
    ::  skip extraneous, XX: is correct?
    ?~  d  
      $(cew t.cew)
    =/  [req=? =quay:neo]  u.d
    =/  =hunt:neo  [(get-care:quay:neo quay) pith]
    =/  =name:neo  (de-pith:name:neo pith)
    ?:  &(req =(~ (moor quay name)))
      ~|  bad-dance/[term name]
      !!
    =.  run  (stalk:rage hunt rely/[term here])
    $(cew t.cew)
  ::
  ++  validate-kids
    ^-  ?
::  ?:  =(1 1)
::    &
::  ?~  par-pith=(parent:of-top here)
::    & :: XX: review
::  =/  parent=room:neo  (got:of-top u.par-pith)
::  =/  parent-firm=firm:neo  ~(firm husk code.parent)
::  =/  sfix  (sub:pith:neo here u.par-pith)
::  ?~  mat=(find:peon:neo sfix ~(key by kids:parent-firm))
::    ~&  >>>  %kids-no-match
::    &
    & :: XX: enforce conformance
  ++  make-plot
    |=  [src=stud:neo =conf:neo]
    work
  ::
  ++  make
    |=  [src=stud:neo init=(unit pail:neo) =crew:neo]
    =/  =wave:neo  [src ~(dock husk src) crew]
    =.  tide  (~(put of:neo tide) here wave)
    =^  bad=(set term)   get.block
      (jazz crew deps:~(kook husk src))
    ?.  =(~ get.block)
      arvo
    ?.  =(~ bad)
      ~|  make-no-dep/~(tap in bad)
      !!
    =.  arvo  (dance crew deps:~(kook husk src))
    =^  cards=(list card:neo)  arvo
      (soft-surf |.(su-abet:(su-make:surf init)))
    (ingest cards)
::  ?:  ~(is-plot husk src)
::    ~|  %cant-make-plot-w-init
::    ?>  ?=(~ init)
::    (make-plot src conf)
::     =/  =firm:neo  ~(firm husk src)
    :: =.  run        (~(start husk src) our.bowl pith)
    ::  =/  old  (get:of-top here)
::     =/  =form:neo  form:firm
:: `arvo

  ++  soft-surf
    |=  tap=(trap (quip card:neo _arvo))
    ^-  (quip card:neo _arvo)
    ::  do not virtualise fastboot
    ?:  &(!ripe =((de-pith:name:neo sys-pith) src.init))
      (tap)
    =/  res=(each (quip card:neo _arvo) tang)
      (mule tap)
    ?:  ?=(%& -.res)
      p.res
    =.  err.block  `[%goof p.res]
    `arvo
  ++  surf
    =/  =wave:neo  (~(got of:neo tide) here)
    =|  cards=(list card:neo)
    =/  =kook:neo  ~(kook husk code.wave)
    |%
    ++  su-core  .
    ++  su-emil  |=(caz=(list card:neo) su-core(cards (welp cards caz)))
    ++  su-bowl  
      =/  hare  [p/our.bowl here]
      ^-  bowl:neo
      :*  src
          our.bowl 
          hare 
          hare 
          now.bowl
          eny.bowl
          su-deps 
          su-kids
      ==
    ++  su-icon
      [p.p q.q ~ ~]:su-saga
    ++  su-saga
      (dall:aux (lexe:aux (~(peek till:aux [loam farm]) %x [p/our.bowl here])) *saga:neo)
    ++  su-pail  q:su-saga
    ++  su-kids
      =/  kids  kids:kook
      ?~  kids
        *lore:neo
      %-  gas-lore
      =/  child  (dall:aux (~(peek till:aux [loam farm]) p.u.kids [p/our.bowl here]) *epic:neo)
      %+  murn  ~(tap by ~(tar of:neo child))
      |=  [=pith:neo =saga:neo]
      ^-  (unit [pith:neo idea:neo])
      ?~  ion=(scion q.u.kids pith saga)
        ~
      `[pith u.ion]
    ++  su-deps
      :: =-  ((slog (deps:dbug:neo -) ~) -)
      %-  ~(gas by *(map term [pith lore:neo]))
      ^-  (list [term pith lore:neo])
      %+  murn  ~(tap by deps:kook)
      |=  [=term required=? =quay:neo]
      ^-  (unit [^term pith:neo lore:neo])
      =/  dep=(unit pith)  (~(get by crew.wave) term)
      ?~  dep
        ~|  invariant-missing-required-conf/term
        ?<  required
        ~
      =/  =name:neo  (de-pith:name:neo u.dep)
      =/  =care:neo  (get-care:quay:neo quay)
      ?~  lor=(moor quay name)
        ?<  required
        ~
::      %-  (slog term (epic:dbug:neo epic) ~)
      `[term u.dep u.lor]
    ::
    ++  su-form  ~(. form:kook [su-bowl su-saga])
    ++  su-abet ::  TODO: bump
      =.  tide  (~(put of:neo tide) here wave)
      [cards arvo]
    ++  su-make
      |=  init=(unit pail:neo)
      =/  [cards=(list card:neo) new=pail:neo]
        (init:su-form init)
      =.  su-core  (su-emil cards)
      (su-grow new)
    ++  su-grow
      |=  =pail:neo
      ^+  su-core
      ::  ?>(check-pail) XX: TODO
      =.  arvo  (grow pail)
      su-core
    ::
    ++  su-give
      |=  =gift:neo
      ?.  (~(has in poke.dock.wave) %gift)
        ~&  skipping-give/here
        su-core
      (su-poke gift/!>(gift))
    ::
    ++  su-poke
      |=  =pail:neo
      =/  [caz=(list card:neo) new=pail:neo]
        (poke:su-form pail)
      =.  su-core  (su-emil caz)
      ?:  =(new su-saga)
        su-core
      (su-grow new)
    --
  --
::  |sys: external interfaces
+|  %sys
++  sys
  |%
  ++  take-next-clay
    |=  syn=sign-arvo
    ?>  ?=([?(%clay %behn) %writ *] syn)
    =.  run  (emit do-std-warp)
    ?~  p.syn
      ~&  next-clay-gone/syn
      run
    copy-clay
  ::
  ++  on-move
    |=  [src=pith:neo dst=pith:neo =note:neo]
    ^+  run
    ?+  dst  !!
      [%clay *]  (call:silt src t.dst note)
      [%iris *]  (call:cttp src t.dst note)
      [%behn *]  (call:bide src t.dst note)
      [%gall *]  (call:rile src t.dst note)
      [%eyre *]  (call:sttp src t.dst note)
    ==
  ++  take-arvo
    |=  [=(pole knot) syn=sign-arvo]
    ^+  run
    ?+  pole  ~|(bad-sys-take/pole !!)
      [%behn %wait rest=*]  (take-wait:bide rest.pole syn)
      [%clay %peer rest=*]  (take-peer:silt rest.pole syn)
      [%eyre %bind rest=*]  (take-bind:sttp rest.pole syn)
      [%iris %req rest=*]   (take-res:cttp rest.pole syn)
    ==
  --
++  silt
  |%
  ++  call
    |=  [src=pith:neo dst=pith:neo =note:neo]
    ?>  ?=(%poke -.note) :: XX: all shanes should be virtualised and hand deliver acks
    ?>  ?=(%clay-req p.pail.note)
    =+  !<(=req:clay:neo q.pail.note)
    ?-    -.req
        %pull
      =.  clay.unix  (~(del by clay.unix) [src pith.req])
      run
    ::
        %peer
      =+  .^(=cass:clay %cw /(scot %p our.bowl)/[desk.peer.req]/(scot %da now.bowl)/sys/kelvin)
      =/  [case=@ud =peer:clay:neo]  (~(gut by clay.unix) [src pith.req] [0 peer.req])
      =.  case  ud.cass
      =.  clay.unix  (~(put by clay.unix) [src pith.req] [case peer])
      (emit (do-peer src pith.req))
    ==
  ++  take-peer
    |=  [wir=(pole knot) syn=sign-arvo]
    ?>  ?=(%writ +<.syn)
    =/  paxs=(pole pith:neo)
      (de:drive:neo (pave:neo wir))
    ?>  ?=([src=* hand=* ~] paxs)
    =/  src=pith  src.paxs
    =/  hand=pith  hand.paxs
    ?~  cas=(~(get by clay.unix) [src hand])
      run
    =/  [case=@ud =peer:clay:neo]  u.cas
    =.  case  +(case)
    ?~  p.syn
      ~&  empty-clay-res/wir
      run
    =+  !<(kids=(list path) q.r.u.p.syn)
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
    ~&  res/~(key by ~(tar of res))
    =.  res  (~(dip of res) path.peer)
    ~&  res/~(key by ~(tar of res))
    =/  =note:neo  [%poke %clay-res !>(`res:clay:neo`[hand case res])]
    ~&  sending-to/src
    =/  =move:neo  [[p/our.bowl #/$/clay] src note]
    =/  =wire      (welp /sys/clay/res wir)
    =.  clay.unix  (~(put by clay.unix) [src hand] [case peer])
    =.  run  (emit (do-move move))
    (emit (do-peer src hand))
  ::
  ++  do-peer
    |=  [src=pith:neo hand=pith:neo]
    ^-  card
    =/  [case=@ud =peer:clay:neo]  (~(got by clay.unix) [src hand])
    =/  =wire  (welp /sys/clay/peer (pout (en:drive:neo ~[src hand])))
    =/  =rave:clay  [%sing [%t ud/case path.peer]]
    (pass wire %arvo %c %warp our.bowl desk.peer `rave)
  --
++  rile
  |%
  ++  here
    `pith:neo`#/[p/our]/$/gall
  ++  gent
    |_  =pith:neo
    ++  here  (welp ^here pith)
    ++  on-start-peek
      |=  [src=pith:neo freq=@dr]
      ^+  run
      =/  =peek:gall:neo  
        (~(gut by peek.gall.unix) pith [~ ~h24])
      =.  refresh.peek  (min freq refresh.peek)
      =/  new=?  =(~ src.peek)
      =.  src.peek  (~(put in src.peek) src)
      =.  peek.gall.unix  (~(put by peek.gall.unix) pith peek)
      ?.  new
        run
      =.  run  on-read-peek
      (emit (do-peek-timer refresh.peek))
    ++  on-stop-peek
      |=  src=pith:neo
      ^+  run
      =/  =peek:gall:neo  
        (~(gut by peek.gall.unix) pith [~ ~h24])
      =.  src.peek  (~(del in src.peek) src)
      =.  peek.gall.unix
        ?:  =(~ src.peek)
          (~(del by peek.gall.unix) pith)
        (~(put by peek.gall.unix) pith peek)
      run
    ::
    ++  do-peek-timer
      |=  freq=@dr
      ^-  card
      =/  wir  (welp /sys/gall/peek (pout pith))
      (pass wir %arvo %b %wait (add now.bowl freq))
    ++  on-read-peek
      =/  =road:neo  pith
      ?>  ?=([dude=@ rest=*] road)
      =/  pax  
        %+  welp  /(scot %p our.bowl)/[dude.road]/(scot %da now.bowl)
        (pout rest.road)
      =/  =pail:neo  noun/!>(.^(* %gx pax))
      =.  run  (on-dirt-card here %grow pail ~ *oath:neo)
      run

    ++  on-wake-peek
      |=  =pith:neo
      =/  =peek:gall:neo  (~(gut by peek.gall.unix) pith [~ ~h24])
      ?:  =(~ src.peek)
        run
      =.  run  (emit (do-peek-timer refresh.peek))
      on-read-peek
    --
  ::
  ++  call
    |=  [src=pith:neo dst=pith:neo =note:neo]
    ?>  ?=(%poke -.note) :: XX: all shanes should be virtualised and hand deliver acks
    ?>  ?=(%gall-req p.pail.note)
    =+  !<(=req:gall:neo q.pail.note)
    =*  gen  ~(. gent dst)
    ?+  -.req  !!
      %peek  (on-start-peek:gen src p.req)
      %keep  (on-stop-peek:gen src)
    ==
  --

++  bide
  |%
  ++  call
    |=  [src=pith:neo dst=pith:neo =note:neo]
    ?>  ?=(%poke -.note) :: XX: all shanes should be virtualised and hand deliver acks
    ?>  ?=(%behn-req p.pail.note)
    =+  !<(=req:behn:neo q.pail.note)
    ?-    -.req
        %rest
      =/  =wire  /sys/behn/wait/(scot %da p.req)
      =.  behn.unix  (~(del ju behn.unix) p.req src)
      ?.  =(~ (~(get ju behn.unix) p.req))
        run
      (emit %pass wire %arvo %b %rest p.req)
    ::
        %wait
      =/  =wire  /sys/behn/wait/(scot %da p.req)
      =.  behn.unix  (~(put ju behn.unix) p.req src)
      ?.  =(1 ~(wyt in (~(get ju behn.unix) p.req)))
        run
      (emit %pass wire %arvo %b %wait p.req)
    ==
  ++  take-wait
    |=  [wir=(pole knot) syn=sign-arvo]
    ?>  ?=([da=@ ~] wir)
    ?>  ?=([%behn %wake *] syn)
    =/  =time  (slav %da da.wir)
    =/  timers  ~(tap in (~(get ju behn.unix) time))
    |-  
    ?~  timers
      =.  behn.unix  (~(del by behn.unix) time)
      run
    =/  src=pith:neo  #/[p/our.bowl]/$/sys/behn
    =/  =res:behn:neo  +.syn
    (emit (do-move src i.timers %poke behn-res/!>(res)))
  --
++  sttp
  |%
  ++  take-bind
    |=  *
    run
  ::
  ++  call
    |=  [src=pith:neo dst=pith:neo =note:neo]
    ?>  ?=(%poke -.note) :: XX: all shanes should be virtualised and hand deliver acks
    ?+  p.pail.note  ~|(bad-eyre-call/p.pail.note !!)
      %eyre-req    (on-eyre-req !<(req:eyre:neo q.pail.note))
      %eyre-sign  (on-eyre-sign src !<(sign:eyre:neo q.pail.note))
    ==
    +$  request-line
      $:  [ext=(unit @ta) site=(list @t)]
          args=(list [key=@t value=@t])
      ==
    ::  +parse-request-line: take a cord and parse out a url
    ::
    ++  parse-request-line
      |=  url=@t
      ^-  request-line
      (fall (rush url ;~(plug apat:de-purl:html yque:de-purl:html)) [[~ ~] ~])
    ::
    ++  on-eyre-req
      |=  [%connect =binding:eyre =pith:neo]
      ?>  =(~ site.binding)
      =.  bind.eyre.unix  (~(put by bind.eyre.unix) binding pith)
      =/  wir=wire  (welp /sys/eyre/bind (pout pith))
      (emit %pass wir %arvo %e %connect binding dap.bowl)
    ::
    ++  on-eyre-sign
      |=  [src=pith:neo eyre-id=@ta =gift:eyre:neo]
      ^+  run
      ::  ?>  =(src (~(got by by-id.eyre.unix) eyre-id))
      =/  =path  /http-response/[eyre-id]
      =;  cag=(unit cage)
        ?~  cag  (give %kick ~[path] ~)
        (give %fact ~[path] u.cag)
      ?-  -.gift
        %head  `http-response-header/!>(response-header.gift)
        %data  `http-response-data/!>(dat.gift)
        %done  ~
      ==
    ++  match-binding
      =|  test=(list @t)
      |=  site=(list @t)
      ^-  (unit pith:neo)
      ?^  res=(~(get by bind.eyre.unix) [~ test])
        `u.res
      =/  nex  (slag (lent test) site)
      ?~  nex
        ~
      $(test (snoc test i.nex))
    ::
    ++  handle-http-request
      |=  [eyre-id=@ta req=inbound-request:eyre]
      ^+  run
      =/  lin=request-line   (parse-request-line url.request.req)
      ?~  bin=(match-binding site.lin)
        (emil (give-simple-payload:app:serv eyre-id not-found:gen:serv))
      =.  by-id.eyre.unix     (~(put by by-id.eyre.unix) eyre-id u.bin)
      =.  by-pith.eyre.unix  (~(put by by-pith.eyre.unix) u.bin eyre-id)
      =/  =card:neo  [u.bin %poke eyre-task/!>(`task:eyre:neo`[eyre-id req])]
      =/  =move:neo  [#/[p/our.bowl]/$/eyre card]
      (emit (do-move move))
    --

++  cttp
  |%
  ++  call
    |=  [src=pith:neo dst=pith:neo =note:neo]
    ?>  ?=(%poke -.note) :: XX: all shanes should be virtualised and hand deliver acks
    ?>  ?=(%iris-req p.pail.note)
    =+  !<(=req:iris:neo q.pail.note)
    =/  wir  (welp /sys/iris/req (pout (en:drive:neo ~[src hand.req])))
    =|  =outbound-config:iris
    (emit (pass wir %arvo %i %request dat.req outbound-config))

  ++  take-res
    |=  [wir=(pole knot) syn=sign-arvo]
    ?>  ?=([%iris %http-response *] syn)
    =/  paxs=(pole pith:neo)
      (de:drive:neo (pave:neo wir))
    ?>  ?=([src=* hand=* ~] paxs)
    =/  src=pith  src.paxs
    =/  hand=pith  hand.paxs
    =/  =pail:neo  iris-res/!>([hand +>.syn])
    (emit (do-move (welp #/[p/our]/$/sys/iris hand) src %poke pail))
  --

::  |util: utilties
+|  %util
++  puff
  |=  [want=stud:neo role=(unit stud:neo) have=saga:neo]
  ^-  (unit idea:neo)
  =;  pal=(unit pail:neo)
    ?~  pal  ~
    `[have ~ u.pal]
  ?:  =(want %pail)
    `q.have
  ?:  =(want p.q.have)
    `q.have
  ?:  =(want %sig)
    `sig/!>(~)
  =/  rol=stud:neo
    (fall role %$)
  ?~  can=(~(get by con.dive) [p.q.have rol want])
    ~
  =/  conv  run:~(do con u.can)
  `[want (slam conv q.q.have)]
::
++  plag
  =|  rol=(unit stud:neo)
  |=  [want=curb:neo have=saga:neo]
  ^-  (unit idea:neo)
  =*  loop   $
  =/  =stud:neo  p.q.have
  ?-    -.want
      %pro  
    (puff p.want rol have)
  ::
      %rol
    $(rol `p.want, want q.want)
  ::
      %only
    ?.  =(p.q.have p.want)
      ~
    `[have ~ q.have]
  ::
      %any
    ?>  =(~ rol) :: XX: not neccessary, but wat means
    `[have ~ q.have]
  ::
      %not
    ?.  =(~ loop(want p.want))
      ~
    loop(want q.want)
  ::
      %or
    |-
    ?~  p.want
      ~
    =/  nex  loop(want i.p.want)
    ?^  nex
      `u.nex
    $(p.want t.p.want)
  ==
::
::?.  (~(has by con.fiesta) [p.have want])
::  ~
::  
::=/  conv   run:~(do con (~(got by con.fiesta) [p.have want]))
::`[want (slam conv q.have)]
::
++  scion
  |=  [want=lads:neo =pith:neo =saga:neo]
  ^-  (unit idea:neo)

  ?~  pis=(find:peon:neo pith ~(key by want))
    ~
  =/  =lash:neo  (~(got by want) u.pis)
  (plag state.lash saga)
::
++  moor
  |=  [want=quay:neo =name:neo]
  ^-  (unit lore:neo)
  =/  =care:neo  (get-care:quay:neo want)
  =/  pic  (~(peek till:aux [loam farm]) care (en-pith:name:neo name))
  ?:  ?=($@(~ [~ ~]) pic)
    ~&  lost-moor/name
    ~
  =;  [fail=? res=(list (pair pith:neo idea:neo))]
    ?:  fail
      ~
    `(gas-lore res)
  %+  roll  ~(tap by ~(tar of:neo u.u.pic))
  |=  [[=pith:neo =saga:neo] [fail=_| res=(list (pair pith:neo idea:neo))]]
  ^+  +<+
  ?:  fail
    [fail ~]
  ?:  =(pith ~)
    ?~  rot=(plag state.p.want saga)
      &/~
    |/:_(res [*pith:neo u.rot])
  ?~  q.want
    |/res
  ?~  ion=(scion q.u.q.want pith saga)
     &/~
  |/:_(res [pith u.ion])
::
++  gas-leaf
  =|  =leaf:neo
  |=  lst=(list [pith:neo mode:neo])
  ^+  leaf
  ?~  lst
    leaf
  =.  leaf   (~(put of:neo leaf) i.lst)
  $(lst t.lst)
 
::
++  gas-epic
  =|  =epic:neo
  |=  lst=(list [pith:neo saga:neo])
  ^+  epic
  ?~  lst
    epic
  =.  epic   (~(put of:neo epic) i.lst)
  $(lst t.lst)
::
++  gas-gest
  =|  =gest:neo
  |=  lst=(list [pith:neo feat:neo])
  ^+  gest
  ?~  lst
    gest
  =.  gest   (~(put of:neo gest) i.lst)
  $(lst t.lst)

::
++  gas-gift
  =|  =gift:neo
  |=  lst=(list [pith:neo loot:neo])
  ^+  gift
  ?~  lst
    gift
  =.  gift   (~(put of:neo gift) i.lst)
  $(lst t.lst)

++  gas-lore
  =|  =lore:neo
  |=  lst=(list [pith:neo idea:neo])
  ^+  lore
  ?~  lst
    lore
  =.  lore   (~(put of:neo lore) i.lst)
  $(lst t.lst)

++  gas-yuga
  =|  =yuga:neo
  |=  lst=(list [pith:neo aeon:neo])
  ^+  yuga
  ?~  lst
    yuga
  =.  yuga   (~(put of:neo yuga) i.lst)
  $(lst t.lst)

::
++  epic-to-yuga
  |=  =epic:neo
 (gas-yuga (turn ~(tap of:neo epic) |=([p=pith:neo s=saga:neo] [p p.s])))
::
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
  ++  saga
    |=  s=saga:neo
    ^-  feat:neo
    [p.s (pail q.s)]
  ++  epic
    |=  =epic:neo
    ^-  gest:neo
    %-  gas-gest
    %+  turn  ~(tap of:neo epic)
    |=  [p=pith:neo s=saga:neo]
    [p (saga s)] ::

  --
++  harden
  |%
  ++  poke
    |=  raw=raw-poke:neo
    ^-  move:neo
    [p.p.raw q.p.raw %poke (vial q.raw)]
  ++  vial
    |=  =vial:neo
    ^-  pail:neo
    :-  p.vial
    (slym ~(get pro p.vial) q.vial)
  ::
  ++  feat
    |=  =feat:neo
    ^-  saga:neo
    [p.feat (vial q.feat)]
    ::  (slym (need ~(get pro p.vial)) q.vial)
  --
++  print-dbug-all
  |=  prefix=pith:neo
  ^-  tang
  =/  lom  (~(dip of:neo loam) prefix)
  =/  fam  (~(dip of:neo farm) prefix)
  =/  rav  (~(dip of:neo riot) prefix)
  :-  >fam<
  :-  >rav<
  %-  zing
  %+  turn   ~(tap by ~(tar of:neo lom))
  |=  [=pith:neo =soil:neo]
  :~  >pith<
      >~(key by soil)<
  ==
::
++  print-dbug
  |=  prefix=pith:neo
  ^-  tang
  =/  lom  (~(dip of:neo loam) prefix)
  =/  fam  (~(dip of:neo farm) prefix)
  %-  zing
  ^-  (list tang)
  %+  turn   ~(tap by ~(tar of:neo lom))
  |=  [=pith:neo =soil:neo]
  ^-  tang
  ?~  val=(ram:on:soil:neo soil)
    ~&  missing-value/pith
    ~
  ?~  q.val.u.val
    ~
  =/  =pail:neo  u.q.val.u.val
  :~  leaf/"Path: {(en-tape:pith:neo pith)}"
      leaf/"{<p.pail>}"
  ==
--

