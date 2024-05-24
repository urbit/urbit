/-  neo
/+  aux=neo-two
/+  default-agent
/+  dbug
/*  txt-hoon-imp    %hoon   /neo/cod/std/src/imp/hoon/hoon
/*  txt-term-imp    %hoon   /neo/cod/std/src/imp/term/hoon
/*  txt-ford-same   %hoon   /neo/cod/std/src/imp/ford-same/hoon
/*  txt-ford-slop   %hoon   /neo/cod/std/src/imp/ford-slop/hoon
/*  txt-ford-slap   %hoon   /neo/cod/std/src/imp/ford-slap/hoon
/*  txt-ford-face   %hoon   /neo/cod/std/src/imp/ford-face/hoon
/*  txt-ford-face   %hoon   /neo/cod/std/src/imp/ford-face/hoon
/*  txt-ford-reef   %hoon   /neo/cod/std/src/imp/ford-reef/hoon
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
      =unix:neo
    ::
      =halt:neo
      dev=_|
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
      abet:(on-arvo:run wire syn)
    [cards this]
  ++  on-fail  on-fail:def
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
    (mean leaf/"nack on sys" u.q.ack)
  =/  src=name:neo  (de-pith:name:neo p.p.ack)
  =/  =wire  nack/(pout p.p.ack)
  (do-poke-her wire ship.src neo-ack+!>(ack))^~
++  do-grow
  |=  [=pith:neo =pail:neo]
  ^-  card:dirt:neo
  [pith %grow pail *oath:neo]
++  do-grow-our
  |=  [=pith:neo =pail:neo]
  ^-  card:dirt:neo
  (do-grow [p/our.bowl pith] pail)
++  do-std-warp
  =/  =rave:clay
    [%next %z da/now.bowl /neo]
  (pass /next-clay %arvo %c %warp our.bowl q.byk.bowl `rave)

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
  ==
++  on-noun
  |=  non=*
  ^+  run
  ?+  non  ~|(bad-noun-poke/non !!)
    %dbug   ((slog (print-dbug ~)) run)
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
  ~&  ack/ack
  run

::
++  on-dirt-card
  |=  =card:dirt:neo
  ^+  run
  +:(take-dirt-card card)
++  on-sync
  |=  =sync:neo
  ^+  run
  ?-    r.sync
    %start   abet:(start:rare [p q]:sync)
    %stop    abet:(stop:rare [p q]:sync)
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
++  on-watch
  |=  =(pole knot)
  ^+  run
  ~|  bad-watch-path/pole
  ?>  ?=([%sync rest=*] pole)
  =/  =pith:neo  (pave:neo rest.pole)
  =/  paxs=(list road:neo)  (de:drive:neo pith)
  ?>  ?=([^ ^ ~] paxs)
  ?+  i.paxs  !!
    [car=@ [%ud since=@] ~]  abet:(serve:rare ;;(care:neo car.i.paxs) i.t.paxs)
  ==
++  on-agent
  |=  [=wire =sign:agent:gall]
  ^+  run
  =/  =road:neo  (pave:neo wire)
  ?+  road  +:(on-agent:def wire sign)
    [%deal rest=*]  (on-deal-sign rest.road sign)
    [%sync rest=*]  abet:(on-sign:rare rest.road sign)
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
    [%sys rest=*]  (take-arvo:sys rest.pole syn)
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
::  |jungle: shurb manipulations
+|  %jungle
::  +crop: build (possibly virtual value)
::
::    TODO: does not work as advertised
++  crop
  |=  =pith:neo
  ^-  [(unit (unit saga:neo)) _run]
  :_  run
  =/  res  (~(peek till:aux [loam farm]) %x pith)
  ?:  ?=($@(~ [~ ~]) res)
    res
  ``(~(got of:neo u.u.res) /)
::  +rare: synchronisation
++  rare
  =/  =town:neo   town
  |%  
  ++  abet  run(town town)
  ++  rare  .
  ++  scry   ~
  ++  wire
    |=  =pith:neo  `^wire`rare/(pout pith)
  ++  care
    |=  mart=(set hunt:neo)
    %+  roll  ~(tap in mart)
    |=  [=hunt:neo =care:neo]
    ?:  ?=(?(%z %c) care.hunt)  %z
    ?.  =(?(%y %b) care.hunt)  %y
    care.hunt
  ++  peer-path
    |=  [=pith:neo =mall:neo]
    %-  pout
    (welp #/sync (en:drive:neo #/[(care mart.mall)]/[ud/0] pith ~))
  ++  stop
    |=  [src=pith:neo =hunt:neo]
    ^+  rare
    !!
  ++  start
    |=  [src=pith:neo =hunt:neo]
    ^+  rare
    =/  ton  (~(dip of:neo town) pith.hunt)
    ?^  fil.ton
      =.  mart.u.fil.ton  (~(put in mart.u.fil.ton) [care.hunt src])
      =.  town  (~(rep of:neo town) pith.hunt ton)
      rare
    =>  .(fil.ton `(unit mall:neo)`fil.ton)
    ::  XX: search upwards for 
    =|  =mall:neo
    =.  mart.mall  (~(put in mart.mall) [care.hunt src])
    =.  del.mall  `*deli:neo
    =/  =wire  sync/(pout pith.hunt)
    =/  =name:neo  (de-pith:name:neo pith.hunt)
    =.  run        (emit (do-watch-her wire ship.name (peer-path pith.hunt mall)))
    =.  fil.ton   `mall
    =.  town      (~(rep of:neo town) pith.hunt ton)
    rare
  ++  resign
    |=  =pith:neo
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
    |=  =pith:neo
    =/  =wire  sync/(pout pith)
    =/  =name:neo  (de-pith:name:neo pith)
    =.  run  (emit (do-leave-her wire ship.name))
    rare
  ::
  ++  gone
    |=  [=pith:neo sub=hunt:neo]
    ^+  rare
    =/  ton  (~(dip of:neo town) pith)
    ?~  fil.ton
      ~&  %gone-no-sub
      rare
    =.  mart.u.fil.ton  (~(del in mart.u.fil.ton) sub)
    ?~  del.u.fil.ton
      rare
    =/  =deli:neo  u.del.u.fil.ton
    ?~  sig=(resign pith)
      ~&  last-standing-ending-sub/pith
      (leave pith)
    !!
  ++  on-sign
    |=  [=pith:neo =sign:agent:gall]
    ^+  rare
    =/  ton  (~(dip of:neo town) pith)
    ?+    -.sign  ~|(bad-sign/-.sign !!)
        %watch-ack
      %.  rare
      ?~  p.sign
        same
      (slog u.p.sign)
    ::
        %fact
      %-  (slog leaf/"got fact" (sell q.cage.sign) ~)
      rare
    ::
        %kick
      ~&  'todo: kick handling'
      rare
    ==
  ++  serve
    |=  =hunt:neo
    ^+  rare
    =|  =epic:neo
    !!

  --
++  rent
  |_  =city:neo
  ++  scry  ~
  --
++  rage
  |% 
  ++  stalk
    |=  [=hunt:neo =howl:neo]
    ^+  run
    =/  rav  (fall (~(get of:neo riot) pith.hunt) *rave:neo)
    =.  yel.rav  (~(put in yel.rav) [care.hunt howl])
    =.  riot  (~(put of:neo riot) pith.hunt rav)
    run
  ::
  ++  fury
    =|  wal=(list wail:neo)
    |_  =pith:neo
    +*  rot  (~(dip of:neo riot) pith)
    ++  fu-rave  (fall fil:rot *rave:neo)
    ++  fu-core  .
    ++  fu-abet  
      ^-  (quip wail:neo _run)
      [wal run]
    ++  fu-peep
      |=  lis=(list dust:neo)
      ^+  fu-core
      ?~  lis
        fu-core
      $(fu-core (fu-tone i.lis), lis t.lis)
    ++  fu-tone
      |=  [change=pith:neo =case:neo =mode:neo] 
      ^+  fu-core
      =/  yel  ~(tap by yel:fu-rave)
      |-
      ?~  yel
        =/  nex  (dif:pith:neo pith change)
        ?~  nex  fu-core
        $(pith (snoc pith i.nex))
      =;  add=?
        ?.  add
          $(yel t.yel)
        $(yel t.yel, wal :_(wal [pith i.yel mode]))
      ?:  =(change pith)
        &
      ?:  ?=(%y p.i.yel)
        =(pith (~(parent plow:aux loam) change))
      ?=(%z p.i.yel)
    --
  --
++  take-dirt-card
  |=  =card:dirt:neo
  ^-  (quip gift:dirt:neo _run)
  =^  gifts=(list gift:dirt:neo)  loam
    (~(call plow:aux loam) card)
  =.  farm  (~(take till:aux [loam farm]) gifts)
  [gifts run]

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
    ?~  prey
      run
    =/  =hunt:neo  i.prey
    =.  by-tour.halt   (~(put by by-tour.halt) hunt flow)
    =.  by-flow.halt   (~(put ju by-flow.halt) flow hunt)
    =.  run  run  :: (grab-tour tour)
    $(prey t.prey)
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
  ++  pith
    ^-  pith:neo
    :-  p/our.bowl  
    (~(pith press imp/stud) %out)
  ++  vase
    ^-  ^vase
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
    !=(~ (~(peek plow:aux loam) p/our.bowl pith))
  ++  exists
    =/  pax  path
    (exists-file %src pax)
  --
::
++  con
  |_  =stud:neo
  ++  do  
    =/  vax=vase  
      q.q:(need fil:(need (need (~(peek till:aux [loam farm]) %x [p/our.bowl pith]))))
    |%
    ++  grab  !<(stud:neo (slot 4 vax))
    ++  thru  !<(stud:neo (slot 10 vax))
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
    q:(need (~(peek plow:aux loam) pith))
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
  =.  paths
    %+  turn  paths
    |=  pax=path
    =.  pax  (snip pax)
    ?>  ?=(^ pax)
    t.pax
  |-
  ?~  paths
    finalize
  =.  run  (read-file i.paths)
  $(paths t.paths)
  ::  +finalize: register conversion
  ++  finalize
    =/  base=pith:neo  /out/std/con
    =/  cons  
      ~(tap by ~(tar of:neo ~(snip of:neo (~(dip of:neo tide) base))))
    |- 
    ?~  cons
      run
    =/  [p=pith:neo *]  i.cons
    =/  =stud:neo
      ?>  ?&(?=(^ p) ?=(@ i.p))
      i.p
    =.  dive  sink:~(do con stud)
    $(cons t.cons)
  ::
  ++  adult  %.n
  ::
  ++  has-modified
    |=  [txt=@t pax=pith:neo]
    ?.  adult
      &
    ?~  pal=(~(peek plow:aux loam) [p/our.bowl pax])
      &
    !=(txt q.q.u.pal)
  ::
  ++  read-file
    |=  pax=path
    ^+  run
    ?:  =((rear pax) %ford-parser)
      run
    =+  .^(src=@t %cx `path`(snoc `path`(welp root pax) %hoon))
    ?.  (has-modified src (pave:neo pax))
      run
    ~?  >>>  adult
      [%update pax]
    =/  =file:ford:neo
      ~|  parsing/pax
      (scan (trip src) (rein:ford:neo [our.bowl (pave:neo pax)]))
    =/  has-imports=?
      ?&  (levy pro.file |=(pro:ford:neo ~(exists pro stud)))
          (levy lib.file |=(lib:ford:neo ~(exists lib loc)))
      ==
    ?.  has-imports
      ~|  pro.file
      ~|  lib.file
      ~|  %no-imports
      !!
    =.  run  (build-pros (turn pro.file tail))
    =.  run  (build-libs (turn lib.file tail))
    =/  built-imports=?
      ?&  (levy pro.file |=(pro:ford:neo ~(built pro stud)))
          (levy lib.file |=(lib:ford:neo ~(built lib loc)))
      ==
    ~|  ~(key by ~(tar of:neo loam))
    ~|  imports/file
    ?>  built-imports
    =^  pre=pith  run  
      (make-prelude pax file)
    =/  =conf:neo
      (~(gas by *conf:neo) [%sut (ours pre)] ~)
    =.  run  (write-hoon pax src)
    =/  pit  (src-to-out pax)
    (ford-slap (src-to-out pax) pre pax)
  ++  build-pros
    |=  pos=(list stud:neo)
    ^+  run
    ?~  pos
      run
    =/  pat  
      (~(path press pro/i.pos) %src)
    =.  run  (read-file pat)
    $(pos t.pos)
  ++  build-libs
    |=  lis=(list loc:ford:neo)
    ^+  run
    ?~  lis
      run
    =.  run  (read-file %src ~(path lib i.lis))
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
    %+  welp
      (turn pro.file |=(p=pro:ford:neo [face.p ~(pith pro stud.p)]))
    (turn lib.file |=(l=lib:ford:neo [face.l %out ~(pith lib loc.l)]))
  ++  make-prelude
    |=  [pax=pith =file:ford:neo]
    ^-  [pith _run]
    =/  pre-path=pith  
      (slip:press %pre pax)
    [pre-path (make-deps pre-path (file-to-deps file))]
  ++  write-hoon
    |=  [pax=pith fil=@t]
    (do-make pax %hoon `hoon/!>(fil) ~)
  ++  src-to-out
    |=  pax=pith:neo
    ^-  pith:neo
    (slip:press %out pax)
  --
::
++  boot
  |^  ^+  run
  =+  .^(neo-vase=vase %ca (welp clay-beak /sur/neo/hoon))
  =/  reef=vase  (slop !>(..zuse) neo-vase(p [%face %neo p.neo-vase]))
  =/  riff=pail:neo  [%vase !>(riff-kook)]
  =.  run  (on-dirt-card (do-grow-our (pess imp/%ford-riff) riff))
  =.  run  (make-riff #/out/reef reef)
  =.  run  (re-export reef %hoon !,(*hoon @t))
  =.  run  (re-export reef %desk !,(*hoon desk))
  =.  run  (make-riff (pess pro/%vase) (vase-pro reef))
  =.  run  (make-riff (pess pro/%ford-in) (ford-in reef))
  =.  run  (make-riff (pess pro/%term) (term reef))
  =.  run  (make-riff-slap (pess imp/%hoon) reef txt-hoon-imp)
  =.  run  (make-riff-slap (pess imp/%term) reef txt-term-imp)
  =.  run  (make-riff-slap (pess imp/%ford-same) reef txt-ford-same)
  =.  run  (make-riff-slap (pess imp/%ford-face) reef txt-ford-face)
  =.  run  (make-riff-slap (pess imp/%ford-slop) reef txt-ford-slop)
  =.  run  (make-riff-slap (pess imp/%ford-slap) reef txt-ford-slap)
  =.  run  (re-export reef %json !,(*hoon json))
  =.  run  (re-export reef %mime !,(*hoon mime))
  =.  run  copy-clay
  ::  =.  run  (emit %pass /bind-site %arvo %e %connect [~ dap.bowl ~] dap.bowl)
  (emit do-std-warp)
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
  ++  make-riff
    |=  [=pith riff=vase]
    ^+  run
    =.  pith  [p/our.bowl pith]
    (on-card pith %make %ford-riff `vase/riff ~)
  --
:: +abduct: check capture
++  abduct
  |=  [par=pith:neo child=pith:neo]
  ^-  ?
  ?~  wav=(~(get of:neo tide) par)
    |
  ?~  kids.dock.u.wav
    |
  ?:  ?=(%y p.u.kids.dock.u.wav)
    =(par (~(parent of:neo tide) child))
  !=(~ (dif:pith:neo par child))
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
      (take-dirt-card [p/our.bowl here] %grow pail *oath:neo)
    =.  grit  (welp grit git)
    arvo
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
      %cull   !!
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
    ?:  =(~ (moor quay name))
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
    ?:  =((de-pith:name:neo sys-pith) src.init)
      (tap)
    =/  res=(each (quip card:neo _arvo) tang)
      (mule tap)
    ?:  ?=(%& -.res)
      p.res
    =.  err.block  `p.res
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
      =/  =lore:neo  (need (moor quay name))
::      %-  (slog term (epic:dbug:neo epic) ~)
      `[term u.dep lore]
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
  ++  on-move
    |=  [src=pith:neo dst=pith:neo =note:neo]
    ^+  run
    ?+  dst  !!
      [%clay *]  (call:silt src t.dst note)
    ==
  ++  take-arvo
    |=  [=(pole knot) syn=sign-arvo]
    ^+  run
    ?+  pole  ~|(bad-sys-take/pole !!)
      [%clay %peer rest=*]  (take-peer:silt rest.pole syn)
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
  =/  =epic:neo  (need (need (~(peek till:aux [loam farm]) care (en-pith:name:neo name))))
  =.  epic  (~(dip of:neo epic) (en-pith:name:neo name))
  =;  [fail=? res=(list (pair pith:neo idea:neo))]
    ?:  fail
      ~
    `(gas-lore res)
  %+  roll  ~(tap by ~(tar of:neo epic))
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
++  gas-epic
  =|  =epic:neo
  |=  lst=(list [pith:neo saga:neo])
  ^+  epic
  ?~  lst
    epic
  =.  epic   (~(put of:neo epic) i.lst)
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

