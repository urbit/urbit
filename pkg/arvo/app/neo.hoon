/-  neo
/+  aux=neo-two
/+  default-agent
/+  dbug
/*  txt-hoon-imp    %hoon   /neo/src/std/imp/hoon/hoon
/*  txt-term-imp    %hoon   /neo/src/std/imp/term/hoon
/*  txt-ford-same   %hoon   /neo/src/std/imp/ford-same/hoon
/*  txt-ford-slop   %hoon   /neo/src/std/imp/ford-slop/hoon
/*  txt-ford-slap   %hoon   /neo/src/std/imp/ford-slap/hoon
/*  txt-ford-face   %hoon   /neo/src/std/imp/ford-face/hoon
/*  txt-ford-face   %hoon   /neo/src/std/imp/ford-face/hoon
/*  txt-ford-reef   %hoon   /neo/src/std/imp/ford-reef/hoon
|%
+$  card  $+(card card:agent:gall)
+$  state-0
  $:  =loam:dirt:neo
      =farm:neo
    ::
      =town:neo
      =city:neo
    ::
      =tide:neo
    ::
      =gang:neo
      =lads:neo
    ::
      =halt:neo
      dev=_|
  ==
++  ford  ford:neo
++  mute
  |=  dev=?
  |*  *
  ?:  dev
    !:  +<
  !.  +<
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
  !:
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
%-  (mute dev)
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
    %neo-dirt-card   =;(f (f !<(_+<.f vase)) on-dirt-card)
    %neo-sync        =;(f (f !<(_+<.f vase)) on-sync)
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
  ?>  =(src.bowl ship.src)
  abet:(arvo move)
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
  |=  [=wire syn=sign-arvo]
  ^+  run
  =.  run   +:(on-arvo:def wire syn)
  run
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
    :+  p/our.bowl  %out
    %-  pave:neo
    ?@  stud 
      /std/imp/[stud]
    ?:  =(our.bowl ship.stud)
      /our/[desk.stud]/imp/[mark.stud]
    /her/(scot %p ship.stud)/[desk.stud]/imp/[mark.stud]
  ++  vase
    ^-  ^vase
    ~|  firm/pith
    =/  =pail:neo  (need (~(peek plow:aux loam) pith))
    =+  !<([cac=(unit ^vase) *] q.pail)
    (need cac)
  ++  kook
    ^-  kook:neo
    !<(kook:neo vase)
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
    (exists-file %src path)
  --
::
++  pro
  |_  =stud:neo
  ++  get  grab
  ++  grab
    =/  =pail:neo  (need (~(peek plow:aux loam) pith))
    =+  !<([cac=(unit vase) *] q.pail)
    cac
  ++  built
    ~&  built/pith
    =-  ~&(- -)
    !=(~ (~(peek plow:aux loam) p/our.bowl pith))
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
  -- ::
++  root
  /(scot %p our.bowl)/[q.byk.bowl]/(scot %da now.bowl)/neo
++  exists-file
  |=  pax=path
  =/  p=path
    (welp root pax)
  =.  p  (snoc p %hoon)
  .^(? %cu p)
++  get-reef
  =+  !<([ref=(unit vase) *] q:(need (~(peek plow:aux loam) #/[p/our.bowl]/out/reef)))
  (need ref)
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
    run
  =.  run  (read-file i.paths)
  $(paths t.paths)
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
    =.  run  (build-pros (turn pro.file tail))
    =.  run  (build-libs (turn lib.file tail))
    =/  built-imports=?
      ?&  (levy pro.file |=(pro:ford ~(built pro stud)))
          (levy lib.file |=(lib:ford ~(built lib loc)))
      ==
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
      ~(path pro i.pos)
    ?>  ?=(^ pat)
    =.  run  (read-file %src t.pat)
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
    (do-make pax %hoon `hoon/!>(fil) ~)
  ++  src-to-out
    |=  pax=pith:neo
    ^-  pith:neo
    ?>  ?=([@ *] pax)
    [%out t.pax]
  --
::
++  boot
  |^  ^+  run
  :: ?:  =(1 1)
    :: run
  =+  .^(neo-vase=vase %ca (welp clay-beak /sur/neo/hoon))
  =/  reef=vase  (slop !>(..zuse) neo-vase(p [%face %neo p.neo-vase]))
  =/  riff=pail:neo
    [%ford-out !>(`[cac=(unit vase) ~]`[`!>(riff-kook) ~])]
  =.  run  (on-dirt-card (do-grow-our #/out/std/imp/ford-riff riff))
  %-  (slog (print-dbug /))
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
  =.  run  copy-clay
  ::  =.  run  (emit %pass /bind-site %arvo %e %connect [~ dap.bowl ~] dap.bowl)
  (emit do-std-warp)
  ++  clay-beak  ^-  path
    /(scot %p our.bowl)/[q.byk.bowl]/(scot %da now.bowl)
  ++  ford-slip
    ^-  dock:neo
    [pro/%ford-out ~ ~]
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
    ++  state  pro/%ford-out
    ++  poke   *(set stud:neo)
    ++  kids  ~
    ++  deps  ~
    ++  form
      ^-  form:neo
      |_  [=bowl:neo =aeon:neo =pail:neo]
      +*  sta  !<([cache=(unit vase) ~] state-vase)
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
    (on-card pith %make %ford-riff `vase/!>([`riff ~]) ~)
  --
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
          grit=(list dust:neo)  :: raw changelist
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
      =.  run    (emil `(list card)`(do-ack [p p.q]:init-move err.block))
      =.  run    (emil (turn up do-move))
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
  ++  grow
    |=  =pail:neo
    ^+  arvo
    =^  git=grit:neo  run
      (take-dirt-card [p/our.bowl here] %grow pail *oath:neo)
    =.  grit  (welp grit git)
    arvo
      
  ++  finalize
    ^+  arvo
::  =.  gifts
::    =-  ~(tap by -)
::    ^-  (map pith:neo gift:neo)
::    %+  roll  ~(tap by change)
::    |=  [[=pith:neo =mode:neo] out=(map pith:neo gift:neo)]
::    ?~  par=(parent:of-top pith)
::      out
::    =/  parent  (~(gut by out) u.par *gift:neo)
::    =.  parent  (~(put by parent) (sub:pith:neo pith u.par) mode)
::    (~(put by out) u.par parent)
::  =.  changes  (~(uni by changes) change)
::  =.  change   *(map pith:neo mode:neo)
::  ?~  gifts
::    arvo
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
::  =/  =room:neo  (got:of-top here)
::  ?:  &(=(%rely p.pail) ~(is-plot husk code.room))
::    =.  change  (~(put by change) here %dif)
::    work
::    
::  =^  cards=(list card:neo)  arvo
::    `arvo :: (soft-site |.(si-abet:(si-poke:site pail)))
    (ingest ~) ::  cards)
  ::  XX: a hack
  ::
  ::    this is implicity recursive, and all external dependencies of
  ::    the children need to be woken up. this also breaks referential 
  ::    transparency
  ++  tomb
    |=  *
    ::  =.  apex  (del:of-top here)
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
  ++  listen-conf
    |=  [=conf:neo =deps:neo]
    ^+  arvo
    arvo
::  =/  conf  ~(tap by conf)
::  |-
::  ?~  conf
::    arvo
::  =/  [=term dep=pith:neo]  i.conf
::  ?>  ?=([[%p @] *] dep)
::  =/  d=(unit [req=? =quay:neo])  (~(get by deps) term)
::  ?~  d 
::    $(conf t.conf)
::  =/  [req=? =quay:neo]  u.d
::  =/  =tour:neo  [(get-care:quay:neo quay) dep]
::  =/  =pith:neo  [p/our.bowl here]
::  ?:  =(our.bowl +.i.dep)
::    =/  =tone:neo  [%rely term pith]
::    =.  sound  (~(put ju sound) [care.tour t.dep] tone)
::    $(conf t.conf)
::  ::
::  =/  =riot:neo  (~(got by foreign) tour)
::  =/  =rave:neo   [term pith]
::  =.  deps.riot  (~(put in deps.riot) rave)
::  =.  foreign    (~(put by foreign) tour riot)
::  $(conf t.conf)
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
    ++  su-kids
      =/  kids  kids:kook
      %-  gas-lore
      =/  child  (dall:aux (~(peek till:aux [loam farm]) %y [p/our.bowl here]) *epic:neo)  
      %+  murn  ~(tap by ~(tar of:neo child))
      |=  [=pith:neo =saga:neo]
      ^-  (unit [pith:neo idea:neo])
      ?~  ion=(scion kids pith saga)
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
      ~&  dep/[term name care]
      =/  =lore:neo  (moor quay name)
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
    --
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
  ?:  =(want stud)
    `q.have
  ?:  =(want %sig)
    `sig/!>(~)
  ~
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
  |=  [want=kids:neo =pith:neo =saga:neo]
  ^-  (unit idea:neo)
  ?~  pis=(find:peon:neo pith ~(key by want))
    ~
  =/  =lash:neo  (~(got by want) u.pis)
  (plag state.lash saga)
::
++  moor
  |=  [want=quay:neo =name:neo]
  ^-  lore:neo
  =/  =care:neo  (get-care:quay:neo want)
  =/  =epic:neo  (need (need (~(peek till:aux [loam farm]) care (en-pith:name:neo name))))
  %-  (slog leaf/"mooring" (epic:dbug:neo epic) ~)
  =.  epic  (~(dip of:neo epic) (en-pith:name:neo name))
  %-  (slog leaf/"mooring" (epic:dbug:neo epic) ~)
  %-  gas-lore
  ^-  (list (pair pith:neo idea:neo))
  %+  murn  ~(tap by ~(tar of:neo epic))
  |=  [=pith:neo =saga:neo]
  ^-  (unit [pith:neo idea:neo])
  ?:  =(pith ~)
    `[*pith:neo (need (plag state.p.want saga))]
  ?~  q.want
    ~
  ?~  ion=(scion q.u.q.want pith saga)
     ~
  `[pith u.ion]
++  gas-epic
  =|  =epic:neo
  |=  lst=(list [pith:neo saga:neo])
  ^+  epic
  ?~  lst
    epic
  =.  epic   (~(put of:neo epic) i.lst)
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
  =/  lom  (~(dip of:neo loam) prefix)
  =/  fam  (~(dip of:neo farm) prefix)
  ^-  tang
  %-  zing
  ^-  (list tang)
  %+  turn   ~(tap by ~(tar of:neo lom))
  |=  [=pith:neo =soil:neo]
  ^-  tang
  ?~  val=(ram:on:soil:neo soil)
    ~
  ?~  q.val.u.val
    ~
  =/  =pail:neo  u.q.val.u.val
  ~|  stud/p.pail
  ~|  pith/pith
  :*  leaf/"Path: {(en-tape:pith:neo pith)}"
      ?.  =(%ford-out p.pail)
        :~  leaf/"Data"
            (sell q.pail)
        ==

      =+  !<(vax=(unit vase) (slot 2 q.pail))
      ?~  vax
        ~[leaf/"Value: Ford build (crashed)"]
      :~  leaf/"Value: Ford build"
          (sell u.vax)
      ==
  ==
--

