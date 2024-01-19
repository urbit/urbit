/-  neo
/+  default-agent, dbug
|%
+$  pith  $+(pith ^pith)
+$  card  card:agent:gall
+$  state-0
  [%0 apex=hall:neo foreign=(map ship room:neo)]
--
=|  state-0
=*  state  -
=<
  %-  agent:dbug
  |_  =bowl:gall
  +*  this  .
      run   ~(. +> [bowl ~])
      def   ~(. (default-agent this %|) bowl)
  ++  on-init  on-init:def
  ++  on-save  !>(state)
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
  ++  on-watch  on-watch:def
  ++  on-leave  on-leave:def
  ++  on-agent  on-agent:def
  ++  on-arvo   on-arvo:def
  ++  on-fail   on-fail:def
  ++  on-peek   on-peek:def
  --
|_  [=bowl:gall cards=(list card)]
++  abet  [(flop cards) state]
++  run  .
++  emit  |=(card run(cards [+< cards]))
++  poke
  |=  [=mark =vase]
  ^+  run
  ?>  ?=(%noun mark)
  =+  ;;(=note:neo q.vase)
  ?+  -.q.note  !!
    %make  (make [p +.q]:note)
    %poke  si-abet:(si-poke:(si-abed:site p.note) +.q.note)
    %link  (link [p +.q]:note)
  ==
::
++  link
  |=  [to=pith from=pith]
  ^+  run
  =.  apex  (put-hall apex to exit/from)
  run
++  get-val-at-path
  |=  =pith
  ^-  (unit *)
  ?~  val=(get-hall pith)
    ~
  `state.icon.u.val
::
++  has-hall
  |=  =pith:neo
  =(~ (get-hall pith))
::
++  put-hall
  |=  [into=hall:neo =pith =hall:neo]
  ^-  hall:neo
  =-  ~&(- -)
  ?~  pith
    hall
  ?.  ?=(%room -.into)
    ~|(%cannot-put-thru-symlink !!)
  ?~  nex=(~(get by yard.into) i.pith)
    ?>  ?=(~ t.pith)
    into(yard (~(put by yard.into) i.pith hall))
    :: ~|(no-ancestors/pith !!)
  into(yard (~(put by yard.into) i.pith $(pith t.pith, into u.nex)))
++  get-hall
  =/  from  apex
  |=  =pith
  ^-  (unit room:neo)
  ?.  ?=(%room -.from)
    $(from apex, pith +.from)
  ?~  pith  `+.from
  ?~  nex=(~(get by yard.from) i.pith)
    ~
  ?.  ?=(%room -.u.nex)
    ~
  ?~  t.pith
    `+.u.nex
  $(from u.nex, pith t.pith)
::
++  make
  |=  [=pith src=path init=(unit *) =conf:neo]
  =+  .^(=firm=vase %ca src) :: TODO: case watching for upgrades, given path can't contain case
  =+  !<(=firm:neo firm-vase)
  =/  =form:neo  form:firm
  =/  =span:neo  [src firm]
  =|  =yard:neo
  =/  =icon:neo  [(init:form init) ~ ~]
  =/  =deps:neo  deps:firm
  ?>  =(~ (check-conf conf deps:firm))
  =/  =room:neo  [span conf yard icon]
  =.  apex  (put-hall apex pith room/room)
  run
++  check-conf
  |=  [conf=(map term pith) =deps:neo]
  ^-  (set term)
  %+  roll  ~(tap by deps)
  |=  [[=term required=? =port:neo] out=(set term)]
  ?.  &(required !(~(has by conf) term))
    out
  (~(put in out) term)
  
::
++  site
  |_  [=pith =room:neo cards=(list card:neo)]
  ++  si-core  .
  ++  si-abet
    =.  apex  (put-hall apex pith room/room)
    :: TODO: process cards
    run
  ++  si-abed
    |=  p=^pith
    =.  pith  p
    =/  r=room:neo
      ~|  missing-room/pith
      (need (get-hall pith))
    si-core(pith p, room r)
  ++  si-init
    |=  foo=*
    ^+  si-core
    =.  state.icon.room  (init:si-form ~)
    si-core
  ++  si-resolve-kids  ~
  ++  si-resolve-deps
    %-  ~(gas by *(map term [^pith *]))
    ^-  (list [term ^pith *])
    %+  murn  ~(tap by deps:si-firm)
    |=  [=term required=? =port:neo]
    ^-  (unit [^term ^pith *])
    =/  dep=(unit ^pith)  (~(get by conf.room) term)
    ?~  dep
      ~|  %invariant-missing-required-conf
      ?<  required
      ~
    =/  val  (get-val-at-path u.dep)
    ?~  val
      ~|  %invariant-no-value-at-path
      !!
    `[term u.dep u.val]
    ::  TODO type this w/ port??
  ++  si-bowl    
    [pith si-resolve-deps si-resolve-kids]
  ++  si-form    ~(. form:si-firm [si-bowl icon.room])
  ++  si-firm    q.span.room
  ++  si-poke
    |=  val=*
    ^+  si-core
    =/  old-state  state.icon.room
    =.  state.icon.room   (reduce:si-form val)
    =.  cards  (call:si-form old-state val)
    si-core
  --
--
