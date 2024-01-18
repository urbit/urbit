/-  neo
/+  default-agent, dbug
|%
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
::
++  put-hall
  |=  [into=hall:neo =pith =hall:neo]
  ^-  hall:neo
  =-  ~&(- -)
  ?~  pith
    hall
  ?.  ?=(%room -.into)
    !!
  ?~  nex=(~(get by yard.into) i.pith)
    ?>  ?=(~ t.pith)
    into(yard (~(put by yard.into) i.pith hall))
    
    :: ~|(no-ancestors/pith !!)
  into(yard (~(put by yard.into) i.pith $(pith t.pith, into u.nex)))

++  make
  |=  [=pith src=path init=(unit *) conf=*]
  =+  .^(=form=vase %ca src) :: TODO: case watching for upgrades, given path can't contain case
  =+  !<(=form:neo form-vase)
  =/  =span:neo  [src form]
  =|  =yard:neo
  =/  =icon:neo  [(init:form init) ~ ~]
  =/  =room:neo  [span yard icon]
  =.  apex  (put-hall apex pith room/room)
  run
::
++  site
  |_  [=pith =room:neo cards=(list card:neo)]
  ++  si-core  .
  ++  si-abet
    =.  apex  (put-hall apex pith room/room)
    :: TODO: process cards
    run
  ++  si-abed
    =/  h=hall:neo  apex
    |=  p=^pith
    =.  pith  p
    |-  
    ?>  ?=(^ p)
    ?>  ?=(%room -.h)
    ?~  nex=(~(get by yard.h) i.p)
      ~|(no-room-at-inn/p !!)
    ?>  ?=(%room -.u.nex)
    ?~  t.p
      si-core(room +.u.nex)
    $(h u.nex, p t.p)
  ++  si-init
    |=  foo=*
    ^+  si-core
    =.  state.icon.room  (init:si-form ~)
    si-core
  ++  si-bowl    *bowl:neo
  ++  si-form    ~(. q.span.room [si-bowl icon.room])
  ++  si-poke
    |=  val=*
    ^+  si-core
    =/  old-state  state.icon.room
    =.  state.icon.room   (reduce:si-form val)
    =.  cards  (call:si-form old-state val)
    si-core
  --
--
