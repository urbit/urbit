/-  neo
/+  default-agent, dbug
|%
+$  pith  $+(pith ^pith)
+$  card  card:agent:gall
+$  state-0
  [%0 apex=hall:neo foreign=(map ship room:neo)]
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
  ++  on-arvo   
    |=  [=(pole knot) syn=sign-arvo]
    ^-  (quip card _this)
    ?.  ?=([%deal pit=*] pole)
      `this
    =^  cards  state
      abet:(take:run (pave pit.pole) syn)
    [cards this]
  ++  on-fail   on-fail:def
  ++  on-peek   on-peek:def
  --
|_  [=bowl:gall cards=(list card)]
++  abet  [(flop cards) state]
++  run  .
++  emit  |=(card run(cards [+< cards]))
++  emil  |=(caz=(list card) run(cards (welp (flop caz) cards)))
++  deal
  |=  =move:neo
  ^-  card
  :+  %pass  deal/(pout p.move)
  ^-  note:agent:gall
  ?:  ?=(%arvo -.q.move)
    q.move
  [%agent [our dap]:bowl %poke noun+!>(+.q.move)]
::
++  take
  |=  [=pith syn=sign-arvo]
  ^+  run
  =^  moves=(list move:neo)  apex
    abet:(~(take arvo [pith ~ ~ ~]) pith syn)
  (emil (turn moves deal))

++  poke
  |=  [=mark =vase]
  ^+  run
  ?>  ?=(%noun mark)
  =+  ;;(=note:neo q.vase)
  =^  moves=(list move:neo)  apex
    abet:(~(apply arvo [p.note ~ ~ ~]) note)
  (emil (turn moves deal)) ::
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
++  check-conf
  |=  [conf=(map term pith) =deps:neo]
  ^-  (set term)
  %+  roll  ~(tap by deps)
  |=  [[=term required=? =port:neo] out=(set term)]
  ?.  &(required !(~(has by conf) term))
    out
  (~(put in out) term)
++  arvo
  =+  verb=&
  |_  [init=pith done=(list note:neo) down=(list note:neo) up=(list move:neo)]
  ++  abet  
    ~&  done/done
    [up apex]
  ++  arvo  .
  ++  emit  |=(=note:neo arvo(down [note down]))
  ++  trace
    |=  =tang
    ?.  verb  same
    (slog tang)
  ++  inside  (cury is-parent init)
  ++  echo  arvo  :: TODO walk done
  ++  work
    ^+  arvo
    |- 
    ?~  down
      arvo
    =/  nex=note:neo  i.down
    =/  new-arvo  (apply:arvo(down t.down) nex) :: XX: weird compiler?
    $(arvo new-arvo, done (snoc done nex))
  ++  take
    |=  [=pith syn=sign-arvo]
    ^+  arvo
    =/  si  (si-abed:site pith)
    =^  caz=(list card:neo)  arvo
      si-abet:(si-take-arvo:si syn)
    (ingest pith caz)
  ::
  ++  apply
    |=  note=note:neo
    %-  (trace leaf/"{<-.q.note>} {(spud (pout p.note))}" ~)
    ^+  arvo
    =^  caz=(list card:neo)  arvo
      ?+  -.q.note  !!
        %make  (make [p +.q]:note)
        %poke  si-abet:(si-poke:(si-abed:site p.note) +.q.note)
        ::  %link  (link [p +.q]:note)
      ==
    (ingest p.note caz)
  ++  ingest
    |=  [=pith caz=(list card:neo)]
    ^+  arvo
    =.  up
      %+  welp  up
      %+  murn  caz
      |=  =card:neo
      ^-  (unit move:neo)
      ?:  ?=(%arvo -.card)
        `[pith card]
      =/  inside  +.card
      ~!  +.card
      ?:  (is-parent pith p.card)
        ~
      `[pith card]

    =.  down
      %-  welp
      :_  down
      %+  murn  caz
      |=  =card:neo
      ^-  (unit note:neo)
      ?:  ?=(%arvo -.card)  ~
      ?.  (is-parent pith p.card)
        ~
      `[p q]:card
    work
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
    si-abet:si-born:(si-abed:site pith)
  ::
  ++  site
    |_  [=pith =room:neo cards=(list card:neo)]
    ++  site  .
    ++  si-emil  |=(caz=(list card:neo) site(cards (welp cards caz)))
    ++  si-abet
      =.  apex  (put-hall apex pith room/room)
      :: TODO: process cards
      [cards arvo]
    ++  si-abed
      |=  p=^pith
      =.  pith  p
      =/  r=room:neo
        ~|  missing-room/pith
        (need (get-hall pith))
      site(pith p, room r)
    ++  si-init
      |=  foo=*
      ^+  site
      =.  state.icon.room  (init:si-form ~)
      site
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
      [src.bowl our.bowl pith now.bowl si-resolve-deps si-resolve-kids]
    ++  si-form    ~(. form:si-firm [si-bowl icon.room])
    ++  si-firm    q.span.room
    ++  si-born   (si-emil born:si-form)
    ++  si-poke
      |=  val=*
      ^+  site
      =/  old-state  state.icon.room
      =.  state.icon.room   (reduce:si-form val)
      (si-emil (call:si-form old-state val))
    ++  si-take-arvo
      |=  syn=sign-arvo
      (si-emil (take:si-form arvo/syn))
    --
  --
--
