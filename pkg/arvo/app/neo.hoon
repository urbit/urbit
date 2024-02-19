/-  neo
/+  default-agent, dbug, verb
|%
+$  pith   pith:neo
+$  card  card:agent:gall
+$  state-0
  [%0 apex=hall:neo foreign=(map name:neo jail:neo)]
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
++  denorm-children
  =|  res=(map pith room:neo)
  |=  [wer=pith =yard:neo]
  =/  kids=(list (pair iota hall:neo))
    ~(tap by yard)
  |-  ^+  res
  ~&  wer/wer
  ?~  kids
    res
  ?.  ?=(%room -.q.i.kids)
    $(kids t.kids)
  =/  =room:neo
    +.q.i.kids
  =.  res
    (~(put by res) (snoc wer p.i.kids) +.q.i.kids)
  =/  recur  $(wer (snoc wer p.i.kids), kids ~(tap by yard.q.i.kids))
  =.  res  (~(uni by res) recur)
  $(kids t.kids)
--
=|  state-0
=*  state  -
=<
  %-  agent:dbug
  %+  verb  &
  |_  =bowl:gall
  +*  this  .
      run   ~(. +> [bowl ~])
      def   ~(. (default-agent this %|) bowl)
  ++  on-init  
    ^-  (quip card _this)
    =.  apex  (put-hall apex (pave /(scot %p our.bowl)) %room *room:neo)
    `this
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
  ++  on-watch  
    |=  =path
    ^-  (quip card _this)
    =^  cards  state
      abet:(watch:run path)
    [cards this]
  ::
  ++  on-leave  on-leave:def
  ++  on-agent  
    |=  [=wire =sign:agent:gall]
    ^-  (quip card _this)
    =^  cards  state
      abet:(take-agent:run wire sign)
    [cards this]
  ++  on-arvo   
    |=  [=(pole knot) syn=sign-arvo]
    ^-  (quip card _this)
    ?.  ?=([%deal pit=*] pole)
      `this
    =^  cards  state
      abet:(take-arvo:run (pave pit.pole) syn)
    [cards this]
  ++  on-fail   on-fail:def
  ++  on-peek   on-peek:def
  --
|_  [=bowl:gall cards=(list card)]
++  abet  [(flop cards) state]
++  run  .
++  emit  |=(card run(cards [+< cards]))
++  emil  |=(caz=(list card) run(cards (welp (flop caz) cards)))
++  sync-room
  |=  =pith:neo
  ^+  run
  =/  =wire  sync/(pout pith) 
  =/  =name:neo   (de-pith:name:neo pith)
  (emit %pass wire %agent [ship.name dap.bowl] %watch [%sync %init (pout pith.name)])
::
++  take-arvo
  |=  [=pith syn=sign-arvo]
  ^+  run
  ?:  ?=(%remote -.pith)
    !! :: abet:(~(take xeno pith) syn)
  abet:(~(take arvo [pith pith ~ ~ ~]) pith syn)
::
++  forward-poke
  |=  [=name:neo pok=*]
  ^+  run
  =/  =wire  forward/(en-path:name:neo name)
  =/  =dock  [ship.name dap.bowl]
  =-  (emit %pass wire %agent dock %poke -)
  noun+!>(`note:neo`[(en-pith:name:neo name) %poke pok])
++  print-dbug
  |^  ^+  same
  %-  %*(. slog pri 1)
  %-  lure
  :+  %rose  [ret "Shrubbery" sep]
  :~  leaf/"Local"
      (local-hall *pith apex)
  ==
  ++  ret  [',' (reap 4 ' ')]
  ++  sep  *tape
  ++  local-yard
    |=  [=pith =yard:neo]
    ~&  pith/pith
    ^-  tank
    ?:  =(~ yard)
      leaf/"No children"
    :+  %rose  [ret "Kids:" sep]
    %+  turn  ~(tap by yard)
    |=  [=iota =hall:neo]
    (local-hall (snoc pith iota) hall)

  ++  local-hall
    |=  [=pith =hall:neo]
    ^-  tank
    ~&  pith/pith
    :+  %rose  [ret (en-tape:pith:neo pith) sep]
    ?:  ?=(%exit -.hall)
      :~  leaf/"%link"
          leaf/(en-tape:pith:neo +.hall)
      ==
    %-  snoc
    :_  (local-yard pith yard.hall)
    ^-  (list tank)
    ?:  =(case.icon.hall 0)
      ~[leaf/"No data at this path"]
    :*  leaf/"State"
        (sell state.icon.hall)
        leaf/"Case: {(scow %ud case.icon.hall)}"
      ::
      ::
        leaf/"Source: {<p.span.hall>}"
        ^-  (list tank)
        ?:  =(~ conf.hall)
          ~
        :_  ~
        :+  %rose  [" " "Dependencies" sep]
        %+  turn  ~(tap by conf.hall)
        |=  [=term p=^pith]
        leaf/"{<term>} -> {(en-tape:pith:neo p)}"

    ==
  --
::
++  poke
  |=  [=mark =vase]
  ^+  run
  ?>  ?=(%noun mark)
  ?:  =(%dbug q.vase)
    ?>  =(our src):bowl
    %-  print-dbug
    run
  =+  ;;(=note:neo q.vase)
  =/  =name:neo  (de-pith:name:neo p.note) 
  ?.  =(our.bowl ship.name)
    ?>  ?=(%poke -.q.note)
    (forward-poke name val.q.note)
  abet:(~(apply arvo [pith.name pith.name ~ ~ ~]) note)
::
++  watch
  |=  =(pole knot)
  ^+  run
  ?+    pole  ~|(bad-path/pole !!)
      [%sync rest=*]
    ?+    rest.pole  !!
        [%init path=*]
      =/  =pith:neo  (pave path.rest.pole)
      =-  (emit %give %fact ~ neo-watch+!>(-))
      =/  ros=(map pith:neo room:neo)
        (get-deep-hall apex pith)
      %+  turn  ~(tap by ros)
      |=  [p=pith:neo =room:neo]
      ^-  update:neo
      [p [case state]:icon.room]


    ==
  ==
++  take-agent
  |=  [=(pole knot) =sign:agent:gall]
  |^  ^+  run
  ?+  pole  ~|(on-agent-bad-wire/pole !!)
    [%sync rest=*]  (sync (pave rest.pole))
    [%forward rest=*]  (forward (pave rest.pole))
  ==
  ++  forward
    |=  =pith
    ?.  ?=(%poke-ack -.sign)
      ~|(weird-forward-sign/-.sign !!)
    %.  run
    ?~  p.sign
      same
    (slog leaf/"failed forward poke {(spud (pout pith))}" u.p.sign)
      

  ++  sync
    |=  =pith
    ?+    -.sign  ~|(weird-sync-sign/-.sign !!)
        %watch-ack
      %.  run
      ?~  p.sign  same
      %+  slog
        leaf/"Failed sync from {(spud (pout pith))}" 
      u.p.sign
    ::  TODO: security vuln, confused deputy
        %fact
      ?>  ?=(%neo-watch p.cage.sign)
      (hear-watch !<(watch:neo q.cage.sign))
    ::
        %kick
      ~&  'TODO: resub logic'
      run
    ==
  --
++  get-val-at-path
  |=  =pith
  ^-  (unit *)
  ?~  val=(get-hall apex pith)
    ~
  `state.icon.u.val
::
++  has-hall
  |=  =pith:neo
  =(~ (get-hall apex pith))
::
++  put-hall
  |=  [into=hall:neo =pith =hall:neo]
  ~|  pith
  |-  ^-  hall:neo
  ?~  pith
    hall
  ?.  ?=(%room -.into)
    ~|(%cannot-put-thru-symlink !!)
  ?~  nex=(~(get by yard.into) i.pith)
    ?>  ?=(~ t.pith)
    into(yard (~(put by yard.into) i.pith hall))
    :: ~|(no-ancestors/pith !!)
  into(yard (~(put by yard.into) i.pith $(pith t.pith, into u.nex)))
++  get-deep-hall
  |=  [from=hall:neo =pith]
  ^-  (map pith:neo room:neo)
  ?~  rom=(get-hall from pith)
    ~
  =/  =room:neo  u.rom
  =/  kids  (denorm-children pith yard.room)
  (~(put by kids) pith room)
++  get-hall
  |=  [from=hall:neo =pith]
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
++  hear-watch
  |=  =watch:neo
  ^+  run
  ?~  watch
    run
  =.  run  
    abet:(~(hear xeno [src.bowl pith.i.watch]) +.i.watch)
  $(watch t.watch)
++  xeno
  |_  =name:neo
  ++  xeno  .
  ++  abet  run
  ++  get  `(unit jail:neo)`(~(get by foreign) name)
  ++  got  (need get)
  ++  put
    |=  =jail:neo
    =.  foreign  (~(put by foreign) name jail)
    xeno
  ++  wire  `^wire`xeno/(pout (en-pith:name:neo name))
  ++  dock  `^dock`[ship.name dap.bowl]
  ++  watch
    =/  =path  [%sync %init (pout pith.name)]
    =.  run
      (emit %pass wire %agent dock %watch path)
    xeno
  ++  hear
    |=  [case=@ud sta=*]
    ^+  xeno
    xeno
    :: =+  jal=get
    :: =;  =jail:neo
    :: (put jail)
    ::?~  jal
    :: [[case sta ~ ~] *cell:neo]
    :: u.jal(case.icon case, state.icon sta) 
  --
::
++  arvo
  =+  verb=&
  |_  [init=pith here=pith done=(list note:neo) down=(list note:neo) up=(list move:neo)]
  ++  abet  
    =.  cards  (welp cards (turn up deal))
    run
  ++  deal
    |=  =move:neo
    ^-  card
    :+  %pass  local/(pout p.move)
    ^-  note:agent:gall
    ?:  ?=(%arvo -.q.move)
      q.move
    =/  =note:neo  +.q.move
    =/  her=ship
      ~|  p.note
      ?>  ?=([[%p @p] *] p.note)
      +.i.p.note
    [%agent [her dap.bowl] %poke noun+!>(+.q.move)]
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
  ::
  ++  link
    |=  [to=pith from=pith]
    ^+  run
    =.  apex  (put-hall apex to exit/from)
    (sync-room from)

  ++  take
    |=  [=pith syn=sign-arvo]
    ^+  arvo
    =/  si  (si-abed:site pith)
    =^  caz=(list card:neo)  arvo
      si-abet:(si-take-arvo:si syn)
    (ingest pith caz)
  ++  poke
    |=  [=pith val=*]
    =/  =name:neo  (de-pith:name:neo pith)
    ?>  =(our.bowl ship.name)
    si-abet:(si-poke:(si-abed:site pith.name) val)
  ::
  ++  apply
    |=  note=note:neo
    %-  (trace leaf/"{<-.q.note>} {(spud (pout p.note))}" ~)
    ^+  arvo
    =^  caz=(list card:neo)  arvo
      ?+    -.q.note  !!
          %make  (make [p +.q]:note)
          %poke  (poke [p +.q]:note)
          %link  
        :-  ~
        =.  run  (link [p from.q]:note)
        arvo
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
    |=  [=pith src=path init=(unit vase) =conf:neo]
    =/  =name:neo  (de-pith:name:neo pith)
    =+  .^(=firm=vase %ca src) :: TODO: case watching for upgrades, given path can't contain case
    =+  !<(=firm:neo firm-vase)
    =/  =form:neo  form:firm
    =/  =span:neo  [src firm]
    =|  =yard:neo
    =/  =icon:neo  [1 (init:form init) ~ ~]
    =/  =deps:neo  deps:firm
    ?>  =(~ (check-conf conf deps:firm))
    =/  =room:neo  [span conf yard icon]
    =.  apex  (put-hall apex pith.name room/room)
    si-abet:si-born:(si-abed:site pith.name)
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
      ?<  ?=([[%p @] *] p)
      =.  pith  p
      =/  r=room:neo
        ~|  missing-room/pith
        (need (get-hall apex pith))
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
      [src.bowl our.bowl [p/our.bowl pith] now.bowl si-resolve-deps si-resolve-kids]
    ++  si-form    ~(. form:si-firm [si-bowl icon.room])
    ++  si-firm    q.span.room
    ++  si-tell
      =/  paths  si-sync-paths
      ?:  =(paths ~)
        site
      =.  run   
        (^emit %give %fact paths neo-watch+!>(si-grab-watch))
      site
    ++  si-grab-watch
      ^-  watch:neo
      [pith [case state]:icon.room]~
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
    ++  si-born   
      =.  site  (si-emil born:si-form)
      si-tell
    ++  si-poke
      |=  val=*
      ^+  site
      =/  old-state  state.icon.room
      =.  state.icon.room   (reduce:si-form val)
      =.  site  (si-emil (call:si-form old-state val))
      ?:  =(old-state state.icon.room)
        site
      =.  case.icon.room  +(case.icon.room)
      si-tell
    ++  si-take-arvo
      |=  syn=sign-arvo
      (si-emil (take:si-form arvo/syn))
    --
  --
--
