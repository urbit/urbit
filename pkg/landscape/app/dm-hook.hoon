::  dm-hook [landscape]: receive and send DMs 
::
/-  hark=hark-store
/+  default-agent, dbug, store=graph-store, graphlib=graph, agentio, resource
/+  sig=signatures, hook=dm-hook
::
|%
::
+$  base-state-0
  $:  screening=? 
      screened=(jug ship [=index:store =node:store])
      pending=(jar ship atom)
  ==
::
+$  state-0  [%0 base-state-0]
+$  state-1  [%1 base-state-0]
+$  state-2  [%2 base-state-0]
+$  versioned-state
  $%  state-0
      state-1
      state-2
  ==
+$  card  card:agent:gall
+$  nodes  (map index:store node:store)
++  orm   orm:store
--
::
=|  state-2
=*  state  -
%-  agent:dbug
^-  agent:gall
::
|_  =bowl:gall
+*  this  .
    def   ~(. (default-agent this %|) bowl)
    gra   ~(. graphlib bowl)
    io    ~(. agentio bowl)
    pass  pass:io
::
++  on-init  
  :_  this
  :_  ~
  =/  dms=(list resource)
    ?.  .^(? %gu (scry:io %graph-store ~))
      ~
    %+  skim  ~(tap in get-keys:gra)
    |=([ship name=term] ?=(^ (rush name ;~(pfix (jest 'dm--') fed:ag))))
  |^
  %+  poke-our:pass  %graph-store
  %+  update:cg:gra  now.bowl
  :+  %add-graph  [our.bowl %dm-inbox]
  [graph `%graph-validator-dm %.n]
  ::
  ++  dm-parser
    ;~(pfix (jest 'dm--') fed:ag)
  ::
  ++  counterparty
    |=  rid=resource
    =/  =ship  (rash name.rid dm-parser)
    ?.  =(our.bowl ship)  ship
    entity.rid
  ::
  ++  update-indices
    |=  [pfix=index:store =graph:store]
    =*  loop  $
    ^-  graph:store
    %+  gas:orm  *graph:store
    %+  turn  (tap:orm graph)
    |=  [=atom =node:store]
    ^-  [^atom node:store]
    =/  =index:store  (snoc pfix atom)
    :-  atom 
    =.  children.node
      ?:  ?=(%empty -.children.node)  children.node
      [%graph loop(pfix index, graph p.children.node)]
    ?:  ?=(%| -.post.node)  node
    node(index.p.post index)
  ::
  ++  graph
    %+  roll  dms
    |=  [rid=resource =graph:store]
    ^-  graph:store
    =/  =ship  (counterparty rid)
    =|  =post:store
    =:  author.post     our.bowl
        index.post      [ship ~]
        time-sent.post  now.bowl
      ==
    =/  dm=graph:store
      (update-indices ~[ship] (get-graph-mop:gra rid))
    (put:orm:store graph `@`ship [%& post] %graph dm)
  --
::
++  on-save  !>(state)
++  on-load
  |=  =old=vase
  ^-  (quip card _this)
  =+  !<(old=versioned-state old-vase)
  =|  cards=(list card)
  |-
  ?-    -.old
      %0
    %_($ -.old %1)
      %1
    %_    $
      -.old  %2
      cards  (weld cards (poke-our:pass %goad noun+!>(%force))^~)
    ==
      %2
    :_  this(state old)
    (weld cards (poke-self:pass noun+!>(%reinit))^~)
  ==
::
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  |^
  ?+  mark  (on-poke:def mark vase)
      %noun
    ?+  q.vase  !!
        %reinit
      ?:  (~(has in get-keys:gra) [our.bowl %dm-inbox])  
        `this
      on-init
    ==
  ::
      %dm-hook-action
    =+  !<(=action:hook vase)
    =^  cards  state
      ?+  -.action  !!
        %accept   (accept-screen ship.action)
        %decline  (decline-screen ship.action)
        %screen   (set-screen screen.action)
      ==
    [cards this]
  ::
      %graph-update-3
    =+  !<(=update:store vase)
    ?+    -.q.update  !!
        %add-nodes
      ?>  ?=([@ %dm-inbox] resource.q.update)
      =^  cards  state
        ?:  =(our.bowl src.bowl)
          (outgoing-add (hash-and-sign nodes.q.update))
        ?:  &(screening !(dm-exists src.bowl))
          (screen-add nodes.q.update)
        (incoming-add nodes.q.update)
      [cards this]
    ==
  ==
  ::
  ++  hash-and-sign
    |=  =nodes
    %-  ~(gas by *^nodes)
    %+  turn  ~(tap by nodes)
    |=  [=index:store =node:store]
    ^-  [index:store node:store]
    :-  index
    ?>  ?=(%& -.post.node)
    =*  p  post.node
    =/  =hash:store
      `@ux`(sham [~ author time-sent contents]:p.p)
    %_  node
      hash.p.post  `hash
    ::
        signatures.p.post
      %-  ~(gas in *signatures:store)
      [(sign:sig our.bowl now.bowl hash)]~
    ==
  ::
  ++  give
    |=  =action:hook
    ^-  card
    (fact:io dm-hook-action+!>(action) ~[/updates]) 
  ::
  ++  accept-screen
    |=  =ship
    ^-  (quip card _state)
    =/  unscreened=nodes
      %-  ~(gas by *nodes)
      ~(tap in (~(get ju screened) ship))
    :_  state(screened (~(del by screened) ship))
    %+  welp  (add-missing-root ship)
    :~  %+  poke-our:pass  %graph-store 
        (update:cg:gra now.bowl %add-nodes [our.bowl %dm-inbox] unscreened)
        ::
        (give %accept ship)
    ==
  ::
  ++  set-screen
    |=  screen=?
    :_  state(screening screen)
    (give %screen screen)^~
  ::
  ++  decline-screen
    |=  =ship
    ^-  (quip card _state)
    :_  state(screened (~(del by screened) ship))
    (give %decline ship)^~
  ::
  ++  screen-add
    |=  =nodes
    ?>  =(1 ~(wyt by nodes))
    =/  ship-screen  (~(get ju screened) src.bowl)
    =.  ship-screen  (~(uni in ship-screen) (normalize-incoming nodes))
    =/  should-notify=?     !(~(has by screened) src.bowl)
    =.  screened  (~(put by screened) src.bowl ship-screen)
    :_  state
    =/  =action:hook
      [%pendings ~(key by screened)]
    :-  (fact:io dm-hook-action+!>(action) ~[/updates])
    ?.  should-notify  ~
    (notify-pending src.bowl)^~
  ::
  ++  dm-exists
    |=  =ship
    =/  =index:store
      [ship ~]
    (check-node-existence:gra [our.bowl %dm-inbox] index)
  ::
  ++  add-node
    |=  [=index:store =node:store]
    ^-  update:store
    :^  now.bowl  %add-nodes  [our.bowl %dm-inbox]
    (~(gas by *nodes) [index node] ~)
  ::
  ++  add-missing-root
    |=  =ship
    ^-  (list card)
    ?:  (dm-exists ship)  ~
    =/  =index:store
      [ship ~]
    =|  =post:store
    =:  author.post     our.bowl
        index.post      index
        time-sent.post  now.bowl
      ==
    =/  =node:store
      [%&^post %empty ~]
    (poke-our:pass %graph-store (update:cg:gra (add-node index node)))^~
  ::
  ++  outgoing-add
    |=  =nodes
    ^-  (quip card _state)
    =/  nodes=(list [=index:store =node:store])
      ~(tap by nodes)
    =|  cards=(list card)
    |-  ^-  (quip card _state)
    ?~  nodes  [cards state]
    ?>  ?=([@ @ ~] index.i.nodes)
    =/  =ship  i.index.i.nodes
    =/  =dock  [ship %dm-hook]
    =/  =wire  /dm/(scot %p ship)
    =/  =cage  
      (update:cg:gra (add-node [index node]:i.nodes))
    %=  $
      nodes    t.nodes
      pending  (~(add ja pending) ship now.bowl)
    ::
        cards
      ;:  welp  
        cards
        ::
        (add-missing-root ship)
        ::
        :-  (poke-our:pass %graph-store cage)
        ?:  =(our.bowl ship)  ~
        (~(poke pass wire) dock cage)^~
      == 
    ==
  ::
  ++  notify-pending
    |=  =ship
    ^-  card
    =/  =bin:hark  [/ [q.byk.bowl /dm/invite]]
    =/  title=(list content:hark)
      [ship/ship text/' invited you to a DM' ~]
    %+  poke-our:pass  %hark-store
    :-  %hark-action
    !>(`action:hark`[%add-note bin title ~ now.bowl / /dm/(scot %p ship)])
  ::
  ++  normalize-incoming
    |=  =nodes
    ^-  ^nodes
    %-  ~(gas by *^nodes)
    %+  turn  ~(tap by nodes)
    |=  [=index:store =node:store]
    ?>  ?=([@ @ ~] index)
    ?>  ?=(%empty -.children.node)
    ?>  ?=(%& -.post.node)
    =/  new-index=index:store
      [src.bowl now.bowl ~]
    =.  index.p.post.node
      new-index
    [new-index node]
  ::
  ++  incoming-add
    |=  =nodes
    ^-  (quip card _state)
    :_  state
    ?>  =(1 ~(wyt by nodes))
    =*  ship  src.bowl
    %+  snoc  (add-missing-root ship)
    %+  poke-our:pass  %graph-store
    %+  update:cg:gra  now.bowl
    [%add-nodes [our.bowl %dm-inbox] (normalize-incoming nodes)]
  --
::
++  on-watch  
  |=  =path
  ?.  ?=([%updates ~] path)
    (on-watch:def path)
  :_  this
  :~  (fact-init:io dm-hook-action+!>([%pendings ~(key by screened)]))
      (fact-init:io dm-hook-action+!>([%screen screening]))
  ==
::
++  on-peek   
  |=  =path
  ^-  (unit (unit cage))
  ?+  path  (on-peek:def path)
    [%x %pendings ~]  ``dm-hook-action+!>([%pendings ~(key by screened)])
  ==
::
++  on-leave  on-leave:def
++  on-agent  
  |=  [=wire =sign:agent:gall]
  ^-  (quip card _this)
  |^
  ?.  ?=([%dm @ ~] wire) 
    (on-agent:def wire sign)
  ?>  ?=(%poke-ack -.sign)
  =/  =ship
    (slav %p i.t.wire)
  =^  acked=atom  state
    (remove-pending ship)
  ?~  p.sign
    `this
  :_  this
  :_  ~
  =+  indices=(~(gas in *(set index:store)) ~[ship acked] ~)
  %+  poke-our:pass  %graph-store
  (update:cg:gra now.bowl %remove-posts [our.bowl %dm-inbox] indices)
  ::
  ++  remove-pending
    |=  =ship
    ^-  [atom _state]
    =/  pend-ship=(list atom)
      (flop (~(get ja pending) ship))
    ?>  ?=(^ pend-ship)
    [i.pend-ship state(pending (~(put by pending) ship (flop t.pend-ship)))]
  --

++  on-arvo   on-arvo:def
++  on-fail   on-fail:def
--
