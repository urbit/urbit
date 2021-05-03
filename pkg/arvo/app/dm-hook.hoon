::  invite-hook [landscape]: receive invites from any source
::
::    only handles %invite actions:
::    - can be poked by the host team to send an invite out to someone.
::    - can be poked by foreign ships to send an invite to us.
::
/+  default-agent, dbug, store=graph-store, graphlib=graph, agentio, resource
/+  sig=signatures
::
|%
+$  state-0  [%0 pending=(jar ship atom)]
+$  card  card:agent:gall
--
::
=|  state-0
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
  %+  poke-our:pass  %graph-store
  %+  update:cg:gra  now.bowl
  :+  %add-graph  [our.bowl %inbox]
  [*graph:store `%graph-validator-dm %.n]
::
++  on-save  !>(state)
++  on-load
  |=  old=vase
  ^-  (quip card _this)
  `this(state !<(state-0 old))
::
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  |^
  ?+  mark  (on-poke:def mark vase)
      %graph-update-2
    =+  !<(=update:store vase)
    ?+    -.q.update  `this
        %add-nodes
      ?>  ?=([@ %inbox] resource.q.update)
      =^  cards  state
        ?:  =(our.bowl src.bowl)
          (outgoing-add nodes.q.update)
        (incoming-add nodes.q.update)
      [cards this]
    ==
  ==
  ::
  ++  add-node
    |=  [=index:store =node:store]
    ^-  update:store
    :^  now.bowl  %add-nodes  [our.bowl %inbox]
    (~(gas by *(map index:store node:store)) [index node] ~)
  ::
  ++  add-missing-root
    |=  =ship
    ^-  (list card)
    =/  =index:store
      [ship ~]
    ?:  (check-node-existence:gra [our.bowl %inbox] index)  ~
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
    |=  nodes=(map index:store node:store)
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
        (add-missing-root ship)
        :~  (~(poke pass wire) dock cage)
            (poke-our:pass %graph-store cage)
        ==
      == 
    ==
  ::
  ++  incoming-add
    |=  nodes=(map index:store node:store)
    ^-  (quip card _state)
    :_  state
    ?>  =(1 ~(wyt by nodes))
    =*  ship  src.bowl
    %+  snoc  (add-missing-root ship)
    %+  poke-our:pass  %graph-store
    %+  update:cg:gra  now.bowl
    :+  %add-nodes  [our.bowl %inbox]
    %-  ~(gas by *(map index:store node:store))
    %+  turn  ~(tap by nodes)
    |=  [=index:store =node:store]
    ?>  ?=([@ @ ~] index)
    ?>  ?=(%empty -.children.node)
    ?>  ?=(%& -.post.node)
    =/  new-index=index:store
      [ship now.bowl ~]
    =.  index.p.post.node
      new-index
    [new-index node]
  --
::
++  on-peek   on-peek:def
++  on-watch  on-watch:def
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
  (update:cg:gra now.bowl %remove-posts [our.bowl %inbox] indices)
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
