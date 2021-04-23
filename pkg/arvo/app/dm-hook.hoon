::  invite-hook [landscape]: receive invites from any source
::
::    only handles %invite actions:
::    - can be poked by the host team to send an invite out to someone.
::    - can be poked by foreign ships to send an invite to us.
::
/+  default-agent, dbug, store=graph-store, graphlib=graph, agentio, resource
::
|%
+$  state-0  [%0 ~]
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
  [~ this(state !<(state-0 old))]
::
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  |^
  :_  this
  ?+  mark  (on-poke:def mark vase)
      %graph-update-1
    =+  !<(=update:store vase)
    ?+    -.q.update  ~
        %add-nodes
      ?>  ?=([@ %inbox] resource.q.update)
      ?:  =(our.bowl src.bowl)
        (outgoing-add nodes.q.update)
      (incoming-add nodes.q.update)
    ==
  ==
  ::
  ++  outgoing-add
    |=  nodes=(map index:store node:store)
    ^-  (list card)
    %-  zing
    %+  turn  ~(tap by nodes) 
    |=  [=index:store =node:store]
    ^-  (list card)
    ?>  ?=([@ @ ~] index)
    =/  =ship  i.index
    =/  =dock  [ship %dm-hook]
    =/  =wire  /dm/(scot %p ship)
    =;  =cage  
      :~  (~(poke pass wire) dock cage)
          (~(poke-our pass wire) %graph-store cage)
      ==
    %+  update:cg:gra  now.bowl  
    :+  %add-nodes  [our.bowl %inbox]
    (~(gas by *(map index:store node:store)) [index node] ~)
  ::
  ++  incoming-add
    |=  nodes=(map index:store node:store)
    ^-  (list card)
    ?>  =(1 ~(wyt by nodes))
    =*  ship  src.bowl
    =/  =wire  /dm/(scot %p ship)
    :_  ~
    %+  ~(poke-our pass wire)  %graph-store
    %+  update:cg:gra  now.bowl
    :+  %add-nodes  [ship %inbox]
    %-  ~(gas by *(map index:store node:store))
    %+  turn  ~(tap by nodes)
    |=  [=index:store =node:store]
    ?>  ?=([@ @ ~] index)
    ?>  ?=(%empty -.children.node)
    =/  new-index=index:store
      [ship now.bowl ~]
    =.  index.post.node
      new-index
    [new-index node]
  --
::
++  on-peek   on-peek:def
++  on-watch  on-watch:def
++  on-leave  on-leave:def
++  on-agent  on-agent:def
++  on-arvo   on-arvo:def
++  on-fail   on-fail:def
--
