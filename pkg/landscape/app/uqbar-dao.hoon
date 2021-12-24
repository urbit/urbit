/-  dao=uqbar-dao
/+  default-agent, dbug, verb
/+  resource, agentio
/+  dao-graph, graphlib=graph, graph-store
|%
+$  card  card:agent:gall
+$  state-zero
  [%0 daos=(map id:dao dao)]
--
=|  state-zero
=*  state  -
=<
  %-  agent:dbug
  %+  verb  &
  ^-  agent:gall
  |_  =bowl:gall
  +*  this  .
      def  ~(. (default-agent this %|) bowl)
      cor  ~(. +> [bowl ~])
  ++  on-init  
    =^(cards state abet:init:cor [cards this])
  ++  on-save  !>(state)
  ++  on-load
    |=  =vase
    =+  !<(old=state-zero vase)
    `this(state old)
  ++  on-poke
    |=  [=mark =vase]
    =^  cards  state
      abet:(poke:cor mark vase)
    [cards this]
  ::
  ++  on-peek  peek:cor
  ++  on-watch
    |=  =path
    =^  cards  state
      abet:(watch:cor path)
    [cards this]
  ::
  ++  on-agent
    |=  [=wire =sign:agent:gall]
    =^  cards  state
      abet:(agent:cor wire sign)
    [cards this]
  ::
  ++  on-arvo  on-arvo:def
  ++  on-fail  on-fail:def
  ++  on-leave  on-leave:def
  --
|_  [=bowl:gall cards=(list card)]
++  abet  [(flop cards) state]
++  cor    .
++  emit  |=(=card cor(cards [card cards]))
++  io    ~(. agentio bowl)
++  pass  pass:io
++  emil  |=(caz=(list card) cor(cards (welp (flop caz) cards)))
++  watch-graph
  (~(watch-our pass /graphs) %graph-store /updates)
++  init  (emit watch-graph)
++  poke
  |=  [=mark =vase]
  ~|  poke/mark
  ?+  mark   ~|(%no-mark !!)
  ::  TODO: remove, debugging purposes
    %noun  cor(state *state-zero)
  ::
      %dao-update
    =+  !<(=update:dao vase)
    abet:(~(diff tao p.update) q.update)
  ::
      %graph-update-3
    %-  emil
    %+  turn  (~(resource-for-update graphlib bowl) vase)
    |=  rid=resource
    ^-  card
    =/  =dock
      [entity.rid ?:(=(entity.rid our.bowl) %graph-store %uqbar-dao)]
    [%pass / %agent dock %poke mark vase]
  ==
++  watch
  |=  =path
  ^+  cor
  ~|  watch/path
  ?+  path  ~|(%bad-path !!)
    [%daos @ *]  abet:(watch:(from-wire:tao path) t.t.path)
  ==
::
++  agent
  |=  [=wire =sign:agent:gall]
  ^+  cor
  ~|  agent/-.sign^wire
  ?+  wire  ~|(%no-wire !!)
    [%daos @ *]  abet:(agent:(from-wire:tao wire) t.t.wire sign)
  ::
      [%graphs ~]
    ?+  -.sign  !!
      %kick  (emit watch-graph)
    ::
        %watch-ack
      ?~  p.sign  cor
      %-  (slog leaf/"Failed subscribe to graph-store" u.p.sign)
      cor
    ::
        %fact
      ?.  =(%graph-update-3 p.cage.sign)  cor
      =+  !<(=update:graph-store q.cage.sign)
      =/  resources=(list resource)
        (~(resource-for-update graphlib bowl) q.cage.sign)
      |-
      ?~  resources  cor
      ?.  =(entity.i.resources our.bowl)
        $(resources t.resources)
      ?~  id=(dao-for-graph i.resources)
        $(resources t.resources)
      =/  d  (~(got by daos) u.id)
      =/  [caz=(list card) new=dao]
        =/  gra  ~(. dao-graph [i.resources d u.id bowl ~])
        abet:(fact:pub:gra q.cage.sign)
      =.  daos  (~(put by daos) u.id new)
      =.  cor  (emil caz)
      $(resources t.resources)
    ==
  ::
  ::  miscellaneous acks
      ~
    ?.  ?=(%poke-ack -.sign)  cor
    ?~  p.sign  cor
    %-  (slog leaf/"Failed poke to {<src.bowl>}" u.p.sign)
    cor

  ==
++  peek
  |=  =path
  ^-  (unit (unit cage))
  [~ ~]
::
++  tao
  |_  =id:dao
  ++  abet  cor
  ++  from-wire
    |=  =wire
    ?>  ?=([%daos @ *] wire)
    =/  i  i.t.wire
    tao(id i)
  ::
  ++  tao  .
  ++  emit  |=(=card tao(cor (emit:cor card)))
  ++  edit
    |=  f=$-(dao dao)
    tao(daos (~(jab by daos) id f))
  ++  te  (~(got by daos) id)
  ++  watch
    |=  =path
    ^+  tao
    ?+  path  ~|(%bad-path !!)
    ::
        [%graphs @ @ *]
      =/  ship  (slav %p i.t.path)
      =/  rid=resource  [ship i.t.t.path]
      =/  [caz=(list card) new=dao]
        =/  gra  ~(. dao-graph [rid te id bowl ~])
        abet:(watch:pub:gra t.t.t.path)
      =.  daos  (~(put by daos) id new)
      =.  cor  (emil caz)
      tao
    ==
  ::
  ++  diff
    |=  =diff:dao
    |^
    ?-  -.diff
      %create  create
      %delete  delete
      %daoist  abet:(~(diff taoist p.p.diff) q.p.diff)
    ::
        %graphs  
      =*  update  p.diff
      =/  rid=resource  p.p.diff
      =/  [caz=(list card) new=dao]
        =/  gra  ~(. dao-graph [rid te id bowl ~])
        =<  abet
        ?-  -.q.p.diff
          %pub  (diff:pub:gra p.q.p.diff)
          %sub  (diff:sub:gra p.q.p.diff)
        ==
      =.  daos  (~(put by daos) id new)
      =.  cor  (emil caz)
      tao
    ==
    ++  create
      =.  daos  (~(put by daos) id *dao)
      tao
    ++  delete
      =.  daos  (~(del by daos) id)
      tao
    --
  ++  taoist
    |_  =ship
    ++  abet  tao
    ++  taoist  .
    ++  diff
      |=  diff=daoist-diff:dao
      ^+  taoist
      ?-  -.diff
        %join   (join +.diff)
        %leave  leave
        %deputise  (deputise +.diff)
        %undeputise  (undeputise +.diff)
      ==
    ++  edit
      |=  f=$-(daoist:dao daoist:dao)
      =.  tao
        %-  edit:tao
        |=  d=dao
        d(daoists (~(jab by daoists.d) ship f))
      taoist
    ::
    ++  leave
      =.  tao
        %-  edit:tao
        |=  d=dao
        d(daoists (~(del by daoists.d) ship))
      taoist
    ::
    ++   deputise
      |=  b=badge:dao
      %-  edit
      |=  d=daoist:dao
      d(roles (~(put in roles.d) b))
    ::
    ++   undeputise
      |=  b=badge:dao
      %-  edit
      |=  d=daoist:dao
      d(roles (~(del in roles.d) b))

    ::
    ++  join
      |=  =tx:dao
      =/   new=daoist:dao 
        %*  .  *daoist:dao
          ship    ship
          joined  now.bowl
          attest  tx
        ==
      =.  tao
        %-  edit:tao
        |=  d=dao
        d(daoists (~(put by daoists.d) ship new))
      taoist
    --

  ++  agent
    |=  [=wire =sign:agent:gall]
    ?+  wire  !!
        [%graphs @ @ *]
      =/  ship  (slav %p i.t.wire)
      =/  rid=resource  [ship i.t.t.wire]
      =/  [caz=(list card) new=dao]
        =/  gra  ~(. dao-graph [rid te id bowl ~])
        abet:(take:sub:gra sign)
      =.  daos  (~(put by daos) id new)
      =.  cor  (emil caz)
      tao
    ==
  --
::  TODO: maybe replace with cached index from graph -> dao ids
++  dao-for-graph
  |=  rid=resource
  ^-  (unit id:dao)
  =/  daos=(list [=id:dao d=dao])  ~(tap by daos)
  |-  
  ?~  daos  ~
  ?.  (~(has by graphs.d.i.daos) rid)
    $(daos t.daos)
  `id.i.daos
--
    
