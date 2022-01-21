/-  dao=uqbar-dao
/+  graphlib=graph, graph-store
/+  versioning
=,  graph:dao
=>
  |%
  +$  card  card:agent:gall
  ++  version  3
  --
|_  [rid=resource =dao =id:dao =bowl:gall cards=(list card)]
++  abet
  [(flop cards) dao]
++  cor  .
++  ver  ~(. versioning [bowl %graph-update 3 3])
++  gra  ~(. graphlib bowl)
++  abed
  |=  [r=resource d=^dao i=id:^dao b=bowl:gall]
  cor(rid r, dao d, id i, bowl b)
::
++  incoming-subscriptions
  %-  ~(gas by *(map path @ud))
  %+  murn
    ~(val by sup.bowl)
  |=  [him=ship pax=path]
  =/  idx=(unit @)
    (find area pax)
  ?~  idx  ~
  =/  ver  (slaw %ud (rear pax))
  ?~  ver  ~
  ?.  =(u.idx 0)  ~
  `[pax u.ver]

::
++  emit  |=(=card cor(cards [card cards]))
++  emil  |=(caz=(list card) cor(cards (welp (flop caz) cards)))
++  area  `path`/daos/(scot %ux id)/graphs/(scot %p entity.rid)/[name.rid]
::  +here: wire for subscriptions
::
++  here  area
::  +there: path for subscriptions
::
++  there   
  |=  time=(unit time)
  ^-  path
  =/  ver  (scot %ud version)
  =/  tim  ?~(time %$ (scot %ud u.time))
  (welp area /[tim]/[ver])
++  mar  current-version:ver
++  put
  |=  =status
  =.  graphs.dao  (~(put by graphs.dao) rid status)
  cor
++  stat  (~(got by graphs.dao) rid)
::
++  del
  =.  graphs.dao  (~(del by graphs.dao) rid)
  cor
::
++  pub
  ~|  %pub-not-host
  ?>  =(our.bowl entity.rid)
  |%
  ++  revoke
    |=  =ship
    cor


  ++  allowed
    =*  ship  src.bowl
    ^-  ?
    =/  status  stat
    ?>  ?=(%pub -.status)
    ?.  (~(has by daoists.dao) ship)  |
    ?:  =(~ p.status)  &
    =/  =daoist:^dao  (~(got by daoists.dao) ship)
    !=(~ (~(int in p.status) roles.daoist))
  ::
  ::  TODO: actual versioning 
  ++  watch
    |=  =path
    ^+  cor
    ?+  path  !!
    ::
        [@ @ ~]
      =/  time  (slaw %da i.path)
      =/  version  (slav %ud i.t.path)
      (watch-log time version)
    ==
  ::
  ++  watch-log
    |=  [time=(unit time) version=@ud]
    ?>  allowed
    =/  log=update:graph-store
      ?~  time  (get-graph:gra rid)
      :-  (fall time *^time)
      :+  %run-updates  rid
      (get-update-log-subset:gra rid u.time)
    =/  =mark  (append-version:ver version)
    ?.  (supported:ver mark)
      unsupported
    (emit %give %fact ~ (convert-from:ver mark !>(log)))
  ::
  ++  unsupported
    =>  (emit %give %fact ~ version+!>(version))
    (emit %give %kick ~ ~)
  ::
  ++  fact
    |=  =vase
    %-  emil
    %+  turn  ~(tap by incoming-subscriptions)
    |=  [=path version=@ud]
    ^-  card
    =/  =mark  (append-version:ver version)
    [%give %fact ~[path] (convert-from:ver mark vase)]
  ::
  ++  diff
    |=  dif=pub-diff
    ?-  -.dif
      %attach  (put %pub p.dif)
      ::  TODO: revoke permissions
      %detach  del
    ==
  ++  update
    |=  [=mark =vase]
    ~|  %bad-mark
    ?>  (is-root:ver mark)
    (emit %pass area %agent [our.bowl %graph-store] %poke (convert-to:ver mark vase))
  --
++  sub
  ?<  =(entity.rid our.bowl)
  |%
  ++  watch-them
    |=  =path
    (emit %pass area %agent [entity.rid dap.bowl] %watch path)
  ::
  ++  leave-them
    (emit %pass area %agent [entity.rid dap.bowl] %leave ~)
  ::
  ++  diff
    |=  dif=sub-diff
    ?-    dif
    ::
        %sync
      ?<  (~(has by graphs.dao) rid)
      =.  cor  (watch-them (there ~))
      (put %sub %active ~)
      
    ::  TODO: archive?
    ::
        %stop
      ?>  (~(has by graphs.dao) rid)
      =.  cor  del
      leave-them
    ==
  ::
  ++  take
    |=  =sign:agent:gall
    ^+  cor
    ?-  -.sign  
      %poke-ack
        ?~  p.sign  cor
        %-  (slog leaf/"failed poke @{<rid>}/{<id>}" u.p.sign)
        cor
        

      %fact  (fact cage.sign)
      %kick  kick
    ::
        %watch-ack
      ?~  p.sign  cor
      %-  (slog leaf/"failed subscribe @{<rid>}/{<id>}" u.p.sign)
      ::  TODO: cleanup ?
      cor
    ==
  ::
  ++  kick
    =/  status  stat
    ?>  ?=(%sub -.status)
    ?.  ?=(%active -.p.status)  cor
    =/  time  (peek-update-log:gra rid)
    (watch-them (there time))
  ::
  ++  fact
    |=  =cage
    ?:  (is-root:ver p.cage)
      (update-fact q:(convert-to:ver cage))
    ?>  =(%version p.cage)
    =+  !<(version=@ud q.cage)
    (version-fact version)
  ::
  ++  update-fact
    |=  =vase
    =+  !<(=update:graph-store vase)
    =/  res=(set resource)  (silt (resource-for-update:gra vase))
    ?>  (~(has in res) rid)
    ~&  update
    ~&  our.bowl
    (emit %pass area %agent [our.bowl %graph-store] %poke mar vase)
  ::
  ++  version-fact 
    |=  req-ver=@ud
    ?:  (lth req-ver version)
      (wait-sub version)
    (wait-pub req-ver)
  ::
  ++  wait-pub
    |=  version=@ud
    =.  cor  (put %sub %pub-ver version)
    (watch-them (snoc here %version))
  ::
  ++  wait-sub
    |=  version=@ud
    (put %sub %sub-ver version)
  --
--
