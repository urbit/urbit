/-  dao=uqbar-dao
/+  graphlib=graph, graph-store
/+  versioning
=,  graph:dao
=>
  |%
  +$  card  card:agent:gall
  ++  version  3
  ++  pull-handler
    $_  ^|
    |_  rid=resource
    ++  resource-for-update  |~(vase *?)
    ++  on-pull-kick         *(unit knot)
    ++  on-pull-nack         *(list card)
    --
  ++  push-handler
    $_  ^|
    |_  [rid=resource =bowl:gall]
    ++  resource-for-update     |~(vase *?)
    ++  transform-proxy-update  |~(vase *vase)
    ++  initial-watch           *vase
    --
  --
|=  $:  pfix=wire
        =pull-handler
        store-name=term
        =mark
        current-version=@ud
        min-version=@ud
    ==
|_  [=bowl:gall rid=resource =status:pull-hook cards=(list card)]
++  abet
  [(flop cards) status]
++  cor  .
++  ver  ~(. versioning [bowl mark current-version min-version])
::
++  incoming-subscriptions
  %-  ~(gas by *(map path @ud))
  %+  murn
    ~(val by sup.bowl)
  |=  [him=ship pax=path]
  =/  idx=(unit @)
    (find pfix pax)
  ?~  idx  ~
  =/  ver  (slaw %ud (rear pax))
  ?~  ver  ~
  ?.  =(u.idx 0)  ~
  `[pax u.ver]

::
++  emit  |=(=card cor(cards [card cards]))
++  pass
  |=  [=wire =dock =task:agent:gall]
  (emit %pass `^wire`(welp pfix wire) %agent dock task)
++  emil  |=(caz=(list card) cor(cards (welp (flop caz) cards)))
++  area  `path`(welp pfix /(scot %p entity.rid)/[name.rid])
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
++  take
  |=  [=wire =sign:agent:gall]
  ~|  bad-wire/wire
  ?>  =(~ wire)
  ?-  -.sign  
    %poke-ack
      ?~  p.sign  cor
      %-  (slog leaf/"failed poke @{<rid>}/{<id>}" u.p.sign)
      cor
  ::
    %fact  (fact cage.sign)
    %kick  kick
  ::
      %watch-ack
    ?~  p.sign  cor
    %-  (slog leaf/"failed subscribe @{<rid>}/{<id>}" u.p.sign)
    ::  TODO: cleanup ?
    cor
  ==
++  watch-them
  |=  resub=(unit knot)
  =/  =path
    %+  welp  pfix
    [(scot %ud current-version) (fall resub %$) ~]
  (emit %pass area %agent [entity.rid dap.bowl] %watch path)
::
++  kick
  ?.  ?=(%active -.status)  cor
  =/  resub  on-pull-kick:pull-handler  
  (watch-them resub)
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
  ?>  (resource-for-update:pull-handler vase)
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
  cor
  :: (watch-them (snoc here %version))
::
++  put
  |=  *
  cor
::
++  wait-sub
  |=  version=@ud
  (put %sub %sub-ver version)
--
