::  btc-bridge.hoon
::  Proxy for accessing BTC full node
::
/-  *btc-bridge
/+  dbug, default-agent
|%
+$  versioned-state
    $%  state-0
    ==
::
+$  state-0  [%0 counter=@]
::
+$  card  card:agent:gall
::
--
%-  agent:dbug
=|  state-0
=*  state  -
^-  agent:gall
|_  =bowl:gall
+*  this      .
    def   ~(. (default-agent this %|) bowl)
    hc    ~(. +> bowl)
::
++  on-init
  ^-  (quip card _this)
  ~&  >  '%btc-bridge initialized successfully'
  :-  ~[[%pass /response %agent [our.bowl %btc-node-hook] %watch /response]]
  this
++  on-save
  ^-  vase
  !>(state)
++  on-load
  |=  old-state=vase
  ^-  (quip card _this)
  ~&  >  '%btc-bridge recompiled successfully'
  `this(state !<(versioned-state old-state))
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  ?+    mark  (on-poke:def mark vase)
      %noun
    ?+    q.vase  (on-poke:def mark vase)
        %status
      :_  this
      ~[[%pass / %agent [our.bowl %btc-node-hook] %poke %btc-node-hook-action !>([%get-block-count ~])]]
    ==
  ==
::
++  on-watch  on-watch:def
++  on-leave  on-leave:def
++  on-peek   on-peek:def
++  on-agent
  |=  [=wire =sign:agent:gall]
  ^-  (quip card _this)
::  ?+  (on-agent:def wire sign)
  ~&  >>  "{<sign>}"
  `this
++  on-arvo   on-arvo:def
++  on-fail   on-fail:def
--
::  helper core
