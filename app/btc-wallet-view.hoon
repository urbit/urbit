::  btc-wallet-view.hoon
::  receive signing requests from btc-wallet-hook
::
/-  *btc-wallet-view
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
::
++  on-init
  ^-  (quip card _this)
  ~&  >  '%btc-wallet-view initialized  successfully'
  =/  filea  [%file-server-action !>([%serve-dir /'~btc-wallet' /app/btc-wallet %.n %.y])]
  :_  this
  :~  [%pass /srv %agent [our.bowl %file-server] %poke filea]
      [%pass /u/[(scot %da now.bowl)] %agent [our.bowl %btc-wallet-hook] %watch /sign-me]
  ==
++  on-save
  ^-  vase
  !>(state)
++  on-load
  |=  old-state=vase
  ^-  (quip card _this)
  ~&  >  '%btc-wallet-view recompiled successfully'
  `this(state !<(versioned-state old-state))
++  on-poke
  |=  [=mark =vase]
  |^  ^-  (quip card _this)
  ?+  mark  (on-poke:def mark vase)
    %btc-wallet-view-action
    (handle-action !<(action vase))
  ==
  ++  handle-action
    |=  =action
    ~&  >>>  action
    `this
  --
::
++  on-watch
  |=  =path
  ^-  (quip card _this)
  ?+    path  (on-watch:def path)
      [%primary ~]
    `this
  ==
++  on-leave  on-leave:def
++  on-peek   on-peek:def
++  on-agent  on-agent:def
++  on-arvo   on-arvo:def
++  on-fail   on-fail:def
--
