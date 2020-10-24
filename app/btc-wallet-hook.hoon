::  btc-wallet-hook.hoon
:: 
::
/-  *btc-wallet-hook
/+  shoe, dbug, default-agent
|%
+$  versioned-state
    $%  state-0
    ==
::
+$  state-0  [%0 provider=(unit ship)]
::
+$  card  card:shoe
+$  command
  $?  %add-xpub
  ==
::
--
=|  state-0
=*  state  -
%-  agent:dbug
^-  agent:gall
%-  (agent:shoe command)
^-  (shoe:shoe command)
=<
|_  =bowl:gall
+*  this      .
    des   ~(. (default:shoe this command) bowl)
    def   ~(. (default-agent this %|) bowl)
    hc    ~(. +> bowl)
::
++  command-parser  command-parser:des
++  tab-list  tab-list:des
++  can-connect  can-connect:des
++  on-command  on-command:des
++  on-connect  on-connect:des
++  on-disconnect  on-disconnect:des
::
++  on-init
  ^-  (quip card _this)
  ~&  >  '%btc-wallet-hook initialized'
  ::  sub to wallet-store
  `this
++  on-save
  ^-  vase
  !>(state)
++  on-load
  |=  old-state=vase
  ^-  (quip card _this)
  ~&  >  '%btc-wallet-hook recompiled'
  `this(state !<(versioned-state old-state))
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  =^  cards  state
  ?+  mark  (on-poke:def mark vase)
      %btc-wallet-hook-action
    (handle-action:hc !<(action vase))
  ==
  [cards this]
::
++  on-watch  on-watch:def
++  on-leave  on-leave:def
++  on-peek   on-peek:def
++  on-agent
  |=  [=wire =sign:agent:gall]
  ^-  (quip card _this)
  ?+  -.sign  (on-agent:def wire sign)
      %watch-ack
    ?:  ?=(%set-provider -.wire)
      `this(provider.state `src.bowl)
    `this
  ==
++  on-arvo   on-arvo:def
++  on-fail   on-fail:def
--
|_  =bowl:gall
++  handle-command
  |=  comm=command
  ^-  (quip card _state)
  ~&  >  comm
  `state
::
::  ==
++  handle-action
  |=  act=action
  ^-  (quip card _state)
  ?-  -.act
      %get-balance
    ~&  >  %get-balance
    ~&  >  addresses.act
    `state
    ::
      %set-provider
    :_  state
    [[%pass /set-provider %agent [provider.act %btc-provider] %watch /clients]]~
    ::
      %add-xpub
    ~&  >  xpub.act
    `state
  ==
--
