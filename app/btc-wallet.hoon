::  btc-wallet.hoon
::  Manages wallet pubkeys
::
/-  *btc-wallet
/+  shoe, dbug, default-agent
|%
+$  versioned-state
    $%  state-0
    ==
::
+$  state-0  [%0 connected=?]
::
+$  card  card:shoe
+$  command
  $?  %add-wallet
  ==
::
--
=|  state-0
=*  state  -
%-  agent:dbug
^-  agent:gall
%-  (agent:shoe command)
^-  (shoe:shoe command)
|_  =bowl:gall
+*  this      .
    des   ~(. (default:shoe this command) bowl)
    def   ~(. (default-agent this %|) bowl)
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
  ~&  >  '%btc-wallet initialized'
  `this
++  on-save
  ^-  vase
  !>(state)
++  on-load
  |=  old-state=vase
  ^-  (quip card _this)
  ~&  >  '%btc-wallet recompiled'
  `this(state !<(versioned-state old-state))
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  `this
::
++  on-watch  on-watch:def
++  on-leave  on-leave:def
++  on-peek   on-peek:def
++  on-agent  on-agent:def
::  TODO: add our provider to status if we're a client when subscription is ack'd here
++  on-arvo   on-arvo:def
++  on-fail   on-fail:def
--

::      ~[[%pass /connect-client %agent [host.comm %btc-provider] %watch /clients]]
