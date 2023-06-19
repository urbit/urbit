/-  eth-provider, rpc=json-rpc
/+  ethereum, default-agent, dbug
|%
+$  versioned-state
  $%  state-0
  ==
+$  state-0  
  $:  %0
      provider-mode=provider-mode:eth-provider
  ==
+$  card  card:agent:gall
--
%-  agent:dbug
=|  state-0
=*  state  -
^-  agent:gall
=<
|_  =bowl:gall
+*  this  .
    def   ~(. (default-agent this %.n) bowl)
::
++  on-init
  ^-  (quip card _this)
  =/  init-state  
  [%0 %local 'http://eth-mainnet.urbit.org:8545']
  :-  ~
  this(state init-state)
::
++  on-save
  ^-  vase
  !>(state)
::
++  on-load
  |=  old-state=vase
  ^-  (quip card _this)
  =/  old  !<(versioned-state old-state)
  ?-  -.old
    %0  `this(state old)
  ==
::
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  |^
  ?+    mark  (on-poke:def mark vase)
      %provider-action
    =^  cards  state
      (handle-poke !<(action:eth-provider vase))
    [cards this]
  ==
  ++  handle-poke
    |=  =action:eth-provider
    ^-  (quip card _state)
    ?-    -.action
        %configure
      ?>  =(src.bowl our.bowl)
      :_  %=  state
          provider-mode  +.action
          ==
      ~
        %provide
      :: Is in client or (kids active and is a kid)
      :: ?:  =(-.provider-mode.state %provider)
      ?>  ?=(%provider -.provider-mode.state)
      =/  provider  `provider:eth-provider`+.provider-mode.state
      ?>  ?|  (~(has in clients.provider) src.bowl)
              ?&  kids.provider
                  =((sein:title our.bowl now.bowl src.bowl) our.bowl)
              ==
          ==
      =/  rid  +<.action
      =/  eth-input  +>.action
      =/  start-args  
      [~ `rid byk.bowl(r da+now.bowl) %eth-provider !>(eth-input)]
      =/  ta-now  `@ta`(scot %da now.bowl)
      :_  state
      :~
          :*
          %pass   /thread/[ta-now] 
          %agent  [our.bowl %spider] 
          %watch  /thread-result/[rid]
          ==
          :*
          %pass   /thread/[ta-now] 
          %agent  [our.bowl %spider]  
          %poke   %spider-start  !>(start-args)
          ==
      ==
    ==
  --
::
++  on-watch  
  |=  =path
  ^-  (quip card _this)
  ?+    path  (on-watch:def path)
      [%responses @ ~]
    ?>  ?=(%provider -.provider-mode.state)
    =/  provider  `provider:eth-provider`+.provider-mode.state
    ?>  (~(has in clients.provider) src.bowl)
    :_  this  ~
  ==
++  on-leave  on-leave:def
++  on-peek   
  |=  =path
  ^-  (unit (unit cage))
  ?+    path  (on-peek:def path)
      [%x %get-provider-mode ~]  
    ``noun+!>(provider-mode.state)
  ==
++  on-agent
   |=  [=wire =sign:agent:gall]
   ^-  (quip card _this)
   ?+    -.wire  (on-agent:def wire sign)
       %thread
     ?+    -.sign  (on-agent:def wire sign)
         %poke-ack
       ?~  p.sign
         :: %-  (slog leaf+"Thread started successfully" ~)
         `this
       %-  (slog leaf+"Thread failed to start" u.p.sign)
       `this
     ::
         %fact
       ?+    p.cage.sign  (on-agent:def wire sign)
           %thread-fail
         =/  err  !<  (pair term tang)  q.cage.sign
         %-  (slog leaf+"Thread failed: {(trip p.err)}" q.err)
         `this
           %thread-done
         =/  res  !<([@ta (list [id=(unit @t) dirty-response:rpc:ethereum])] q.cage.sign)
         =/  eth-output  +.res
         :-  
         :: ~
         :~
         [%give %fact ~[[%responses -.res ~]] %ethout !>(eth-output)]
         ==
         this
       ==
     ==
   ==
++  on-arvo   on-arvo:def
++  on-fail   on-fail:def
--
::  helper core
|%
++  get-url  'url'
--
