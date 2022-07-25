/-  eth-provider
/+  ethereum, default-agent, dbug
|%
+$  versioned-state
  $%  state-0
  ==
+$  state-0  
  $:  %0
      active=active:eth-provider
      local=local:eth-provider
      provider=provider:eth-provider
      client=client:eth-provider
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
  [%0 %local 'http://localhost:8545' ['http://localhost:8545' %.n ~] ~zod]
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
        %set-local
      ?>  =(src.bowl our.bowl)
      :_  %=  state
          active  %local
          local  +.action
          ==
      ~
        %set-provider
      ?>  =(src.bowl our.bowl)
      :_  %=  state
          active  %provider
          provider  +.action
          ==
      ~
        %set-client
      ?>  =(src.bowl our.bowl)
      :_  %=  state
          active  %client
          client  +.action
          ==
      ~
        %provide
      :: Is in client or (kids active and is a kid)
      ?>  =(active.state %provider)
      ?>  ?|  (~(has in clients:provider) src.bowl)
              ?&  kids:provider
                  =((sein:title our.bowl now.bowl src.bowl) our.bowl)
              ==
          ==
      =/  tid  +<.action
      =/  eth-input  +>.action
      =/  start-args  
      [~ `tid byk.bowl(r da+now.bowl) %eth-provider !>(eth-input)]
      =/  ta-now  `@ta`(scot %da now.bowl)
      :_  state
      :~
          :*
          %pass   /thread/[ta-now] 
          %agent  [our.bowl %spider] 
          %watch  /thread-result/[tid]
          ==
          :*
          %pass   /thread/[ta-now] 
          %agent  [our.bowl %spider]  
          %poke   %spider-start  !>(start-args)
          ==
      ==
        %set-kids
      ?>  =(src.bowl our.bowl)
      :_  %=  state
          provider  provider(kids +.action)
          ==
      ~
        %add-client
      ?>  =(src.bowl our.bowl)
      :_  %=  state
          provider  provider(clients (~(put in clients:provider) +.action))
          ==
      ~
        %remove-client
      ?>  =(src.bowl our.bowl)
      :_  %=  state
          provider  provider(clients (~(del in clients:provider) +.action))
          ==
      ~
    ==
  --
::
++  on-watch  
  |=  =path
  ^-  (quip card _this)
  ?+    path  (on-watch:def path)
      [%updates @ ~]
    :_  this  ~
  ==
++  on-leave  on-leave:def
++  on-peek   
  |=  =path
  ^-  (unit (unit cage))
  ?+    path  (on-peek:def path)
      [%x %get-state ~]  
    ``noun+!>(+.state)
  ==
++  on-agent
   |=  [=wire =sign:agent:gall]
   ^-  (quip card _this)
   ?+    -.wire  (on-agent:def wire sign)
       %thread
     ?+    -.sign  (on-agent:def wire sign)
         %poke-ack
       ?~  p.sign
         %-  (slog leaf+"Thread started successfully" ~)
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
         =/  res  !<([@ta ethout:eth-provider] q.cage.sign)
         =/  eth-output  +.res
         :-  
         :~
         [%give %fact ~[[%updates -.res ~]] %ethout !>(eth-output)]
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
