=>
|%
+$  card  card:agent:gall
+$  state
  $:  =incoming
      =outgoing
  ==
+$  incoming  (map deal-id incoming-deal-state)
+$  outgoing  (map path (map ship outgoing-deal-state))
+$  incoming-deal-state
  $:  =ship
      =deal
      =contract-state
  ==
+$  contract-state
  $%  [%proposed ~]
      [%ongoing file-data=octs]
      [%complete ~]
  ==
+$  outgoing-deal-state
  %-  list
  $:  challenge=@ux
      response=@ux
  ==
+$  poke
  $%  [%propose-deal =deal]
      [%accept-deal =deal-id]
      [%reject-deal =deal-id]
  ==
+$  deal
  $:  file-hash=@uw
      num-bytes=@ud
      weeks=@ud
      wei-per-week=@ud
      wei-insurance=@ud
  ==
+$  deal-id  @uw
+$  fact
  $%  [%challenge-submitted nonce=@ux]
      [%response-submitted hash=@ux]
      [%challenge-failed ~]
  ==
--
%-  agent:dbug
%+  verb  |
^-  agent:gall
|_  bol=bowl:gall
+*  this  .
    def   ~(. (default-agent this %|) bol)
::
++  on-init  on-init:def
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  ::
  ?+    mark  (on-poke:def mark vase)
      %handle-http-request
    ?>  (team:title our.bol src.bol)
    =+  !<([eyre-id=@ta =inbound-request:eyre] vase)
    !!
  ==
++  on-watch
  |=  =path
  ^-  (quip card _this)
  !!
++  on-agent
  |=  [=wire =sign:agent:gall]
  ^-  (quip card _this)
  !!
++  on-arvo
  |=  [=wire =sign-arvo]
  ^-  (quip card _this)
  !!
::
++  on-save   on-save:def
++  on-load   on-load:def
++  on-leave  on-leave:def
++  on-peek   on-peek:def
++  on-fail   on-fail:def
--
