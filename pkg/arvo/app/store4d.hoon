=>
|%
+$  card  card:agent:gall
+$  state
  $:  =incoming
      =outgoing
  ==
+$  incoming
  $:  receptive=?
      price-rate=@ud  ::  wei per byte per month
      pending=(map ship [date=@da =deal])
      storing=(map ship deal-state)
  ==
+$  outgoing
  $:  pending=(map ship [date=@da =deal])
      storing=(map path (map ship deal-state))
  ==
+$  deal-state
  $:  =deal-id
      =deal
      start-date=@da
      end-date=@da
      =contract-state
  ==
+$  contract-state
  $%  [%proposed ~]
      [%live file-data=octs]
      [%complete ~]
  ==
+$  poke
  $%  [%web =web-poke]
      [%peer =agent-poke]
  ==
+$  web-poke
  $%  [%propose-deal =ship =web-pre-deal]
      [%accept-deal =deal-id]
      [%reject-deal =deal-id]
  ==
+$  peer-poke
  $%  [%propose-deal =ship =deal]
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
+$  web-pre-deal
  $:  =path
      weeks=@ud
      wei-per-week=@ud
      wei-insurance=@ud
  ==
+$  deal-id  @uw
+$  fact
  $%  [%contract =contract-fact]
      [%peer =agent-fact]
      [%web =web-fact]
  ==
+$  contract-fact
  $:  eth-address=@ux
      =deal-id
      $%  [%contract-started =deal-id eth-address=@ux]  ::  TODO other data?
          [%hash-posted =deal-id hash=@u]
          [%contract-expired =deal-id eth-address=@ux]
  ==  ==
+$  peer-fact
  $%  [%download =deal-id data-hash=@uw =octs]
  ==
+$  web-fact
  $%  [%incoming-live deals=(map ship deal-state)]
      [%incoming-pending pending=(map ship [date=@da =deal])]
      [%outgoing-live deals=(map path (map ship deal-state))]
      [%outgoing-pending pending=(map ship [date=@da =deal])]
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
  ::
      %request-deal
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
