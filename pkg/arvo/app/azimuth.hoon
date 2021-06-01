/-  eth-watcher
/+  ethereum, azimuth, naive, default-agent, verb, dbug
/*  snap  %eth-logs  /app/azimuth/logs/eth-logs
::
=/  last-snap  ::  maybe just use the last one?
  %+  roll  `(list event-log:rpc:ethereum)`~  ::snap
  |=  [log=event-log:rpc:ethereum last=@ud]
  ?~  mined.log
    last
  (max block-number.u.mined.log last)
::
=,  jael
|%
++  app-state
  $:  %2
      url=@ta
      whos=(set ship)
      nas=^state:naive
      logs=(list =event-log:rpc:ethereum)
  ==
+$  poke-data
  $%  ::  %listen
      ::
      [%listen whos=(list ship) =source:jael]
      ::  %watch: configure node url
      ::
      [%watch url=@ta]
  ==
+$  tagged-diff  [=id:block diff:naive]
::
+$  network  ?(%mainnet %ropsten %local)
--
::
|%
++  net
  ^-  network
  ::  TODO: add poke action to allow switching?
  ::        eth snapshot could also be considered
  ::
  %mainnet
::  TODO: maybe flop the endianness here so metamask signs it in normal
::  order?
::
++  verifier
  ^-  ^verifier:naive
  |=  [dat=octs v=@ r=@ s=@]
  ?:  (gth v 3)  ~  ::  TODO: move to jet
  =/  result
    %-  mule
    |.
    =,  secp256k1:secp:crypto
    %-  address-from-pub:key:ethereum
    %-  serialize-point
    (ecdsa-raw-recover (keccak-256:keccak:crypto dat) v r s)
  ?-  -.result
    %|  ~
    %&  `p.result
  ==
::
++  topics
  |=  ships=(set ship)
  ^-  (list ?(@ux (list @ux)))
  ~
::
++  data-to-hex
  |=  data=@t
  ?~  data  *@ux
  ?:  =(data '0x')  *@ux
  (hex-to-num:ethereum data)
::
++  run-logs
  |=  [nas=^state:naive logs=(list event-log:rpc:ethereum) =network]
  ^-  [(list tagged-diff) ^state:naive]
  =/  [contract=@ux * chain-id=@ *]  (get-network network)
  ?~  logs
    `nas
  ?~  mined.i.logs
    $(logs t.logs)
  =^  raw-effects  nas
    =/  =^input:naive
      ?:  =(contract address.i.logs)
        =/  data  (data-to-hex data.i.logs)
        =/  =event-log:naive
          [address.i.logs data topics.i.logs]
        [%log event-log]
      ?~  input.u.mined.i.logs
        [%bat *@]
      [%bat u.input.u.mined.i.logs]
    =/  res
      %-  mule
      |.((%*(. naive lac |) verifier chain-id nas input))
    ?-  -.res
      %&  p.res
      %|  ((slog 'naive-fail' p.res) `nas)
    ==
  =/  effects-1
    =/  =id:block  [block-hash block-number]:u.mined.i.logs
    (turn raw-effects |=(=diff:naive [id diff]))
  =^  effects-2  nas  $(logs t.logs)
  [(welp effects-1 effects-2) nas]
::
++  to-udiffs
  |=  effects=(list tagged-diff)
  ^-  =udiffs:point
  %+  murn  effects
  |=  tag=tagged-diff
  ^-  (unit [=ship =udiff:point])
  ?.  ?=(%point +<.tag)  ~
  ?+    +>+<.tag  ~
      %rift     `[ship.tag id.tag %rift rift.tag]
      %sponsor  `[ship.tag id.tag %spon sponsor.tag]
      %keys
    =/  =pass
      (pass-from-eth:azimuth 32^crypt.keys.tag 32^auth.keys.tag suite.keys.tag)
    `[ship.tag id.tag %keys life.keys.tag suite.keys.tag pass]
  ==
::
++  jael-update
  |=  =udiffs:point
  ^-  (list card:agent:gall)
  ?:  &  ~  ::  XX
  :-  [%give %fact ~[/] %azimuth-udiffs !>(udiffs)]
  |-  ^-  (list card:agent:gall)
  ?~  udiffs
    ~
  =/  =path  /(scot %p ship.i.udiffs)
  ::  Should really give all diffs involving each ship at the same time
  ::
  :-  [%give %fact ~[path] %azimuth-udiffs !>(~[i.udiffs])]
  $(udiffs t.udiffs)
::
++  aggregator-update
  |=  effects=(list tagged-diff)
  ^-  (list card:agent:gall)
  %+  murn  effects
  |=  tag=tagged-diff
  ^-  (unit card:agent:gall)
  ?.  ?=(%tx +<.tag)  ~
  %-  some
  ^-  card:agent:gall
  [%give %fact ~[/aggregator] %naive-diffs !>(+.tag)]
::
++  get-network
  |=  =network
  ^-  [@ux @ux @ @]
  =<  [azimuth naive chain-id launch]
  =,  azimuth
  ?-  network
    %mainnet  mainnet-contracts
    %ropsten  ropsten-contracts
    %local    local-contracts
  ==
::
++  start
  |=  [state=app-state =network our=ship dap=term]
  ^-  card:agent:gall
  =/  [azimuth=@ux naive=@ux * launch=@ud]  (get-network network)
  =/  args=vase  !>
    :+  %watch  /[dap]
    ^-  config:eth-watcher
    :*  url.state  =(%czar (clan:title our))  ~m5  ~h30
        (max launch last-snap)
        ~[azimuth]
        ~[naive]
        (topics whos.state)
    ==
  [%pass /wa %agent [our %eth-watcher] %poke %eth-watcher-poke args]
--
::
=|  state=app-state
%-  agent:dbug
%+  verb  |
^-  agent:gall
|_  =bowl:gall
+*  this  .
    def   ~(. (default-agent this %|) bowl)
::
++  on-init
  ^-  (quip card:agent:gall agent:gall)
  :_  this  :_  ~
  ^-  card:agent:gall
  [%pass /eth-watcher %agent [our.bowl %eth-watcher] %watch /logs/[dap.bowl]]
::
++  on-save   !>(state)
++  on-load
  |=  old=vase
  |^
  =+  !<(old-state=app-states old)
  =?  old-state  ?=(%0 -.old-state)
    %=    old-state
        -  %1
        logs
      %+  turn  logs.old-state
      |=  =event-log-0
      event-log-0(mined ?~(mined.event-log-0 ~ `mined.event-log-0))
    ==
  =?  old-state  ?=(%1 -.old-state)
    %=    old-state
        -    %2
        nas  *^state:naive
    ==
  `this(state ?>(?=(%2 -.old-state) old-state))
  ::
  ++  app-states  $%(app-state-0 app-state-1 app-state)
  ++  app-state-1
    $:  %1
        url=@ta
        whos=(set ship)
        nas=*
        logs=(list =event-log:rpc:ethereum)
    ==
  ++  app-state-0
    $:  %0
        url=@ta
        whos=(set ship)
        nas=*
        logs=(list =event-log-0)
    ==
  ::
  +$  event-log-0
    $:  $=  mined  %-  unit
        $:  log-index=@ud
            transaction-index=@ud
            transaction-hash=@ux
            block-number=@ud
            block-hash=@ux
            removed=?
        ==
      ::
        address=@ux
        data=@t
        topics=(lest @ux)
    ==
  --
::
++  on-poke
  |=  [=mark =vase]
  ?:  =(%noun mark)
    ?+    q.vase  !!
        %rerun
      ~&  [%rerunning (lent logs.state)]
      =^  effects  nas.state  (run-logs *^state:naive logs.state net)
      `this
    ::
        %resub
      :_  this  :_  ~
      [%pass /eth-watcher %agent [our.bowl %eth-watcher] %watch /logs/[dap.bowl]]
    ::
        %resnap
      =.  logs.state  snap
      $(mark %noun, vase !>(%rerun))
    ==
  ?:  =(%eth-logs mark)
    =+  !<(logs=(list event-log:rpc:ethereum) vase)
    =.  logs.state  logs
    $(mark %noun, vase !>(%rerun))
  ::
  ?.  ?=(%azimuth-poke mark)
    (on-poke:def mark vase)
  =+  !<(poke=poke-data vase)
  ?-    -.poke
      %listen  [[%pass /lo %arvo %j %listen (silt whos.poke) source.poke]~ this]
      %watch
    =.  url.state  url.poke
    [[(start state net [our dap]:bowl) ~] this]
  ==
::
++  on-watch
  |=  =path
  ^-  (quip card:agent:gall _this)
  ?<  =(/sole/drum path)
  ?:  =(/aggregator path)
    :_  this
    [%give %fact ~ %naive-state !>(nas.state)]~
  =/  who=(unit ship)
    ?~  path  ~
    ?:  ?=([@ ~] path)  ~
    `(slav %p i.path)
  =.  whos.state
    ?~  who
      ~
    (~(put in whos.state) u.who)
  :_  this  :_  ~
  (start state net [our dap]:bowl)
::
++  on-leave  on-leave:def
++  on-peek
  |=  =path
  ^-  (unit (unit cage))
  ?+  path  (on-peek:def path)
      [%x %logs ~]
    ``logs+!>(logs.state)
  ::
      [%x %nas ~]
    ``nas+!>(nas.state)
  ::
      [%x %nas @t ~]
    ?~  ship=(rush i.t.t.path ;~(pfix sig fed:ag))
      ``noun+!>(*(unit point:naive))
    ``noun+!>((~(get by points.nas.state) u.ship))
  ==
::
++  on-agent
  |=  [=wire =sign:agent:gall]
  ?.  ?=([%eth-watcher ~] wire)
    (on-agent:def wire sign)
  ?.  ?=(%fact -.sign)
    (on-agent:def wire sign)
  ?.  ?=(%eth-watcher-diff p.cage.sign)
    (on-agent:def wire sign)
  =+  !<(diff=diff:eth-watcher q.cage.sign)
  ?:  ?=(%disavow -.diff)
    [(jael-update [*ship id.diff %disavow ~]~) this]
  ::
  =.  logs.state
    ?-  -.diff
      :: %history  loglist.diff
      %history  (welp logs.state loglist.diff)
      %logs     (welp logs.state loglist.diff)
    ==
  =?  nas.state  ?=(%history -.diff)  *^state:naive
  =^  effects  nas.state
    %^    run-logs
        ?-  -.diff
          ::  %history  *^state:naive
          %history  nas.state
          %logs     nas.state
        ==
      loglist.diff
    net
  ::
  :_  this
  %+  weld
    (aggregator-update effects)
  (jael-update (to-udiffs effects))
::
++  on-arvo   on-arvo:def
++  on-fail   on-fail:def
--
