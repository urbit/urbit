/-  eth-watcher
/+  ethereum, azimuth, naive, default-agent, verb, dbug
/*  snap  %eth-logs  /app/naive/logs/eth-logs
::
=/  last-snap  ::  maybe just use the last one?
  %+  roll  `(list event-log:rpc:ethereum)`snap
  |=  [log=event-log:rpc:ethereum last=@ud]
  ?~  mined.log
    last
  (max block-number.u.mined.log last)
::
=,  jael
|%
++  app-state
  $:  %1
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
--
::
|%
::  TODO: is `dat` supposed to be a 32-byte hash?  I guess so
::
++  verifier
  ^-  ^verifier:naive
  |=  [dat=@ v=@ r=@ s=@]
  =/  result
    %-  mule
    |.
    =,  secp256k1:secp:crypto
    %-  address-from-pub:key:ethereum
    %-  serialize-point
    (ecdsa-raw-recover dat v r s)
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
  |=  [nas=^state:naive logs=(list event-log:rpc:ethereum)]
  ^-  [(list tagged-diff) ^state:naive]
  ?~  logs
    `nas
  ?~  mined.i.logs
    $(logs t.logs)
  =^  raw-effects  nas
    =/  =^input:naive
      ?:  =(azimuth:contracts:azimuth address.i.logs)
        =/  data  (data-to-hex data.i.logs)
        =/  =event-log:naive
          [address.i.logs data topics.i.logs]
        [%log event-log]
      ?~  input.u.mined.i.logs
        [%bat *@]
      ?.  =(0x2688.7f26 (end [3 4] (swp 5 u.input.u.mined.i.logs)))
        [%bat *@]
      [%bat (rsh [3 4] u.input.u.mined.i.logs)]
    =/  res  (mule |.((%*(. naive lac |) verifier nas input)))
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
++  run-batch
  |=  [nas=^state:naive batch=@]
  ^+  *naive
  (%*(. naive lac |) verifier nas %bat batch)
::
++  to-udiffs
  |=  effects=(list tagged-diff)
  ^-  =udiffs:point
  %+  murn  effects
  |=  tag=tagged-diff
  ^-  (unit [=ship =udiff:point])
  ?.  ?=(%point +<.tag)  ~
  ?+  +>+<.tag  ~
    %rift     `[ship.tag id.tag %rift rift.tag]
    %keys     `[ship.tag id.tag %keys [life crypto-suite pass]:tag]
    %sponsor  `[ship.tag id.tag %spon sponsor.tag]
  ==
::
++  jael-update
  |=  =udiffs:point
  ^-  (list card:agent:gall)
  ?:  &  ~
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
++  start
  |=  [state=app-state our=ship dap=term]
  ^-  card:agent:gall
  =/  args=vase  !>
    :+  %watch  /[dap]
    ^-  config:eth-watcher
    :*  url.state  =(%czar (clan:title our))  ~m5  ~h30
        (max launch:contracts:azimuth last-snap)
        ~[azimuth:contracts:azimuth]
        ~[naive:contracts:azimuth]
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
  `this(state ?>(?=(%1 -.old-state) old-state))
  ::
  ++  app-states  $%(app-state-0 app-state)
  ++  app-state-0
    $:  %0
        url=@ta
        whos=(set ship)
        nas=^state:naive
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
      =^  effects  nas.state  (run-logs *^state:naive logs.state)
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
  ?.  ?=(%azimuth-tracker-poke mark)
    (on-poke:def mark vase)
  =+  !<(poke=poke-data vase)
  ?-    -.poke
      %listen  [[%pass /lo %arvo %j %listen (silt whos.poke) source.poke]~ this]
      %watch
    =.  url.state  url.poke
    [[(start state [our dap]:bowl) ~] this]
  ==
::
++  on-watch
  |=  =path
  ^-  (quip card:agent:gall _this)
  ?<  =(/sole/drum path)
  ?>  ?=(?(~ [@ ~]) path)
  =/  who=(unit ship)
    ?~  path  ~
    `(slav %p i.path)
  =.  whos.state
    ?~  who
      ~
    (~(put in whos.state) u.who)
  :_  this  :_  ~
  (start state [our dap]:bowl)
::
++  on-leave  on-leave:def
++  on-peek
  |=  =path
  ?:  =(/x/nas path)
    ``nas+!>(nas.state)
  ?:  =(/x/logs path)
    ``logs+!>(logs.state)
  ~
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
    %+  run-logs
      ?-  -.diff
        ::  %history  *^state:naive
        %history  nas.state
        %logs     nas.state
      ==
    loglist.diff
  ::
  [(jael-update (to-udiffs effects)) this]
::
++  on-arvo   on-arvo:def
++  on-fail   on-fail:def
--
