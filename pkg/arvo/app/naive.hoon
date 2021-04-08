/-  eth-watcher
/+  ethereum, azimuth, naive, default-agent, verb, dbug
=,  jael
|%
++  app-state
  $:  %0
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
  ?:  &  ~
  ::  The first topic should be one of these event types
  ::
  :-  =>  azimuth-events:azimuth
      :~  broke-continuity
          changed-keys
          lost-sponsor
          escape-accepted
      ==
  ::  If we're looking for a specific set of ships, specify them as
  ::  the second topic.  Otherwise don't specify the second topic so
  ::  we will match all ships.
  ::
  ?:  =(~ ships)
    ~
  [(turn ~(tap in ships) ,@) ~]
::
++  run-logs
  |=  [nas=^state:naive logs=(list event-log:rpc:ethereum)]
  ^-  [(list tagged-diff) ^state:naive]
  ?~  logs
    `nas
  ?~  mined.i.logs
    $(logs t.logs)
  =^  raw-effects  nas
    =/  data
      ?~  data.i.logs  *@ux
      ?:  =(data.i.logs '0x')  *@ux
      ~|  data.i.logs
      (hex-to-num:ethereum data.i.logs)
    =/  =event-log:naive
      [address.i.logs data topics.i.logs]
    =/  res  (mule |.((naive verifier nas %log event-log)))
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
  (naive verifier nas %bat batch)
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
        launch:contracts:azimuth
        ~[azimuth:contracts:azimuth]
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
  `this(state !<(app-state old))
::
++  on-poke
  |=  [=mark =vase]
  ?:  =(%noun mark)
    ?+    q.vase  !!
        %rerun
      =^  effects  nas.state  (run-logs *^state:naive logs.state)
      `this
    ::
        %resub
      :_  this  :_  ~
      [%pass /eth-watcher %agent [our.bowl %eth-watcher] %watch /logs/[dap.bowl]]
    ==
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
      %history  loglist.diff
      %logs     (welp logs.state loglist.diff)
    ==
  =^  effects  nas.state
    %+  run-logs
      ?-  -.diff
        %history  *^state:naive
        %logs     nas.state
      ==
    loglist.diff
  ::
  [(jael-update (to-udiffs effects)) this]
::
++  on-arvo   on-arvo:def
++  on-fail   on-fail:def
--
