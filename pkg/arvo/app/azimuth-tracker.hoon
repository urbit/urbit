/-  eth-watcher
/+  default-agent, verb
=,  able:jael
|%
++  app-state
  $:  %0
      url=@ta
      whos=(set ship)
  ==
+$  poke-data
  $%  ::  %listen
      ::
      [%listen whos=(list ship) =source:jael]
      ::  %watch: configure node url
      ::
      [%watch url=@ta]
  ==
--
::
|%
++  topics
  |=  ships=(set ship)
  ^-  (list ?(@ux (list @ux)))
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
++  event-logs-to-udiffs
  |=  event-logs=(list =event-log:rpc:ethereum)
  ^-  =udiffs:point
  %+  murn  event-logs
  |=  =event-log:rpc:ethereum
  ^-  (unit [=ship =udiff:point])
  ?~  mined.event-log
    ~
  ?:  removed.u.mined.event-log
    ~&  [%removed-log event-log]
    ~
  =/  =id:block  [block-hash block-number]:u.mined.event-log
  =,  azimuth-events:azimuth
  =,  abi:ethereum
  ?:  =(broke-continuity i.topics.event-log)
    =/  who=@  (decode-topics t.topics.event-log ~[%uint])
    =/  num=@  (decode-results data.event-log ~[%uint])
    `[who id %rift num]
  ?:  =(changed-keys i.topics.event-log)
    =/  who=@  (decode-topics t.topics.event-log ~[%uint])
    =/  [enc=octs aut=octs sut=@ud rev=@ud]
        %+  decode-results  data.event-log
        ~[[%bytes-n 32] [%bytes-n 32] %uint %uint]
    `[who id %keys rev sut (pass-from-eth:azimuth enc aut sut)]
  ?:  =(lost-sponsor i.topics.event-log)
    =/  [who=@ pos=@]
        (decode-topics t.topics.event-log ~[%uint %uint])
    `[who id %spon ~]
  ?:  =(escape-accepted i.topics.event-log)
    =/  [who=@ wer=@]
        (decode-topics t.topics.event-log ~[%uint %uint])
    `[who id %spon `wer]
  ~&  [%bad-topic event-log]
  ~
::
++  jael-update
  |=  =udiffs:point
  ^-  (list card:agent:gall)
  ?~  udiffs
    ~
  =/  =path  /(scot %p ship.i.udiffs)
  :*  [%give %fact ~[/] %azimuth-udiff !>(i.udiffs)]
      [%give %fact ~[path] %azimuth-udiff !>(i.udiffs)]
      $(udiffs t.udiffs)
  ==
::
++  start
  |=  [state=app-state our=ship dap=term]
  ^-  card:agent:gall
  =/  args=vase  !>
    :+  %watch  /[dap]
    ^-  config:eth-watcher
    :*  url.state  =(%czar (clan:title our))  ~m5  ~m30
        launch:contracts:azimuth
        ~[azimuth:contracts:azimuth]
        (topics whos.state)
    ==
  [%pass /wa %agent [our %eth-watcher] %poke %eth-watcher-poke args]
--
::
=|  state=app-state
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
++  on-peek   on-peek:def
++  on-agent
  |=  [=wire =sign:agent:gall]
  ?.  ?=([%eth-watcher ~] wire)
    (on-agent:def wire sign)
  ?.  ?=(%fact -.sign)
    (on-agent:def wire sign)
  ?.  ?=(%eth-watcher-diff p.cage.sign)
    (on-agent:def wire sign)
  =+  !<(diff=diff:eth-watcher q.cage.sign)
  :_  this
  ^-  (list card:agent:gall)
  %-  jael-update
  ?-  -.diff
    %history  (event-logs-to-udiffs loglist.diff)
    %log      (event-logs-to-udiffs event-log.diff ~)
    %disavow  [*ship id.diff %disavow ~]~
  ==
::
++  on-arvo   on-arvo:def
++  on-fail   on-fail:def
--
