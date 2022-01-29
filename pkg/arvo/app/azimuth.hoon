/-  eth-watcher, *dice
/+  ethereum,
    azimuth,
    naive,
    dice,
    default-agent,
    verb,
    dbug
::  Generally don't update the snapshot until we have clay tombstoning.
::
/*  snap  %azimuth-snapshot  /app/azimuth/version-0/azimuth-snapshot
::  To update, run from dojo:
::    -azimuth-snap-state %default 'version-0'
::
::  To recreate from a full list of logs (at /app/azimuth/logs/eth-logs):
::    -azimuth-snap-logs %default 'version-0'
::
=/  snap=snap-state  snap
=/  last-snap=@      number.id.snap
::
=,  jael
|%
+$  app-state
  $:  %5
      url=@ta
      =net
      whos=(set ship)
      nas=^state:naive
      own=owners
      spo=sponsors
      logs=(list =event-log:rpc:ethereum)
  ==
::
+$  poke-data
  $%  ::  %listen
      ::
      [%listen whos=(list ship) =source:jael]
      ::  %watch: configure node url and network
      ::
      [%watch url=@ta =net]
  ==
::
+$  tagged-diff  [=id:block diff:naive]
+$  card         card:agent:gall
::  TODO: add to state?
::
++  refresh      ~m5
--
::
=|  state=app-state
%-  agent:dbug
%+  verb  |
^-  agent:gall
=<
  |_  =bowl:gall
  +*  this  .
      do    ~(. +> bowl)
      def   ~(. (default-agent this %|) bowl)
  ::
  ++  on-init
    ^-  (quip card _this)
    =/  points=@ud  ~(wyt by points.nas.snap)
    %-  %-  slog
        [leaf+"ship: loading azimuth snapshot ({<points>} points)"]~
    ::
    =:  net.state   %default
        nas.state   nas.snap
        own.state   owners.snap
        spo.state   sponsors.snap
        url.state   'http://eth-mainnet.urbit.org:8545'
      ==
    :_  this
    ?:  .^(? %j /(scot %p our.bowl)/fake/(scot %da now.bowl))
      ~
    :~  :*  %pass  /old-tracker  %agent  [our.bowl %hood]
            %poke  %kiln-nuke  !>([%azimuth-tracker %|])
        ==
      ::
        [%pass /init %arvo %b %wait now.bowl]
    ==
  ::
  ++  on-save   !>(state)
  ++  on-load
    |=  old=vase
    ^-  (quip card _this)
    |^
    =+  !<(old-state=app-states old)
    =?  old-state  ?=(%0 -.old-state)
      =,  old-state
      [%1 url net whos nas own *sponsors logs]
    =^  cards-1  old-state
      ?.  ?=(%1 -.old-state)
        `old-state
      %-  %-  slog  :_  ~
          leaf+"ship: loading snapshot with {<(lent logs.old-state)>} events"
      =.  +.state  +.old-state
      =^  cards  state
        (%*(run-logs do nas.state *^state:naive) logs.state)
      [(jael-update:do (to-udiffs:do cards)) state]
    =^  cards-2  old-state
      ?.  ?=(%2 -.old-state)
        `old-state
      ~&  >  '%azimuth: updating to state 3'
      =.  +.state  +.old-state
      ::  replace naive state and indices with snapshot
      ::
      =:  nas.state   nas.snap
          own.state   owners.snap
          spo.state   sponsors.snap
          logs.state  ~
          ::  TODO: shouldn't be needed but have seen eth-watcher
          ::        threads use a url='' if this is not used
          ::
          url.state   'http://eth-mainnet.urbit.org:8545'
        ==
      =/  points=@ud  ~(wyt by points.nas.state)
      %-  %-  slog  :_  ~
          leaf+"ship: processing azimuth snapshot ({<points>} points)"
      =/  snap-cards=udiffs:point  (run-state:do id.snap points.nas.state)
      :_  [%3 +.state]
      %+  weld
        (jael-update:do snap-cards)
      ::  start getting new logs after the last id:block in the snapshot
      ::
      start:do
    =^  cards-3  old-state
      ?.  ?=(%3 -.old-state)  [cards-2 old-state]
      :_  old-state(- %4)
      ~&  >  '%azimuth: updating to state 4'
      [%pass /resend-pk %arvo %j %resend ~]^cards-2
    =^  cards-4  old-state
      ?.  ?=(%4 -.old-state)  [cards-3 old-state]
      =^  cards  this
        %-  %*(. on-poke +.state.this +.old-state)
        [%azimuth-poke !>([%watch [url net]:old-state])]
      ~&  >  '%azimuth: updating to state 5'
      [cards state.this(- %5)]
    ?>  ?=(%5 -.old-state)
    [cards-4 this(state old-state)]
    ::
    ++  app-states  $%(state-0 state-1-2-3-4 app-state)
    ::
    +$  state-1-2-3-4
      $:  ?(%1 %2 %3 %4)
          url=@ta
          =net
          whos=(set ship)
          nas=^state:naive
          own=owners
          spo=sponsors
          logs=(list =event-log:rpc:ethereum)
      ==
    ::
    ++  state-0
      $:  %0
          url=@ta
          =net
          whos=(set ship)
          nas=^state:naive
          own=owners
          logs=(list =event-log:rpc:ethereum)
      ==
    --
  ::
  ++  on-poke
    |=  [=mark =vase]
    ^-  (quip card _this)
    ?:  =(%noun mark)
      ?+    q.vase  !!
      ::
          %rerun
        =/  points=@ud  ~(wyt by points.nas.state)
        ~&  >  "rerunning ({<points>} points)"
        =/  =udiffs:point
          (run-state:do (last-block-id:dice logs.state) points.nas.state)
        [(jael-update:do udiffs) this]
      ::
          %resub
        :_  this  :_  ~
        :*  %pass  /eth-watcher  %agent  [our.bowl %eth-watcher]
            %watch  /logs/[dap.bowl]
        ==
      ::
          %resnap
        =:  nas.state  nas.snap
            own.state  owners.snap
            spo.state  sponsors.snap
          ==
        `this
      ==
    ::
    ?.  ?=(%azimuth-poke mark)
      (on-poke:def mark vase)
    =+  !<(poke=poke-data vase)
    ?-    -.poke
        %listen
      [[%pass /lo %arvo %j %listen (silt whos.poke) source.poke]~ this]
    ::
        %watch
      =:  nas.state   ?:(?=(%default net.poke) nas.snap *^state:naive)
          own.state   ?:(?=(%default net.poke) owners.snap ~)
          spo.state   ?:(?=(%default net.poke) sponsors.snap ~)
          net.state   net.poke
          url.state   url.poke
          logs.state  ~
        ==
      [start:do this]
    ==
  ::
  ++  on-watch
    |=  =path
    ^-  (quip card _this)
    ?<  =(/sole/drum path)
    ?:  =(/event path)
      :_  this
      [%give %fact ~ %naive-state !>([nas.state own.state spo.state])]~
    =/  who=(unit ship)
      ?~  path  ~
      ?:  ?=([@ ~] path)  ~
      `(slav %p i.path)
    =.  whos.state
      ?~  who
        ~
      (~(put in whos.state) u.who)
    ^-  (quip card _this)
    ::  Slow to recalculate all the diffs, but this is necessary to make
    ::  sure Jael gets the updates from the snapshot
    ::
    =/  points=@ud  ~(wyt by points.nas.state)
    %-  %-  slog  :_  ~
        :-  %leaf
        "ship: processing azimuth snapshot ({<points>} points)"
    =/  snap-cards=udiffs:point
      (%*(run-state do logs.state ~) id.snap points.nas.state)
    [(weld (jael-update:do snap-cards) start:do) this]
  ::
  ++  on-leave  on-leave:def
  ++  on-peek
    |=  =path
    ^-  (unit (unit cage))
    |^
    ?+  path  (on-peek:def path)
        [%x %logs ~]     ``noun+!>(logs.state)
        [%x %nas ~]      ``noun+!>(nas.state)
        [%x %dns ~]      ``noun+!>(dns.nas.state)
        [%x %own ~]      ``noun+!>(own.state)
        [%x %spo ~]      ``noun+!>(spo.state)
        [%x %refresh ~]  ``atom+!>(refresh)
        [%x %point @ ~]  ``noun+(point i.t.t.path)
    ==
    ::
    ++  point
      |=  wat=@t
      ^-  vase
      !>  ^-  (unit point:naive)
      ?~  ship=(rush wat ;~(pfix sig fed:ag))
        ~
      (get:orm:naive points.nas.state u.ship)
    --
  ::
  ++  on-agent
    |=  [=wire =sign:agent:gall]
    ^-  (quip card _this)
    ?.  ?=([%eth-watcher ~] wire)
      (on-agent:def wire sign)
    ?.  ?=(%fact -.sign)
      (on-agent:def wire sign)
    ?.  ?=(%eth-watcher-diff p.cage.sign)
      (on-agent:def wire sign)
    =+  !<(diff=diff:eth-watcher q.cage.sign)
    ?:  ?=(%disavow -.diff)
      [(jael-update:do [*ship id.diff %disavow ~]~) this]
    ::
    =.  logs.state
      ?-  -.diff
        :: %history  loglist.diff
        %history  (welp logs.state loglist.diff)
        %logs     (welp logs.state loglist.diff)
      ==
    ::  doing :azimuth|watch caused a l2-sig-fail when using the eth-log
    ::  snapshot because we were not updating nas with the saved logs.
    ::
    ::  now (L: 189) nas.state is loaded with the contents of the snapshot,
    ::  if we are on the %default network.
    ::
    =^  effects  state  (run-logs:do loglist.diff)
    :_  this
    %+  weld
      (event-update:do effects)
    (jael-update:do (to-udiffs:do effects))
  ::
  ++  on-arvo
    |=  [=wire =sign-arvo]
    ?.  &(=(/init wire) ?=(%wake +<.sign-arvo))
      (on-arvo:def wire sign-arvo)
    ?^  error.sign-arvo
      %-  (slog 'azimuth: failed to initialize!' ~)
      `this
    :_  this
    :~  :*  %pass  /eth-watcher  %agent  [our.bowl %eth-watcher]
            %watch  /logs/[dap.bowl]
        ==
      ::
        [%pass /lo %arvo %j %listen ~ [%| dap.bowl]]
    ==
  ::
  ++  on-fail   on-fail:def
  --
|_  =bowl:gall
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
++  run-state
  |=  [=id:block =points:naive]
  ::%-  road  |.  :: count memory usage in a separate road
  ^-  =udiffs:point
  %-  flop
  %+  roll  (tap:orp:dice points)
  |=  [[=ship naive-point=point:naive] =udiffs:point]
  =,  naive-point
  =/  =pass
    (pass-from-eth:azimuth [32^crypt 32^auth suite]:keys.net)
  ^-  (list [@p udiff:point])
  :*  [ship id %rift rift.net %.y]
      [ship id %keys [life.keys.net suite.keys.net pass] %.y]
      [ship id %spon ?:(has.sponsor.net `who.sponsor.net ~)]
      udiffs
  ==
::
++  run-logs
  |=  [logs=(list event-log:rpc:ethereum)]
  ^-  (quip tagged-diff _state)
  =+  net=(get-network:dice net.state)
  =|  effects=(list tagged-diff)
  !.  ::  saves 700MB replaying snapshot
  =-  =/  res  (mule -)
      ?-  -.res
        %&  p.res
        %|  (mean 'naive: fail!' p.res)
      ==
  |.
  ?~  logs
    [(flop effects) state]
  ?~  mined.i.logs
    $(logs t.logs)
  =+  cache=nas.state
  =^  raw-effects  nas.state
    =/  =^input:naive
      :-  block-number.u.mined.i.logs
      ?:  =(azimuth.net address.i.logs)
        =/  data  (data-to-hex:dice data.i.logs)
        =/  =event-log:naive
          [address.i.logs data topics.i.logs]
        [%log event-log]
      ?~  input.u.mined.i.logs
        [%bat *@]
      [%bat u.input.u.mined.i.logs]
    (%*(. naive lac |) verifier chain-id.net nas.state input)
  ::  TODO: make index update optional?
  ::
  =/  =indices  [own spo]:state
  =.  indices
    ?:  =(naive.net address.i.logs)
      (tx-effects:dice chain-id.net raw-effects cache indices)
    =<  indices
    (point-effects:dice raw-effects points.cache points.nas.state indices)
  =:  own.state  own.indices
      spo.state  spo.indices
    ==
  =/  effects-1
    =/  =id:block  [block-hash block-number]:u.mined.i.logs
    (turn raw-effects |=(=diff:naive [id diff]))
  =.  effects  (welp (flop effects-1) effects)
  $(logs t.logs)
::
++  to-udiffs
  |=  effects=(list tagged-diff)
  ^-  =udiffs:point
  %+  murn  effects
  |=  tag=tagged-diff
  ^-  (unit [=ship =udiff:point])
  ?.  ?=(%point +<.tag)  ~
  ?+    +>+<.tag  ~
      %rift     `[ship.tag id.tag %rift rift.tag %.n]
      %sponsor  `[ship.tag id.tag %spon sponsor.tag]
      %keys
    =/  =pass
      (pass-from-eth:azimuth 32^crypt.keys.tag 32^auth.keys.tag suite.keys.tag)
    `[ship.tag id.tag %keys [life.keys.tag suite.keys.tag pass] %.n]
  ==
::
++  jael-update
  |=  =udiffs:point
  ^-  (list card)
  ::  ?:  &  ~  ::  XX
  :-  [%give %fact ~[/] %azimuth-udiffs !>(udiffs)]
  |-  ^-  (list card)
  ?~  udiffs
    ~
  =/  =path  /(scot %p ship.i.udiffs)
  ::  Should really give all diffs involving each ship at the same time
  ::
  :-  [%give %fact ~[path] %azimuth-udiffs !>(~[i.udiffs])]
  $(udiffs t.udiffs)
::
++  event-update
  |=  effects=(list tagged-diff)
  ^-  (list card)
  %+  murn  effects
  |=  tag=tagged-diff
  ^-  (unit card)
  ?.  |(?=(%tx +<.tag) ?=(%point +<.tag))  ~
  %-  some
  ^-  card
  [%give %fact ~[/event] %naive-diffs !>(+.tag)]
::
++  start
  ^-  (list card)
  =+  net=(get-network:dice net.state)
  =/  args=vase  !>
    :+  %watch  /[dap.bowl]
    ^-  config:eth-watcher
    :*  url.state  =(%czar (clan:title our.bowl))  refresh  ~h30
        (max launch.net ?:(=(net.state %default) +(last-snap) 0))
        ~[azimuth.net]
        ~[naive.net]
        (topics whos.state)
    ==
  [%pass /wa %agent [our.bowl %eth-watcher] %poke %eth-watcher-poke args]~
--
