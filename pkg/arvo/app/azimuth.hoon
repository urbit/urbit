/-  eth-watcher, *dice, *hood
/+  ethereum,
    azimuth,
    naive,
    dice,
    default-agent,
    verb,
    dbug
::  To update, run from dojo:
::    -azimuth-snap-state %default 'version-0'
::
::  To recreate from a full list of logs (at /app/azimuth/logs/eth-logs):
::    -azimuth-snap-logs %default 'version-0'
::
=,  jael
|%
+$  app-state
  $:  %7
      url=@ta
      =net
      refresh=_~m5
      whos=(set ship)
      nas=^state:naive
      own=owners
      spo=sponsors
      logs=(list =event-log:rpc:ethereum)
      sap=snap-state
  ==
::
+$  poke-data
  $%  ::  %load: load snapshot
      ::
      [%load snap=snap-state]
      ::  %listen
      ::
      [%listen whos=(list ship) =source:jael]
      ::  %watch: configure node url and network
      ::
      [%watch url=@ta =net]
      ::  %kick: re-start %azimuth subscriptions
      ::
      [%kick ~]
  ==
::
+$  tagged-diff  [=id:block diff:naive]
+$  card         card:agent:gall
--
::
=|  state=app-state
%-  agent:dbug
%+  verb  |
^-  agent:gall
::  Cards
::
=>  |%
    ++  subscribe-to-eth-watcher
      |=  =bowl:gall
      ^-  card
      :*  %pass  /eth-watcher  %agent  [our.bowl %eth-watcher]
          %watch  /logs/[dap.bowl]
      ==
    ::
    ++  listen-to-azimuth
      |=  [ships=(set ship) =source:jael]
      ^-  card
      [%pass /lo %arvo %j %listen ships source]
    ::
    ++  nuke-azimuth-tracker
      |=  =bowl:gall
      ^-  card
      :*  %pass  /old-tracker  %agent  [our.bowl %hood]
          %poke  %kiln-nuke  !>([%azimuth-tracker %|])
      ==
    ::
    ++  init-timer
      |=  at=@da
      ^-  card
      [%pass /init %arvo %b %wait at]
    ::
    ++  start-log-retrieval
      |=  [=ship args=vase]
      ^-  card
      [%pass /wa %agent [ship %eth-watcher] %poke %eth-watcher-poke args]
    ::
    ++  start-azimuth-load
      ^-  card
      [%pass /al %arvo %k %fard %base %azimuth-load %noun !>(~)]
    --
::
=<
  |_  =bowl:gall
  +*  this  .
      do    ~(. +> bowl)
      def   ~(. (default-agent this %|) bowl)
  ::
  ++  on-init
    :_  this
    ?:  .^(? %j /(scot %p our.bowl)/fake/(scot %da now.bowl))
      ~
    ~[(init-timer now.bowl)]
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
      =.  +.state  +:(state-6-to-7 (state-5-to-6 old-state))
      =^  cards  state
        (%*(run-logs do nas.state *^state:naive) logs.state)
      [(jael-update:do (to-udiffs:do cards)) state]
    =^  cards-2  old-state
      ?.  ?=(%2 -.old-state)
        `old-state
      ~&  >  '%azimuth: updating to state 3'
      =.  +.state  +:(state-6-to-7 (state-5-to-6 old-state))
      ::  replace naive state and indices with snapshot
      ::
      =:  nas.state   nas.sap.state
          own.state   owners.sap.state
          spo.state   sponsors.sap.state
          logs.state  ~
          ::  TODO: shouldn't be needed but have seen eth-watcher
          ::        threads use a url='' if this is not used
          ::
          url.state   'http://eth-mainnet.urbit.org:8545'
        ==
      =/  points=@ud  ~(wyt by points.nas.state)
      %-  %-  slog  :_  ~
          leaf+"ship: processing azimuth snapshot (~{<points>} points)"
      =/  snap-cards=udiffs:point  (run-state:do id.sap.state points.nas.state)
      :_  [%3 url net whos nas own spo logs]:state
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
        %-  %*(. on-poke +.state.this +:(state-6-to-7 (state-5-to-6 old-state)))
        [%azimuth-poke !>([%watch [url net]:old-state])]
      ~&  >  '%azimuth: updating to state 5'
      [cards [%5 url net whos nas own spo logs]:state.this]
    =?  old-state  ?=(%5 -.old-state)
      (state-5-to-6 old-state)
    =?  old-state  ?=(%6 -.old-state)
      (state-6-to-7 old-state)
    ?>  ?=(%7 -.old-state)
    [cards-4 this(state old-state)]
    ::
    ++  app-states  $%(state-0 state-1-2-3-4-5 state-6 app-state)
    ::
    +$  state-6
      $:  %6
          url=@ta
          =net
          refresh=_~m5
          whos=(set ship)
          nas=^state:naive
          own=owners
          spo=sponsors
          logs=(list =event-log:rpc:ethereum)
      ==
    +$  state-1-2-3-4-5
      $:  ?(%1 %2 %3 %4 %5)
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
    ++  state-5-to-6
      |=  state-1-2-3-4-5
      ^-  state-6
      [%6 url net ~m5 whos nas own spo logs]
    ::
    ++  state-6-to-7
      |=  state-6
      ^-  app-state
      [%7 url net refresh whos nas own spo logs *snap-state]
    --
  ::
  ++  on-poke
    |=  [=mark =vase]
    ^-  (quip card _this)
    ?>  (team:title [our src]:bowl)
    ?:  =(%noun mark)
      ?+    q.vase  !!
          [%refresh-rate @]
        =.  refresh.state  +.q.vase
        [start:do this]
      ::
          %rerun
        =/  points=@ud  ~(wyt by points.nas.state)
        ~&  >  "rerunning ({<points>} points)"
        =/  =udiffs:point
          (run-state:do (last-block-id:dice logs.state) points.nas.state)
        [(jael-update:do udiffs) this]
      ::
          %resub
        :_  this
        [(subscribe-to-eth-watcher bowl)]~
      ::
          %resnap
        =:  nas.state  nas.sap.state
            own.state  owners.sap.state
            spo.state  sponsors.sap.state
          ==
        `this
      ==
    ::
    ?.  ?=(%azimuth-poke mark)
      (on-poke:def mark vase)
    =+  !<(poke=poke-data vase)
    |-
    ?-    -.poke
        %load
      =/  points=@ud  ~(wyt by points.nas.snap.poke)
      %-  %-  slog
          [leaf+"ship: loading azimuth snapshot ({<points>} points)"]~
      ::
      =:  net.state   %default
          nas.state   nas.snap.poke
          own.state   owners.snap.poke
          spo.state   sponsors.snap.poke
          url.state   'http://eth-mainnet.urbit.org:8545'
          sap.state   snap.poke
          logs.state  ~
        ==
      $(poke [%kick ~])
    ::
        %listen
      [[(listen-to-azimuth (silt whos.poke) source.poke)]~ this]
    ::
        %kick
      =/  last-block=@
        ?^  logs.state
          number:(last-block-id:dice logs.state)
        ::  ~&  >>  %no-logs-in-azimuth-state
        number.id.sap.state
      =+  [our=(scot %p our.bowl) now=(scot %da now.bowl)]
      =+  .^(dudes=(set [dude:gall ?]) %ge our %base now /$)
      =/  running=?  (~(has in dudes) [%eth-watcher &])
      =/  installed=?
        |((~(has in dudes) [%eth-watcher &]) (~(has in dudes) [%eth-watcher |]))
      :_  this
      =/  cards=(list card)
        ?:  installed
          ~
        ::  reinstall %base desk
        ::
        =+  spo=(sein:title [our now our]:bowl)
        ~&  >>  re-installing-base-from+spo
        =/  fresh=[desk ship desk]  [%base spo %kids]
        [%pass /fresh %agent [our.bowl %hood] %poke kiln-install+!>(fresh)]~
      =?  cards  !running
        ::  restart %eth-watcher
        ::
        ~&  >>  %starting-eth-watcher
        =/  rein=[desk rein]  [%base [%eth-watcher %&] ~ ~]
        :_  cards
        [%pass /rein %agent [our.bowl %hood] %poke kiln-rein+!>(rein)]
      =.  cards
        ::  we poke eth-watcher to retrieve logs from the latest we have
        ::
        (weld %*(start do number.id.sap.state last-block) cards)
      =?  cards  !(~(has by wex.bowl) [/eth-watcher our.bowl %eth-watcher])
        ::  resubscribe if we somehow get unsubscribed from eth-watcher
        ::
        [(subscribe-to-eth-watcher bowl) cards]
      =.  cards
        ::  %jael will re-subscribe to get all azimuth diffs
        ::
        [(listen-to-azimuth ~ [%| dap.bowl]) cards]
      (flop cards)
    ::
        %watch
      =:  nas.state   ?:(?=(%default net.poke) nas.sap.state *^state:naive)
          own.state   ?:(?=(%default net.poke) owners.sap.state ~)
          spo.state   ?:(?=(%default net.poke) sponsors.sap.state ~)
          net.state   net.poke
          url.state   url.poke
          logs.state  ~
        ==
      `this
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
      (%*(run-state do logs.state ~) id.sap.state points.nas.state)
    [(weld (jael-update:do snap-cards) start:do) this]
  ::
  ++  on-leave  on-leave:def
  ++  on-peek
    |=  =path
    ^-  (unit (unit cage))
    |^
    ?+  path  (on-peek:def path)
        [%x %logs ~]       ``noun+!>(logs.state)
        [%x %nas ~]        ``noun+!>(nas.state)
        [%x %dns ~]        ``noun+!>(dns.nas.state)
        [%x %own ~]        ``noun+!>(own.state)
        [%x %spo ~]        ``noun+!>(spo.state)
        [%x %refresh ~]    ``atom+!>(refresh.state)
        [%x %point @ ~]    ``noun+(point i.t.t.path)
        [%x %last-snap ~]  ``noun+!>(sap.state)
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
    ::  now nas.state is loaded with the contents of the snapshot,
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
    ?:  &(=(/al wire) ?=(%arow +<.sign-arvo))
      ?-    -.p.sign-arvo
          %&  `this
          %|
        %-  (slog 'loading azimuth snapshot failed! still trying' p.p.sign-arvo)
        [~[(init-timer (add ~s10 now.bowl))] this]
      ==
    ?.  &(=(/init wire) ?=(%wake +<.sign-arvo))
      (on-arvo:def wire sign-arvo)
    ?^  error.sign-arvo
      %-  (slog 'azimuth: failed to initialize!' ~)
      `this
    :_  this
    ~[start-azimuth-load]
  ::
  ++  on-fail   on-fail:def
  --
|_  =bowl:gall
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
  :*  [ship id %keys [life.keys.net suite.keys.net pass] %.y]
      [ship id %rift rift.net %.y]
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
    :*  url.state  =(%czar (clan:title our.bowl))  refresh.state  ~h30
        (max launch.net ?:(=(net.state %default) +(number.id.sap.state) 0))
        ~
        ~[azimuth.net]
        ~[naive.net]
        (topics whos.state)
    ==
  [(start-log-retrieval our.bowl args)]~
--
