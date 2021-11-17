/-  eth-watcher, *dice
/+  ethereum,
    azimuth,
    naive,
    dice,
    default-agent,
    verb,
    dbug
::  Generally don't update the snapshot until we have clay tombstoning.
::  To update, run:
::    =e -build-file %/lib/ethereum/hoon
::    =l .^((list event-log:rpc:e) %gx /=azimuth=/logs/noun)
::    */app/azimuth/logs/eth-logs &eth-logs l
::
/*  snap  %eth-logs  /app/azimuth/logs/eth-logs
::
=/  last-snap  ::  maybe just use the last one?
  %+  roll  `(list event-log:rpc:ethereum)`snap  ::~
  |=  [log=event-log:rpc:ethereum last=@ud]
  ?~  mined.log
    last
  (max block-number.u.mined.log last)
::
=,  jael
|%
+$  app-state
  $:  %1
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
    =:  net.state   %default
        logs.state  snap
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
      ^-  app-state
      =,  old-state
      [%1 url net whos nas own *sponsors logs]
    ?>  ?=(%1 -.old-state)
    `this(state old-state)
    ::
    ++  app-states  $%(state-0 app-state)
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
          %rerun
        ~&  [%rerunning (lent logs.state)]
        =.  points.nas.state  ~
        =.  own.state  ~
        =^  *  state  (run-logs:do logs.state)
        `this
      ::
          %resub
        :_  this  :_  ~
        :*  %pass  /eth-watcher  %agent  [our.bowl %eth-watcher]
            %watch  /logs/[dap.bowl]
        ==
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
        %listen
      [[%pass /lo %arvo %j %listen (silt whos.poke) source.poke]~ this]
    ::
        %watch
      :: TODO: only wipe out state when switching networks?
      :: ?:  =(net.state net.poke)
      ::   [~ this]
      =:  nas.state   *^state:naive
          net.state   net.poke
          url.state   url.poke
          own.state   ~
          spo.state   ~
          logs.state  ?:(?=(%default net.poke) snap ~)
        ==
      %-  %-  slog  :_  ~
          leaf+"azimuth: loading snapshot with {<(lent logs.state)>} events"
      =^  snap-cards  state  (run-logs:do logs.state)
      [(weld (jael-update:do (to-udiffs:do snap-cards)) start:do) this]
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
    %-  %-  slog  :_  ~
        leaf+"azimuth: loading snapshot with {<(lent logs.state)>} events"
    =/  res  (%*(run-logs do nas.state *^state:naive) logs.state)
    [(weld (jael-update:do (to-udiffs:do -.res)) start:do) this]
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
    =?  nas.state  ?=(%history -.diff)  *^state:naive
    =^  effects  state  (run-logs:do loglist.diff)
    ::
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
++  data-to-hex
  |=  data=@t
  ?~  data  *@ux
  ?:  =(data '0x')  *@ux
  (hex-to-num:ethereum data)
::
++  run-logs
  |=  [logs=(list event-log:rpc:ethereum)]
  ^-  (quip tagged-diff _state)
  =+  net=(get-network net.state)
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
  =/  [raw-effects=effects:naive new-nas=_nas.state]
    =/  =^input:naive
      :-  block-number.u.mined.i.logs
      ?:  =(azimuth.net address.i.logs)
        =/  data  (data-to-hex data.i.logs)
        =/  =event-log:naive
          [address.i.logs data topics.i.logs]
        [%log event-log]
      ?~  input.u.mined.i.logs
        [%bat *@]
      [%bat u.input.u.mined.i.logs]
    (%*(. naive lac |) verifier chain-id.net nas.state input)
  ::  TODO: move to /lib/dice ?
  ::
  =/  [new-own=_own.state new-spo=_spo.state]
    =<  [own spo]
    ?.  =(azimuth.net address.i.logs)
      %:  apply-effects:dice
        chain-id.net
        raw-effects
        nas.state
        own.state
        spo.state
      ==
    %:  update-indices:dice
      raw-effects
      nas.state
      new-nas
      own.state
      spo.state
    ==
  =:  nas.state  new-nas
      own.state  new-own
      spo.state  new-spo
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
++  get-network
  |=  =net
  ^-  [azimuth=@ux naive=@ux chain-id=@ launch=@]
  =<  [azimuth naive chain-id launch]
  =,  azimuth
  ?-  net
    %mainnet  mainnet-contracts
    %ropsten  ropsten-contracts
    %local    local-contracts
    %default  contracts
  ==
::
++  start
  ^-  (list card)
  =+  net=(get-network net.state)
  =/  args=vase  !>
    :+  %watch  /[dap.bowl]
    ^-  config:eth-watcher
    :*  url.state  =(%czar (clan:title our.bowl))  refresh  ~h30
        (max launch.net ?:(=(net.state %default) last-snap 0))
        ~[azimuth.net]
        ~[naive.net]
        (topics whos.state)
    ==
  [%pass /wa %agent [our.bowl %eth-watcher] %poke %eth-watcher-poke args]~
--
