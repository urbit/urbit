/-  eth-watcher, *ethers
/+  default-agent, eth-abi
|%
+$  card  card:agent:gall
+$  note
  $%
    [%arvo =note-arvo]
    [%agent [=ship name=term] =task:agent:gall]
  ==
+$  state-zero
  $:  $0
      abis=(map @tas abi-form)
      watchpups=(map path (list address:abi:ethereum))
      contracts=(map address:ethereum @tas)
  ==
+$  abi-form
  $:  events=event-maps
  ==
+$  event-maps  [=event-hmap =event-upd-tmap =event-sub-tmap]
+$  event-hmap  (map @ux @tas)
+$  event-upd-tmap  (map @tas (list etyp:abi:ethereum))
+$  event-sub-tmap  (map @tas (list etyp:abi:ethereum))
--
=/  transer-event=@ux  0xddf2.52ad.1be2.c89b.69c2.b068.fc37.8daa.952b.a7f1.63c4.a116.28f5.5a4d.f523.b3ef
=|  state-zero
=*  state  -
^-  agent:gall
=<
  |_  =bowl:gall
  +*  this  .
      test-eth-core  +>
      tc          ~(. test-eth-core bowl)
      def   ~(. (default-agent this %|) bowl)

  ::
  :: Set local counter to 1 by default
  ++  on-init
    ^-  (quip card _this)
    `this
  :: Expose state for saving
  ++  on-save
    !>(state)
  ::
  ++  on-load
    |=  old=vase
    ^-  (quip card _this)
    =/  loaded=state-zero
      !<(state-zero old)
    `this(state loaded)
    :: ?-  -.loaded
    ::     %0
    ::   `this(local local.loaded)
    ::     %1
    ::   `this(state loaded)
    :: ==
  ::
  ++  on-poke
    |=  [=mark =vase]
    ^-  (quip card _this)
    ?>  ?=(%ethers-action mark)
    =^  cards  state  (poke-eth-test:tc !<(poke vase))
    [cards this]

  ++  on-leave
    |=  =path
    ^-  (quip card _this)
    ~&  "Unsubscribed by: {<src.bowl>} on: {<path>}"
    `this
  ::
  ++  on-watch  on-watch:def
    :: |=  =path
    :: ^-  (quip card _this)
    :: `this
    :: :: :_  this
    :: :: Crash if we see a subscription we don't recognise
    :: :: ?+  path  ~|("unexpected subscription" !!)
    :: :: ==
  ::
  ++  on-peek  on-peek:def
    :: |=  =path
    :: ^-  (unit (unit cage))
    :: [~ ~]
    :: ?+  path  [~ ~]
    :: ==
  ::
  :: Handle sign from agent
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
    ?-  -.diff
      %history
    %.  `this
    %-  slog
    :~  leaf+"got-simple-token-history"
        >loglist.diff<
    ==
  ::
      %log      %-  (slog ~[leaf+"%got-simple-token-event" >event-log.diff<])  `this
      %disavow  %-  (slog ~[leaf+"%got-simple-token-disavow" >id.diff<])  `this
    ==
  ::
  :: Handle arvo signs
  ::
  :: We never give any cards to arvo. Therefore we never need to handle any signs
  :: from arvo. We use the default-agent library to avoid implementing this arm,
  :: as gall apps must have all the arms.
  ::
  ++  on-arvo  on-arvo:def
  ::
  :: Handle error
  ::
  :: Print errors when they happen
  ::
  ++  on-fail
    |=  [=term =tang]
    ^-  (quip card _this)
    %-  (slog leaf+"error in {<dap.bowl>}" >term< tang)
    `this
  --
|_  bol=bowl:gall
::  copied with modification from +setup-eth-watcher in /app/gaze.hoon
++  to-eth-watcher
  |=  [=wire =task:agent:gall]
  ^-  card
  [%pass wire %agent [our.bol %eth-watch] task]

::
:: ++  start-contract-read
::   |=  [=wire req=proto-read-request:rpc:ethereum]
::   ^-  (list card)
::   =/  tid=@ta  :((cury cat 3) dap.bol '--' to.req '--' (scot %uw (mug wire)))
::   =/  args
::     [~ `tid %eth-read-contract !>([node-url.state req])]
::   :~  (watch-spider wire /thread-result/[tid])
::       (poke-spider wire %spider-start !>(args))
::   ==
:: ::
:: ++  read-from-contract
::   |=  $:  =wire
::           =address:eth
::           abi=contract:eth-abi
::           func-name=@t
::           args=(list data:abi:ethereum)
::       ==
::   ^-  (list card)
::   =/  func  (~(got by write-functions.abi) func-name)
::   %:  start-contract-read
::     wire
::     `func-name
::     address
::     ^-  dat=call-data:rpc:ethereum
::     [(get-selector:eth-abi func-name input-sol.func) args]
::   ==
:: ++  setup-eth-watcher
::   |=  =address:abi:ethereum
::   ^-  card
::   %+  to-eth-watcher  /setup-simple-token
::   :+  %poke   %eth-watcher-poke
::   !>  ^-  poke:eth-watcher
::   :+  %watch  /simple-token
::   :*  'http://127.0.0.1:8545/'
::       |
::       ~s10
::       0
::       [0x91b5.17a3.e903.96ea.5ab1.4343.4dda.12b6.a678.f0cc ~]
::       ~
::   ==
::

++  poke-eth-test
  |=  act=poke
  ^-  (quip card state-zero)
  ?-  -.act
      %call
    :: =/  =card
    ::   (setup-eth-watcher address.act)
    :: [[card ~] state]
    `state
  ::
      %send-tx
    `state
  ::   :_  state
  ::   [[%pass /eth-watcher %agent [our.bol %eth-watch] %watch /logs/simple-token] ~]
  :: ::
      %event-subscribe
    ?~  contracts.config.act  `state
    =.  contracts
      %-  ~(gas by contracts)
      ^-  (list (pair @ux @tas))
      %+  turn  contracts.config.act
      |=  =address:ethereum
      [address abi.config.act]
    =/  abi-name=@tas  abi.config.act
    =/  =abi-form
      %-  ~(got by abis)  abi-name
    =/  [hash=@ux name=@tas]
      %+  snag  0
      %+  skim  ~(tap by event-hmap.events.abi-form)
      |=([hash=@ux name=@tas] =(name name.topics.config.act))
    =/  types=(list etyp:abi:ethereum)
      %-  ~(got by event-sub-tmap.events.abi-form)  name
    =/  pretopics=(list ?(@ (list @)))
    [hash args.topics.config.act]
    =/  watcher-action=vase
    !>  ^-  poke:eth-watcher
    :+  %watch  path.act
    :*  url=url.config.act
        eager=eager.config.act
        refresh-rate=refresh-rate.config.act
        from=from.config.act
        contracts=contracts.config.act
        topics=(encode-topics:eth-abi types pretopics)
    ==
    :_  state
    :~  :*  %pass  path.act
            %agent  [our.bol %eth-watch]
            %poke  %eth-watcher-poke
            watcher-action
        ==
        :*  %pass  /eth-watcher
            %agent  [our.bol %eth-watch]
            %watch  [%logs path.act]
        ==
    ==
      %clear
    `state
      %add-abi
    =/  =contract:eth-abi  (parse-contract:eth-abi json.act)
    =/  sur-card=card
        %+  write-file  /sur/eth-contracts/[name.act]/hoon
      [%hoon !>((code-gen-types:eth-abi contract))]
    =/  mark-card=card
        %+  write-file  /mar/[(crip (zing ~["eth-contracts-" (trip name.act) "-update"]))]/hoon
      [%hoon !>((code-gen-mark:eth-abi (zing ~["eth-contracts-" (trip name.act)]) "gift"))]
    =/  events  ~(tap by events.contract)
    =/  =event-maps
      :*  %-  molt
          %+  turn  events
          |=  [hash=@ux =event:eth-abi]
          [hash name.event]
      ::
          %-  molt
          %+  turn  events
          |=  [hash=@ux =event:eth-abi]
          :-  name.event
          %+  turn  inputs.event
          |=  =event-input:eth-abi
          type.event-input
      ::
          %-  molt
          %+  turn  events
          |=  [hash=@ux =event:eth-abi]
          :-  name.event
          %+  turn  (skim inputs.event |=(e=event-input:eth-abi indexed.e))
          |=  =event-input:eth-abi
          type.event-input
      ==
    =.  abis  %+  ~(put by abis)  name.act
    [events=event-maps]
    :_  state
    [sur-card mark-card ~]
  ::
  ==
  :: ++  handle-build
  :: |=  [=path =gift:able:ford]
  :: ^-  (quip card _state)
  :: ?.  ?=([%made *] gift)
  ::   [~ state]
  :: ?.  ?=([%complete *] result.gift)
  ::   [~ state]
  :: =/  uri=@t
  ::   (snag 1 path)
  :: =/  =build-result:ford
  ::   build-result.result.gift
  :: ?+  build-result  [~ state]
  ::       ::
  ::   ::   [%success %plan *]
  ::   :: =.  preludes
  ::   ::   (~(put by preludes) uri -:vase.build-result)
  ::   :: [~ state]
  ::       ::
  ::     [%success %core *]
  ::   =.  abi-builds
  ::     (~(put by abi-builds) uri (_abi-form +:vase.build-result))
  ::   %-  (slog ~[leaf+"build success"])
  ::   `state
  ::       ::
  ::     [%error *]
  ::   %-  (slog message.build-result)
  ::   `state
  :: ==
  ++  our-beak  /(scot %p our.bol)/[q.byk.bol]/(scot %da now.bol)
  ++  write-file
    |=  [pax=path cay=cage]
    ^-  card
    =.  pax  (weld our-beak pax)
    [%pass (weld /write pax) %arvo %c %info (foal:space:userlib pax cay)]
  ::
--
