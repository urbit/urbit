/-  *erc20-store, erc20=eth-contracts-erc20
/+  default-agent
=*  eth-key  key:ethereum
|%
+$  card  card:agent:gall
+$  note
  $%
    [%arvo =note-arvo]
    [%agent [=ship name=term] =task:agent:gall]
  ==
+$  state-0
  $:  %0
      key-path=path
      =address:ethereum
      node-url=_'http://localhost:8545'
      =balances
      $=  pending  $:
        next=@ud
        txns=(map wire pending-txn)
        bals=(map wire [=contract-id =address:ethereum])
      ==
  ==
--
=|  state-0
=*  state  -
^-  agent:gall
=<
  |_  bol=bowl:gall
  +*  this  .
      test-eth-core  +>
      tc          ~(. test-eth-core bol)
      def   ~(. (default-agent this %|) bol)

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
    =/  loaded=state-0
      !<(state-0 old)
    `this(state loaded)
  ::
  ++  on-poke
    |=  [=mark =vase]
    ^-  (quip card _this)
    ?+  mark  (on-poke:def mark vase)
        %erc20-store-poke
      =/  =poke  !<(poke vase)
      ?-  poke
        [%send-erc20 *]
      !!
        [%add-erc20 *]
      ?.  =(address 0x0)  %-  (slog leaf+"expected ethereum key" ~)  `this
      ::?~  address  [~ this]
      =.  balances
      %+  ~(put by balances)
        contract-id.poke
      [address.poke 0 ~]
     =/  from-me-sub=^vase
     !>  ^-  poke:erc20
     :+  %event-subscribe  /[dap.bol]
     :*  url='http://localhost:8545'
         abi=%erc20
         eager=%&
         refresh-rate=~s15
         from=0
         contracts=[address.poke ~]
         topics=[%transfer address ~ ~]
     ==
     =/  to-me-sub=^vase
     !>  ^-  poke:erc20
     :+  %event-subscribe  /[dap.bol]
     :*  url='http://localhost:8545'
         abi=%erc20
         eager=%&
         refresh-rate=~s15
         from=0
         contracts=[address.poke ~]
         topics=[%transfer ~ address ~]
     ==
     :_  this
     ^-  (list card)
     :~  [%pass /eth-watcher %agent [our.bol %ethers] %watch /logs/[dap.bol]]
         [%pass /eth-config %agent [our.bol %ethers] %poke %ethers-action from-me-sub]
         [%pass /eth-config %agent [our.bol %ethers] %poke %ethers-action to-me-sub]
     ==
    ::
        [%set-key *]
      =/  =path  (get-path:tc key-path.poke)
      ?>  ?=(^ =<(fil .^(arch %cy path)))
      =.  key-path  key-path.poke
      =/  new-address=@ux  (address-from-prv:eth-key fetch-key:tc)
      `this(address new-address)
    ::
      ==
 ::
    ==
::

  ++  on-leave
    |=  =path
    ^-  (quip card _this)
    ~&  "Unsubscribed by: {<src.bol>} on: {<path>}"
    `this
  ++  on-watch
    |=  =path
    ^-  (quip card _this)
    ?>  (team:title our.bol src.bol)
    ?.  =(/primary path)  (on-watch:def path)
    :_  this
    [[%give %fact ~[/primary] %erc20-store-gift !>([%initial balances])] ~]
::
  ++  on-peek
    |=  =path
    ^-  (unit (unit cage))
    ?+  path  ~
      [%x %address ~]
    ``atom+!>(address)
      [%x %balance ^]
    ?.  (~(has by balances) i.t.t.path)  ~
    ``noun+!>((~(got by balances) i.t.t.path))
    ==
::
  ++  on-agent
    |=  [=wire =sign:agent:gall]
    ^-  (quip card _this)
    ?.  ?=([%eth-watcher ~] wire)
      (on-agent:def wire sign)
    ?.  ?=(%fact -.sign)
      (on-agent:def wire sign)
    ?.  ?=(%eth-contracts-erc20-update p.cage.sign)
      (on-agent:def wire sign)
    =+  !<(diff=gift:erc20 q.cage.sign)
    ?-  diff
      [%history *]
      =.  state  (apply-events:tc loglist.diff)
    :_  this
    [[%give %fact ~[/primary] %eth-contracts-erc20-update !>(diff)] ~]
    ::=^  cards  state  (apply-events:tc loglist.diff)
    ::[cards this]
      [%log *]
      =.  state  (apply-event:tc event-log.diff)
    :_  this
    [[%give %fact ~[/primary] %eth-contracts-erc20-update !>(diff)] ~]
    ::=^  card  state  (apply-event:tc event-log.diff)
    ::?~  card  `this
    ::[[card ~] this]
      [%read-call *]
    `this
      [%read-tx *]
    `this
    ==
  ++  on-arvo  on-arvo:def
  ++  on-fail  on-fail:def
  --
|_  bol=bowl:gall
++  addr-to-contract
  ^-  (map address:ethereum [=contract-id balance=@ud =txn-log])
  %-  molt
  %+  turn  ~(tap by balances)
  |=  [=contract-id [=address:ethereum balance=@ud =txn-log]]
  [address [contract-id balance txn-log]]
++  apply-events
  |=  =loglist:erc20
  ^-  _state
  |-
  ?~  loglist  state
  $(state (apply-event i.loglist), loglist t.loglist)
++  apply-event
  |=  =event-log:erc20
  ^-  _state
  ?-  event-data.event-log
    [%approval *]
  state
    [%transfer *]
  =/  [=contract-id balance=@ud =txn-log]  (~(got by addr-to-contract) address.event-log)
  =/  new-balance=@ud
  ?.  =(address from.event-data.event-log)
    ?.  =(address to.event-data.event-log)  ~|(["unexpected event" event-log] !!)
    (add balance value.event-data.event-log)
  (sub balance value.event-data.event-log)
  =.  txn-log
    :_  txn-log
    :*  from.event-data.event-log
        to.event-data.event-log
        value.event-data.event-log
    ==
  =.  balances
  %+  ~(put by balances)
    contract-id
  [address.event-log new-balance txn-log]
  state
::
  ==
++  fetch-key
   ^-  @ux
   %+  scan  (trip (of-wain:format .^(wain %cx (get-path key-path))))
   ;~(pfix (jest '0x') hex)
++  get-path
  |=  =path
  ^-  ^path
  ~|  path
  :*
    (scot %p our.bol)
    %home
    (scot %da now.bol)
    path
  ==
--
