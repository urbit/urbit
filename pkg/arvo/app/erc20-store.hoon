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
      balances=(map contract-id [=address:ethereum balance=@ud])
      $=  pending  $:
        next=@ud
        txns=(map wire txn)
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
      [address.poke 0]
     =/  from-me-sub=vase
     !>  ^-  poke:erc20
     :+  %watch  path.act
     :*  url=url.config.act
         eager=eager.config.act
         refresh-rate=refresh-rate.config.act
         from=from.config.act
         contracts=contracts.config.act
         topics=[%transfer address ~ ~]
     ==
     =/  from-me-sub=vase
     !>  ^-  poke:erc20
     :+  %watch  path.act
     :*  url=url.config.act
         eager=eager.config.act
         refresh-rate=refresh-rate.config.act
         from=from.config.act
         contracts=contracts.config.act
         topics=[%transfer ~ address ~]
     ==
     =/  to-me-cage=cage
     :-  %ethers-action
     !>
     :_  this
     :~  [%pass /ethers %agent [our.bol %ethers] %poke %ethers-action from-me-sub]
         [%pass /ethers %agent [our.bol %ethers] %poke %ethers-action to-me-sub]
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
    ?.  =(/tx path)  (on-watch:def path)
    [~ this]
::
  ++  on-peek
    |=  =path
    ^-  (unit (unit cage))
    ?+  path  ~
      [%x %address ~]
    ``atom+!>(address)
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
      %history
    !!
      %log
    (parse-event:tc event-log.diff)
      %read-call
    !!
      %read-tx
    !!
    ==

  ++  on-arvo  on-arvo:def
  ++  on-fail  on-fail:def
  --
|_  bol=bowl:gall
++  parse-event
  |=  =event-update:erc20
  ?-  event-update
    [%approval *]
  !!
    [%transfer *]
  !!
::
  ==
++  parse-event-2
  |=  =event-update:erc20
  ?-  event-update
    [%approval @ @ @]
  !!
    [%transfer @ @ @]
  !!
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
