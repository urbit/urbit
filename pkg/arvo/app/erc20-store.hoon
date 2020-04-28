/-  *erc20-store
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
=/  transer-event=@ux  0xddf2.52ad.1be2.c89b.69c2.b068.fc37.8daa.952b.a7f1.63c4.a116.28f5.5a4d.f523.b3ef
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
     `this
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
  ++  on-agent  on-agent:def
  ++  on-arvo  on-arvo:def
  ++  on-fail  on-fail:def
  --
|_  bol=bowl:gall
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
