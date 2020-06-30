::  bitcoin: A Store for Bitcoin using Bcoin as an SPV node
::
::    data:            scry command:
::    ________         _________________________________________
::    xpub             .^(tape %gx /=bitcoin=/xpub/noun)
::    depth            .^(@ud %gx /=bitcoin=/depth/noun)
::    all payers       .^((list (set @p)) %gx /=bitcoin=/payers/noun)
::    payers/address   .^((unit (set @p)) %gx /=bitcoin=/payers/<btc-@uc>/noun)
::    addresses        .^((set @uc) %gx /=bitcoin=/addresses/noun)
::
/-  *bitcoin
/+  *server, default-agent, verb, bip32, *bitcoin
::
=>  |%
    +$  card  card:agent:gall
    ::
    +$  state
      $%  [%0 state-zero]
      ==
    ::
    +$  state-zero
      $:  =xpub
          depth=@ud
          max-look-ahead=@ud
          payers=(map @uc (set @p))
      ==
    --
::
=|  state-zero
=*  state  -
::  Main
::
%+  verb  |
^-  agent:gall
=<  |_  =bowl:gall
    +*  this          .
        bitcoin-core  +>
        bc            ~(. bitcoin-core bowl)
        def           ~(. (default-agent this %|) bowl)
    ::
    ++  on-init
      ^-  (quip card _this)
      ::  From bcoin: Account.MAX_LOOKAHEAD = 40;
      ::  source:
      ::  github.com/bcoin-org/bcoin/blob/master/lib/wallet/account.js#L966
      ::
      :_  this(xpub ~, max-look-ahead 40)
      :~  :*  %pass
              /srv
              %agent
              [our.bowl %file-server]
              %poke
            ::
              :-  %file-server-action
              !>([%serve-dir /'~wallet' /app/wallet %.n])
      ==  ==
    ::
    ++  on-save
      !>(state)
    ::
    ++  on-load
      |=  old=vase
      `this(state !<(state-zero old))
    ::
    ++  on-poke
      |=  [=mark =vase]
      ^-  (quip card _this)
      ?+    mark  (on-poke:def mark vase)
          %json
        =^  cards  state
          (handle-json:bc !<(json vase))
        [cards this]
      ::
        ::   %handle-http-request
        :: =+  !<([eyre-id=@ta =inbound-request:eyre] vase)
        :: :_  this
        :: %+  give-simple-payload:app  eyre-id
        :: %+  require-authorization:app  inbound-request
        :: poke-handle-http-request:bc
      ::
          %bitcoin-action
        ~&  !<(bitcoin-action vase)
        =^  cards  state
          (handle-bitcoin-action:bc !<(bitcoin-action vase))
        [cards this]
      ==
    ::
    ++  on-watch
      |=  =path
      ^-  (quip card _this)
      :_  this
      ?+    path  ~|([%peer-bitcoin-strange path] !!)
          [%http-response *]  ~
          [%primary *]        [send-xpubkey]~
          [%bitcointile ~]    [%give %fact ~ %json !>(*json)]~
      ==
    ::
    ++  on-agent  on-agent:def
    ::
    ++  on-arvo
      |=  [=wire =sign-arvo]
      ^-  (quip card _this)
      ?:  ?=(%bound +<.sign-arvo)
        [~ this]
      (on-arvo:def wire sign-arvo)
    ::
    ++  on-leave  on-leave:def
    ::  +on-peek: read from app state
    ::
    ++  on-peek
      |=  =path
      ^-  (unit (unit cage))
      ?+  path  (on-peek:def path)
          [%x %xpub ~]       ``noun+!>(xpub)
          [%x %depth ~]      ``noun+!>(depth)
        ::
          [%x %payers @t ~]
          ~&  i.t.t.path
        ``noun+!>((~(get by payers) (parse-btc i.t.t.path)))
        ::
          [%x %payers ~]     ``noun+!>(~(val by payers))
          [%x %addresses ~]  ``noun+!>(~(key by payers))
      ==
    ++  on-fail   on-fail:def
    --
::
=,  bip32
|_  =bowl:gall
++  derive-poke
  |=  act=bitcoin-action
  ?>  ?=(%derive -.act)
  ^-  card
  [%pass /derive-poke %agent [ship.act %bitcoin] %poke %bitcoin-action !>(act)]
::
++  receive-poke
  |=  [=ship act=bitcoin-action]
  ^-  card
  [%pass /receive-poke %agent [ship %bitcoin] %poke %bitcoin-action !>(act)]
::
++  send-xpubkey
  ^-  card
  :*  %give
      %fact
      ~
      %json
      !>((frond:enjs:format [%initial s+(crip xpub)]))
  ==
::
++  handle-json
  |=  jon=json
  ^-  (quip card _state)
  ?>  (team:title our.bowl src.bowl)
  (handle-bitcoin-action (json-to-bitcoin-action jon))
::
++  handle-bitcoin-action
  |=  act=bitcoin-action
  ^-  (quip card _state)
  |^
  ?-  -.act
      %add      (handle-add xpub.act)
      %remove   handle-remove
      %request  (handle-request [ship.act net.act])
      %derive   (handle-derive [ship.act net.act])
      %receive  (handle-receive +.act)
  ==
  ::
  ++  handle-add
    |=  xpub=tape
    ^-  (quip card _state)
    ?>  (team:title our.bowl src.bowl)
    ~&  "New xpubkey: {xpub}"
    [~ state(xpub xpub)]
  ::
  ++  handle-remove
    ^-  (quip card _state)
    ?>  (team:title our.bowl src.bowl)
    [~ state(xpub ~)]
  ::
  ++  handle-request
    |=  [=ship =network]
    ^-  (quip card _state)
    [[(derive-poke [%derive [ship network]])]~ state]
  ::
  ++  handle-derive
    |=  [payer=@p net=network]
    ^-  (quip card _state)
    |^
    =/  addr=@uc  (derive-address (mod depth max-look-ahead) net)
    :_  %_    state
            depth
          +(depth)
        ::
            payers
          ?.  (~(has by payers) addr)
            (~(put by payers) [addr (~(put in *(set @p)) payer)])
          (~(jab by payers) [addr |=(p=(set @p) (~(put in p) payer))])
        ==
    ?:  (team:title our.bowl src.bowl)
      ::  Local derive
      ::
      ~&(addr ~)
    ::  Foreign derive
    ::
    ~&  "{<src.bowl>} requests a new address..."
    [(receive-poke src.bowl [%receive addr])]~
    ::
    ++  derive-address
      |=  [index=@ =network]
      ^-  @uc
      ::  BIP 44: m / purpose' / coin_type' / account' / change / index
      ::  xpub generated with:  m / 44' / network / 0 <- default-account
      ::
      =;  hd-path
        (~(address hd-path +<.hd-path) network)
      =>  [(from-extended xpub) .]
      (derive-path "m/0/{((d-co:co 1) index)}")
    ::
    ++  type-from-network
      |=  =network
      ^-  tape
      ?-  network
        %main     "0"
        %regtest  "1"
        %testnet  "1"
      ==
    ::
    ++  full-derivation-path
      |=  [account=@ index=@ =network]
      ^-  tape
      ::  With an extended public key, we can only derive *non-hardened* keys
      ::
      ::  Warning: (https://bitcoin.stackexchange.com/a/37489) [1]
      ::
      ::  Non-hardened public keys are weaker because if xpubkey is leaked
      ::  together with one of the non-hardened private keys, it would allow
      ::  an attacker to know the private key of the extended public key,
      ::  and all the address derived from it.
      ::
      ::  But, "even if an attacker gets ahold of one of the private keys,
      ::  in situations where the attacker doesn't have access to the extended
      ::  public key, non-hardened is equivalent to hardened security." [1]
      ::
      ::  BIP 44: m / purpose / coin_type / account / change / address_index
      ::
      =/  coin-type=tape  (type-from-network network)
      ~&  coin-type+coin-type
      "m/44/{coin-type}/{((d-co:co 1) account)}/0/{((d-co:co 1) index)}"
    --
  ::
  ++  handle-receive
    |=  address=@uc
    ^-  (quip card _state)
    =/  message=json
      (frond:enjs:format [%address s+(base58-to-cord address)])
    :_  state
    [%give %fact ~[/primary] %json !>(message)]~
  --
::
++  base58-to-cord
  |=  b=@uc
  ^-  @t
  ::  Removes leading 0c
  ::
  (rsh 3 2 (scot %uc b))
::
++  random-index
  ^-  @ud
  ::  This is problematic.
  ::  See: https://github.com/bcoin-org/bcoin/issues/671#issuecomment-455669516
  ::  tl;dr: a wallet won't pick up txs above the max lookahead unless
  ::  a rescan is triggered, which is very ineficient, specially if the
  ::  wallet is on a browser and uses a javascript-only implementation.
  ::
  (~(rad og eny.bowl) (pow 2 31))
::
++  parse-btc
  |=  b=@t
  `@uc`(rash b fim:ag)
--
