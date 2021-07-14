::  btc-node-store: data store for state received from a bitcoin full node
::
::    data:            scry command:
::
::  default-wallet    .^(@t %gx /=btc-node-store=/default-wallet/noun)
::  n-wallets         .^(@ud %gx /=btc-node-store=/n-wallets/noun)
::  [def-wallet attr] .^((unit wallet) %gx /=btc-node-store=/wallet/noun)
::  [name attr]       .^((unit wallet) %gx /=btc-node-store=/wallet/<name>/noun)
::
::
/-  *btc-node-store
/+  *btc-node-json, default-agent, verb
::
=>  |%
    ::
    +$  card  card:agent:gall
    ::
    +$  state
      $%  [%0 state-zero]
      ==
    ::
    +$  state-zero
      $:  =wallets
          default-wallet=@t
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
    +*  this      .
        btc-core  +>
        bc        ~(. btc-core bowl)
        def       ~(. (default-agent this %|) bowl)
    ::
    ++  on-init
      ^-  (quip card _this)
      :-  ~
      %_    this
        wallets  (~(put by wallets) [*@t *wallet])
      ==
    ::
    ++  on-save   !>(state)
    ++  on-load
      |=  old=vase
      `this(state !<(state-zero old))
    ::
    ++  on-poke
      |=  [=mark =vase]
      ^-  (quip card _this)
      |^
      ?+    mark    (on-poke:def mark vase)
          %btc-node-store-action
        (store-action !<(btc-node-store-action vase))
      ::
          %btc-node-store-command
        (store-command !<(btc-node-store-command vase))
      ==
      ::
      ++  store-action
        |=  action=btc-node-store-action
        ^-  (quip card _this)
        =^  cards  state
          ?+  -.action  ~|([%unsupported-action -.action] !!)
            %add-wallet     (handle-add:bc +.action)
            %load-wallet    (handle-switch:bc +.action)
            %list-wallets   handle-list-wallet:bc
            %update-wallet  (handle-update-wallet:bc +.action)
          ==
        [cards this]
      ::
      ++  store-command
        |=  command=btc-node-store-command
        ^-  (quip card _this)
        =^  cards  state
          ?+  -.command  ~|([%unsupported-command -.command] !!)
              %switch-wallet  (handle-switch:bc +.command)
          ==
        [cards this]
      --
    ++  on-watch  on-watch:def
    ++  on-leave  on-leave:def
    ::  +on-peek: read from app state
    ::
    ++  on-peek
      |=  =path
      ^-  (unit (unit cage))
      ?+  path  (on-peek:def path)
          [%x %default-wallet ~]  ``noun+!>(default-wallet)
          [%x %n-wallets ~]       ``noun+!>(~(wyt by wallets))
          [%x %wallet @t ~]       ``noun+!>((~(get by wallets) i.t.t.path))
          [%x %wallet ~]          ``noun+!>((~(get by wallets) default-wallet))
      ==
    ::
    ++  on-agent  on-agent:def
    ++  on-arvo   on-arvo:def
    ++  on-fail   on-fail:def
    --
::
|_  =bowl:gall
::
++  handle-add
  |=  [name=@t warning=(unit @t)]
  ^-  (quip card _state)
  ?:  (~(has by wallets) name)
    ~&  "This wallet already exists..."
    [~ state]
  :-  ~
  ~&  "Wallet {<name>} added succesfully..."
  %_    state
    wallets  (~(put by wallets) [name name ~])
  ==
::
++  handle-list-wallet
  ^-  (quip card _state)
  =/  wallet-names=(list @t)
    (turn ~(tap by wallets) |=([n=@t *] n))
  :_  state
  :_  ~
  :*  %pass  /  %arvo  %d  %flog
      %text  "local-wallets={<`wain`wallet-names>}"
  ==
::
++  handle-update-wallet
  |=  [name=@t attrs=wallet-attr]
  ^-  (quip card _state)
  =/  w=wallet  [name (some attrs)]
  :-  ~
  %_    state
      wallets
    ?:  (~(has by wallets) name.w)
      ~&  "The wallet exists. Updating..."
      (~(put by wallets) name.w w)
    ::
    ~&  "The wallet doesn't exist. Creating..."
    (~(put by wallets) [name.w w])
  ==
::
++  handle-switch
  |=  name=@t
  ^-  (quip card _state)
  :_  state(default-wallet name)
  :_  ~
  :*  %pass  /  %arvo  %d  %flog
      %text
      %+  weld
        "New default-wallet: {<name>}"
      ?:  (~(has by wallets) name)
        ""
      " (wallet is not local)"
  ==
--
