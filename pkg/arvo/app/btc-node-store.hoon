::  btc-node-store: data store for state received from a bitcoin full node
::
/-  *btc-node-store
/+  *btc-node-json
::
|%
+$  move  [bone card]
::
+$  card
  $%  [%quit ~]
  ==
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
|_  [bol=bowl:gall state]
::
++  this  .
::
++  prep
  |=  old=(unit state)
  ^-  (quip move _this)
  :-  ~
  ?^  old
    this(+<+ u.old)
  %=    this
    wallets  (~(put by wallets) [*@t *wallet])
  ==
::
::  +peek: read from app state
::
::    .^(@t %gx /=btc-node-store=/default-wallet/noun)
::
++  peek-x-default-wallet
  |=  =path
  ^-  (unit (unit [%noun @t]))
  [~ ~ %noun default-wallet]
::
::  +peek: read from app state
::
::    .^(@ud %gx /=btc-node-store=/n-wallets/noun)
::
++  peek-x-n-wallets
  |=  =path
  ^-  (unit (unit [%noun @ud]))
  [~ ~ %noun ~(wyt by wallets)]
::
++  poke-btc-node-store-action
  |=  action=btc-node-store-action
  ^-  (quip move _this)
  ?.  =(src.bol our.bol)
    [~ this]
  ?+  -.action  ~|  [%unsupported-action -.action]  !!
    %add-wallet     (handle-add +.action)
    %list-wallets   handle-list-wallet
    %update-wallet  (handle-update-wallet +.action)
  ==
::
++  poke-btc-node-store-command
  |=  command=btc-node-store-command
  ^-  (quip move _this)
  ?>  (team:title our.bol src.bol)
  ?+  -.command  ~|  [%unsupported-command -.command]  !!
      %switch-wallet  (handle-switch +.command)
  ==
::
++  handle-add
  |=  [name=@t warning=(unit @t)]
  ^-  (quip move _this)
  ?:  (~(has by wallets) name)
    ~&  "This wallet already exists..."
    [~ this]
  :-  ~
  ~&  "Wallet {<name>} added succesfully..."
  %=    this
    wallets  (~(put by wallets) [name name ~])
  ==
::
++  handle-list-wallet
  ^-  (quip move _this)
  ~&  [%local-wallets ~(tap by wallets)]
  ::  TODO: connect to %sole to print to console
  [~ this]
::
++  handle-update-wallet
  |=  [name=@t attrs=wallet-attr]
  ^-  (quip move _this)
  =/  w=wallet  [name (some attrs)]
  :-  ~
  ?:  (~(has by wallets) name.w)
    ~&  "The wallet exists. Updating..."
    this(wallets (~(jab by wallets) name.w |=(* w)))
  ~&  "The wallet doesn't exist. Creating..."
  this(wallets (~(put by wallets) [name.w w]))
::
++  handle-switch
  |=  name=@t
  ^-  (quip move _this)
  ?.  (~(has by wallets) name)
    ~&  "The wallet doesn't exist..."
    [~ this]
  ~&  "New default-wallet: {<name>}"
  [~ this(default-wallet name)]
::
--
