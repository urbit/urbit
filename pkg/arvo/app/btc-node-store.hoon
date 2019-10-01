::  btc-node-store: data store for state received from a bitcoin full node
::
/+  *btc-node-json
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
  $:  example=@t
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
  ?~  old
    [~ this]
  [~ this(+<+ u.old)]
::
++  poke-btc-node-store-action
  |=  action=btc-node-store-action
  ^-  (quip move _this)
  ?.  =(src.bol our.bol)
    [~ this]
  ?-  -.action
      %add       (handle-add action)
  ==
::
++  handle-add
  |=  action=btc-node-store-action
  ^-  (quip move _this)
  [~ this]
::
--

