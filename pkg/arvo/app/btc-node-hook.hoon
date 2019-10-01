::  btc-node-hook: send JSON rpc requests to bitcoin full node
::  and poke the responses into the btc-node-store
::
/-  *btc-node-hook
/+  *btc-node-json
|%
+$  move  [bone card]
+$  card
  $%  [%request wire request:http outbound-config:iris]
      [%poke wire dock [%btc-node-store-action btc-node-store-action]]
  ==
::
+$  state
  $%  [%0 state-zero]
  ==
::
+$  state-zero
  $:  full-node-url=@
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
++  poke-noun
  |=  act=btc-node-hook-action
  ^-  (quip move _this)
  ::  TODO: send a JSON rpc request based on the action
  =/  req  [%'GET' full-node-url [['Accept' 'application/json']]~ *(unit octs)]
  =/  out  *outbound-config:iris
  :_  this
  [ost.bol %request /[(scot %da now.bol)] req out]~
::
++  http-response
  |=  [=wire response=client-response:iris]
  ^-  (quip move _this)
  ::  ignore all but %finished
  ?.  ?=(%finished -.response)
    [~ this]
  :: TODO: decode the JSON-RPC response and poke the btc-node-store
  :: with the returned data
  [~ this]
::
++  btc-node-store-poke
  |=  [pax=path act=btc-node-store-action]
  ^-  move
  [ost.bol %poke pax [our.bol %btc-node-store] [%btc-node-store-action act]]
::
--

