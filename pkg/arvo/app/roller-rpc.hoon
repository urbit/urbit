::  Roller JSON-RPC API
::
/-  rpc=json-rpc, *dice
/+  naive,
    azimuth-roll-rpc,
    json-rpc,
    *server,
    default-agent,
    verb,
    dbug,
    agentio
|%
::
+$  card  card:agent:gall
::
+$  state-0  [%0 ~]
--
::
%+  verb  |
%-  agent:dbug
::
=|  state-0
=*  state  -
::
^-  agent:gall
=<
  |_  =bowl:gall
  +*  this  .
      do    ~(. +> bowl)
      def   ~(. (default-agent this %|) bowl)
  ::
  ++  on-init
    ^-  (quip card _this)
    ~&  >  'init'
    :_  this
    [%pass /bind %arvo %e %connect [~ /v1/roller] dap.bowl]~
  ::
  ++  on-save  !>(state)
  ++  on-load
    |=  old=vase
    ^-  (quip card _this)
    [~ this(state !<(state-0 old))]
  ::
  ++  on-poke
    |=  [=mark =vase]
    ^-  (quip card _this)
    |^
    ?+  mark  (on-poke:def mark vase)
        %handle-http-request
      =+  !<([id=@ta req=inbound-request:eyre] vase)
      :_  this
      (handle-http-request id req)
    ::
        %azimuth-action
      ?>  (team:title our.bowl src.bowl)
      =+  !<([%disconnect bind=binding:eyre] vase)
      ~&  >>>  "disconnecting at {<bind>}"
      :_  this
      [%pass /bind %arvo %e %disconnect bind]~
    ==
    ::
    ++  handle-http-request
      |=  [id=@ta =inbound-request:eyre]
      ^-  (list card)
      |^
      =*  req       request.inbound-request
      =*  headers   header-list.req
      =/  req-line  (parse-request-line url.req)
      ?.  =(method.req %'POST')
        ::  TODO: method not supported
        ::
        (give-simple-payload:app id not-found:gen)
      ?~  rpc-request=(validate-request:json-rpc body.req)
        ::  TODO: malformed request
        ::
        (give-simple-payload:app id not-found:gen)
      =/  [data=(list cage) response=simple-payload:http]
        (process-rpc-request:do u.rpc-request)
      %+  weld
        (give-simple-payload:app id response)
      |-
      ?~  data  ~
      :_  $(data t.data)
      ^-  card
      [%pass / %agent [our.bowl %roller] %poke i.data]
      --
    --
  ::
  ++  on-watch
    |=  =path
    ^-  (quip card _this)
    ?+  path  (on-watch:def path)
        [%http-response *]  [~ this]
    ==
  ::
  ++  on-arvo
    |=  [=wire =sign-arvo]
    ^-  (quip card _this)
    ?+  sign-arvo  (on-arvo:def wire sign-arvo)
        [%eyre %bound *]
      ~?  !accepted.sign-arvo
        [dap.bowl 'bind rejected!' binding.sign-arvo]
      [~ this]
    ==
  ::
  ++  on-leave  on-leave:def
  ++  on-peek   on-peek:def
  ++  on-agent  on-agent:def
  ++  on-fail   on-fail:def
  --
::
|_  =bowl:gall
++  process-rpc-request
  |=  req=batch-request:rpc
  ^-  [(list cage) simple-payload:http]
  |^
    ?-  -.req
        %o
      =/  [data=(unit cage) =response:rpc]
        (process p.req)
      [(drop data) (render response)]
    ::
        %a
      =|  data=(list cage)
      =|  resp=(list response:rpc)
      |-
      ?~  p.req
        [(flop data) (render %batch (flop resp))]
      =/  [dat=(unit cage) res=response:rpc]
        (process i.p.req)
      =?  data  ?=(^ dat)  [u.dat data]
      $(p.req t.p.req, resp [res resp])
    ==
  ::
  ++  render
    |=  res=response:rpc
    %-  json-response:gen
    (response-to-json:json-rpc res)
  ::
  ++  process
    |=  request:rpc
    ?.  ready:scry
      ::  TODO: move to lib
      ::
      `[%error id '-32003' 'Roller is not ready']
    =,  azimuth-roll-rpc
    ?.  ?=([%map *] params)
      [~ ~(parse error:json-rpc id)]
    =/  method=@tas  (enkebab method)
    ?:  ?=(l2-tx method)
      (process-rpc id +.params method over-quota:scry)
    ?+  method  [~ ~(method error:json-rpc id)]
      %cancel-transaction      (cancel-tx id +.params)
      %when-next-batch         `(next-timer id +.params next-batch:scry)
      %when-next-slice         `(next-timer id +.params next-slice:scry)
      %spawns-remaining        `(spawns-remaining id +.params unspawned:scry)
      %get-remaining-quota     `(quota-remaining id +.params ship-quota:scry)
      %get-allowance           `(ship-allowance id +.params allowance:scry)
      %get-point               `(get-point id +.params point:scry)
      %get-ships               `(get-ships id +.params ships:scry)
      %get-spawned             `(get-spawned id +.params spawned:scry)
      %get-unspawned           `(get-spawned id +.params unspawned:scry)
      %get-sponsored-points    `(sponsored-points id +.params sponsored:scry)
      %get-owned-points        `(get-ships id +.params owned:scry)
      %get-transferring-for    `(get-ships id +.params transfers:scry)
      %get-manager-for         `(get-ships id +.params manager:scry)
      %get-voting-for          `(get-ships id +.params voting:scry)
      %get-spawning-for        `(get-ships id +.params spawning:scry)
      %get-all-pending         `(all:pending id +.params all:pending:scry)
      %get-pending-by-ship     `(ship:pending id +.params ship:pending:scry)
      %get-pending-by-address  `(addr:pending id +.params addr:pending:scry)
      %get-pending-tx          `(hash:pending id +.params hash:pending:scry)
      %get-transaction-status  `(status id +.params tx-status:scry)
      %get-predicted-state     `(get-naive id +.params predicted:scry)
      %get-nonce               `(nonce id +.params nonce:scry)
      %get-history             `(history id +.params addr:history:scry)
      %get-roller-config       `(get-config id +.params config:scry)
      %get-unsigned-tx         `(hash-transaction id +.params chain:scry & |)
      %prepare-for-signing     `(hash-transaction id +.params chain:scry | &)
      %hash-raw-transaction    `(hash-raw-transaction id +.params)
    ==
  --
::
++  scry
  |%
  ++  point
    |=  =ship
    .^  (unit point:naive)
        %gx
        (~(scry agentio bowl) %roller /point/(scot %p ship)/noun)
    ==
  ::
  ++  ships
    |=  =address:naive
    .^  (list ship)
        %gx
        (~(scry agentio bowl) %roller /ships/(scot %ux address)/noun)
    ==
  ::
  ++  spawned
    |=  =ship
    .^  (list @p)
        %gx
        (~(scry agentio bowl) %roller /spawned/(scot %p ship)/noun)
    ==
  ::
  ++  unspawned
    |=  =ship
    .^  (list @p)
        %gx
        (~(scry agentio bowl) %roller /unspawned/(scot %p ship)/noun)
    ==
  ::
  ++  owned
    |=  =address:naive
    .^  (list ship)
        %gx
        (~(scry agentio bowl) %roller /owned/(scot %ux address)/noun)
    ==
  ::
  ++  sponsored
    |=  parent=ship
    .^  [residents=(list ship) requests=(list ship)]
        %gx
        (~(scry agentio bowl) %roller /sponsored/(scot %p parent)/noun)
    ==
  ::
  ++  transfers
    |=  =address:naive
    .^  (list ship)
        %gx
        (~(scry agentio bowl) %roller /transfers/(scot %ux address)/noun)
    ==
  ::
  ++  manager
    |=  =address:naive
    .^  (list ship)
        %gx
        (~(scry agentio bowl) %roller /manager/(scot %ux address)/noun)
    ==
  ::
  ++  voting
    |=  =address:naive
    .^  (list ship)
        %gx
        (~(scry agentio bowl) %roller /voting/(scot %ux address)/noun)
    ==
  ::
  ++  spawning
    |=  =address:naive
    .^  (list ship)
        %gx
        (~(scry agentio bowl) %roller /spawning/(scot %ux address)/noun)
    ==
  ::
  ++  pending
    |%
    ++  all
      .^  (list pend-tx)
          %gx
          (~(scry agentio bowl) %roller /pending/noun)
      ==
    ::
    ++  ship
      |=  =^ship
      .^  (list pend-tx)
          %gx
          (~(scry agentio bowl) %roller /pending/(scot %p ship)/noun)
      ==
    ::
    ++  addr
      |=  =address:naive
      .^  (list pend-tx)
          %gx
          %+  ~(scry agentio bowl)  %roller
          /pending/[(scot %ux address)]/noun
      ==
    ::
    ++  hash
      |=  keccak=@ux
      .^  (unit pend-tx)
          %gx
          %+  ~(scry agentio bowl)  %roller
          /pending-tx/[(scot %ux keccak)]/noun
      ==
    --
  ::
  ++  history
    |%
    ++  addr
      |=  =address:naive
      .^  (list hist-tx)
          %gx
          (~(scry agentio bowl) %roller /history/(scot %ux address)/noun)
      ==
    --
  ::
  ++  tx-status
    |=  keccak=@ux
    .^  ^tx-status
        %gx
        (~(scry agentio bowl) %roller /tx/(scot %ux keccak)/status/noun)
    ==
  ::
  ++  next-batch
    .^  time
        %gx
        (~(scry agentio bowl) %roller /next-batch/atom)
    ==
  ::
  ++  next-slice
    .^  time
        %gx
        (~(scry agentio bowl) %roller /next-slice/atom)
    ==
  ::
  ++  nonce
    |=  [=ship =proxy:naive]
    .^  (unit @)
        %gx
        %+  ~(scry agentio bowl)
          %roller
        /nonce/(scot %p ship)/[proxy]/noun
    ==
  ::
  ++  config
    ^-  [azimuth-config roller-config]
    :-  refresh
    .^  roller-config
        %gx
        %+  ~(scry agentio bowl)
          %roller
        /config/noun
    ==
  ::
  ++  chain
    .^  @
        %gx
        %+  ~(scry agentio bowl)
          %roller
        /chain-id/noun
    ==
  ::
  ++  predicted
    .^  ^state:naive
        %gx
        (~(scry agentio bowl) %roller /predicted/noun)
    ==
  ::
  ++  refresh
    .^  @dr
        %gx
        (~(scry agentio bowl) %azimuth /refresh/noun)
    ==
  ::
  ++  over-quota
    |=  =ship
    .^  ?
        %gx
        (~(scry agentio bowl) %roller /over-quota/(scot %p ship)/atom)
    ==
  ::
  ++  ship-quota
    |=  =ship
    .^  @ud
        %gx
        (~(scry agentio bowl) %roller /ship-quota/(scot %p ship)/atom)
    ==
  ::
  ++  allowance
    |=  =ship
    .^  (unit @ud)
        %gx
        (~(scry agentio bowl) %roller /allowance/(scot %p ship)/noun)
    ==
  ::
  ++  ready
    .^  ?
        %gx
        (~(scry agentio bowl) %roller /ready/atom)
    ==
  --
--
