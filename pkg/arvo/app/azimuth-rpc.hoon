::  Azimuth JSON-RPC API
::
/-  rpc=json-rpc, *aggregator
/+  naive,
    azimuth-rpc,
    json-rpc,
    *server,
    default-agent,
    verb,
    dbug,
    version,
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
    [%pass /bind %arvo %e %connect [~ /v1/azimuth] dap.bowl]~
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
    ?>  (team:title our.bowl src.bowl)
    ?+  mark  (on-poke:def mark vase)
        %handle-http-request
      =+  !<([id=@ta req=inbound-request:eyre] vase)
      :_  this
      (handle-http-request id req)
    ::
        %azimuth-action
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
      ?~  rpc-request=(validate-request:json-rpc body.req parse-method)
        ::  TODO: malformed request
        ::
        (give-simple-payload:app id not-found:gen)
      =/  [data=(unit cage) response=simple-payload:http]
        (process-rpc-request:do u.rpc-request)
      %+  weld
        (give-simple-payload:app id response)
      ?~  data  ~
      :_  ~
      ^-  card
      [%pass / %agent [our.bowl %aggregator] %poke u.data]
      ::  TODO: validate that format is e.g. 'getPoint'
      ::  TODO: maybe use getPoint and translate to %get-point
      ::
      ++  parse-method  |=(t=@t `term`t)
      --
    --
  ::
  ++  on-watch
    |=  =path
    ^-  (quip card _this)
    ?>  (team:title our.bowl src.bowl)
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
  |=  request:rpc
  ^-  [(unit cage) simple-payload:http]
  =;  [data=(unit cage) =response:rpc]
    :-  data
    %-  json-response:gen
    (response-to-json:json-rpc response)
  =,  azimuth-rpc
  ?.  ?=([%map *] params)
    [~ ~(parse error:json-rpc id)]
  =/  method=@tas  (enkebab method)
  ?+  method  [~ ~(method error:json-rpc id)]
    %get-point               `(get-point id +.params point:scry)
    %get-points              `(get-points id +.params points:scry)
    %get-dns                 `(get-dns id +.params dns:scry)
    %transfer-point          (transfer-point id +.params)
    %cancel-transaction      (cancel-tx id +.params)
    %get-spawned             `(get-spawned id +.params spawned:scry)
    %configure-keys          (configure-keys id +.params)
    %spawn                   (spawn id +.params)
    %escape                  (escape id +.params method)
    %cancel-escape           (cancel-escape id +.params method)
    %adopt                   (adopt id +.params method)
    %detach                  (detach id +.params method)
    %reject                  (reject id +.params method)
    %set-management-proxy    (management-proxy id +.params method)
    %set-spawn-proxy         (spawn-proxy id +.params method)
    %set-transfer-proxy      (transfer-proxy id +.params method)
    %get-all-pending         `(all:pending id +.params all:pending:scry)
    %get-pending-by-ship     `(ship:pending id +.params ship:pending:scry)
    %get-pending-by-address  `(addr:pending id +.params addr:pending:scry)
    %get-transaction-status  `(status id +.params tx-status:scry)
    %when-next-batch         `(next-batch id +.params next-batch:scry)
    %get-nonce               `(nonce id +.params nonce:scry)
    %get-history             `(history id +.params addr:history:scry)
    %get-roller-config       `(get-config id +.params config:scry)
  ==
::
++  scry
  |%
  ++  point
    |=  =ship
    .^  (unit point:naive)
        %gx
        (~(scry agentio bowl) %aggregator /point/(scot %p ship)/noun)
    ==
  ::
  ++  points
    |=  =address:naive
    .^  (list [ship point:naive])
        %gx
        (~(scry agentio bowl) %azimuth /points/(scot %ux address)/noun)
    ==
  ::
  ++  spawned
    |=  =ship
    .^  (list [@p @ux])
        %gx
        (~(scry agentio bowl) %aggregator /spawned/(scot %p ship)/noun)
    ==
  ::
  ++  pending
    |%
    ++  all
      .^  (list pend-tx)
          %gx
          (~(scry agentio bowl) %aggregator /pending/noun)
      ==
    ::
    ++  ship
      |=  =^ship
      .^  (list pend-tx)
          %gx
          (~(scry agentio bowl) %aggregator /pending/(scot %p ship)/noun)
      ==
    ::
    ++  addr
      |=  =address:naive
      .^  (list pend-tx)
          %gx
          %+  ~(scry agentio bowl)  %aggregator
          /pending/[(scot %ux address)]/noun
      ==
    --
  ::
  ++  history
    |%
    ++  addr
      |=  =address:naive
      .^  (list roller-tx)
          %gx
          (~(scry agentio bowl) %aggregator /history/(scot %ux address)/noun)
      ==
    --
  ::
  ++  tx-status
    |=  keccak=@ux
    .^  ^tx-status
        %gx
        (~(scry agentio bowl) %aggregator /tx/(scot %ux keccak)/status/noun)
    ==
  ::
  ++  next-batch
    .^  time
        %gx
        (~(scry agentio bowl) %aggregator /next-batch/noun)
    ==
  ::
  ++  nonce
    |=  [=ship =proxy:naive]
    .^  (unit @)
        %gx
        %+  ~(scry agentio bowl)
          %aggregator
        /nonce/(scot %p ship)/[proxy]/noun
    ==
  ::
  ++  dns
    .^  (list @t)
        %gx
        %+  ~(scry agentio bowl)
          %azimuth
        /dns/noun
    ==
  ::
  ++  config
    .^  roller-config
        %gx
        %+  ~(scry agentio bowl)
          %aggregator
        /config/noun
    ==
  --
--
