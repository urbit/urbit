::  Azimuth JSON-RPC API
::
/-  rpc=json-rpc
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
::  FIXME: import tx-status, pend-tx from aggregator
::
+$  tx-status
  $:  status=?(%unknown %pending %sent %confirmed %failed)
      tx=(unit @ux)
  ==
::
+$  pend-tx  [force=? =raw-tx:naive]
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
    [%pass /bind %arvo %e %connect [~ [%v1 %azimuth ~]] dap.bowl]~
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
      [[%pass /bind %arvo %e %disconnect bind]]~
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
    [~ ~(parse error id)]
  ?+    method  [~ ~(method error id)]
    %get-point             [~ (get-point id +.params point:scry)]
    %transfer-point        (transfer-point id +.params)
    %configure-keys        (configure-keys id +.params)
    %spawn                 (spawn id +.params)
    %escape                (escape id +.params method)
    %cancel-escape         (cancel-escape id +.params method)
    %adopt                 (adopt id +.params method)
    %detach                (detach id +.params method)
    %reject                (reject id +.params method)
    %set-management-proxy  (management-proxy id +.params method)
    %set-spawn-proxy       (spawn-proxy id +.params method)
    %set-transfer-proxy    (transfer-proxy id +.params method)
    %pending               [~ (all:pending id +.params all:pending:scry)]
    %pending-by-ship       [~ (ship:pending id +.params ship:pending:scry)] 
    %pending-by-address    [~ (addr:pending id +.params addr:pending:scry)] 
    %status                [~ (status id +.params tx-status:scry)]  
    :: %history               [~ (history id +.params all:history:scry)] 
  ==
::
++  scry
  |%
  ++  point
    |=  =ship
    .^  (unit point:naive) 
        %gx 
        (~(scry agentio bowl) %azimuth /nas/[(scot %p ship)]/noun)
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
          (~(scry agentio bowl) %aggregator /pending/[(scot %p ship)]/noun)
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
    ++  all
      ::  FIXME: use proper type from aggregator/index
      ::
      .^  (list tx:naive)
          %gx 
          (~(scry agentio bowl) %aggregator /history/noun)
      ==    
    ::
    ++  ship
      |=  =^ship
      ::  FIXME: use proper type from aggregator/index
      ::
      .^  (list tx:naive)
          %gx 
          (~(scry agentio bowl) %aggregator /history/[(scot %p ship)]/noun)
      ==    
    ::
    ++  addr
      |=  =address:naive
      ::  FIXME: use proper type from aggregator/index
      ::
      .^  (list tx:naive)
          %gx 
          (~(scry agentio bowl) %aggregator /history/[(scot %ux address)]/noun)
      ==    
    --
  ::
  ++  tx-status
    |=  keccak=@ux
    .^  ^tx-status
        %gx 
        (~(scry agentio bowl) %aggregator /tx/[(scot %ux keccak)]/status/noun)
    ==
  ::
  ++  nonce
    |=  [=ship =address:naive]
    ::  FIXME: use proper type from aggregator/index
    .^  @
        %gx 
        %+  ~(scry agentio bowl)  
          %aggregator
        /nonce/[(scot %p ship)]/[(scot %ux address)]/atom
    ==
  --
--
