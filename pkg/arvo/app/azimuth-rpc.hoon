::  Azimuth RPC API
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
      [%pass / %agent [our.bowl %dice] %poke u.data]
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
    %get-point             [~ (get-point id +.params scry-point)]
    %transfer-point        (transfer-point id +.params)
    %configure-keys        (configure-keys id +.params)
    %spawn                 (spawn id +.params)
    %escape                (escape id +.params)
    %cancel-escape         (cancel-escape id +.params)
    %adopt                 (adopt id +.params)
    %detach                (detach id +.params)
    %reject                (reject id +.params)
    %set-management-proxy  (management-proxy id +.params)
    %set-spawn-proxy       (spawn-proxy id +.params)
    %set-transfer-proxy    (transfer-proxy id +.params)
    %pending               [~ (all:pending id +.params all:pending-rolls)]
    %pending-by-ship       [~ (ship:pending id +.params ship:pending-rolls)]
    %pending-by-address    [~ (addr:pending id +.params addr:pending-rolls)]
    %history               [~ (history id +.params scry-history)]
  ==
::
++  scry-point
  |=  =ship
  .^  (unit point:naive) 
      %gx 
      (~(scry agentio bowl) %azimuth /nas/[(scot %p ship)]/noun)
  ==
::
++  pending-rolls 
  |%
  ++  all
  .^  (list tx:naive)
      %gx 
      (~(scry agentio bowl) %dice /pending/all/noun)
  ==
  ++  ship
    |=  =^ship
    .^  (list tx:naive)
        %gx 
        (~(scry agentio bowl) %dice /pending/ship/[(scot %p ship)]/noun)
    ==
  ++  addr
    |=  =address:naive
    .^  (list tx:naive)
        %gx 
        %+  ~(scry agentio bowl)  %dice
        /pending/address/[(scot %ux address)]/noun
    ==
  --
::
++  scry-history
  |=  [=ship =proxy:naive]
  ::  FIXME: use proper type from aggregator/index
  ::
  .^  (list [tx:naive ?])
      %gx 
      %+  ~(scry agentio bowl)  %dice
      /history/[(scot %p ship)]/proxy/noun
  ==
--
