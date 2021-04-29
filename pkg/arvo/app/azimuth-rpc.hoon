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
    version
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
      %+  give-simple-payload:app  id
      (handle-http-request req)
    ::
        %azimuth-action
      =+  !<([%disconnect bind=binding:eyre] vase)
      ~&  >>>  "disconnecting at {<bind>}"
      :_  this
      [[%pass /bind %arvo %e %disconnect bind]]~
    ==
    ::
    ++  handle-http-request
      |=  =inbound-request:eyre
      ^-  simple-payload:http
      |^
      =*  req       request.inbound-request
      =*  headers   header-list.req
      =/  req-line  (parse-request-line url.req)
      ?.  =(method.req %'POST')
        ::  TODO: method not supported
        ::
        not-found:gen
      ?~  rpc-request=(validate-request:json-rpc body.req parse-method)
        ::  TODO: malformed request
        ::
        not-found:gen
      (process-rpc-request:do u.rpc-request)
      ::  TODO: validate that format is e.g. 'getPoint'
      ::  TODO: maybe use getPoint and translate to %get-point
      ::
      ++  parse-method  |=(t=@t t)
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
  |=  req=request:rpc
  ^-  simple-payload:http
  |^
  ?+    method.req  ~|([%unsupported-azimuth-request method.req] !!)
    %get-point   (get-point id.req params.req)
  ==
  ::  TODO: handle rpc error responses properly
  ::
  ++  get-point
    |=  [id=@t params=request-params:rpc]
    %-  json-response:gen
    %-  response-to-json:json-rpc
    ?.  ?=([%object *] params)  
      [%error id 'X' 'RPC params must be an object']
    ?>  ?=(^ +.params)
    ?.  =('ship' p.i.+.params)  
      [%error id 'X' 'A "ship" key must exist']
    =/  ship=(unit ship)
      (rush (so:dejs:format q.i.+.params) ;~(pfix sig fed:ag))
    ?~  ship  
      [%error id 'X' 'Ship @p invalid']
    ?~  point=(scry-point u.ship)
      [%error id 'X' 'Ship @p not found']
    [%result id (point-to-json:azimuth-rpc u.point)]
  --
::
++  scry-point
  |=  =ship
  .^  (unit point:naive)
      %gx
      (scot %p our.bowl)
      %naive
      (scot %da now.bowl)
      %nas
      (scot %p ship)
      /noun
  ==
--
