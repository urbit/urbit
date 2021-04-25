::  Azimuth RPC API
::
/-  rpc=json-rpc
/+  naive, *server, default-agent, verb, dbug, version
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
      ?~  json-rpc=(validate-json-rpc body.req)
        ::  TODO: malformed request
        ::
        not-found:gen
      (process-rpc-request:do u.json-rpc)
      ::  TODO: validate that format is e.g. 'get-point'
      ::  TODO: maybe replace with getPoint and translate to %term
      ::
      ++  parse-method  |=(t=@t t)
      ::  TODO: move to library
      ::
      ++  validate-json-rpc
        |=  body=(unit octs)
        ^-  (unit request:rpc)
        ?~  body  ~
        ?~  jon=(de-json:html q.u.body)  ~
        ::  ignores non-object responses
        ::
        :: ?.  ?=([%o *] json)  ~|([%format-not-valid json] !!)
        ?.  ?=([%o *] u.jon)  ~
        %-  some
        %.  u.jon
        =,  dejs:format
        %-  ot
        :~  ['id' no]
            ['jsonrpc' so]
            ['method' (cu parse-method so)]
          ::
            :-  'params'
            |=  =json
            ^-  request-params:rpc
            ?:  =(%a -.json)
              [%list ((ar same) json)]
            ?.  =(%o -.json)
              !!
            [%object ~(tap by ((om same) json))]
        ==
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
  ::  TODO: move this to a library
  ::  TODO: handle rpc error responses properly
  ::
  ++  get-point
    |=  [id=@t params=request-params:rpc]
    %-  json-response:gen
    %-  rpc-response-to-json
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
    [%result id (point-to-json u.point)]
  ::
  ++  point-to-json
    |=  =point:naive
    ^-  json
    |^
    :: :+  %result  id
    :-  %o
    %-  molt
    ^-  (list [@t json])
    :~  ['dominion' s+dominion.point]
      ::
        :-  'ownership'
        :-  %o
        %-  molt
        =*  own  own.point
        ^-  (list [@t json])
        :~  ['owner' (own-to-json owner.own)]
            ['spawnProxy' (own-to-json spawn-proxy.own)]
            ['managementProxy' (own-to-json spawn-proxy.own)]
            ['votingProxy' (own-to-json management-proxy.own)]
            ['transferProxy' (own-to-json transfer-proxy.own)]
        ==
      ::
        :-  'network'
        :-  %o
        =,  enjs:format
        %-  molt
        =*  net  net.point
        ^-  (list [@t json])
        :*  ['life' (numb life.net)]
            ['pass' s+(crip ((x-co:co 20) pass.net))]
            ['rift' (numb rift.net)]
            :-  'sponsor'
            :-  %o
            %-  molt  ^-  (list [@t json])
            ~[['has' b+has.sponsor.net] ['who' (ship who.sponsor.net)]]
          ::
            ?~  escape.net  ~
            ['escape' (ship u.escape.net)]~
    ==  ==
    ::
    ++  own-to-json
      |=  [=address:naive =nonce:naive]
      ^-  json
      :-  %o
      %-  molt  ^-  (list [@t json])
      :~  ['address' s+(crip "0x{((x-co:co 20) address)}")]
          ['nonce' (numb:enjs:format nonce)]
      ==
    --
  :: TODO: move to rpc library
  ::
  ++  rpc-response-to-json
    |=  =response:rpc
    ^-  json
    ::  TODO: consider all cases
    ::
    ?+  -.response  ~|([%unsupported-rpc-response response] !!)
        %result
      :-  %o
      %-  molt
      ^-  (list [@t json])
      ~[['id' s+id.response] ['res' res.response]]
    ::
        %error
      :-  %o
      %-  molt
      ^-  (list [@t json])
      :~  ['id' s+id.response] 
          ['code' s+code.response]
          ['message' s+message.response]
      ==
    ==
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
