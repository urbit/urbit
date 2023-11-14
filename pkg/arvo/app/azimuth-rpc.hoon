::  Azimuth JSON-RPC API
::
/-  rpc=json-rpc
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
    ?+  mark  (on-poke:def mark vase)
        %handle-http-request
      =+  !<([id=@ta req=inbound-request:eyre] vase)
      :_  this
      (handle-http-request id req)
    ::
        %azimuth-action
      ?>  (team:title [our src]:bowl)
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
      [%pass / %agent [our.bowl %azimuth] %poke i.data]
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
    =,  azimuth-roll-rpc
    ?.  ?=([%map *] params)
      [~ ~(parse error:json-rpc id)]
    =/  method=@tas  (enkebab method)
    ?+  method  [~ ~(method error:json-rpc id)]
      %get-point        `(get-point id +.params point:scry)
      %get-dns          `(get-dns id +.params dns:scry)
      %get-naive-state  `(get-naive id +.params naive-state:scry)
      %get-refresh      `(get-refresh id +.params refresh:scry)
    ==
  --
::
++  scry
  |%
  ++  point
    |=  =ship
    .^  (unit point:naive)
        %gx
        (~(scry agentio bowl) %azimuth /point/(scot %p ship)/noun)
    ==
  ::
  ++  dns
    .^  (list @t)
        %gx
        (~(scry agentio bowl) %azimuth /dns/noun)
    ==
  ::
  ++  naive-state
    .^  ^state:naive
        %gx
        (~(scry agentio bowl) %azimuth /nas/noun)
    ==
  ::
  ++  refresh
    .^  @dr
        %gx
        (~(scry agentio bowl) %azimuth /refresh/noun)
    ==
  --
--
