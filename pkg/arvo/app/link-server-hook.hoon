::  link-server: accessing link-store via eyre
::
::    only accepts requests authenticated as the host ship.
::
::    GET requests:
::      /~link/local-pages/[some-path].json?p=0
::        our submissions on path, with optional pagination
::
::    POST requests:
::      /~link/add/[some-path]
::        send {title url} json, will save link at path
::
/+  *link, *server, default-agent, verb
::
|%
+$  state-0
  $:  %0
      ~
      ::NOTE  this means we could get away with just producing cards everywhere,
      ::      never producing new state outside of the agent interface core.
      ::      we opt to keep ^-(quip card _state) in place for most logic arms
      ::      because it doesn't cost much, results in unsurprising code, and
      ::      makes adding any state in the future easier.
  ==
::
+$  card  card:agent:gall
--
::
=|  state-0
=*  state  -
::
%+  verb  |
^-  agent:gall
=<
  |_  =bowl:gall
  +*  this  .
      do    ~(. +> bowl)
      def   ~(. (default-agent this %|) bowl)
  ::
  ++  on-init
    ^-  (quip card _this)
    :_  this
    [start-serving:do]~
  ::
  ++  on-save  !>(state)
  ++  on-load
    |=  old=vase
    ^-  (quip card _this)
    [~ this(state !<(state-0 old))]
  ::
  ++  on-watch
    |=  =path
    ^-  (quip card _this)
    ?:  ?=([%http-response *] path)
      [~ this]
    (on-watch:def path)
  ::
  ++  on-poke
    |=  [=mark =vase]
    ^-  (quip card _this)
    ?.  ?=(%handle-http-request mark)
      (on-poke:def mark vase)
    :_  this
    =+  !<([eyre-id=@ta =inbound-request:eyre] vase)
    (handle-http-request:do eyre-id inbound-request)
  ::
  ++  on-arvo
    |=  [=wire =sign-arvo]
    ^-  (quip card _this)
    ?.  ?=(%bound +<.sign-arvo)
      (on-arvo:def wire sign-arvo)
    [~ this]
  ::
  ++  on-agent
    |=  [=wire =sign:agent:gall]
    ^-  (quip card _this)
    ?.  ?=(%poke-ack -.sign)
      (on-agent:def wire sign)
    ?~  p.sign  [~ this]
    =/  =tank
      leaf+"{(trip dap.bowl)} failed writing to %link-store"
    %-  (slog tank u.p.sign)
    [~ this]
  ::
  ++  on-peek   on-peek:def
  ++  on-leave  on-leave:def
  ++  on-fail   on-fail:def
  --
::
|_  =bowl:gall
::
++  start-serving
  ^-  card
  [%pass / %arvo %e %connect [~ /'~link'] dap.bowl]
::
++  do-action
  |=  =action
  ^-  card
  [%pass / %agent [our.bowl %link-store] %poke %link-action !>(action)]
::
++  do-add
  |=  [=path title=@t =url]
  ^-  card
  (do-action %add path title url)
::
++  handle-http-request
  |=  [eyre-id=@ta =inbound-request:eyre]
  ^-  (list card)
  ::NOTE  we don't use +require-authorization because it's too restrictive
  ::      on the flow we want here.
  ::
  ?.  ?&  authenticated.inbound-request
          =(src.bowl our.bowl)
      ==
    ::TODO  `*octs -> ~ everywhere once no-data bug is fixed
    (give-simple-payload:app eyre-id [[403 ~] `*octs])
  ::  request-line: parsed url + params
  ::
  =/  =request-line
    %-  parse-request-line
    url.request.inbound-request
  =*  req-head  header-list.request.inbound-request
  =-  ::TODO  =;  [cards=(list card) =simple-payload:http]
    %+  weld  cards
    (give-simple-payload:app eyre-id simple-payload)
  ^-  [cards=(list card) =simple-payload:http]
  ?+  method.request.inbound-request  [~ not-found:gen]
      %'OPTIONS'
    [~ (include-cors-headers req-head [[200 ~] `*octs])]
  ::
      %'GET'
    [~ (handle-get req-head request-line)]
  ::
      %'POST'
    (handle-post req-head request-line body.request.inbound-request)
  ==
::
++  handle-post
  |=  [request-headers=header-list:http =request-line body=(unit octs)]
  ^-  [(list card) simple-payload:http]
  =-  ::TODO  =;  [success=? cards=(list card)]
    :-  cards
    %+  include-cors-headers
      request-headers
    ::TODO  it would be more correct to wait for the %poke-ack instead of
    ::      sending this response right away... but link-store pokes can't
    ::      actually fail right now, so it's fine.
    [[?:(success 200 400) ~] `*octs]
  ^-  [success=? cards=(list card)]
  ?~  body  [| ~]
  ?+  request-line  [| ~]
      [[~ [%'~link' %add ^]] ~]
    ^-  [? (list card)]
    =/  jon=(unit json)  (de-json:html q.u.body)
    ?~  jon  [| ~]
    =/  page=(unit [title=@t =url])
      %.  u.jon
      (ot title+so url+so ~):dejs-soft:format
    ?~  page  [| ~]
    [& [(do-add t.t.site.request-line [title url]:u.page) ~]]
  ==
::
++  handle-get
  |=  [request-headers=header-list:http =request-line]
  %+  include-cors-headers
    request-headers
  ^-  simple-payload:http
  ::  args: map of params
  ::  p: pagination index
  ::
  =/  args
    %-  ~(gas by *(map @t @t))
    args.request-line
  =/  p=(unit @ud)
    %+  biff  (~(get by args) 'p')
    (curr rush dim:ag)
  ?+  request-line  not-found:gen
  ::TODO  expose submissions, other data
  ::  local links by recency as json
  ::
      [[[~ %json] [%'~link' %local-pages ^]] *]
    %-  json-response:gen
    %-  json-to-octs  ::TODO  include in +json-response:gen
    ^-  json
    :-  %a
    %+  turn
      `pages`(get-pages t.t.site.request-line p)
    `$-(page json)`page:en-json
  ==
::
++  include-cors-headers
  |=  [request-headers=header-list:http =simple-payload:http]
  ^+  simple-payload
  =*  out-heads  headers.response-header.simple-payload
  =;  =header-list:http
    |-
    ?~  header-list  simple-payload
    =*  new-head  i.header-list
    =.  out-heads
      (set-header:http key.new-head value.new-head out-heads)
    $(header-list t.header-list)
  =/  origin=@t
    =/  headers=(map @t @t)
      (~(gas by *(map @t @t)) request-headers)
    (~(gut by headers) 'origin' '*')
  :~  'Access-Control-Allow-Origin'^origin
      'Access-Control-Allow-Credentials'^'true'
      'Access-Control-Request-Method'^'OPTIONS, GET, POST'
      'Access-Control-Allow-Methods'^'OPTIONS, GET, POST'
      'Access-Control-Allow-Headers'^'content-type'
  ==
::
++  page-size  25
++  get-pages
  |=  [=path p=(unit @ud)]
  ^-  pages
  =;  =pages
    ?~  p  pages
    %+  scag  page-size
    %+  slag  (mul u.p page-size)
    pages
  .^  pages
    %gx
    (scot %p our.bowl)
    %link-store
    (scot %da now.bowl)
    %local-pages
    (snoc path %noun)
  ==
--