:: Three ways we interactg with this app
:: 1. .^(%gx /=gh=/endpoint)
:: 2. [%peer [our %gh] /endpoint]
:: 3. :gh &gh-poke %post /gists json-data


::  This is a driver for the Github API v3.
::
::  You can interact with this in a few different ways:
::
::    - .^(%gx /=gh=/read{/endpoint}) or subscribe to
::      /scry/x/read{/endpoint} for authenticated reads.
::
::    - subscribe to /scry/x/listen/{owner}/{repo}/{events...}
::      for webhook-powered event notifications.  For event list,
::      see https://developer.github.com/webhooks/.
::
::  See the %github app for example usage.
::
/?  314
/+  http
//  /%/split
::/-  gmail
::  /ape/gh/split.hoon defines ++split, which splits a request
::  at the end of the longest possible endpoint.
::

=>  |%                              :: => only used for indentation
    ++  move  (pair bone card)
    ++  subscription-result
      $%  [%arch arch]
          [%json json]      
          [%null ~]
      ==
    ++  card
      $%  [%diff subscription-result]
          [%hiss wire [~ ~] %httr [%hiss hiss]]
      ==
    --
::
|_  [hid=bowl count=@ web-hooks=(map ,@t ,[id=@t listenters=(set bone)])]

::  We can't actually give the response to pretty much anything
::  without blocking, so we just block unconditionally.
::
++  peek
  |=  [ren=@tas pax=path]
  ^-  (unit (unit (pair mark ,*)))
  ~&  'arrived at peek'
  ~
::
++  peer-scry
  |=  pax=path
  ~&  'arrived at peer-scry'
  ^-  [(list move) _+>.$]
  ?>  ?=([care ^] pax)                ::  assert %u
  ~&  'arrived at peer-scry, passed the care assert'
  =>  (help i.pax i.t.pax t.t.pax)
  ~&  'past help'
  =>  scry  ::HERE IS THE PROBLEM
  ~&  mow.+>.$
  ~&  'arrived after scry'
  make-move
::
++  poke-gmail-req
  |=  [method=meth endpoint=path quy=quay]:: jon=(unit json)]  :: XX [%get json]?
  ^-  [(list move) _+>.$]
  ~&  [endpoint/endpoint method/method]
  ?>  ?=(valid-get-endpoint endpoint)
  :_  +>.$  :_  ~
  ^-  move
  :*  ost.hid  %hiss  /poke/[method]  `~  %httr  %hiss
      `purl`[[& ~ [%& /com/googleapis/www]] [~ gmail/v1/users/me/`valid-get-endpoint`endpoint] ~]
      :+  method  ~
      ~
      ::?~  jon  ~
      ::(some (taco (crip (pojo u.jon)))) ::
  ==
::
::  HTTP response.  We make sure the response is good, then
::  produce the result (as JSON) to whoever sent the request.
::

++  sigh-httr
  |=  [wir=wire res=httr]
  ~&  'reached sigh-httr'
  ~&  wir
  ^-  [(list move) _+>.$]
  ?.  ?=([care @ @ @ *] wir)
  ::  pokes don't return anything
    [~ +>.$]
    ~&  [i.t.t.wir wir]
  =+  arg=(path (cue (slav %uv i.t.t.wir)))
  :_  +>.$
  ~


++  sigh
  |=  *
  ~&  sigh-no-httr/.
  :_  +>.$  ~
::
++  help
  |=  [ren=care style=@tas pax=path]
  =^  arg  pax  [+ -]:(split pax)
  =|  mow=(list move)
  |%
  ::  Resolve core
  ::
  ++  make-move
    ^-  [(list move) _+>.$]
    ~&  'make move is called'
    [(flop mow) +>.$]
    ::
  ++  endpoint-to-purl
    |=  endpoint=path
    =+  pnt=(scan "https://googleapis.com/v1/users/me{<`path`endpoint>}" auri:epur)
    ~&  pnt  pnt
    ::  Send an HTTP req
  ++  send-http
    |=  hiz=hiss
    ^+  +>
    =+  wir=`wire`[ren (scot %ud count) (scot %uv (jam arg)) style pax]
    =+  new-move=[ost.hid %hiss wir `~ %httr [%hiss hiz]]
    +>.$(mow [new-move mow])
  ::
  ++  scry
    ^+  .
    ~&  style/style
    ?+  style  ~|(%invalid-style !!)
      %read   ~&  'reached read'  read
::        %listen listen
    ==
  :: Standard GET request
  ++  read  ~&((send-http (endpoint-to-purl pax) %get ~ ~) (send-http (endpoint-to-purl pax) %get ~ ~))
  
  ::  Subscription request
::    ++  listen
::      ^+  .
::      =+  events=?>(?=([@ @ *] pax) t.t.pax)
::      |-  ^+  +>.$
::      ?~  events
::        +>.$
::      ?:  (~(has by web-hooks) i.events)                ::  if hook exists
::        =.  +>.$  (update-hook i.events) 
::        $(events t.events)
::      =.  +>.$  (create-hook i.events)
::      $(events t.events)
    ::
  --
--        
  


  
  




