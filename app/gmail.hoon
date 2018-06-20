:: Three ways we interact with this app
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
::  See the%github app for example usage.
::
/?  314
/-  rfc, gmail-label, gmail-message
/+  http
::::
/=  rfctext  /:  /%/rfc  /txt/
::
//  /%/split
::/-  gmail
::  /ape/gh/split.hoon defines ++split, which splits a request
::  at the end of the longest possible endpoint.
::
=,  mimes:html
=,  html
=>  |%                              :: => only used for indentation
    ++  move  (pair bone card)
    ++  subscription-result
      $%  {$arch arch}
          {$json json}
          {$null $~}
          {$inbox (list {message-id/@t thread-id/@t})}
          {$message from/@t subject/@t}
      ==
    ++  card
      $%  {$diff subscription-result}
          {$hiss wire {$~ $~} $httr {$hiss hiss:eyre}}
      ==
    ++  easy-ot
      =,  dejs-soft:format
      |*  {key/@t parser/fist}
      (ot [key parser] ~)
    ++  sifo-google
      |=  a/cord  ^-  cord
      =;  fel  (crip (scan (en-base64 a) fel))
      (star ;~(pose (cold '-' (just '+')) (cold '_' (just '/')) next))
    ++  ofis-google
      |=  a/cord  ^-  cord
      =;  fel  (de-base64 (crip (rash a fel)))
      (star ;~(pose (cold '+' (just '-')) (cold '/' (just '_')) next))
    --
::
=,  gall
|_  $:  hid/bowl  count/@
        web-hooks/(map @t {id/@t listeners/(set bone)})
        received-ids/(list @t)
    ==

::  We can't actually give the response to pretty much anything
::  without blocking, so we just block unconditionally.
::
++  prep  ~&  'prep'  _`.  ::
::
++  peek
  |=  {ren/@tas pax/path}
  ^-  (unit (unit (pair mark *)))
  ~
::
++  peer-scry
  |=  pax/path
  ^-  {(list move) _+>.$}
  ?>  ?=({care:clay ^} pax)                             ::  assert %u
  =>  (help i.pax i.t.pax t.t.pax)
  =>  scry
  %=  make-move
    count  +(count)
  ==
::
++  poke-email
  |=  {adr/@ta tyl/tape mez/wall}  ^-  (quip move _+>)
  ?>  =(our.hid src.hid)
  %-  poke-gmail-req  :*
    %post
    /messages/send
    ~['uploadType'^%simple]
    ['urbit' 'urbit.org'] :: [(crip "urbit+{<our.hid>}") 'urbit.org']
  ::
    =-  (rash adr -)
    [;~((glue pat) . .)]:(cook crip (plus ;~(less pat next)))  :: /[^@]+@[^@]+/
  ::
    (crip tyl)
    (of-wain:format (turn mez crip))
  ==
::
++  poke-gmail-req
  |=  $:  method/meth:eyre  endpoint/path  quy/quay:eyre
          mes/message:rfc
          :: label-req:gmail-label
      ==
  ^-  {(list move) _+>.$}
  ?>  ?=(valid-get-endpoint endpoint)
  ?>  =(our.hid src.hid)
  :_  +>.$  :_  ~
  ^-  move
  :*  ost.hid  %hiss  /poke/[method]  `~  %httr  %hiss
      ^-  purl:eyre
      :+  [& ~ [%& /com/googleapis/www]]
        [~ gmail+v1+users+me+`valid-get-endpoint`endpoint]
      `quay:eyre`[[%alt %json] ~]
  ::
      :+  method  `math:eyre`(malt ~[content-type+['application/json']~])
      =/  hoon-json-object
        (frond:enjs:format %raw s+(sifo-google (message-to-rfc822:rfc mes)))
      =+  request-body=(as-octt (en-json hoon-json-object))
      (some request-body)
      ::(some (en-json label-req-to-json:gmail-label label-req:gmail-label ~)) XX
  ==
::
::  HTTP response.  We make sure the response is good, then
::  produce the result (as JSON) to whoever sent the request.
::

++  sigh-httr
  |=  {wir/wire res/httr:eyre}
  ^-  {(list move) _+>.$}
  :: ~&  wir+wir
  ?.  ?=({care:clay @ @ @ *} wir)
    ::  pokes don't return anything
    ~&  sigh-poke+p.res
    [~ +>.$]
  =+  arg=(path (cue (slav %uv i.t.t.wir)))
  :: ~&  ittwir+i.t.t.wir
  :_  +>.$  :_  ~
  :+  ost.hid  %diff
  ?+  i.wir  null+~
      $x
    =,  enjs:format
    ?~  r.res
      json+(pairs err+s+%empty-response code+(numb p.res) ~)
    =+  jon=(rush q.u.r.res apex:de-json)
    ?~  jon
      json+(pairs err+s+%bad-json code+(numb p.res) body+s+q.u.r.res ~)
    ?.  =(2 (div p.res 100))
      json+(pairs err+s+%request-rejected code+(numb p.res) msg+u.jon ~)
    ::
    ::  Once we know we have good data, we drill into the JSON
    ::  to find the specific piece of data referred to by 'arg'
    ::
  |-  ^-  subscription-result
  ?~  arg
    =+  switch=t.t.t.t.wir
    ?+  switch  [%json `json`u.jon]
      {$messages $~}
    =/  new-mezes
      ((ot messages+(ar (ot id+so 'threadId'^so ~)) ~):dejs-soft:format u.jon)
    ::%+  turn  new-mezes
    ::|=  id
    ::?<  ?=($~ new-mezes)
    ::=.  received-ids  [new-mezes received-ids]
    ::~&  received-ids
    ::=.  received
    [%inbox (need new-mezes)]
      ::
      {$messages @t $~}
      ::
    :: =+  body-parser==+(jo (ot body+(ot data+(cu ofis-google so) ~) ~)) :: (ok /body/data so):jo
    :: ~&  %.(u.jon (om (om |=(a/json (some -.a))):jo))
    :: ~&  %.(u.jon (ot headers+(cu milt (ar (ot name+so value+so ~))) ~))
    =+  ^-  $:  headers/{from/@t subject/@t}
                ::body-text/wain
            ==
        ~|  u.jon
        =-  (need (reparse u.jon))
        ^=  reparse
        =,  dejs-soft:format
        =+  ^=  from-and-subject
            |=  a/(map @t @t)  ^-  {@t @t}
            [(~(got by a) 'From') (~(got by a) 'Subject')]
        =+  ^=  text-body
            |=  a/(list {@t @t})  ^-  wain
            %-  to-wain
            %-  ofis-google
            (~(got by (~(gas by *(map @t @t)) a)) 'text/plain')
        %+  easy-ot  %payload
        %-  ot  :~
          headers+(cu from-and-subject (cu ~(gas by *(map @t @t)) (ar (ot name+so value+so ~))))
          :: parts+(cu text-body (ar (ot 'mimeType'^so body+(ot data+so ~) ~)))
        ==
    :: =+  parsed-headers==+(jo ((ot payload+(easy-ot 'headers' (ar some)) ~) u.jon)) ::
    :: =+  parsed-message==+(jo ((ot payload+(easy-ot 'parts' (ar body-parser)) ~) u.jon)) ::
    ::~&  [headers body-text]
    ::=+  body==+(jo ((ot body+(easy-ot 'body' (easy-ot 'data' so))) parsed-message))
    [%message headers]
    ==
  ::
  =+  dir=((om:dejs-soft:format some) u.jon)
  ?~  dir  json+(pairs:enjs:format err+s+%no-children ~)
  =+  new-jon=(~(get by u.dir) i.arg)
  `subscription-result`$(arg t.arg, u.jon ?~(new-jon ~ u.new-jon))
           ::  redo with next argument
  ::
    $y
  ?~  r.res
    ~&  [err+s+%empty-response code+(numb:enjs:format p.res)]
      arch+*arch
  =+  jon=(rush q.u.r.res apex:de-json)
  ?~  jon
    ~&  [err+s+%bad-json code+(numb:enjs:format p.res) body+s+q.u.r.res]
    arch+*arch
  ?.  =(2 (div p.res 100))
    ~&  [err+s+%request-rejected code+(numb:enjs:format p.res) msg+u.jon]
    arch+*arch
    ::
    ::  Once we know we have good data, we drill into the JSON
    ::  to find the specific piece of data referred to by 'arg'
    ::
    |-  ^-  subscription-result
    =+  dir=((om:dejs-soft:format some) u.jon)
    ?~  dir
      [%arch `(shax (jam u.jon)) ~]
    ?~  arg
      [%arch `(shax (jam u.jon)) (~(run by u.dir) $~)]
    =+  new-jon=(~(get by u.dir) i.arg)
    $(arg t.arg, u.jon ?~(new-jon ~ u.new-jon))
  ==
::
++  sigh-tang  |=({a/wire b/tang} (mean >gmail+a< b))
++  sigh
  |=  a/*
  ~&  a+a
  :_  +>.$  ~
::
++  help
  |=  {ren/care:clay style/@tas pax/path}
  =^  query  pax
    =+  xap=(flop pax)
    ?~  xap  [~ ~]
    =+  query=(rush i.xap ;~(pfix wut yquy:de-purl))
    ?~  query  [~ pax]
    [u.query (flop t.xap)]
  =^  arg  pax  ~|(pax [+ -]:(split pax))
  ~|  [pax=pax arg=arg query=query]
  =|  mow/(list move)
  |%
  ::  Resolve core
  ::
  ++  make-move
    ^-  {(list move) _+>.$}
    [(flop mow) +>.$]
    ::
  ++  endpoint-to-purl
    |=  endpoint/path
    ^-  purl:eyre
    %+  scan
      "https://www.googleapis.com/gmail/v1/users/me{<`path`endpoint>}"
    auri:de-purl
    ::  Send an HTTP req
  ++  send-http
    |=  hiz/hiss:eyre
    ^+  +>
    =+  wir=`wire`[ren (scot %ud count) (scot %uv (jam arg)) style pax]
    =+  new-move=[ost.hid %hiss wir `~ %httr [%hiss hiz]]
    +>.$(mow [new-move mow])
  ::
  ++  scry
    ^+  .
    ?+  style  ~|(%invalid-style !!)
      $read   read
::        $listen listen
    ==
  :: Standard GET request
  ++  read  (send-http (endpoint-to-purl pax) %get ~ ~)

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
