/-  *aquarium
/+  ethereum, azimuth
::
|%
::
++  extract-request
  |=  [uf=unix-effect dest=@t]
  ^-  (unit [num=@ud =request:http])
  ?.  ?=(%request -.q.uf)  ~
  ?.  =(dest url.request.q.uf)  ~
  `[id.q.uf request.q.uf]
::
++  router
  |=  [our=ship her=ship uf=unix-effect azi=az-state]
  ^-  (unit card:agent:gall)
  =,  enjs:format
  =/  ask  (extract-request uf 'http://fake.aqua.domain/')
  ?~  ask
    ~
  ?~  body.request.u.ask
    ~
  =/  req  q.u.body.request.u.ask
  |^  ^-  (unit card:agent:gall)
  =/  method  (get-method req)
  ?:  =(method 'eth_blockNumber')
    :-  ~
    %+  answer-request  req
    s+(crip (num-to-hex:ethereum latest-block))
  ?:  =(method 'eth_getBlockByNumber')
    :-  ~
    %+  answer-request  req
    :-  %o
    =/  number  (hex-to-num:ethereum (get-first-param req))
    =/  hash  (number-to-hash number)
    =/  parent-hash  (number-to-hash ?~(number number (dec number)))
    %-  malt
    ^-  (list (pair term json))
    :~  hash+s+(crip (prefix-hex:ethereum (render-hex-bytes:ethereum 32 hash)))
        number+s+(crip (num-to-hex:ethereum number))
        'parentHash'^s+(crip (num-to-hex:ethereum parent-hash))
    ==
  ?:  =(method 'eth_getLogs')
    :-  ~
    %+  answer-request  req
    ?^  (get-param-obj-maybe req 'blockHash')
      %-  logs-by-hash
      (get-param-obj req 'blockHash')
    %+  logs-by-range
      (get-param-obj req 'fromBlock')
    (get-param-obj req 'toBlock')
  ~&  [%ph-azimuth-miss req]
  ~
  ::
  ++  latest-block
    (add launch:contracts:azimuth (dec (lent logs.azi)))
  ::
  ++  get-single-req
    |=  req=@t
    =/  batch
      ((ar:dejs:format same) (need (de:json:html req)))
    ?>  ?=([* ~] batch)
    i.batch
  ::
  ++  get-id
    |=  req=@t
    =,  dejs:format
    %.  (get-single-req req)
    (ot id+so ~)
  ::
  ++  get-method
    |=  req=@t
    =,  dejs:format
    ~|  req=req
    %.  (get-single-req req)
    (ot method+so ~)
  ::
  ++  get-param-obj
    |=  [req=@t param=@t]
    =,  dejs:format
    %-  hex-to-num:ethereum
    =/  array
      %.  (get-single-req req)
      (ot params+(ar (ot param^so ~)) ~)
    ?>  ?=([* ~] array)
    i.array
  ::
  ++  get-param-obj-maybe
    |=  [req=@t param=@t]
    ^-  (unit @ud)
    =,  dejs-soft:format
    =/  array
      %.  (get-single-req req)
      (ot params+(ar (ot param^so ~)) ~)
    ?~  array
      ~
    :-  ~
    ?>  ?=([* ~] u.array)
    %-  hex-to-num:ethereum
    i.u.array
  ::
  ++  get-first-param
    |=  req=@t
    =,  dejs:format
    =/  id
      %.  (get-single-req req)
      (ot params+(at so bo ~) ~)
    -.id
  ::
  ++  answer-request
    |=  [req=@t result=json]
    ^-  card:agent:gall
    =/  resp
      %-  en:json:html
      :-  %a  :_  ~
      %-  pairs
      :~  id+s+(get-id req)
          jsonrpc+s+'2.0'
          result+result
      ==
    =/  events=(list aqua-event)
      :_  ~
      :*  %event
          her
          /i/http-client/0v1n.2m9vh
          %receive
          num.u.ask
          [%start [200 ~] `(as-octs:mimes:html resp) &]
      ==
    :*  %pass  /aqua-events
        %agent  [our %aqua]
        %poke  %aqua-events
        !>(events)
    ==
  ::
  ++  number-to-hash
    |=  =number:block:jael
    ^-  @
    ?:  (lth number launch:contracts:azimuth)
      (cat 3 0x5364 (sub launch:contracts:azimuth number))
    (cat 3 0x5363 (sub number launch:contracts:azimuth))
  ::
  ++  hash-to-number
    |=  =hash:block:jael
    (add launch:contracts:azimuth (div hash 0x1.0000))
  ::
  ++  logs-by-range
    |=  [from-block=@ud to-block=@ud]
    %+  logs-to-json  (max launch:contracts:azimuth from-block)
    ?:  (lth to-block launch:contracts:azimuth)
      ~
    %+  swag
      ?:  (lth from-block launch:contracts:azimuth)
         [0 +((sub to-block launch:contracts:azimuth))]
      :-  (sub from-block launch:contracts:azimuth)
      +((sub to-block from-block))
    logs.azi
  ::
  ++  logs-by-hash
    |=  =hash:block:jael
    =/  =number:block:jael  (hash-to-number hash)
    (logs-by-range number number)
  ::
  ++  logs-to-json
    |=  [count=@ud selected-logs=(list az-log)]
    ^-  json
    :-  %a
    |-  ^-  (list json)
    ?~  selected-logs
      ~
    :_  $(selected-logs t.selected-logs, count +(count))
    %-  pairs
    :~  'logIndex'^s+'0x0'
        'transactionIndex'^s+'0x0'
        :+  'transactionHash'  %s
        (crip (prefix-hex:ethereum (render-hex-bytes:ethereum 32 `@`0x5362)))
      ::
        :+  'blockHash'  %s
        =/  hash  (number-to-hash count)
        (crip (prefix-hex:ethereum (render-hex-bytes:ethereum 32 hash)))
      ::
        :+  'blockNumber'  %s
        (crip (num-to-hex:ethereum count))
      ::
        :+  'address'  %s
        (crip (address-to-hex:ethereum azimuth:contracts:azimuth))
      ::
        'type'^s+'mined'
      ::
        'data'^s+data.i.selected-logs
        :+  'topics'  %a
        %+  turn  topics.i.selected-logs
        |=  topic=@ux
        ^-  json
        :-  %s
        %-  crip
        %-  prefix-hex:ethereum
        (render-hex-bytes:ethereum 32 `@`topic)
    ==
  --
::
++  get-keys
  |=  [who=@p lyfe=life]
  ^-  acru:ames
  %+  pit:nu:crub:crypto  32
  (can 5 [1 (scot %p who)] [1 (scot %ud lyfe)] ~)
::
++  get-public
  |=  [who=@p lyfe=life typ=?(%auth %crypt)]
  =/  bod  (rsh 3 pub:ex:(get-keys who lyfe))
  =+  [enc=(rsh 8 bod) aut=(end 8 bod)]
  ?:  =(%auth typ)
    aut
  enc
::
::  Generate logs
::
++  lo
  =,  azimuth-events:azimuth
  |%
  ++  broke-continuity
    |=  [who=ship rut=rift]
    ^-  az-log
    :-  ~[^broke-continuity who]
    %-  crip
    %-  prefix-hex:ethereum
    (render-hex-bytes:ethereum 32 `@`rut)
  ::
  ++  changed-keys
    |=  [who=ship enc=@ux aut=@ux crypto=@ud lyfe=life]
    ^-  az-log
    :-  ~[^changed-keys who]
    %-  crip
    %-  prefix-hex:ethereum
    ;:  welp
        (render-hex-bytes:ethereum 32 `@`enc)
        (render-hex-bytes:ethereum 32 `@`aut)
        (render-hex-bytes:ethereum 32 `@`crypto)
        (render-hex-bytes:ethereum 32 `@`lyfe)
    ==
  --
--
