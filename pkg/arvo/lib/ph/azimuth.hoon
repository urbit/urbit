::  Mock Azimuth
::
/+  ph, ph-util, ph-philter
=,  ph
=,  ph-util
=,  ph-philter
=>  |%
    +$  az-log  [topics=(lest @) data=@t]
    --
=|  logs=(list az-log)  ::  oldest logs first
=|  lives=(map ship [lyfe=life rut=rift])
=|  $=  eth-filters
    $:  next=_1  ::  jael assumes != 0
        all=(map @ud [from-block=@ud last-block=@ud address=@ux])
    ==
|%
++  this-az  .
++  add-logs
  |=  new-logs=(list az-log)
  ^+  this-az
  =.  logs  (weld logs new-logs)
  this-az
::
++  router
  =/  n  (philter ,_this-az)
  ^-  form:n
  |%
  ++  stay  this-az
  ++  run
    |=  pin=ph-input
    ^-  output:n
    =,  enjs:format
    =/  ask  (extract-request uf.pin 'http://localhost:8545/')
    ?~  ask
      [& ~ %wait ~]
    ?~  body.request.u.ask
      [& ~ %wait ~]
    =/  req  q.u.body.request.u.ask
    |^  ^-  output:n
    =/  method  (get-method req)
    ::  =;  a  ~&  [%give-azimuth-response a]  -
    ?:  =(method 'eth_blockNumber')
      :-  |  :_  [%wait ~]
      %+  answer-request  req
      s+(crip (num-to-hex:ethereum latest-block))
    ?:  =(method 'eth_getBlockByNumber')
      :-  |  :_  [%wait ~]
      %+  answer-request  req
      :-  %o
      =/  number  (hex-to-num:ethereum (get-first-param req))
      =/  hash  (number-to-hash number)
      ~&  who=who.pin
      ~&  number=number
      ~&  hash=hash
      =/  parent-hash  (number-to-hash ?~(number number (dec number)))
      ~&  parent-hash=parent-hash
      %-  malt
      ^-  (list (pair term json))
      :~  hash+s+(crip (prefix-hex:ethereum (render-hex-bytes:ethereum 32 hash)))
          number+s+(crip (num-to-hex:ethereum number))
          'parentHash'^s+(crip (num-to-hex:ethereum parent-hash))
      ==
    ?:  =(method 'eth_getLogs')
      :-  |  :_  [%wait ~]
      %+  answer-request  req
      ?^  (get-param-obj-maybe req 'blockHash')
        %-  logs-by-hash
        (get-param-obj req 'blockHash')
      %+  logs-by-range
        (get-param-obj req 'fromBlock')
      (get-param-obj req 'toBlock')
    ?:  =(method 'eth_newFilter')
      :+  |
        (answer-request req s+(scot %ux next.eth-filters))
      =.  all.eth-filters
        %+  ~(put by all.eth-filters)
          next.eth-filters
        :+
            (get-param-obj req 'fromBlock')
          (get-param-obj req 'fromBlock')
        (get-param-obj req 'address')
      =.  next.eth-filters  +(next.eth-filters)
      [%cont ..stay]
    ?:  =(method 'eth_getFilterLogs')
      =/  fil  (~(get by all.eth-filters) (get-filter-id req))
      ?~  fil
        ~|(%no-filter-not-implemented !!)
      :+  |
        %+  answer-request  req
        ~|  [eth-filters latest-block]
        (logs-by-range from-block.u.fil latest-block)
      =.  last-block.u.fil  latest-block
      [%cont ..stay]
    ?:  =(method 'eth_getFilterChanges')
      =/  fil-id  (get-filter-id req)
      =/  fil  (~(get by all.eth-filters) fil-id)
      ?~  fil
        ~|(%no-filter-not-implemented !!)
      :+  |
        %+  answer-request  req
        (logs-by-range last-block.u.fil latest-block)
      =.  all.eth-filters
        %+  ~(put by all.eth-filters)
          fil-id
        u.fil(last-block latest-block)
      [%cont ..stay]
    ~&  [%ph-azimuth-miss req]
    [& ~ %wait ~]
    ::
    ++  latest-block
      (add launch:contracts:azimuth (dec (lent logs)))
    ::
    ++  get-id
      |=  req=@t
      =,  dejs:format
      %.  (need (de-json:html req))
      (ot id+so ~)
    ::
    ++  get-method
      |=  req=@t
      =,  dejs:format
      %.  (need (de-json:html req))
      (ot method+so ~)
    ::
    ++  get-param-obj
      |=  [req=@t param=@t]
      =,  dejs:format
      %-  hex-to-num:ethereum
      =/  array
        %.  (need (de-json:html req))
        (ot params+(ar (ot param^so ~)) ~)
      ?>  ?=([* ~] array)
      i.array
    ::
    ++  get-param-obj-maybe
      |=  [req=@t param=@t]
      ^-  (unit @ud)
      =,  dejs-soft:format
      =/  array
        %.  (need (de-json:html req))
        (ot params+(ar (ot param^so ~)) ~)
      ?~  array
        ~
      :-  ~
      ?>  ?=([* ~] u.array)
      %-  hex-to-num:ethereum
      i.u.array
    ::
    ++  get-filter-id
      |=  req=@t
      =,  dejs:format
      %-  hex-to-num:ethereum
      =/  id
        %.  (need (de-json:html req))
        (ot params+(ar so) ~)
      ?>  ?=([* ~] id)
      i.id
    ::
    ++  get-first-param
      |=  req=@t
      =,  dejs:format
      =/  id
        %.  (need (de-json:html req))
        (ot params+(at so bo ~) ~)
      -.id
    ::
    ++  answer-request
      |=  [req=@t result=json]
      ^-  (list ph-event)
      =/  resp
        %-  crip
        %-  en-json:html
        %-  pairs
        :~  id+s+(get-id req)
            jsonrpc+s+'2.0'
            result+result
        ==
      :_  ~
      :*  %event
          who.pin
          //http-client/0v1n.2m9vh
          %receive
          num.u.ask
          [%start [200 ~] `(as-octs:mimes:html resp) &]
      ==
    ::
    ++  number-to-hash
      |=  =number:block:able:kale
      ^-  @
      ?:  (lth number launch:contracts:azimuth)
        (cat 3 0x5364 (sub launch:contracts:azimuth number))
      (cat 3 0x5363 (sub number launch:contracts:azimuth))
    ::
    ++  hash-to-number
      |=  =hash:block:able:kale
      (add launch:contracts:azimuth (div hash 0x1.0000))
    ::
    ++  logs-by-range
      |=  [from-block=@ud to-block=@ud]
      %+  logs-to-json  (max launch:contracts:azimuth from-block)
      ?:  (lth to-block launch:contracts:azimuth)
        ~
      %+  swag
        ~&  [%logs-by-range from-block to-block launch:contracts:azimuth]
        ?:  (lth from-block launch:contracts:azimuth)
           [0 +((sub to-block launch:contracts:azimuth))]
        :-  (sub from-block launch:contracts:azimuth)
        +((sub to-block from-block))
      logs
    ::
    ++  logs-by-hash
      |=  =hash:block:able:kale
      =/  =number:block:able:kale  (hash-to-number hash)
      (logs-by-range number +(number))
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
  --
::
++  dawn
  |=  who=ship
  ^-  dawn-event
  =/  spon
    =/  =ship  (^sein:title who)
    =/  life-rift  (~(got by lives) ship)
    =/  =life  lyfe.life-rift
    =/  =rift  rut.life-rift
    =/  =pass
      %^    pass-from-eth:azimuth
          (as-octs:mimes:html (get-public ship life %crypt))
        (as-octs:mimes:html (get-public ship life %auth))
      1
    [ship rift life pass (^sein:title ship)]
  =/  life-rift  (~(got by lives) who)
  =/  =life  lyfe.life-rift
  :*  [who life sec:ex:(get-keys who life) ~]
      spon
      get-czars
      ~[~['arvo' 'netw' 'ork']]
      0
      `(need (de-purl:html 'http://localhost:8545'))
      ~
  ==
::
::  Should only do galaxies
::
++  get-czars
  ^-  (map ship [rift life pass])
  %-  malt
  %+  murn
    ~(tap by lives)
  |=  [who=ship lyfe=life rut=rift]
  ?.  =(%czar (clan:title who))
    ~
  %-  some
  :^  who  rut  lyfe
  %^    pass-from-eth:azimuth
      (as-octs:mimes:html (get-public who lyfe %crypt))
    (as-octs:mimes:html (get-public who lyfe %auth))
  1
::
++  spawn
  |=  who=@p
  ?<  (~(has by lives) who)
  =.  lives  (~(put by lives) who [1 0])
  %-  add-logs
  %+  welp
    ?:  =(%czar (clan:title who))
      ~
    ~[(spawned:lo (^sein:title who) who)]
  :~  (activated:lo who)
      (owner-changed:lo who 0xdead.beef)
      %-  changed-keys:lo
      :*  who
          (get-public who 1 %crypt)
          (get-public who 1 %auth)
          1
          1
      ==
  ==
::
::  our: host ship
::  who: cycle keys
::  her: wait until hears about cycle
::
++  cycle-keys-and-hear
  |=  [our=@p who=@p her=@p]
  =.  this-az  (cycle-keys who)
  =/  new-lyfe  lyfe:(~(got by lives) who)
  =/  m  (ph ,_this-az)
  ;<  [this-az=_this-az ~]  bind:m
    %+  (wrap-philter ,_this-az ,~)
      router:this-az
    ^+  *form:(ph ,~)
    |=  pin=ph-input
    :+  &  ~
    =/  aqua-pax
      :-  %i
      /(scot %p her)/j/(scot %p her)/life/(scot %da now.pin)/(scot %p who)/noun
    =/  lyfe  (scry-aqua noun our now.pin aqua-pax)
    ~&  [new-lyfe=[0 new-lyfe] lyfe=lyfe]
    ?:  =([~ new-lyfe] lyfe)
      [%done ~]
    [%wait ~]
  (pure:m this-az)
::
++  cycle-keys
  |=  who=@p
  =/  prev  (~(got by lives) who)
  =/  lyfe  +(lyfe.prev)
  =.  lives  (~(put by lives) who [lyfe rut.prev])
  %-  add-logs
  :_  ~
  %-  changed-keys:lo
  :*  who
      (get-public who lyfe %crypt)
      (get-public who lyfe %auth)
      1
      lyfe
  ==
::
::  our: host ship
::  who: breachee
::  her: wait until hears about breach
::
++  breach-and-hear
  |=  [our=@p who=@p her=@p]
  =.  this-az  (breach who)
  =.  this-az  ?:((~(has by lives) ~fes) (cycle-keys ~fes) (spawn ~fes))
  =/  new-rut  rut:(~(got by lives) who)
  =/  m  (ph ,_this-az)
  ;<  [this-az=_this-az ~]  bind:m
    %+  (wrap-philter ,_this-az ,~)
      router:this-az
    ^+  *form:(ph ,~)
    |=  pin=ph-input
    :+  &  ~
    =/  aqua-pax
      :-  %i
      /(scot %p her)/k/(scot %p her)/rift/(scot %da now.pin)/(scot %p who)/noun
    =/  rut  (scry-aqua noun our now.pin aqua-pax)
    ?:  =([~ new-rut] rut)
      [%done ~]
    [%wait ~]
  (pure:m this-az)
::
++  breach
  |=  who=@p
  =.  this-az  (cycle-keys who)
  =/  prev  (~(got by lives) who)
  =/  rut  +(rut.prev)
  =.  lives  (~(put by lives) who [lyfe.prev rut])
  %-  add-logs
  :_  ~
  (broke-continuity:lo who rut)
::
++  get-keys
  |=  [who=@p lyfe=life]
  ^-  acru:ames
  %+  pit:nu:crub:crypto  32
  (can 5 [1 (scot %p who)] [1 (scot %ud lyfe)] ~)
::
++  get-public
  |=  [who=@p lyfe=life typ=?(%auth %crypt)]
  =/  bod  (rsh 3 1 pub:ex:(get-keys who lyfe))
  =+  [enc=(rsh 8 1 bod) aut=(end 8 1 bod)]
  ?:  =(%auth typ)
    aut
  enc
::
::  Generate logs
::
++  lo
  =,  azimuth-events:azimuth
  |%
  ++  activated
    |=  who=ship
    ^-  az-log
    [~[^activated who] '']
  ::
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
  ::
  ++  owner-changed
    |=  [who=ship owner=@ux]
    ^-  az-log
    [~[^owner-changed who owner] '']
  ::
  ++  spawned
    |=  [par=ship who=ship]
    ^-  az-log
    [~[^spawned par who] '']
  --
--
