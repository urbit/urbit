::  Pass-through Eyre driver
::
/-  spider, *aquarium
/+  ph-io, util=ph-util, threadio
=,  thread=thread:spider
|%
+$  state
  $:  logs=(list az-log)  ::  oldest logs first
      lives=(map ship [lyfe=life rut=rift])
  ==
::
+$  azimuth-command
  $%  [%spawn =ship]
      [%create-ship =ship]
      [%breach =ship]
      [%breach-and-hear who=ship her=ship]
  ==
::
+$  az-log  [topics=(lest @) data=@t]
--
=;  core
  ^-  imp:spider
  |=  [=bowl:spider args=vase]
  =/  m  (thread ,vase)
  ^-  form:m
  ;<  ~  bind:m  (watch-our:threadio /effects %aqua /effect)
  ;<  ~  bind:m
    %-  (main-loop:threadio ,state)
    :~  |=(=state ~(handle-unix-effect core state))
        |=(=state ~(handle-poke core state))
        pure:(thread ,state)
    ==
  (pure:m *vase)
::
|_  =state
++  handle-unix-effect
  =/  m  (thread ,_state)
  ^-  form:m
  ;<  [her=ship =unix-effect]  bind:m
    ((handle:threadio ,[ship unix-effect]) take-unix-effect:ph-io)
  ;<  our=ship                 bind:m  get-our:ph-io
  =/  card  (router our her unix-effect)
  ?~  card
    (pure:m state)
  ::  send in next event to avoid inverting subscription flow.  real
  ::  solution is probably for gall to drip subscription updates.
  ::
  ;<  ~                        bind:m  (sleep:threadio ~s0)
  ;<  ~                        bind:m  (send-raw-cards:threadio u.card ~)
  (pure:m state)
::
++  router
  |=  [our=ship her=ship uf=unix-effect]
  ^-  (unit card:agent:mall)
  =,  enjs:format
  =/  ask  (extract-request:util uf 'http://localhost:8545/')
  ?~  ask
    ~
  ?~  body.request.u.ask
    ~
  =/  req  q.u.body.request.u.ask
  |^  ^-  (unit card:agent:mall)
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
    (add launch:contracts:azimuth (dec (lent logs.state)))
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
    ^-  card:agent:mall
    =/  resp
      %-  crip
      %-  en-json:html
      %-  pairs
      :~  id+s+(get-id req)
          jsonrpc+s+'2.0'
          result+result
      ==
    =/  events=(list aqua-event)
      :_  ~
      :*  %event
          her
          //http-client/0v1n.2m9vh
          %receive
          num.u.ask
          [%start [200 ~] `(as-octs:mimes:html resp) &]
      ==
    :*  %pass  /aqua-events
        %agent  [our %aqua]
        %poke  %aqua-events
        !>(aqua-events)
    ==
  ::
  ++  number-to-hash
    |=  =number:block:able:jael
    ^-  @
    ?:  (lth number launch:contracts:azimuth)
      (cat 3 0x5364 (sub launch:contracts:azimuth number))
    (cat 3 0x5363 (sub number launch:contracts:azimuth))
  ::
  ++  hash-to-number
    |=  =hash:block:able:jael
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
    logs.state
  ::
  ++  logs-by-hash
    |=  =hash:block:able:jael
    =/  =number:block:able:jael  (hash-to-number hash)
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
++  handle-poke
  =/  m  (thread ,_state)
  ^-  form:m
  ;<  =vase  bind:m  ((handle:ph-io ,vase) (take-poke:ph-io %azimuth-command))
  =/  command  !<(azimuth-command vase)
  ?-  -.command
    %spawn            (spawn +.command)
    %create-ship      (raw-real-ship +.command)
    %breach           (breach +.command)
    %breach-and-hear  (breach-and-hear +.command)
  ==
::
++  raw-real-ship
  |=  who=ship
  =/  m  (thread ,_state)
  ^-  form:m
  ?.  =(%earl (clan:title who))
    ;<  ~  bind:m  (raw-ship:ph-io who `(dawn who ~))
    (pure:m state)
  =/  spon=ship  (^sein:title who)
  =/  cub  (pit:nu:crub:crypto 512 (shaz (jam who life=1 %entropy)))
  =/  =seed:able:jael
    [who 1 sec:ex:cub ~]
  =/  =pass  pub:ex:cub
  =/  com=tape  "|moon {(scow %p who)}, =public-key {(scow %uw pass)}"
  ;<  ~  bind:m  (dojo:ph-io spon com)
  ;<  ~  bind:m  (raw-ship:ph-io who `(dawn who `seed))
  (pure:m state)
::
++  dawn
  |=  [who=ship seed=(unit seed:able:jael)]
  =-  ~&  >>  [%dawn-event -]  -
  ^-  dawn-event:able:jael
  =/  spon=(list [ship point:azimuth])
    |-  ^-  (list [ship point:azimuth])
    =/  =ship  (^sein:title who)
    =/  a-point=[^ship point:azimuth]
      =/  spon-spon  [& (^sein:title ship)]
      =/  life-rift  ~|([ship lives.state] (~(got by lives.state) ship))
      =/  =life  lyfe.life-rift
      =/  =rift  rut.life-rift
      =/  =pass
        %^    pass-from-eth:azimuth
            (as-octs:mimes:html (get-public ship life %crypt))
          (as-octs:mimes:html (get-public ship life %auth))
        1
      :^    ship
          *[address address address address]:azimuth
        `[life=life pass rift spon-spon ~]
      ~
    ?:  ?=(%czar (clan:title ship))
      [a-point]~
    [a-point $(who ship)]
  =/  =seed:able:jael
    ?^  seed
      u.seed
    =/  life-rift  (~(got by lives.state) who)
    =/  =life  lyfe.life-rift
    [who life sec:ex:(get-keys who life) ~]
  :*  seed
      spon
      get-czars
      ~[~['arvo' 'netw' 'ork']]
      0
      `(need (de-purl:html 'http://localhost:8545'))
  ==
::
::  Should only do galaxies
::
++  get-czars
  ^-  (map ship [rift life pass])
  %-  malt
  %+  murn
    ~(tap by lives.state)
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
  =/  m  (thread ,_state)
  ^-  form:m
  ?<  (~(has by lives.state) who)
  =.  lives.state  (~(put by lives.state) who [1 0])
  =.  logs.state
    %+  weld  logs.state
    :~  %-  changed-keys:lo
        :*  who
            (get-public who 1 %crypt)
            (get-public who 1 %auth)
            1
            1
        ==
    ==
  (spam-logs 30)
::
++  cycle-keys
  |=  who=@p
  =/  m  (thread ,_state)
  ^-  form:m
  =/  prev  (~(got by lives.state) who)
  =/  lyfe  +(lyfe.prev)
  =.  lives.state  (~(put by lives.state) who [lyfe rut.prev])
  =.  logs.state
    %+  weld  logs.state
    :_  ~
    %-  changed-keys:lo
    :*  who
        (get-public who lyfe %crypt)
        (get-public who lyfe %auth)
        1
        lyfe
    ==
  (pure:m state)
::
::  who: breachee
::  her: wait until hears about breach
::
++  breach-and-hear
  |=  [who=@p her=@p]
  =/  m  (thread ,_state)
  ^-  form:m
  ;<  =new=^state              bind:m  (breach who)
  =.  state  new-state
  =/  new-rut  rut:(~(got by lives.state) who)
  |-  ^-  form:m
  =*  loop  $
  ;<  [him=ship =unix-effect]  bind:m  take-unix-effect:ph-io
  ;<  =bowl:spider             bind:m  get-bowl:ph-io
  =/  aqua-pax
    :-  %i
    /(scot %p her)/j/(scot %p her)/rift/(scot %da now.bowl)/(scot %p who)/noun
  =/  rut  (scry-aqua:util noun our.bowl now.bowl aqua-pax)
  ?:  =([~ new-rut] rut)
    (pure:m state)
  loop
::
++  breach
  |=  who=@p
  =/  m  (thread ,_state)
  ^-  form:m
  ;<  =new=^state  bind:m  (cycle-keys who)
  =.  state  new-state
  =/  prev  (~(got by lives.state) who)
  =/  rut  +(rut.prev)
  =.  lives.state  (~(put by lives.state) who [lyfe.prev rut])
  =.  logs.state
    %+  weld  logs.state
    [(broke-continuity:lo who rut) ~]
  (spam-logs 30)
::
++  spam-logs
  |=  n=@
  =/  m  (thread ,_state)
  ^-  form:m
  =*  loop  $
  ?:  =(n 0)
    (pure:m state)
  ;<  =new=^state  bind:m
    ?.  (~(has by lives.state) ~fes)
      (spawn ~fes)
    (cycle-keys ~fes)
  =.  state  new-state
  loop(n (dec n))
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
