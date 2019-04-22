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
=|  eth-filter=(unit [from-block=@ud last-block=@ud address=@ux])
=,  azimuth-events:azimuth
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
    =/  thus  (extract-thus-to uf.pin 'http://localhost:8545')
    ?~  thus
      [& ~ %wait ~]
    ?~  r.mot.u.thus
      [& ~ %wait ~]
    =/  req  q.u.r.mot.u.thus
    |^  ^-  output:n
    =/  method  (get-method req)
    ?:  =(method 'eth_blockNumber')
      :-  |  :_  [%wait ~]
      %+  answer-request  req
      s+(crip (num-to-hex:ethereum latest-block))
    ?:  =(method 'eth_getLogs')
      :-  |  :_  [%wait ~]
      %+  answer-request  req
      %+  logs-to-json
        (get-param-obj req 'fromBlock')
      (get-param-obj req 'toBlock')
    ?:  =(method 'eth_newFilter')
      :+  |
        (answer-request req s+'0xa')
      =.  eth-filter
        :^    ~
            (get-param-obj req 'fromBlock')
          (get-param-obj req 'fromBlock')
        (get-param-obj req 'address')
      [%cont ..stay]
    ?:  =(method 'eth_getFilterLogs')
      ~&  [%filter-logs latest-block eth-filter]
      ?~  eth-filter
        ~|(%no-filter-not-implemented !!)
      :+  |
        %+  answer-request  req
        ~|  [eth-filter latest-block]
        (logs-to-json from-block.u.eth-filter latest-block)
      =.  last-block.u.eth-filter  latest-block
      [%cont ..stay]
    ?:  =(method 'eth_getFilterChanges')
      ~&  [%filter-changes latest-block eth-filter]
      ?~  eth-filter
        ~|(%no-filter-not-implemented !!)
      :+  |
        %+  answer-request  req
        (logs-to-json last-block.u.eth-filter latest-block)
      =.  last-block.u.eth-filter  latest-block
      [%cont ..stay]
    [& ~ %wait ~]
    ::
    ++  latest-block
      (add launch:contracts:azimuth (lent logs))
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
          //http/0v1n.2m9vh
          %they
          num.u.thus
          [200 ~ `(as-octs:mimes:html resp)]
      ==
    ::
    ++  logs-to-json
      |=  [from-block=@ud to-block=@ud]
      ^-  json
      :-  %a
      =/  selected-logs
        %+  swag
          [(sub from-block launch:contracts:azimuth) (sub to-block from-block)]
        logs
      =/  count  from-block
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
          (crip (prefix-hex:ethereum (render-hex-bytes:ethereum 32 `@`0x5363)))
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
++  spawn-galaxy
  |=  who=@p
  %-  add-logs
  :~  [~[activated who] '']
      [~[owner-changed who 0xdead.beef] '']
      :-  ~[changed-keys who]
      %-  crip
      %-  prefix-hex:ethereum
      ;:  welp
        (get-keys who 1 %auth)
        (get-keys who 1 %crypt)
        (render-hex-bytes:ethereum 32 `@`1)
        (render-hex-bytes:ethereum 32 `@`1)
      ==
  ==
::
++  get-keys
  |=  [who=@p life=@ud typ=?(%auth %crypt)]
  %+  render-hex-bytes:ethereum  32
  %-  keccak-256:keccak:crypto
  %-  as-octs:mimes:html
  :((cury cat 3) (scot %p who) (scot %ud life) typ)
::
::  XX replace
::
++  legacy
  |%
  ++  dawn
    |=  who=ship
    ^-  dawn-event
    :*  (need (private-key who))
        (^sein:title who)
        czar
        ~[~['arvo' 'netw' 'ork']]
        0
        `(need (de-purl:html 'http://localhost:8545'))
        ~
    ==
  ::
  ++  czar
    ^-  (map ship [life pass])
    %-  my
    ^-  (list (pair ship [life pass]))
    %+  murn  (gulf 0x0 0xff)
    |=  her=ship
    ^-  (unit [ship life pass])
    =/  pub  (public-key her)
    ?~  pub
      ~
    `[her u.pub]
  ::
  ++  private-key
    |=  who=ship
    =-  (~(get by -) who)
    ^-  (map ship seed:able:jael)
    %-  my
    :~  [~bud ~bud 1 'BbudB' ~]
        [~dev ~dev 1 'Bdev' ~]
    ==
  ::
  ++  public-key
    |=  who=ship
    ^-  (unit [life pass])
    =/  priv  (private-key who)
    ?~  priv
      ~
    =/  cub  (nol:nu:crub:crypto key.u.priv)
    `[lyf.u.priv pub:ex:cub]
  --
--
