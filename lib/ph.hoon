::
::::  /hoon/ph/lib
  ::
/-  aquarium
=,  aquarium
=>  .
|%
+$  ph-input
  [who=ship uf=unix-effect]
::
++  ph
  |*  a=mold
  |%
  ++  ph-output  (ph-output-raw a)
  ++  ph-output-raw
    |*  a=mold
    $~  [& ~ %done *a]
    $:  thru=?
        events=(list ph-event)
        $=  next
        $%  [%wait ~]
            [%cont self=(data-raw a)]
            [%fail ~]
            [%done value=a]
        ==
    ==
  ::
  ++  data  (data-raw a)
  ++  data-raw
    |*  a=mold
    $-(ph-input (ph-output-raw a))
  ::
  ++  return
    |=  arg=a
    ^-  data
    |=  ph-input
    [& ~ %done arg]
  ::
  ++  bind
    |*  b=mold
    |=  [m-b=(data-raw b) fun=$-(b data)]
    ^-  data
    |=  input=ph-input
    =/  b-res=(ph-output-raw b)
      (m-b input)
    ^-  ph-output
    :+  thru.b-res  events.b-res
    ?-    -.next.b-res
      %wait  [%wait ~]
      %cont  [%cont ..$(m-b self.next.b-res)]
      %fail  [%fail ~]
      %done  [%cont (fun value.next.b-res)]
    ==
  --
::
++  m-test-lib
  |%
  ++  stall
    |=  ph-input
    [& ~ %wait ~]
  ::
  ++  boot-ship
    |=  [her=ship keys=(unit dawn-event)]
    ^+  *data:(ph ,~)
    |=  ph-input
    ~&  %first-i
    [& (init her keys) %done ~]
  ::
  ++  check-ship-booted
    |=  her=ship
    ^+  *data:(ph ,~)
    |=  ph-input
    =;  done=?
      ~&  [%second-i done]
      :+  &  ~
      ?:  done
        [%done ~]
      [%wait ~]
    ::  This is a pretty bad heuristic, but in general galaxies will
    ::  hit the first of these cases, and other ships will hit the
    ::  second.
    ::
    ?|
      %^  is-dojo-output  her  who  :-  uf
      "+ /{(scow %p her)}/base/2/web/testing/udon"
    ::
      %^  is-dojo-output  her  who  :-  uf
      "is your neighbor"
    ==
  ::
  ++  send-hi
    |=  [from=@p to=@p]
    =/  m  (ph ,~)
    ^-  data:m
    ;<  ~  bind:m
      ^-  data:m
      |=  ph-input
      [& (dojo from "|hi {(scow %p to)}") %done ~]
    ^-  data:m
    |=  input=ph-input
    ^-  ph-output:m
    :+  &  ~
    ?.  (is-dojo-output from who.input uf.input "hi {(scow %p to)} successful")
      [%wait ~]
    [%done ~]
  ::
  ++  raw-ship
    |=  [her=ship keys=(unit dawn-event)]
    =/  m  (ph ,~)
    ^-  data:m
    ;<  ~  bind:m  (boot-ship her keys)
    ;<  ~  bind:m  (check-ship-booted her)
    (return:m ~)
  --
::
++  philter
  |*  o=mold
  |%
  ++  output
    $~  [& ~ %wait ~]
    $:  thru=?
        events=(list ph-event)
        $=  next
        $%  [%wait ~]
            [%cont self=data]
        ==
    ==
  ++  data
    $_  ^?
    |%
    ++  stay  *o
    ++  run   |~(ph-input *output)
    --
  --
::
++  wrap-filter
  |*  [o=mold i=mold]
  |=  [outer=_*data:(philter o) inner=_*data:(ph i)]
  ^+  *data:(ph ,[o i])
  |=  input=ph-input
  =/  res-i=_*ph-output:(ph i)
    (inner input)
  ?.  thru.res-i
    :+  thru.res-i  events.res-i
    ?-  -.next.res-i
      %wait  [%wait ~]
      %cont  [%cont ..$(inner self.next.res-i)]
      %fail  [%fail ~]
      %done  [%done stay:outer value.next.res-i]
    ==
  =/  res-o=_*output:(philter o)
    (run:outer input)
  ^+  *ph-output:(ph ,[o i])
  :+  thru.res-o  (welp events.res-i events.res-o)
  ?-    -.next.res-i
      %wait
  ^+  +>:*ph-output:(ph ,[o i])
    ?-  -.next.res-o
      %wait  [%wait ~]
      %cont  [%cont ..$(outer self.next.res-o)]
    ==
  ::
      %cont
  ^+  +>:*ph-output:(ph ,[o i])
    =.  inner  self.next.res-i
    ?-  -.next.res-o
      %wait  [%cont ..$]
      %cont  [%cont ..$(outer self.next.res-o)]
    ==
  ::
      %fail  [%fail ~]
      %done
  ^+  +>:*ph-output:(ph ,[o i])
    ?-  -.next.res-o
      %wait  [%done stay:outer value.next.res-i]
      %cont  [%done stay:self.next.res-o value.next.res-i]
    ==
  ==
::
::  Defines a complete integration test.
::
++  raw-test-core
  $_  ^|
  |_  now=@da
  ::
  ::  Unique name, used as a cache label.
  ::
  ++  label  *@ta
  ::
  ::  List of ships that are part of the test.
  ::
  ::    We'll only hear effects from these ships, and only these will
  ::    be in the cache points.
  ::
  ++  ships  *(list ship)
  ::
  ::  Called first to kick off the test.
  ::
  ++  start  *(quip ph-event _^|(..start))
  ::
  ::  Called on every effect from a ship.
  ::
  ::    The loobean in the return value says whether we should pass on
  ::    the effect to vane drivers.  Usually this should be yes.
  ::
  ++  route  |~([ship unix-effect] *[? (quip ph-event _^|(..start))])
  --
::
::  A simpler interface for when you don't need all the power.
::
::    Doesn't allwow you to explicitly subscribe to certain ships or
::    blocking certain effects from going to their usual vane drivers.
::
::    Use with +porcelain-test
::
++  porcelain-test-core
  $_  ^|
  |_  now=@da
  ::  Called first to kick off the test.
  ::
  ++  start  *(quip ph-event _^|(..start))
  ::
  ::  Called on every effect from a ship.
  ::
  ++  route  |~([ship unix-effect] *(quip ph-event _^|(..start)))
  --
::
::  A simpler interface for when you don't need test state.
::
::    Use with +stateless-test
::
++  stateless-test-core
  $_  ^|
  |_  now=@da
  ::  Called first to kick off the test.
  ::
  ++  start  *(list ph-event)
  ::
  ::  Called on every effect from a ship.
  ::
  ++  route  |~([ship unix-effect] *(list ph-event))
  --
::
++  ph-event
  $%  [%test-done p=?]
      aqua-event
  ==
::
::  Call with a +porecelain-test-core create a stateless test.
::
++  porcelain-test
  |=  [label=@ta porcelain=porcelain-test-core]
  ^-  raw-test-core
  |_  now=@da
  ++  label  ^label
  ++  ships  ~
  ++  start
    =^  events  porcelain  ~(start porcelain now)
    [events ..start]
  ::
  ++  route
    |=  args=[ship unix-effect]
    =^  events  porcelain  (~(route porcelain now) args)
    [& events ..start]
  --
::
::  Call with a +stateless-test-core create a stateless test.
::
++  stateless-test
  |=  [label=@tas stateless=stateless-test-core]
  %+  porcelain-test
    label
  ^-  porcelain-test-core
  |_  now=@da
  ++  start
    [~(start stateless now) ..start]
  ::
  ++  route
    |=  args=[ship unix-effect]
    [(~(route stateless now) args) ..start]
  --
::
::  Turn [ship (list unix-event)] into (list ph-event)
::
++  send-events-to
  |=  [who=ship what=(list unix-event)]
  ^-  (list ph-event)
  %+  turn  what
  |=  ue=unix-event
  [%event who ue]
::
::  Start a ship (low-level; prefer +raw-ship)
::
++  init
  |=  [who=ship keys=(unit dawn-event)]
  ^-  (list ph-event)
  [%init-ship who keys]~
::
::  Send dojo command
::
++  dojo
  |=  [who=ship what=tape]
  ^-  (list ph-event)
  %+  send-events-to  who
  ^-  (list unix-event)
  :~
    [//term/1 %belt %ctl `@c`%e]
    [//term/1 %belt %ctl `@c`%u]
    [//term/1 %belt %txt ((list @c) what)]
    [//term/1 %belt %ret ~]
  ==
::
::  Inject a file into a ship
::
++  insert-file
  |=  [who=ship des=desk pax=path txt=@t]
  ^-  (list ph-event)
  ?>  ?=([@ @ @ *] pax)
  =/  file  [/text/plain (as-octs:mimes:html txt)]
  %+  send-events-to  who
  :~
    [//sync/0v1n.2m9vh %into des | [t.t.t.pax `file]~]
  ==
::
::  Checks whether the given event is a dojo output blit containing the
::  given tape
::
++  is-dojo-output
  |=  [who=ship her=ship uf=unix-effect what=tape]
  ?&  =(who her)
      ?=(%blit -.q.uf)
    ::
      %+  lien  p.q.uf
      |=  =blit:dill
      ?.  ?=(%lin -.blit)
        |
      !=(~ (find what p.blit))
  ==
::
::  Test is successful if +is-dojo-output
::
++  expect-dojo-output
  |=  [who=ship her=ship uf=unix-effect what=tape]
  ^-  (list ph-event)
  ?.  (is-dojo-output who her uf what)
    ~
  [%test-done &]~
::
::  Check whether the given event is an ergo
::
++  is-ergo
  |=  [who=ship her=ship uf=unix-effect]
  ?&  =(who her)
      ?=(%ergo -.q.uf)
  ==
::
::  Check if given effect is an http request; extract
::
++  extract-thus-to
  |=  [uf=unix-effect dest=@t]
  ^-  (unit [num=@ud mot=moth:eyre])
  ?.  ?=(%thus -.q.uf)  ~
  ?~  q.q.uf  ~
  ?.  =(p.u.q.q.uf (rash dest auri:de-purl:html))  ~
  `[p.q.uf q.u.q.q.uf]
::
+$  az-log  [topics=(lest @) data=@t]
++  az
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
    ^+  *data:n
    |%
    ++  stay  this-az
    ++  run
      |=  [who=ship uf=unix-effect]
      ^-  output:n
      =,  enjs:format
      =/  thus  (extract-thus-to uf 'http://localhost:8545')
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
        s+(crip (num-to-hex:ethereum (lent logs)))
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
        ~&  [%filter-logs (lent logs) eth-filter]
        ?~  eth-filter
          ~|(%no-filter-not-implemented !!)
        :+  |
          %+  answer-request  req
          (logs-to-json from-block.u.eth-filter (lent logs))
        =.  last-block.u.eth-filter  (lent logs)
        [%cont ..stay]
      ?:  =(method 'eth_getFilterChanges')
        ~&  [%filter-changes (lent logs) eth-filter]
        ?~  eth-filter
          ~|(%no-filter-not-implemented !!)
        :+  |  
          %+  answer-request  req
          (logs-to-json last-block.u.eth-filter (lent logs))
        =.  last-block.u.eth-filter  (lent logs)
        [%cont ..stay]
      [& ~ %wait ~]
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
            who
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
            [from-block (sub to-block from-block)]
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
  --
::
++  ph-azimuth
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
::
++  test-lib
  |_  our=ship
  ::
  ::  Run one test, then the next.
  ::
  ::    Caches the result of the first test.
  ::
  ++  compose-tests
    |=  [a=raw-test-core b=raw-test-core]
    ^-  raw-test-core
    =/  done-with-a  |
    =>
      |%
      ++  filter-a
        |=  [now=@da events=(list ph-event)]
        ^-  (quip ph-event _..filter-a)
        =+  ^-  [done=(list ph-event) other-events=(list ph-event)]
          %+  skid  events
          |=  e=ph-event
          =(%test-done -.e)
        ?~  done
          [other-events ..filter-a]
        ?>  ?=(%test-done -.i.done)
        ?.  p.i.done
          [[%test-done |]~ ..filter-a]
        =.  done-with-a  &
        =/  snap-event  [%snap-ships label:a ships:a]
        =^  events-start  b  ~(start b now)
        [(welp other-events [snap-event events-start]) ..filter-a]
      --
    |_  now=@da
    ::
    ::  Cache lookup label
    ::
    ++  label  `@tas`:((cury cat 3) label:a '--' label:b)
    ::
    ::  Union of ships in a and b
    ::
    ++  ships  ~(tap in (~(uni in (silt ships.a)) (silt ships.b)))
    ::
    ::  Start with start of a
    ::
    ++  start
      ^-  (quip ph-event _..start)
      =/  have-cache
        (scry-aqua ? now /fleet-snap/[label:a]/noun)
      ::  ?:  have-cache
      ::    ~&  [%caching-in label:a label]
      ::    =.  done-with-a  &
      ::    =/  restore-event  [%restore-snap label:a]
      ::    =^  events-start  b  ~(start b now)
      ::    =^  events  ..filter-a  (filter-a now restore-event events-start)
      ::    [events ..start]
      =^  events  a  ~(start a now)
      [events ..start]
    ::
    ::  Keep going on a until it's done.  If success, go to b.
    ::
    ::    In theory, we should be able to just swap out the whole core
    ::    for b, but in practice the types are hard, and we generally
    ::    try to avoid changing the structure of a core in the middle
    ::    like that.
    ::
    ++  route
      |=  [who=ship uf=unix-effect]
      ^-  [? (quip ph-event _..start)]
      ?:  done-with-a
        =+  ^-  [thru=? events=(list ph-event) cor=raw-test-core]
            (~(route b now) who uf)
        =.  b  cor
        [thru events ..start]
      =+  ^-  [thru=? events=(list ph-event) cor=raw-test-core]
          (~(route a now) who uf)
      =.  a  cor
      =^  events  ..filter-a  (filter-a now events)
      [thru events ..start]
    --
  ::
  ::  Wrap a test with an effect filter.
  ::
  ::    This allows intercepting particular effects for special
  ::    handling.
  ::
  ++  wrap-test
    |=  $:  lab=@ta
            filter=$-([ship unix-effect] [thru=? pe=(list ph-event)])
            cor=raw-test-core
        ==
    ^-  raw-test-core
    |_  now=@da
    ++  label  :((cury cat 3) label:cor '--w--' lab)
    ++  ships  ships:cor
    ++  start
      =^  events  cor  ~(start cor now)
      [events ..start]
    ::
    ++  route
      |=  [who=ship uf=unix-effect]
      ^-  [? (quip ph-event _^|(..start))]
      =+  ^-  [thru-test=? events-test=(list ph-event) cor-test=_cor]
        (~(route cor now) who uf)
      =.  cor  cor-test
      ?.  thru-test
        [| events-test ..start]
      =+  ^-  [thru-filter=? events-filter=(list ph-event)]
        (filter who uf)
      [thru-filter (weld events-test events-filter) ..start]
    --
  ::
  ::  Wrap a test with an effect filter.
  ::
  ::    This allows intercepting particular effects for special
  ::    handling.
  ::
  ++  wrap-test-stateful
    |=  $:  lab=@ta
          ::
            $=  filter
            $_  |~  [ship unix-effect]
            *[thru=? pe=(list ph-event) self=_^|(..$)]
          ::
            cor=raw-test-core
        ==
    ^-  raw-test-core
    |_  now=@da
    ++  label  :((cury cat 3) label:cor '--ws--' lab)
    ++  ships  ships:cor
    ++  start
      =^  events  cor  ~(start cor now)
      [events ..start]
    ::
    ++  route
      |=  [who=ship uf=unix-effect]
      ^-  [? (quip ph-event _^|(..start))]
      =+  ^-  [thru-test=? events-test=(list ph-event) cor-test=_cor]
        (~(route cor now) who uf)
      =.  cor  cor-test
      ?.  thru-test
        [| events-test ..start]
      =+  ^-  res=[thru=? events=(list ph-event) filter=_filter]
        (filter who uf)
      =.  filter  filter.res
      [thru.res (weld events-test events.res) ..start]
    --
  ::
  ::  Mock HTTP responses to particular requests
  ::
  ++  wrap-test-http
    |=  [url=@t responses=(list $-(@t (unit @t))) cor=raw-test-core]
    %^    wrap-test
        (cat 3 'http-' (scot %uw (mug url responses)))
      |=  [who=ship uf=unix-effect]
      ^-  [? (list ph-event)]
      =/  thus  (extract-thus-to uf url)
      ?~  thus
        [& ~]
      ?~  r.mot.u.thus
        [& ~]
      |-  ^-  [? (list ph-event)]
      ?~  responses
        [& ~]
      =/  resp  (i.responses q.u.r.mot.u.thus)
      ?~  resp
        $(responses t.responses)
      :-  |  :_  ~
      :*  %event
          who
          //http/0v1n.2m9vh
          %they
          num.u.thus
          [200 ~ `(as-octs:mimes:html u.resp)]
      ==
    cor
  ::
  ::  Don't use directly unless you've already started any parent.
  ::
  ::    Consider ++galaxy, ++star, ++planet, and ++ship-with-ancestors.
  ::
  ++  raw-ship
    |=  [her=ship keys=(unit dawn-event)]
    ^-  raw-test-core
    |_  now=@da
    ++  label  :((cury cat 3) 'init-' (scot %p her) '-' (scot %uw (mug (fall keys *dawn-event))))
    ++  ships  ~[her]
    ++  start
      ^-  (quip ph-event _..start)
      [(init her keys) ..start]
    ::
    ++  route
      |=  [who=ship uf=unix-effect]
      ^-  [? (quip ph-event _..start)]
      :-  &
      :_  ..start
      %-  zing
      ::  This is a pretty bad heuristic, but in general galaxies will
      ::  hit the first of these cases, and other ships will hit the
      ::  second.
      ::
      :~
        ?.  %^  is-dojo-output  her  who  :-  uf
            "+ /{(scow %p her)}/base/2/web/testing/udon"
          ~
        [%test-done &]~
      ::
        ?.  %^  is-dojo-output  her  who  :-  uf
            "is your neighbor"
          ~
        [%test-done &]~
      ==
    --
  ::
  ++  galaxy
    |=  her=ship
    ?>  =(%czar (clan:title her))
    (raw-ship her ~)
  ::
  ++  star
    |=  her=ship
    ?>  =(%king (clan:title her))
    %+  compose-tests  (galaxy (^sein:title her))
    (raw-ship her ~)
  ::
  ++  planet
    |=  her=ship
    ?>  =(%duke (clan:title her))
    %+  compose-tests  (star (^sein:title her))
    (raw-ship her ~)
  ::
  ++  ship-with-ancestors
    |=  her=ship
    %.  her
    ?-  (clan:title her)
      %czar  galaxy
      %king  star
      %duke  planet
      %earl  ~|(%moon-not-implemented !!)
      %pawn  ~|(%comet-not-implemented !!)
    ==
  ::
  ::  Touches /sur/aquarium/hoon on the given ship.
  ::
  ++  touch-file
    |=  [her=ship des=desk]
    %+  porcelain-test
      (cat 3 'touch-file-' (scot %p her))
    =|  [warped=@t change-sent=_|]
    ^-  porcelain-test-core
    |_  now=@da
    ++  start
      ^-  (pair (list ph-event) _..start)
      :_  ..start
      (dojo her "|mount /={(trip des)}=")
    ::
    ++  route
      |=  [who=ship uf=unix-effect]
      ^-  (quip ph-event _..start)
      ?.  (is-ergo her who uf)
        `..start
      ?.  change-sent
        =/  host-pax
          /(scot %p our)/home/(scot %da now)/sur/aquarium/hoon
        =.  warped  (cat 3 '=>  .  ' .^(@t %cx host-pax))
        =.  change-sent  &
        [(insert-file her des host-pax warped) ..start]
      :_  ..start
      =/  pax  /i/(scot %p her)/[des]/(scot %da now)/sur/aquarium/hoon/noun
      ?:  =(warped (need (scry-aqua (unit @) now pax)))
        [%test-done &]~
      ~
    --
  ::
  ::  Check that /sur/aquarium/hoon has been touched, as by ++touch-file
  ::
  ++  check-file-touched
    |=  [her=ship des=desk]
    %+  stateless-test
      (cat 3 'check-file-touched-' (scot %p her))
    |_  now=@da
    ++  start
      ::  mounting is not strictly necessary since we check via scry,
      ::  but this way we don't have to check on every event, just
      ::  ergos (and dojo because we can't guarantee an ergo if the desk
      ::  is already mounted)
      ::
      (dojo her "|mount /={(trip des)}=")
    ::
    ++  route
      |=  [who=ship uf=unix-effect]
      ^-  (list ph-event)
      ?.  ?|  (is-ergo her who uf)
              (is-dojo-output her who uf ">=")
          ==
        ~
      =/  pax  /home/(scot %da now)/sur/aquarium/hoon
      =/  warped  (cat 3 '=>  .  ' .^(@t %cx (weld /(scot %p our) pax)))
      =/  aqua-pax
        ;:  weld
            /i/(scot %p her)
            pax(- des)
            /noun
        ==
      ?:  =(warped (need (scry-aqua (unit @) now aqua-pax)))
        [%test-done &]~
      ~
    --
  ::
  ::  Reload vane from filesystem
  ::
  ++  reload-vane
    |=  [her=ship vane=term]
    %+  stateless-test
      :((cury cat 3) 'reload-vane-' (scot %p her) '-' vane)
    |_  now=@da
    ++  start
      ^-  (list ph-event)
      =/  pax
        /(scot %p our)/home/(scot %da now)/sys/vane/[vane]/hoon
      %-  zing
      :~  (dojo her "|mount /=home=")
          (insert-file her %home pax .^(@t %cx pax))
          [%test-done &]~
      ==
    ::
    ++  route
      |=  [who=ship uf=unix-effect]
      ~
    --
  ::
  ::  Send hi from one ship to another
  ::
  ++  send-hi
    |=  [from=@p to=@p]
    %+  stateless-test
      :((cury cat 3) 'hi-' (scot %p from) '-' (scot %p to))
    |_  now=@da
    ++  start
      (dojo from "|hi {(scow %p to)}")
    ::
    ++  route
      |=  [who=ship uf=unix-effect]
      (expect-dojo-output from who uf "hi {(scow %p to)} successful")
    --
  ::
  ::  Scry into a running aqua ship
  ::
  ++  scry-aqua
    |*  [a=mold now=@da pax=path]
    .^  a
        %gx
        (scot %p our)
        %aqua
        (scot %da now)
        pax
    ==
  --
--
