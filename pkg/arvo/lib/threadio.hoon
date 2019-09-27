/+  libthread=thread
=,  thread=thread:libthread
=,  thread-fail=thread-fail:libthread
|%
++  send-raw-card
  |=  =card:agent:mall
  =/  m  (thread ,~)
  ^-  form:m
  |=  thread-input:thread
  [[card ~] %done ~]
::
++  get-time
  =/  m  (thread ,@da)
  ^-  form:m
  |=  tin=thread-input:thread
  `[%done now.bowl.tin]
::
++  handle-poke
  |=  =mark
  =/  m  (thread ,vase)
  ^-  form:m
  |=  tin=thread-input:thread
  ?+  in.tin  `[%fail %ignore ~]
      ~  `[%wait ~]
      [~ %poke @ *]
    ?.  =(mark p.cage.u.in.tin)
      `[%fail %ignore ~]
    `[%done q.cage.u.in.tin]
  ==
::
++  take-poke
  |=  =mark
  =/  m  (thread ,vase)
  ^-  form:m
  |=  tin=thread-input:thread
  ?+  in.tin  `[%skip ~]
      ~  `[%wait ~]
      [~ %poke @ *]
    ?.  =(mark p.cage.u.in.tin)
      `[%skip ~]
    `[%done q.cage.u.in.tin]
  ==
::
++  echo
  =/  m  (thread ,~)
  ^-  form:m
  ;:  (main-loop ,~)
    |=  ~
    ^-  form:m
    ;<  =vase  bind:m  (handle-poke %echo)
    =/  message=tape  !<(tape vase)
    %-  (slog leaf+"{message}..." ~)
    ;<  ~      bind:m  (sleep ~s2)
    %-  (slog leaf+"{message}.." ~)
    (pure:m ~)
  ::
    |=  ~
    ^-  form:m
    ;<  =vase  bind:m  (handle-poke %over)
    %-  (slog leaf+"over..." ~)
    (pure:m ~)
  ==
::
++  take-wake
  |=  until=@da
  =/  m  (thread ,~)
  ^-  form:m
  |=  tin=thread-input:thread
  ?+  in.tin  `[%skip ~]
      ~  `[%wait ~]
      [~ %sign [%wait @ ~] %b %wake *]
    ?.  =(`until (slaw %da i.t.wire.u.in.tin))
      `[%skip ~]
    ?~  error.sign-arvo.u.in.tin
      `[%done ~]
    `[%fail %timer-error u.error.sign-arvo.u.in.tin]
  ==
::
++  wait
  |=  until=@da
  =/  m  (thread ,~)
  ^-  form:m
  =/  =card:agent:mall
    [%pass /wait/(scot %da until) %arvo %b %wait until]
  ;<  ~  bind:m  (send-raw-card card)
  (take-wake until)
::
++  sleep
  |=  for=@dr
  =/  m  (thread ,~)
  ^-  form:m
  ;<  now=@da  bind:m  get-time
  (wait (add now for))
::
++  set-timeout
  |*  computation-result=mold
  =/  m  (thread ,computation-result)
  |=  [time=@dr computation=form:m]
  ^-  form:m
  ;<  now=@da  bind:m  get-time
  =/  when  (add now time)
  =/  =card:agent:mall
    [%pass /timeout/(scot %da when) %arvo %b %wait when]
  ;<  ~        bind:m  (send-raw-card card)
  |=  tin=thread-input:thread
  =*  loop  $
  ?:  ?&  ?=([~ %sign [%timeout @ ~] %b %wake *] in.tin)
          =((scot %da when) i.t.wire.u.in.tin)
      ==
    `[%fail %timeout ~]
  =/  c-res  (computation tin)
  ?:  ?=(%cont -.next.c-res)
    c-res(self.next ..loop(computation self.next.c-res))
  ?:  ?=(%done -.next.c-res)
    =/  =card:agent:mall
      [%pass /timeout/(scot %da when) %arvo %b %rest when]
    c-res(cards [card cards.c-res])
  c-res
::
++  send-request
  |=  =request:http
  =/  m  (thread ,~)
  ^-  form:m
  (send-raw-card %pass /request %arvo %i %request request *outbound-config:iris)
::
++  take-client-response
  =/  m  (thread ,client-response:iris)
  ^-  form:m
  |=  tin=thread-input:thread
  ?+  in.tin  `[%skip ~]
      ~  `[%wait ~]
      [~ %sign [%request ~] %i %http-response %finished *]
    `[%done client-response.sign-arvo.u.in.tin]
  ==
::
++  extract-body
  |=  =client-response:iris
  =/  m  (thread ,cord)
  ^-  form:m
  ?>  ?=(%finished -.client-response)
  ?>  ?=(^ full-file.client-response)
  (pure:m q.data.u.full-file.client-response)
::
++  fetch-json
  |=  url=tape
  =/  m  (thread ,json)
  ^-  form:m
  =/  =request:http  [%'GET' (crip url) ~ ~]
  ;<  ~                      bind:m  (send-request request)
  ;<  =client-response:iris  bind:m  take-client-response
  ;<  =cord                  bind:m  (extract-body client-response)
  =/  json=(unit json)  (de-json:html cord)
  ?~  json
    (thread-fail %json-parse-error ~)
  (pure:m u.json)
::
::  Queue on skip, try next on fail %ignore
::
++  main-loop
  |*  a=mold
  =/  m  (thread ,~)
  =/  m-a  (thread ,a)
  =|  queue=(qeu (unit input:thread))
  =|  active=(unit [?(%one %two) =form:m-a])
  =|  state=a
  |=  [one=$-(a form:m-a) two=$-(a form:m-a)]
  ^-  form:m
  |=  tin=thread-input:thread
  =*  top  `form:m`..$
  =.  queue  (~(put to queue) in.tin)
  |^  (continue bowl.tin)
  ::
  ++  continue
    |=  =bowl:mall
    ^-  output:m
    ?>  =(~ active)
    ?:  =(~ queue)
      `[%cont top]
    =^  in=(unit input:thread)  queue  ~(get to queue)
    ^-  output:m
    =.  active  `one+(one state)
    ^-  output:m
    (run bowl in)
  ::
  ++  run
    ^-  form:m
    |=  tin=thread-input:thread
    ^-  output:m
    ?>  ?=(^ active)
    =/  res  (form.u.active tin)
    =/  =output:m
      ?-  -.next.res
          %wait  `[%wait ~]
          %skip  `[%cont ..$(queue (~(put to queue) in.tin))]
          %cont  `[%cont ..$(active `one+self.next.res)]
          %done  (continue(active ~, state value.next.res) bowl.tin)
          %fail
        ?:  &(?=(%one -.u.active) ?=(%ignore p.err.next.res))
          $(active `two+(two state))
        `[%fail err.next.res]
      ==
    [(weld cards.res cards.output) next.output]
  --
--
