/+  libthread=thread
=,  thread=thread:libthread
=,  thread-fail=thread-fail:libthread
|%
++  send-raw-cards
  |=  cards=(list =card:agent:mall)
  =/  m  (thread ,~)
  ^-  form:m
  |=  thread-input:thread
  [cards %done ~]
::
++  send-raw-card
  |=  =card:agent:mall
  =/  m  (thread ,~)
  ^-  form:m
  (send-raw-cards card ~)
::
++  ignore
  |=  tin=thread-input:thread
  `[%fail %ignore ~]
::
++  get-bowl
  =/  m  (thread ,bowl:mall)
  ^-  form:m
  |=  tin=thread-input:thread
  `[%done bowl.tin]
::
++  get-time
  =/  m  (thread ,@da)
  ^-  form:m
  |=  tin=thread-input:thread
  `[%done now.bowl.tin]
::
++  get-our
  =/  m  (thread ,ship)
  ^-  form:m
  |=  tin=thread-input:thread
  `[%done our.bowl.tin]
::
++  get-entropy
  =/  m  (thread ,@uvJ)
  ^-  form:m
  |=  tin=thread-input:thread
  `[%done eny.bowl.tin]
::
::  Convert skips to %ignore failures.
::
::    This tells the main loop to try the next handler.
::
++  handle
  |*  a=mold
  =/  m  (thread ,a)
  |=  =form:m
  ^-  form:m
  |=  tin=thread-input:thread
  =/  res  (form tin)
  =?  next.res  ?=(%skip -.next.res)
    [%fail %ignore ~]
  res
::
::  Wait for a poke with a particular mark
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
::
::
++  take-sign-arvo
  =/  m  (thread ,[wire sign-arvo])
  ^-  form:m
  |=  tin=thread-input:thread
  ?+  in.tin  `[%skip ~]
      ~  `[%wait ~]
      [~ %sign *]
    `[%done [wire sign-arvo]:u.in.tin]
  ==
::
::  Wait for a subscription update on a wire
::
++  take-subscription-update
  |=  =wire
  =/  m  (thread ,cage)
  ^-  form:m
  |=  tin=thread-input:thread
  ?+  in.tin  `[%skip ~]
      ~  `[%wait ~]
      [~ %agent * %subscription-update *]
    ?.  =(subscribe+wire wire.u.in.tin)
      `[%skip ~]
    `[%done cage.gift.u.in.tin]
  ==
::
++  echo
  =/  m  (thread ,~)
  ^-  form:m
  %-  (main-loop ,~)
  :~  |=  ~
      ^-  form:m
      ;<  =vase  bind:m  ((handle ,vase) (take-poke %echo))
      =/  message=tape  !<(tape vase)
      %-  (slog leaf+"{message}..." ~)
      ;<  ~      bind:m  (sleep ~s2)
      %-  (slog leaf+"{message}.." ~)
      (pure:m ~)
  ::
      |=  ~
      ^-  form:m
      ;<  =vase  bind:m  ((handle ,vase) (take-poke %over))
      %-  (slog leaf+"over..." ~)
      (pure:m ~)
  ==
::
++  take-subscribe
  =/  m  (thread ,path)
  |=  tin=thread-input:thread
  ?+  in.tin  `[%skip ~]
      ~  `[%wait ~]
      [~ %subscribe *]
    `[%done path.u.in.tin]
  ==
::
++  take-wake
  |=  until=(unit @da)
  =/  m  (thread ,~)
  ^-  form:m
  |=  tin=thread-input:thread
  ?+  in.tin  `[%skip ~]
      ~  `[%wait ~]
      [~ %sign [%wait @ ~] %b %wake *]
    ?.  |(?=(~ until) =(`u.until (slaw %da i.t.wire.u.in.tin)))
      `[%skip ~]
    ?~  error.sign-arvo.u.in.tin
      `[%done ~]
    `[%fail %timer-error u.error.sign-arvo.u.in.tin]
  ==
::
++  take-poke-ack
  |=  =wire
  =/  m  (thread ,~)
  ^-  form:m
  |=  tin=thread-input:thread
  ?+  in.tin  `[%skip ~]
      ~  `[%wait ~]
      [~ %agent * %poke-ack *]
    ?.  =(wire wire.u.in.tin)
      `[%skip ~]
    ?~  p.gift.u.in.tin
      `[%done ~]
    `[%fail %poke-fail u.p.gift.u.in.tin]
  ==
::
++  take-subscription-ack
  |=  =wire
  =/  m  (thread ,~)
  ^-  form:m
  |=  tin=thread-input:thread
  ?+  in.tin  `[%skip ~]
      ~  `[%wait ~]
      [~ %agent * %subscription-ack *]
    ?.  =(subscribe+wire wire.u.in.tin)
      `[%skip ~]
    ?~  p.gift.u.in.tin
      `[%done ~]
    `[%fail %subscription-ack-fail u.p.gift.u.in.tin]
  ==
::
++  poke
  |=  [=dock =cage]
  =/  m  (thread ,~)
  ^-  form:m
  =/  =card:agent:mall  [%pass /poke %agent dock %poke cage]
  ;<  ~  bind:m  (send-raw-card card)
  (take-poke-ack /poke)
::
++  poke-our
  |=  [=term =cage]
  =/  m  (thread ,~)
  ^-  form:m
  ;<  our=@p  bind:m  get-our
  (poke [our term] cage)
::
++  subscribe
  |=  [=wire =dock =path]
  =/  m  (thread ,~)
  ^-  form:m
  =/  =card:agent:mall  [%pass subscribe+wire %agent dock %subscribe path]
  ;<  ~  bind:m  (send-raw-card card)
  (take-subscription-ack wire)
::
++  subscribe-our
  |=  [=wire =term =path]
  =/  m  (thread ,~)
  ^-  form:m
  ;<  our=@p  bind:m  get-our
  (subscribe wire [our term] path)
::
++  unsubscribe
  |=  [=wire =dock]
  =/  m  (thread ,~)
  ^-  form:m
  =/  =card:agent:mall  [%pass subscribe+wire %agent dock %unsubscribe ~]
  (send-raw-card card)
::
++  unsubscribe-our
  |=  [=wire =term]
  =/  m  (thread ,~)
  ^-  form:m
  ;<  our=@p  bind:m  get-our
  (unsubscribe wire [our term])
::
++  wait
  |=  until=@da
  =/  m  (thread ,~)
  ^-  form:m
  ;<  ~  bind:m  (send-wait until)
  (take-wake `until)
::
++  sleep
  |=  for=@dr
  =/  m  (thread ,~)
  ^-  form:m
  ;<  now=@da  bind:m  get-time
  (wait (add now for))
::
++  send-wait
  |=  until=@da
  =/  m  (thread ,~)
  ^-  form:m
  =/  =card:agent:mall
    [%pass /wait/(scot %da until) %arvo %b %wait until]
  (send-raw-card card)
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
++  send-cancel-request
  =/  m  (thread ,~)
  ^-  form:m
  (send-raw-card %pass /request %arvo %i %cancel-request ~)
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
++  take-maybe-response
  =/  m  (thread ,(unit client-response:iris))
  ^-  form:m
  |=  tin=thread-input:thread
  ?+  in.tin  `[%skip ~]
      ~  `[%wait ~]
      [~ %sign [%request ~] %i %http-response %cancel *]
    `[%done ~]
      [~ %sign [%request ~] %i %http-response %finished *]
    `[%done `client-response.sign-arvo.u.in.tin]
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
  =|  active=(unit [in=(unit input:thread) =form:m-a forms=(list $-(a form:m-a))])
  =|  state=a
  |=  forms=(lest $-(a form:m-a))
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
    =.  active  `[in (i.forms state) t.forms]
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
          %cont  `[%cont ..$(active `[in.u.active self.next.res forms.u.active])]
          %done  (continue(active ~, state value.next.res) bowl.tin)
          %fail
        ?:  &(?=(^ forms.u.active) ?=(%ignore p.err.next.res))
          %=  $
            active  `[in.u.active (i.forms.u.active state) t.forms.u.active]
            in.tin  in.u.active
          ==
        `[%fail err.next.res]
      ==
    [(weld cards.res cards.output) next.output]
  --
--
