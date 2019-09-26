/+  libthread=thread
=,  thread=thread:libthread
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
    ;<  =vase  bind:m  (handle-poke %echo)
    =/  message=tape  !<(tape vase)
    %-  (slog leaf/"{message}..." ~)
    ;<  ~      bind:m  (sleep ~s2)
    %-  (slog leaf/"{message}.." ~)
    (pure:m ~)
  ::
    ;<  =vase  bind:m  (handle-poke %over)
    %-  (slog leaf/"over..." ~)
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
    `[%done ~]
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
::  Queue on skip, try next on fail %ignore
::
++  main-loop
  |*  a=mold
  =/  m  (thread ,a)
  =|  queue=(qeu (unit input:thread))
  =|  active=(unit [?(%one %two) =form:m])
  |=  [one=form:m two=form:m]
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
    =.  active  `one+one
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
          %done  (continue(active ~) bowl.tin)
          %fail
        ?:  &(?=(%one -.u.active) ?=(%ignore p.err.next.res))
          $(active `two+two)
        `[%fail err.next.res]
      ==
    [(weld cards.res cards.output) next.output]
  --
--
