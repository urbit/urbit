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
  ;<  =vase  bind:m  (take-poke %echo)
  =/  message=tape  !<(tape vase)
  %-  (slog leaf/"{message}..." ~)
  ;<  ~      bind:m  (sleep ~s2)
  %-  (slog leaf/"{message}.." ~)
  echo
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
--
