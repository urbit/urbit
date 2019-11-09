::  wrap an http handler without having to worry about subscriptions
::
|%
+$  response  simple-payload:http
+$  handler  $-(inbound-request:eyre response)
--
|=  [=handler =agent:mall]
=|  state=[count=@ud map=(map app-id=@ud response)]
^-  agent:mall
|_  =bowl:mall
+*  this  .
    ag    ~(. agent bowl)
::
++  on-init
  ^-  (quip card:agent:mall agent:mall)
  =^  cards  agent  on-init:ag
  [cards this]
::
++  on-save
  ^-  vase
  !>([on-save:ag state])
::
++  on-load
  |=  old-state=vase
  ^-  (quip card:agent:mall agent:mall)
  =^  old  state  !<([vase _state] old-state)
  =^  cards  agent  (on-load:ag old)
  [cards this]
::
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card:agent:mall agent:mall)
  ?.  ?=(%handle-http-request mark)
    =^  cards  agent  (on-poke:ag mark vase)
    [cards this]
  =+  !<([eyre-id=@ud =inbound-request:eyre] vase)
  =/  response  (handler inbound-request)
  =/  app-id  count.state
  =:  count.state  +(count.state)
      map.state    (~(put by map.state) app-id response)
    ==
  :_  this  :_  ~
  [%pass / %arvo %e %start-watching eyre-id app-id]
::
++  on-watch
  |=  =path
  ^-  (quip card:agent:mall agent:mall)
  ?.  ?=([%http-response @ ~] path)
    =^  cards  agent  (on-watch:ag path)
    [cards this]
  =/  app-id  (slav %ud i.t.path)
  =/  response  (~(get by map.state) app-id)
  :_  this(map.state (~(del by map.state) app-id))
  ?~  response
    ^-  (list card:agent:mall)
    :~  [%give %fact `path %http-response-cancel !>(~)]
        [%give %kick `path ~]
    ==
  ^-  (list card:agent:mall)
  :~  [%give %fact `path %http-response-header !>(response-header.u.response)]
      [%give %fact `path %http-response-data !>(data.u.response)]
      [%give %kick `path ~]
  ==
::
++  on-leave
  |=  =path
  ^-  (quip card:agent:mall agent:mall)
  =^  cards  agent  (on-leave:ag path)
  [cards this]
::
++  on-peek
  |=  =path
  ^-  (unit (unit cage))
  (on-peek:ag path)
::
++  on-agent
  |=  [=wire =sign:agent:mall]
  ^-  (quip card:agent:mall agent:mall)
  =^  cards  agent  (on-agent:ag wire sign)
  [cards this]
::
++  on-arvo
  |=  [=wire =sign-arvo]
  ^-  (quip card:agent:mall agent:mall)
  =^  cards  agent  (on-arvo:ag wire sign-arvo)
  [cards this]
::
++  on-fail
  |=  [=term =tang]
  ^-  (quip card:agent:mall agent:mall)
  =^  cards  agent  (on-fail:ag term tang)
  [cards this]
--
