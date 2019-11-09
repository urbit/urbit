::  delay incoming http requests until eyre is subscribed to responses.
::
|=  =agent:mall
=|  state=[count=@ud map=(map app-id=@ud inbound-request:eyre)]
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
  =/  app-id  count.state
  =:  count.state  +(count.state)
      map.state    (~(put by map.state) app-id inbound-request)
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
  =/  request  (~(get by map.state) app-id)
  =.  map.state  (~(del by map.state) app-id)
  ?~  request
    :_  this
    ^-  (list card:agent:mall)
    :~  [%give %fact `path %http-response-cancel !>(~)]
        [%give %kick `path ~]
    ==
  =^  cards  agent
    (on-poke:ag %http-request !>([path u.request]))
  [cards this]
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
