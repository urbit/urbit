/-  aquarium, spider
/+  aqua-vane-thread
=,  aquarium
|%
+$  pier  next-timer=(unit @da)
--
::
=|  piers=(map ship pier)
::
|%
++  pe
  |=  [bowl:spider who=ship]
  =+  (~(gut by piers) who *pier)
  =*  pier-data  -
  =|  cards=(list card:agent:gall)
  |%
  ++  this  .
  ++  abet-pe
    ^-  (quip card:agent:gall _piers)
    =.  piers  (~(put by piers) who pier-data)
    [(flop cards) piers]
  ::
  ++  emit-cards
    |=  cs=(list card:agent:gall)
    %_(this cards (weld cs cards))
  ::
  ++  emit-aqua-events
    |=  aes=(list aqua-event)
    %-  emit-cards
    [%pass /aqua-events %agent [our %aqua] %poke %aqua-events !>(aes)]~
  ::
  ++  handle-sleep
    ^+  ..abet-pe
    =<  ..abet-pe(pier-data *pier)
    ?~  next-timer
      ..abet-pe
    cancel-timer
  ::
  ++  handle-restore
    ^+  ..abet-pe
    =.  this
      %-  emit-aqua-events
      [%event who [//behn/0v1n.2m9vh %born ~]]~
    ..abet-pe
  ::
  ++  handle-doze
    |=  [way=wire %doze tim=(unit @da)]
    ^+  ..abet-pe
    ?~  tim
      ?~  next-timer
        ..abet-pe
      cancel-timer
    ?~  next-timer
      (set-timer u.tim)
    (set-timer:cancel-timer u.tim)
  ::
  ++  set-timer
    |=  tim=@da
    ~?  debug=|  [who=who %setting-timer tim]
    =.  next-timer  `tim
    =.  this  (emit-cards [%pass /(scot %p who) %arvo %b %wait tim]~)
    ..abet-pe
  ::
  ++  cancel-timer
    ~?  debug=|  [who=who %cancell-timer (need next-timer)]
    =.  this
      (emit-cards [%pass /(scot %p who) %arvo %b %rest (need next-timer)]~)
    =.  next-timer  ~
    ..abet-pe
  ::
  ++  take-wake
    |=  [way=wire error=(unit tang)]
    ~?  debug=|  [who=who %aqua-behn-wake now error=error]
    =.  next-timer  ~
    =.  this
      %-  emit-aqua-events
      :_  ~
      ^-  aqua-event
      :+  %event  who
      :-  //behn/0v1n.2m9vh
      ?~  error
        [%wake ~]
      [%crud %fail u.error]
    ..abet-pe
  --
--
::
%+  aqua-vane-thread  ~[%sleep %restore %doze]
|_  =bowl:spider
+*  this  .
++  handle-unix-effect
  |=  [who=@p ue=unix-effect:aquarium]
  ^-  (quip card:agent:gall _this)
  =^  cards  piers
    ?+  -.q.ue  `piers
      %sleep    abet-pe:handle-sleep:(pe bowl who)
      %restore  abet-pe:handle-restore:(pe bowl who)
      %doze     abet-pe:(handle-doze:(pe bowl who) ue)
    ==
  [cards this]
::
++  handle-arvo-response
  |=  [=wire =sign-arvo]
  ?>  ?=([%b %wake *] sign-arvo)
  ?>  ?=([@ *] wire)
  =/  who  (,@p (slav %p i.wire))
  =^  cards  piers
    abet-pe:(take-wake:(pe bowl who) t.wire error.sign-arvo)
  [cards this]
--
