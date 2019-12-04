::  Pass-through Eyre driver
::
/-  aquarium, spider
/+  aqua-vane-thread
=,  aquarium
|%
+$  pier  http-requests=(set @ud)
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
    ..abet-pe(pier-data *pier)
  ::
  ++  handle-restore
    ^+  ..abet-pe
    =.  this
      %-  emit-aqua-events
      [%event who [//http/0v1n.2m9vh %born ~]]~
    ..abet-pe
  ::
  ++  handle-thus
    |=  [way=wire %thus num=@ud req=(unit hiss:eyre)]
    ^+  ..abet-pe
    ?~  req
      ?.  (~(has in http-requests) num)
        ..abet-pe
      ::  Eyre doesn't support cancelling HTTP requests from userspace,
      ::  so we remove it from our state so we won't pass along the
      ::  response.
      ::
      ~&  [who=who %aqua-eyre-cant-cancel-thus num=num]
      =.  http-requests  (~(del in http-requests) num)
      ..abet-pe
    ~&  [who=who %aqua-eyre-requesting u.req]
    =.  http-requests  (~(put in http-requests) num)
    =.  this
      %-  emit-cards  :_  ~
      :*  %pass
          /(scot %p who)/(scot %ud num)
          %arvo
          %i
          %request
          (hiss-to-request:html u.req)
          *outbound-config:iris
      ==
    ..abet-pe
  ::
  ::  Pass HTTP response back to virtual ship
  ::
  ++  take-sigh-httr
    |=  [way=wire res=httr:eyre]
    ^+  ..abet-pe
    ?>  ?=([@ ~] way)
    =/  num  (slav %ud i.way)
    ?.  (~(has in http-requests) num)
      ~&  [who=who %ignoring-httr num=num]
      ..abet-pe
    =.  http-requests  (~(del in http-requests) num)
    =.  this
      (emit-aqua-events [%event who [//http/0v1n.2m9vh %receive num [%start [p.res q.res] r.res &]]]~)
    ..abet-pe
  ::
  ::  Got error in HTTP response
  ::
  ++  take-sigh-tang
    |=  [way=wire tan=tang]
    ^+  ..abet-pe
    ?>  ?=([@ ~] way)
    =/  num  (slav %ud i.way)
    ?.  (~(has in http-requests) num)
      ~&  [who=who %ignoring-httr num=num]
      ..abet-pe
    =.  http-requests  (~(del in http-requests) num)
    %-  (slog tan)
    ..abet-pe
  --
--
::
%+  aqua-vane-thread  ~[%sleep %restore %thus]
|_  =bowl:spider
+*  this  .
++  handle-unix-effect
  |=  [who=@p ue=unix-effect:aquarium]
  ^-  (quip card:agent:gall _this)
  =^  cards  piers
    ?+  -.q.ue  `piers
      %sleep    abet-pe:handle-sleep:(pe bowl who)
      %restore  abet-pe:handle-restore:(pe bowl who)
      %thus     abet-pe:(handle-thus:(pe bowl who) ue)
    ==
  [cards this]
::
++  handle-arvo-response
  |=  [=wire =sign-arvo]
  ^-  (quip card:agent:gall _this)
  ?>  ?=([%i %http-response %finished *] sign-arvo)
  ?>  ?=([@ *] wire)
  =/  who  (,@p (slav %p i.wire))
  =/  =httr:eyre
    (to-httr:iris [response-header full-file]:client-response.sign-arvo)
  =^  cards  piers
    abet-pe:(take-sigh-httr:(pe bowl who) t.wire httr)
  [cards this]
--
