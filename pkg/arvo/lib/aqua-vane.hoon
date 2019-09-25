/-  aquarium
/+  default-agent
=,  aquarium
|%
++  vane-handler
  $_  ^|
  |_  bowl:mall
  ++  handle-unix-effect
    |~  [ship unix-effect]
    *(quip card:agent:mall _^|(..handle-unix-effect))
  ::
  ++  handle-arvo-response
    |~  [wire sign-arvo]
    *(quip card:agent:mall _^|(..handle-unix-effect))
  --
--
::
|=  handler=vane-handler
^-  agent:mall
=|  subscribed=_|
|_  =bowl:mall
+*  this  .
    def  ~(. default-agent bowl this)
++  handle-init            handle-init:def
++  handle-extract-state   !>(subscribed)
++  handle-upgrade-state
  |=  old-state=vase
  `this(subscribed !<(_| old-state))
::
++  handle-poke
  |=  [=mark =vase]
  ^-  (quip card:agent:mall _this)
  ?.  ?=(%aqua-vane-control mark)
    (handle-poke:def mark vase)
  =/  command  !<(?(%subscribe %unsubscribe) vase)
  =.  subscribed  =(command %subscribe)
  :_  this
  ?-    command
      %subscribe
    %+  weld
      ^-  (list card:agent:mall)
      ?.  subscribed
        ~
      [%pass /aqua %agent [our.bowl %ph] %unsubscribe ~]~
    ^-  (list card:agent:mall)
    [%pass /aqua %agent [our.bowl %ph] %subscribe /effects]~
  ::
      %unsubscribe
    ?.  subscribed
      ~
    [%pass /aqua %agent [our.bowl %ph] %unsubscribe ~]~
  ==
::
++  handle-subscribe       handle-subscribe:def
++  handle-unsubscribe     handle-unsubscribe:def
++  handle-peek            handle-peek:def
++  handle-agent-response
  |=  [=wire =gift:agent:mall]
  ?.  ?=([%subscription-update * %aqua-effects *] gift)
    (handle-agent-response:def wire gift)
  =/  afs  !<(aqua-effects q.cage.gift)
  |-  ^-  (quip card:agent:mall _this)
  ?~  ufs.afs
    `this
  =^  cards-1  handler
    (~(handle-unix-effect handler bowl) who.afs i.ufs.afs)
  =^  cards-2  this
    $(ufs.afs t.ufs.afs)
  [(welp cards-1 cards-2) this]
::
++  handle-arvo-response
  |=  [=wire =sign-arvo]
  ^-  (quip card:agent:mall _this)
  =^  cards  handler
    (~(handle-arvo-response handler bowl) wire sign-arvo)
  [cards this]
::
++  handle-error           handle-error:def
--
