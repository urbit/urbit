/+  store=hark-store, resource, agentio, default-agent
|%
+$  card  card:agent:gall
++  interval
  ^-  @dr
  ~h2
::
::
+$  state-0  [%0 timer=@da]
--
=|  state-0
=*  state  -
=>
  |_  =bowl:gall
  +*  io  ~(. agentio bowl)
      pass  pass:io
  ::
  ++  set-timer
    ^-  (quip card _state)
    :_  state(timer now.bowl)
    (~(wait pass /timer) (add now.bowl interval))^~
  ::
  ++  solaris-url
    %-  crip
    "https://solaris.tlon.network/notifications/{(scow %p our.bowl)}"
  ::
  ++  solaris-request
    |=  jon=json
    ^-  request:http
    =/  =octs
      (as-octt:mimes:html (en-json:html jon))
    [%'POST' solaris-url ~ `octs]
  ::
  ++  call-solaris
    ^-  card
    =+  .^(jon=json %gx (scry:io %hark-store /initial/json))
    (~(request pass /solaris) (solaris-request jon))
  --
^-  agent:gall
|_  =bowl:gall
+*  io  ~(. agentio bowl)
    pass  pass:io
    this  .
    def  ~(. (default-agent this %|) bowl)
    hc   ~(. +> bowl)
::
++  on-init
  ^-  (quip card _this)
  =^  cards  state
    set-timer
  [cards this]
::
++  on-save  !>(state)
::
++  on-load 
  |=  =vase
  =+  !<(old=state-0 vase)
  `this(state old)
::
++  on-poke  
  |=  [=mark =vase]
  ?.  =(%noun mark)
    (on-poke:def mark vase)
  :_  this
  ~[call-solaris:hc]
::
++  on-watch  on-watch:def
::
++  on-peek  on-peek:def
::
++  on-agent  on-agent:def
::
++  on-arvo  
  |=  [=wire =sign-arvo]
  ^-  (quip card _this)
  ?+    wire  (on-arvo:def wire sign-arvo)
    ::
      [%timer ~]
    ?>  ?=([%behn %wake *] sign-arvo)
    =^  cards  state
      set-timer:hc
    :_  this
    [call-solaris:hc cards]
    ::
      [%solaris ~]
    ?>  ?=([%iris %http-response *] sign-arvo)
    =*  res  client-response.sign-arvo
    ?.  ?=(%finished -.res)
      `this
    ?.  =(200 status-code.response-header.res)
      ~&  >>>  'failed to POST to solaris'
      `this
    `this
  ==
::
++  on-leave  on-leave:def
::
++  on-fail  on-fail:def
--

