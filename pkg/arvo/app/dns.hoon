/-  dns, hall
/+  tapp, stdio
::
::  tapp types and boilerplate
::
=>  |%
    ++  collector-app  `dock`[~zod %dns-collector]
    +$  app-state
      $:  %0
          requested=(unit address:dns)
          completed=(unit binding:dns)
      ==
    +$  peek-data  _!!
    +$  in-poke-data
      $%  ::  XX ames-domains unused, remove
          ::
          [%dns-auto ames-domains=(list turf)]
          [%dns-address =address:dns]
      ==
    +$  out-poke-data
      $%  [%dns-address =address:dns]
          [%hall-action %phrase audience:hall (list speech:hall)]
      ==
    +$  in-peer-data
      $%  [%dns-binding =binding:dns]
      ==
    +$  out-peer-data  ~
    ++  tapp
      %:  ^tapp
        app-state
        peek-data
        in-poke-data
        out-poke-data
        in-peer-data
        out-peer-data
      ==
    ++  stdio  (^stdio out-poke-data out-peer-data)
    --
::
::  monadic helpers (XX move to stdio?)
::
=>  |%
    ::  +backoff: exponential backoff timer
    ::
    ++  backoff
      |=  [try=@ud limit=@dr]
      =/  m  (async:stdio ,~)
      ^-  form:m
      ;<  eny=@uvJ  bind:m  get-entropy:stdio
      ;<  now=@da   bind:m  get-time:stdio
      %-  wait:stdio
      %+  add  now
      %+  min  limit
      ?:  =(0 try)  ~s0
      %+  add
        (mul ~s1 (bex (dec try)))
      (mul ~s0..0001 (~(rad og eny) 1.000))
    ::
    ++  request
      |=  =hiss:eyre
      =/  m  (async:stdio (unit httr:eyre))
      ^-  form:m
      ;<  ~  bind:m  (send-hiss:stdio hiss)
      take-maybe-sigh:stdio
    ::
    ::  +self-check-http: confirm our availability at .host on port 80
    ::
    ::    XX needs better success/failure predicates
    ::    XX bind route to self and handle request inside tx?
    ::
    ++  self-check-http
      |=  [=host:eyre max=@ud]
      =/  m  (async:stdio ?)
      ^-  form:m
      ::  XX also scry into eyre
      ::  q:.^(hart:eyre %e /(scot %p our)/host/real)
      =/  =hiss:eyre
        =/  url=purl:eyre
          [[sec=| por=~ host] [ext=`~.udon path=/static] query=~]
        [url %get ~ ~]
      =/  try=@ud  0
      |-  ^-  form:m
      =*  loop  $
      ?:  =(try max)
        (pure:m |)
      ;<  ~                     bind:m  (backoff try ~h1)
      ;<  rep=(unit httr:eyre)  bind:m  (request hiss)
      ?:  ?&  ?=(^ rep)
              |(=(200 p.u.rep) =(307 p.u.rep))
          ==
        (pure:m &)
      ?.  ?|  ?=(~ rep)
              =(504 p.u.rep)
          ==
        (pure:m |)
      loop(try +(try))
    ::
    ++  app-message
      |=  [app=term =cord =tang]
      =/  m  (async:stdio ,~)
      ^-  form:m
      =/  msg=tape  :(weld (trip app) ": " (trip cord))
      ;<  ~  bind:m  (flog-text:stdio msg)
      (flog-tang:stdio tang)
    ::
    ::  XX disabled due to :hall's status
    ::
    ++  hall-app-message-disabled
      |=  [app=term =cord =tang]
      =/  m  (async:stdio ,~)
      ^-  form:m
      =/  msg=speech:hall
        :+  %app  app
        =/  line  [%lin & cord]
        ?~(tang line [%fat [%tank tang] line])
      ;<  our=@p  bind:m  get-identity:stdio
      =/  act
        [%phrase (sy [our %inbox] ~) [msg ~]]
      (poke-app:stdio [our %hall] %hall-action act)
    --
::
::  application actions
::
=>  |%
    ::  +turf-confirm-install: self check and install domain
    ::
    ++  turf-confirm-install
      |=  =turf
      =/  m  (async:stdio ?)
      ^-  form:m
      ;<  good=?  bind:m  (self-check-http &+turf 5)
      ?.  good
        (pure:m |)
      ;<  ~       bind:m  (install-domain:stdio turf)
      (pure:m &)
    ::
    ::  +galaxy-domains
    ::
    ++  galaxy-domains
      =/  m  (async:stdio ,~)
      ^-  form:m
      ;<  our=@p   bind:m  get-identity:stdio
      ;<  now=@da  bind:m  get-time:stdio
      =/  ames-domains=(list turf)
        .^((list turf) %j /(scot %p our)/turf/(scot %da now))
      |-  ^-  form:m
      =*  loop  $
      ?~  ames-domains
        (pure:m ~)
      =/  =turf
        (weld i.ames-domains /(crip +:(scow %p our)))
      ;<  good=?   bind:m  (turf-confirm-install turf)
      =/  msg=(pair cord tang)
        ?:  good
          [(cat 3 'confirmed access via ' (en-turf:html turf)) ~]
        :-  (cat 3 'unable to access via ' (en-turf:html turf))
        :~  leaf+"XX check via nslookup"
            leaf+"XX confirm port 80"
        ==
      ;<  ~        bind:m  (app-message %dns msg)
      loop(ames-domains t.ames-domains)
    ::
    ::  +request-by-ip
    ::
    ++  request-by-ip
      |=  if=@if
      =/  m  (async:stdio ?)
      ^-  form:m
      ;<  good=?  bind:m  (self-check-http |+if 5)
      ?.  good
        ::  XX details
        ~&  %bail-early
        (pure:m |)
      ;<  ~       bind:m  (poke-app:stdio collector-app [%dns-address %if if])
      ;<  our=@p  bind:m  get-identity:stdio
      ;<  ~       bind:m  (peer-app:stdio collector-app /(scot %p our))
      (pure:m &)
    --
::
=*  tapp-async    tapp-async:tapp
=*  default-tapp  default-tapp:tapp
%-  create-tapp-all:tapp
^-  tapp-core-all:tapp
|_  [=bowl:gall state=app-state]
::
++  handle-init  handle-init:default-tapp
++  handle-peek  handle-peek:default-tapp
++  handle-peer  handle-peer:default-tapp
::
++  handle-poke
  |=  =in-poke-data
  =/  m  tapp-async
  ^-  form:m
  ?.  (team:title [our src]:bowl)
    ~|  %configure-yoself  !!
  ?-  -.in-poke-data
  ::
  ::  "automatic" dns binding -- currently only for galaxies
  ::
  ::    XX could be in +handle-init
  ::    XX use ip reflection for other classes
  ::
      %dns-auto
    ?.  ?=(%czar (clan:title our.bowl))
      ::  XX details
      ::
      ~&  %galaxy-only
      (pure:m state)
    ;<  ~  bind:m  galaxy-domains
    (pure:m state)
  ::
  ::  manual dns binding -- by explicit ipv4
  ::
      %dns-address
    =*  adr  address.in-poke-data
    =/  rac  (clan:title our.bowl)
    ?.  ?=(?(%king %duke) rac)
      ~|  [%dns-collector-bind-invalid rac]  !!
    ?:  (reserved:eyre if.adr)
      ~|  [%dns-collector-reserved-address if.adr]  !!
    ;<  requested=?  bind:m  (request-by-ip if.adr)
    ::  XX save failure?
    ::
    ~?  =(requested.state (some address.in-poke-data))
      %re-requesting
    =?  requested.state   requested
      (some address.in-poke-data)
    (pure:m state)
  ==
::
++  handle-diff
  |=  [=dock =path =in-peer-data]
  =/  m  tapp-async
  ^-  form:m
  ?.  =(dock collector-app)
    ~|  [%unexpected-diff-dock-wat-do dock]  !!
  ?.  =(path /(scot %p our.bowl))
    ~|  [%unexpected-diff-path-wat-do path]  !!
  ?-  -.in-peer-data
      %dns-binding
    =*  binding  binding.in-peer-data
    ?~  requested.state
      ~|  %unexpected-binding-wat-do  !!
    ?.  =(u.requested.state address.binding)
      ~|  %mismatch-binding-wat-do  !!
    ;<  good=?  bind:m  (turf-confirm-install turf.binding)
    =/  msg=(pair cord tang)
      ?:  good
        [(cat 3 'confirmed access via ' (en-turf:html turf.binding)) ~]
      :-  (cat 3 'unable to access via ' (en-turf:html turf.binding))
      :~  leaf+"XX check via nslookup"
          leaf+"XX confirm port 80"
      ==
    ;<  ~       bind:m  (app-message %dns msg)
    =?  completed.state  good  (some binding)
    ::  XX save failure?s
    ::  XX unsubscribe?
    (pure:m state)
  ==
::
++  handle-take
  |=  =sign:tapp
  =/  m  tapp-async
  ^-  form:m
  ?+  -.sign
    ~|  [%unexpected-sign sign]  !!
  ::  print %poke nacks
  ::
      %coup
    ?.  =(collector-app dock.sign)
      (pure:m state)
    ?~  error.sign
      =/  msg=cord
        (cat 3 'request for DNS sent to ' (scot %p p:collector-app))
      ;<  ~  bind:m  (app-message %dns msg ~)
      (pure:m state)
    ::  XX details
    ~&  %dns-ip-request-failed
    %-  (slog u.error.sign)
    (pure:m state(requested ~))
  ::  re-subscribe if (involuntarily) unsubscribed
  ::
      %quit
    ?.  =(path.sign /(scot %p our.bowl))
      ~|  [%unexpected-quit-path-wat-do path.sign]  !!
    ;<  ~  bind:m  (peer-app:stdio collector-app /(scot %p our.bowl))
    (pure:m state)
  ::  print %peer nacks
  ::
      %reap
    ?.  =(path.sign /(scot %p our.bowl))
      ~|  [%unexpected-reap-path-wat-do path.sign]  !!
    ?~  error.sign
      =/  msg=cord
        (cat 3 'awaiting response from ' (scot %p p:collector-app))
      ;<  ~  bind:m  (app-message %dns msg ~)
      (pure:m state)
    ::  XX details
    ~&  %dns-domain-subscription-failed
    %-  (slog u.error.sign)
    (pure:m state)
  ==
--
