/-  dns
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
      $%  [%dns-auto ~]
          [%dns-address =address:dns]
      ==
    +$  out-poke-data
      $%  [%dns-address =address:dns]
      ==
    +$  in-peer-data
      $%  [%dns-binding =binding:dns]
      ==
    +$  out-peer-data  ~
    ++  tapp   (^tapp app-state peek-data in-poke-data out-poke-data in-peer-data out-peer-data)
    ++  stdio  (^stdio out-poke-data out-peer-data)
    --
::
::  monadic helpers (XX move to stdio?)
::
=>  |%
    ::  +backoff: exponential backoff timer
    ::
    ++  backoff
      |=  try=@ud
      =/  m  (async:stdio ,~)
      ^-  form:m
      ;<  eny=@uvJ  bind:m  get-entropy:stdio
      ;<  now=@da   bind:m  get-time:stdio
      %-  wait:stdio
      %+  add
        now
      ?:  =(0 try)  ~s0
      %+  add
        (mul ~s1 (bex (dec try)))
      (mul ~s0..0001 (~(rad og eny) 1.000))
    ::      
    ::  +self-check-http: confirm our availability at .host on port 80
    ::
    ++  self-check-http
      |=  [=host:eyre max=@ud]
      =/  m  (async:stdio ?)
      ^-  form:m
      =/  req=hiss:eyre
        =/  url=purl:eyre
          [[sec=| por=~ host] [ext=`~.udon path=/static] query=~]
        [url %get ~ ~]
      =/  try=@ud  0
      |-  ^-  form:m
      =*  loop  $
      ?:  =(try max)
        (pure:m |)
      ;<  ~           bind:m  (backoff try)
      ;<  ~           bind:m  (send-hiss:stdio req)
      ;<  =httr:eyre  bind:m  take-sigh:stdio
      ::  XX needs a better predicate. LTE will make this easier
      ::
      ?:  =(200 p.httr)
        (pure:m &)
      loop(try +(try))
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
      ;<  ?  bind:m  (turf-confirm-install turf)
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
    ;<  requested=?  bind:m  (request-by-ip if.address.in-poke-data)
    ::  XX save failure?
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
    ;<  installed=?  bind:m  (turf-confirm-install turf.binding)
    =?  completed.state  installed  (some binding)
    ::  XX save failure?
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
    ?~  error.sign
      (pure:m state)
    ::  XX details
    ~&  %dns-ip-request-failed
    %-  (slog u.error.sign)
    (pure:m state)
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
      (pure:m state)
    ::  XX details
    ~&  %dns-domain-subscription-failed
    %-  (slog u.error.sign)
    (pure:m state)
  ==
--
