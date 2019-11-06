/-  spider, dns
/+  threadio
=,  thread=thread:spider
::
::  types and boilerplate
::
=>  |%
    ++  collector-app  `dock`[~zod %dns-collector]
    +$  app-state
      $:  %0
          requested=(unit address:dns)
          completed=(unit binding:dns)
      ==
    --
::
=<  ^-  imp:spider
    |=  =bowl:mall
    =/  m  (thread ,~)
    ^-  form:m
    ~&  >  'Entering dns loop'
    %-  (main-loop:threadio ,app-state)
    :~  handle-dns-auto
        handle-dns-address
        handle-diff
      ::
        |=  state=app-state
        =/  m  (thread ,app-state)
        ^-  form:m
        ;<  ~  bind:m
          (resubscribe:threadio /sub collector-app /(scot %p our.bowl))
        (pure:m state)
    ==
::
::  monadic helpers (XX move to threadio?)
::
=>  |%
    ++  request
      |=  =hiss:eyre
      =/  m  (thread ,(unit httr:eyre))
      ^-  form:m
      ;<  ~  bind:m  (send-request:threadio (hiss-to-request:html hiss))
      take-maybe-sigh:threadio
    ::
    ::  +self-check-http: confirm our availability at .host on port 80
    ::
    ::    XX needs better success/failure predicates
    ::    XX bind route to self and handle request inside tx?
    ::
    ++  self-check-http
      |=  [=host:eyre max=@ud]
      =/  m  (thread ,?)
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
      ;<  ~                     bind:m  (backoff:threadio try ~h1)
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
      =/  m  (thread ,~)
      ^-  form:m
      =/  msg=tape  :(weld (trip app) ": " (trip cord))
      ;<  ~  bind:m  (flog-text:threadio msg)
      (flog-tang:threadio tang)
    --
::
::  application actions
::
=>  |%
    ::  +turf-confirm-install: self check and install domain
    ::
    ++  turf-confirm-install
      |=  =turf
      =/  m  (thread ,?)
      ^-  form:m
      ;<  good=?  bind:m  (self-check-http &+turf 5)
      ?.  good
        (pure:m |)
      ;<  ~       bind:m  (install-domain:threadio turf)
      (pure:m &)
    ::
    ::  +galaxy-domains
    ::
    ++  galaxy-domains
      =/  m  (thread ,~)
      ^-  form:m
      ;<  our=@p   bind:m  get-our:threadio
      ;<  now=@da  bind:m  get-time:threadio
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
      =/  m  (thread ,?)
      ^-  form:m
      ;<  good=?  bind:m  (self-check-http |+if 5)
      ?.  good
        ::  XX details
        ~&  %bail-early
        (pure:m |)
      ;<  ~       bind:m  (poke:threadio collector-app %dns-address !>([%if if]))
      =/  msg=cord
        (cat 3 'request for DNS sent to ' (scot %p p:collector-app))
      ;<  ~       bind:m  (app-message %dns msg ~)
      ;<  our=@p  bind:m  get-our:threadio
      ;<  ~       bind:m  (subscribe:threadio /sub collector-app /(scot %p our))
      =/  msg=cord
        (cat 3 'awaiting response from ' (scot %p p:collector-app))
      ;<  ~  bind:m  (app-message %dns msg ~)
      (pure:m &)
    --
::
|%
++  handle-dns-auto
  |=  state=app-state
  =/  m  (thread ,app-state)
  ^-  form:m
  ;<  =vase     bind:m
    ((handle:threadio ,vase) (take-poke:threadio %dns-auto))
  ;<  our=ship  bind:m  get-our:threadio
  ?.  ?=(%czar (clan:title our))
    ~&  %not-galaxy
    (pure:m state)
  ;<  ~         bind:m  galaxy-domains
  (pure:m state)
::
++  handle-dns-address
  |=  state=app-state
  =/  m  (thread ,app-state)
  ^-  form:m
  ~&  %stuff
  ;<  =vase        bind:m
    ((handle:threadio ,vase) (take-poke:threadio %dns-address))
  =/  adr  !<(address:dns vase)
  ~&  [%dns-stuff adr]
  ;<  our=ship     bind:m  get-our:threadio
  =/  rac  (clan:title our)
  ?.  ?=(?(%king %duke) rac)
    ~|  [%dns-collector-bind-invalid rac]  !!
  ?:  (reserved:eyre if.adr)
    ~|  [%dns-collector-reserved-address if.adr]  !!
  ;<  requested=?  bind:m  (request-by-ip if.adr)
  ::  XX save failure?
  ::
  ~?  =(requested.state (some adr))
    %re-requesting
  =?  requested.state   requested
    (some adr)
  (pure:m state)
::
++  handle-diff
  |=  state=app-state
  =/  m  (thread ,app-state)
  ^-  form:m
  ;<  our=ship     bind:m  get-our:threadio
  ;<  =cage   bind:m
    ((handle:threadio ,cage) (take-subscription-update:threadio /(scot %p our)))
  ?>  ?=(%dns-binding p.cage)
  =/  =binding:dns  !<(binding:dns q.cage)
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
--
