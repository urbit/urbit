/-  *dns, hall
!:
::
::  moves and state
::
=>  |%
+$  move  (pair bone card)
+$  poke
  $%  [%dns-command command]
      [%hall-action %phrase audience:hall (list speech:hall)]
  ==
+$  card
  $%  [%hiss wire [~ ~] %httr %hiss hiss:eyre]
      [%poke wire dock poke]
      [%rule wire %turf %put turf]
      [%wait wire @da]
  ==
::  +state: complete app state
::
+$  state
  $:  :: dom: the set of our bindings
      ::
      dom=(set turf)
      :: per: per-dependent ips &c
      ::
      per=(map ship relay)
      :: nem: authoritative state
      ::
      nem=(unit nameserver)
  ==
--
::
::  helpers
::
=>  |%
::  +join: join list of cords with separator
::
::    XX move to zuse?
::    XX dedup with lib/pkcs
::
++  join
  |=  [sep=@t hot=(list @t)]
  ^-  @t
  =|  out=(list @t)
  ?>  ?=(^ hot)
  |-  ^-  @t
  ?~  t.hot
    (rap 3 [i.hot out])
  $(out [sep i.hot out], hot t.hot)
::  +name: fully-qualified domain name for :ship
::
++  name
  |=  [=ship =turf]
  (cat 3 (join '.' (weld turf /(crip +:(scow %p ship)))) '.')
::  +endpoint: append :path to :purl
::
++  endpoint
  |=  [=purl:eyre =path]
  ^+  purl
  purl(q.q (weld q.q.purl path))
::  +print-path: serialize a +path to a +cord
::
++  print-path
  |=  =path
  (crip ~(ram re (sell !>(path))))
--
::
::  service providers
::
=>  |%
::  |gcloud: provider-specific functions
::
++  gcloud
  |_  aut=authority
  ::  +base: provider service endpoint
  ::
  ++  base
    (need (de-purl:html 'https://www.googleapis.com/dns/v1/projects'))
  ::  +record: JSON-formatted provider-specific dns record
  ::
  ++  record
    |=  [him=ship tar=target]
    ^-  json
    =/  type
      ?:(?=(%direct -.tar) 'A' 'CNAME')
    =/  data
      ?:  ?=(%direct -.tar)
        [%s (crip +:(scow %if p.tar))]
      [%s (name p.tar dom.aut)]
    :-  %o
    %-  ~(gas by *(map @t json))
    :~  ['name' %s (name him dom.aut)]
        ['type' %s type]
        ::  XX make configureable?
        ::
        ['ttl' %n ~.300]
        ['rrdatas' %a data ~]
    ==
  ::  +create: provider-specific record-creation request
  ::
  ++  create
    =,  eyre
    |=  [him=ship tar=target pre=(unit target)]
    ^-  hiss
    =/  url=purl
      %+  endpoint  base
      /[project.pro.aut]/['managedZones']/[zone.pro.aut]/changes
    =/  hed=math
      (my content-type+['application/json' ~] ~)
    =/  bod=octs
      %-  as-octt:mimes:html
      %-  en-json:html
      :-  %o
      %-  ~(gas by *(map @t json))
      :-  ['additions' %a (record him tar) ~]
      ?~  pre  ~
      [['deletions' %a (record him u.pre) ~] ~]
    [url %post hed `bod]
  ::  +list: list existing records stored by provider
  ::
  ++  list
    =,  eyre
    |=  page=(unit @t)
    ^-  hiss
    =/  url=purl
      %+  endpoint  base
      /[project.pro.aut]/['managedZones']/[zone.pro.aut]/rrsets
    =/  hed=math
      ?~  page  ~
      (~(put by *math) 'pageToken' [u.page]~)
    [url %get hed ~]
  ::  +parse existing records stored by provider
  ::
  ++  parse
    =<  |=  bod=octs
        =/  jon  (de-json:html q.bod)
        ?~  jon  [~ ~]
        (response u.jon)
    ::
    =,  dejs:format
    |%
    ++  response
      ^-  $-  json
          (pair (list (pair ship target)) (unit @t))
      %-  ou  :~
        ::  'kind'^(su (jest "dns#resourceRecordSetsListResponse'))
        ::
        'rrsets'^(uf ~ record-set)
        'nextPageToken'^(uf ~ (mu so))
      ==
    ::
    ++  record-set
      %+  cu
        |=  a=(list (unit (pair ship target)))
        ?~  a  ~
        ?:  ?|  ?=(~ i.a)
                ?=(%czar (clan:title p.u.i.a))
            ==
          $(a t.a)
        [u.i.a $(a t.a)]
      (ar record)
    ::
    ++  record
      %+  cu
        |=  [typ=@t nam=@t dat=(list @t)]
        ^-  (unit (pair ship target))
        =/  him  (name nam)
        ?:  ?|  ?=(~ him)
                ?=(~ dat)
                ?=(^ t.dat)
            ==
          ~
        ?+  typ
          ~
        ::
            %'A'
          =/  adr  (rush i.dat lip:ag)
          ?~  adr  ~
          `[u.him %direct %if u.adr]
        ::
            %'CNAME'
          =/  for  (name i.dat)
          ?~  for  ~
          `[u.him %indirect u.for]
        ==
      ::
      %-  ot  :~
        ::  'kind'^(su (jest "dns#resourceRecordSet'))
        ::
        'type'^so
        'name'^so
        'rrdatas'^(ar so)
      ==
    ::
    ++  name
      |=  nam=@t
      ^-  (unit ship)
      =/  dom=(unit host:eyre)
        (rush nam ;~(sfix thos:de-purl:html dot))
      ?:  ?|  ?=(~ dom)
              ?=(%| -.u.dom)
              ?=(~ p.u.dom)
          ==
        ~
      =/  who
        (rush (head (flop p.u.dom)) fed:ag)
      ?~  who  ~
      ?.  =(dom.aut (flop (tail (flop p.u.dom))))
        ~
      `u.who
    --
  --
--
::
::  the app itself
::
|_  [bow=bowl:gall state]
::  +this: is sparta
::
++  this  .
:: +request: generic http request
::
++  request
  |=  [=wire =hiss:eyre]
  ^-  card
  [%hiss wire [~ ~] %httr %hiss hiss]
::  +notify: send :hall notification
::
++  notify
  |=  [=ship =cord =tang]
  ^-  card
  =/  msg=speech:hall
    :+  %app  dap.bow
    =/  line  [%lin & cord]
    ?~(tang line [%fat [%tank tang] line])
  =/  act
    [%phrase (sy [ship %inbox] ~) [msg ~]]
  [%poke / [our.bow %hall] %hall-action act]
::  +wait: set a timer
::
++  wait
  |=  [=wire lull=@dr]
  ^-  card
  [%wait wire (add now.bow lull)]
::  +backoff: calculate exponential backoff
::
++  backoff
  |=  try=@ud
  ^-  @dr
  ?:  =(0 try)  ~s0
  %+  add
    (mul ~s1 (bex (dec try)))
  (mul ~s0..0001 (~(rad og eny.bow) 1.000))
::  +poke-noun: debugging
::
++  poke-noun
  |=  a=*
  ^-  (quip move _this)
  ~&  +<+:this
  [~ this]
::  +sigh-httr: accept http response
::
++  sigh-httr
  |=  [=wire rep=httr:eyre]
  ^-  (quip move _this)
  ?+  wire
    ~&  [%strange-http-response wire rep]
    [~ this]
  ::
      [%authority *]
    ?~  nem
      ~&  [%not-an-authority %http-response wire rep]
      [~ this]
    abet:(~(http-response bind u.nem) t.wire rep)
  ::
      [%relay %him @ *]
    =/  him=ship  (slav %p i.t.t.wire)
    abet:(http-response:(tell him) t.t.t.wire rep)
  ==
::  +sigh-tang: failed to make http request
::
++  sigh-tang
  |=  [=wire =tang]
  ^-  (quip move _this)
  ?+  wire
    ~&  [%strange-sigh-tang wire]
    [((slog tang) ~) this]
  ::
      [%authority *]
    ?~  nem
      ~&  [%not-an-authority %http-crash wire]
      [((slog tang) ~) this]
    abet:(~(http-crash bind u.nem) t.wire tang)
  ::
      [%relay %him @ *]
    =/  him=ship  (slav %p i.t.t.wire)
    abet:(http-crash:(tell him) t.t.t.wire tang)
  ==
::  +wake: timer callback
::
++  wake
  |=  [=wire ~]
  ^-  (quip move _this)
  ?+    wire
      ~&  [%strange-wake wire]
      [~ this]
  ::
      [%relay %him @ *]
    =/  him=ship  (slav %p i.t.t.wire)
    abet:(retry:(tell him) t.t.t.wire)
  ==
::  +poke-dns-command: act on command
::
++  poke-dns-command
  |=  com=command
  ^-  (quip move _this)
  ?-  -.com
  ::  configure self as an authority
  ::
  ::    [%authority authority]
  ::
      %authority
    ~|  %authority-reset-wat-do
    ?<  ?=(^ nem)
    abet:(init:bind aut.com)
  ::  create binding (if authority) and forward request
  ::
  ::    [%bind for=ship him=ship target]
  ::
      %bind
    =/  rac  (clan:title him.com)
    ?:  ?=(%czar rac)
      ~|(%bind-galazy !!)
    ?:  ?&  ?=(%king rac)
            ?=(%indirect -.tar.com)
        ==
      ~|(%bind-indirect-star !!)
    ::  always forward, there may be multiple authorities
    ::
    =^  zom=(list move)  ..this
      abet:(forward:(tell him.com) [for tar]:com)
    =^  zam=(list move)  ..this
      ?~  nem  [~ this]
      abet:(~(create bind u.nem) [for him tar]:com)
    [(weld zom zam) this]
  ::  process established dns binding
  ::
  ::    [%bond for=ship him=ship turf]
  ::
      %bond
    ?:  ?&  =(our.bow for.com)
            !=(our.bow src.bow)
        ==
      abet:(learn:(tell him.com) dom.com)
    ::
    ?:  =(our.bow him.com)
      =/  msg
        (cat 3 'domain name established at ' (join '.' dom.com))
      :_  this(dom (~(put in dom) dom.com))
      :~  [ost.bow (notify our.bow msg ~)]
          [ost.bow %rule /bound %turf %put dom.com]
      ==
    ::
    ~&  [%strange-bond com]
    [~ this]
  ::  manually set our ip, request direct binding
  ::
  ::    [%ip %if addr=@if]
  ::
      %ip
    ?.  =(our.bow src.bow)
      ~&  %dns-ip-no-foreign
      [~ this]
    abet:(hear:(tell our.bow) `addr.com)
  ::  meet sponsee, request indirect binding
  ::
  ::    [%meet him=ship]
  ::
      %meet
    ?.  =(our.bow (sein:title our.bow now.bow him.com))
      ~&  [%dns-meet-not-sponsored him.com]
      [~ this]
    abet:(hear:(tell him.com) ~)
  ==
::  +coup: general poke acknowledgement or error
::
++  coup
  |=  [=wire saw=(unit tang)]
  ?~  saw  [~ this]
  ~&  [%coup-fallthru wire]
  [((slog u.saw) ~) this]
::  +prep: adapt state
::
::  ++  prep  _[~ this]
++  prep
  |=  old=(unit state)
  ^-  (quip move _this)
  ?^  old
    [~ this(+<+ u.old)]
  ::  XX print :dns|ip config instructions for stars?
  ::
  [~ this]
::  |bind: acting as zone authority
::
++  bind
  =/  abort=?  |
  =|  moz=(list move)
  |_  nam=nameserver
  ++  this  .
  ::  +abet: finalize state changes, produce moves
  ::
  ++  abet
    ^-  (quip move _^this)
    :-  (flop moz)
    ?:  abort
      ~&  %clearing-authority
      ^this(nem ~)
    ^this(nem `nam)
  ::  +emit: emit a move
  ::
  ++  emit
    |=  car=card
    ^+  this
    this(moz [[ost.bow car] moz])

  ::  +http-wire: build a wire for a |tell request
  ::
  ++  http-wire
    |=  [try=@ud =wire]
    ^-  ^wire
    (weld /authority/try/(scot %ud try) wire)
  ::  +http-crash: handle failed http request
  ::
  ++  http-crash
    |=  [=wire =tang]
    ^+  this
    ?>  ?=([%try @ @ *] wire)
    =/  try  (slav %ud i.t.wire)
    ?+  t.t.wire
      ~&([%bind %unknown-crash wire] this)
    ::
        [%confirm ~]
      %-  emit(abort &)
      (notify our.bow 'authority confirmation failed' tang)
    ::
        [%create *]
      ::  XX retry pending bindings
      ::
      =/  msg
        (cat 3 'failed to create binding: ' (print-path t.t.wire))
      (emit (notify our.bow msg tang))
    ::
        [%update ~]
      ::  XX retry binding retrieval
      ::
      (emit (notify our.bow 'failed to retrieve bindings' tang))
    ==
  ::  +http-response: handle http response
  ::
  ++  http-response
    |=  [=wire rep=httr:eyre]
    ^+  this
    ?>  ?=([%try @ @ *] wire)
    =/  try  (slav %ud i.t.wire)
    ?+  t.t.wire
      ~&([%bind %unknown-response wire rep] this)
    ::  response confirming a valid nameserver config
    ::
        [%confirm ~]
      ?:  =(200 p.rep)
        (update ~)
      %-  emit(abort &)
      ::  XX include response
      ::
      (notify our.bow 'authority confirmation failed' ~)
    ::  response to a binding creation request
    ::
        [%create @ %for @ ~]
      ?.  =(200 p.rep)
        ::  XX set a retry timeout?
        ::
        ~&  [%authority-create-fail wire rep]
        this
      =/  him=ship  (slav %p i.t.t.t.wire)
      =/  for=ship  (slav %p i.t.t.t.t.t.wire)
      (confirm for him)
    ::  response to an existing-binding retrieval request
    ::
        [%update ~]
      ?.  =(200 p.rep)
        ::  XX retry
        ::
        this
      ?~  r.rep
        this
      (restore u.r.rep)
    ==
  ::  +init: establish zone authority (request confirmation)
  ::
  ++  init
    |=  aut=authority
    =/  =wire  (http-wire 0 /confirm)
    =/  url=purl:eyre
      %+  endpoint  base:gcloud
      /[project.pro.aut]/['managedZones']/[zone.pro.aut]
    %-  emit(nam [aut ~ ~ ~])
    (request wire url %get ~ ~)
  ::  +update: retrieve existing remote nameserver records
  ::
  ++  update
    |=  page=(unit @t)
    ^+  this
    (emit (request (http-wire 0 /update) (~(list gcloud aut.nam) page)))
  ::  +restore: restore existing remote nameserver records
  ::
  ++  restore
    |=  bod=octs
    =+  ^-  [dat=(list (pair ship target)) page=(unit @t)]
      (~(parse gcloud aut.nam) bod)
    |-  ^+  this
    ?~  dat
      ?~(page this (update page))
    =/  nob=bound  [now.bow q.i.dat ~]
    $(dat t.dat, bon.nam (~(put by bon.nam) p.i.dat nob))
  ::  +create: bind :him, on behalf of :for
  ::
  ++  create
    |=  [for=ship him=ship tar=target]
    ?:  ?&  ?=(%indirect -.tar)
            !(~(has by bon.nam) p.tar)
        ==
      ::  defer %indirect where target isn't yet bound
      ::
      this(dep.nam (~(add ja dep.nam) p.tar [him tar]))
    ::  ignore if binding is pending
    ::
    =/  pending  (~(get by pen.nam) him)
    ?:  ?&  ?=(^ pending)
            =(tar u.pending)
        ==
      this
    ::  re-notify if binding already exists
    ::
    =/  existing  (~(get by bon.nam) him)
    ?:  ?&  ?=(^ existing)
            =(tar cur.u.existing)
      ==
      (bond for him)
    ::  create new or replace existing binding
    ::
    =/  =wire
      (http-wire 0 /create/(scot %p him)/for/(scot %p for))
    =/  pre=(unit target)
      =/  bon=(unit bound)  (~(get by bon.nam) him)
      ?~(bon ~ `cur.u.bon)
    =/  req=hiss:eyre
      (~(create gcloud aut.nam) him tar pre)
    ::  XX save :for relay state?
    ::
    =.  pen.nam  (~(put by pen.nam) him tar)
    (emit (request wire req))
  ::  +dependants: process deferred dependant bindings
  ::
  ++  dependants
    |=  for=ship
    ^+  this
    =/  dep=(list [him=ship tar=target])
      (~(get ja dep.nam) for)
    =.  dep.nam  (~(del by dep.nam) for)
    |-  ^+  ..this
    ?~  dep   this
    $(dep t.dep, ..this (create for him.i.dep tar.i.dep))
  ::  +confirm: successfully bound
  ::
  ++  confirm
    |=  [for=ship him=ship]
    =/  tar=target  (~(got by pen.nam) him)
    =/  bon=(unit bound)
      (~(get by bon.nam) him)
    =/  nob=bound
      [now.bow tar ?~(bon ~ [[wen.u.bon cur.u.bon] hit.u.bon])]
    =:  pen.nam  (~(del by pen.nam) him)
        bon.nam  (~(put by bon.nam) him nob)
      ==
    (dependants:(bond for him) him)
  ::  +bond: send binding confirmation
  ::
  ++  bond
    |=  [for=ship him=ship]
    =/  wir=wire
      /bound/(scot %p him)/for/(scot %p for)
    =/  dom=turf
      (weld dom.aut.nam /(crip +:(scow %p him)))
    =/  com=command
      [%bond for him dom]
    (emit [%poke wir [for dap.bow] %dns-command com])
  --
::  |tell: acting as planet parent or relay
::
++  tell
  |=  him=ship
  =|  moz=(list move)
  =/  rel=(unit relay)  (~(get by per) him)
  |%
  ++  this  .
  ::  +abet: finalize state changes, produce moves
  ::
  ++  abet
    ^-  (quip move _^this)
    :-  (flop moz)
    =?  per  ?=(^ rel)
      (~(put by per) him u.rel)
    ^this
  ::  +emit: emit a move
  ::
  ++  emit
    |=  car=card
    ^+  this
    this(moz [[ost.bow car] moz])
  ::  +http-wire: build a wire for a |tell request
  ::
  ++  http-wire
    |=  [try=@ud act=@tas]
    ^-  wire
    /relay/him/(scot %p him)/try/(scot %ud try)/[act]
  ::  +http-crash: handle failed http request
  ::
  ++  http-crash
    |=  [=wire =tang]
    ^+  this
    ?>  ?=([%try @ @ ~] wire)
    =/  try  (slav %ud i.t.wire)
    =*  act  i.t.t.wire
    ?+  act
      ~&([%tell %unknown-crash act] this)
    ::
        %check-before
      ::  XX confirm max retries
      ::
      ?:  (gth try 3)
        (fail %crash)
      =.  try  +(try)
      (emit (wait (http-wire try %check-before) (backoff try)))
    ::
        %check-after
      =/  msg
        %+  rap  3
        :~  'failed to confirm binding '
            (print-path t.wire)
            ', retrying in ~m10'
        ==
      %-  emit:(emit (notify our.bow msg tang))
      ::  no max retries, the binding has been created
      ::  XX notify after some number of failures
      ::
      =.  try  +(try)
      (wait (http-wire try %check-after) (max ~h1 (backoff try)))
    ==
  ::  +http-response: handle http response
  ::
  ++  http-response
    |=  [=wire rep=httr:eyre]
    ^+  this
    ?>  ?=([%try @ @ ~] wire)
    =/  try  (slav %ud i.t.wire)
    =*  act  i.t.t.wire
    ?+  act
      ~&([%tell %unknown-response act rep] this)
    ::  validating a binding target
    ::
        %check-before
      ?:  =(200 p.rep)
        bind
      ::  cttp timeout
      ::
      ?:  =(504 p.rep)
        %-  emit
        =.  try  +(try)
        (wait (http-wire try %check-before) (max ~h1 (backoff try)))
      ::  XX specific messages per status code
      ::  XX above some threshold?
      ::
      (fail %failed-request)
    ::  validating an established binding
    ::
        %check-after
      ?:  =(200 p.rep)
        bake
      ::  no max retries, the binding has been created
      ::  XX notify after some number of failures
      ::
      %-  emit
      =.  try  +(try)
      (wait (http-wire try %check-after) (max ~h1 (backoff try)))
    ==
  ::  +retry: re-attempt http request after timer
  ::
  ++  retry
    |=  =wire
    ^+  this
    ?>  ?=([%try @ @ ~] wire)
    =/  try  (slav %ud i.t.wire)
    =*  act  i.t.t.wire
    ?+    act
        ~&([%tell %unknown-wake act] this)
      %check-before  (check-before try)
      %check-after   (check-after try)
    ==
  ::  +hear: hear ip address, maybe emit binding request
  ::
  ++  hear
    |=  addr=(unit @if)
    ^+  this
    =/  tar=target
      ?:  |(?=(~ addr) ?=(%duke (clan:title him)))
        [%indirect our.bow]
      [%direct %if u.addr]
    ::  re-notify if binding already exists
    ::
    ::    XX deduplicate with +bake:tell and +bond:bind
    ::
    ?:  ?&  ?=(^ rel)
            ?=(^ dom.u.rel)
            =(tar tar.u.rel)
        ==
      =/  wir=wire
        /bound/(scot %p him)/for/(scot %p our.bow)
      =/  com=command
        [%bond our.bow him u.dom.u.rel]
      (emit [%poke wir [him dap.bow] %dns-command com])
    ::  check binding target validity, store and forward
    ::
    =.  rel  `[wen=now.bow addr dom=~ tar]
    ?:  ?=(%indirect -.tar)
      bind
    (check-before 0)
  ::  +check-before: confirm %direct target is accessible
  ::
  ++  check-before
    |=  try=@ud
    ^+  this
    ?>  ?=(^ rel)
    ?>  ?=(%direct -.tar.u.rel)
    ?:  (reserved:eyre p.tar.u.rel)
      (fail %reserved-ip)
    =/  =wire  (http-wire try %check-before)
    =/  url=purl:eyre
      :-  [sec=| por=~ host=[%| `@if`p.tar.u.rel]]
      [[ext=`~.umd path=/static] query=~]
    (emit (request wire url %get ~ ~))
  ::  +fail: %direct target is invalid or inaccessible
  ::
  ++  fail
    |=  err=@tas
    ^+  this
    ?>  ?=(^ rel)
    ::  XX add failure-specific messages
    ::
    =/  msg
      ?+  err
          'dns binding failed'
      ::
          %reserved-ip
        ?>  ?=(%direct -.tar.u.rel)
        =/  addr  (scot %if p.tar.u.rel)
        (cat 3 'unable to create dns binding for reserved ip address' addr)
      ==
    ::  XX save failure state?
    ::
    %-  emit:(emit (notify him msg ~))
    =/  msg
      (rap 3 (scot %p him) ' fail: ' msg ~)
    (notify our.bow msg ~)
  ::  +bind: request binding for target
  ::
  ::    Since we may be an authority, we poke ourselves.
  ::
  ++  bind
    ^+  this
    ?>  ?=(^ rel)
    ::  XX save binding request state?
    ::
    =/  wir=wire
      /bind/(scot %p him)/for/(scot %p our.bow)
    =/  com=command
      [%bind our.bow him tar.u.rel]
    (emit [%poke wir [our.bow dap.bow] %dns-command com])
  ::  +learn: of new binding
  ::
  ++  learn
    |=  dom=turf
    ^+  this
    ?>  ?=(^ rel)
    ::  XX track bound-state per-domain
    ::
    (check-after(dom.u.rel `dom) 0)
  ::  +check-after: confirm binding propagation
  ::
  ++  check-after
    |=  try=@ud
    ^+  this
    ?>  ?&  ?=(^ rel)
            ?=(^ dom.u.rel)
        ==
    =*  dom  u.dom.u.rel
    =/  =wire  (http-wire try %check-after)
    =/  url=purl:eyre
      :-  [sec=| por=~ host=[%& dom]]
      [[ext=`~.umd path=/static] query=~]
    (emit (request wire url %get ~ ~))
  ::  +bake: successfully bound
  ::
  ++  bake
    ^+  this
    ?>  ?=(^ rel)
    ?>  ?=(^ dom.u.rel)
    =/  wir=wire
      /forward/bound/(scot %p him)/for/(scot %p our.bow)
    =*  dom  u.dom.u.rel
    =/  com=command
      [%bond our.bow him dom]
    =/  msg
      (cat 3 'relaying new dns binding: ' (join '.' dom))
    ::  XX save notification state?
    ::
    %-  emit:(emit (notify our.bow msg ~))
    [%poke wir [him dap.bow] %dns-command com]
  ::  +forward: sending binding request up the network
  ::
  ++  forward
    |=  [for=ship tar=target]
    ^+  this
    ?:  ?=(%~zod our.bow)
      this
    =/  wir=wire
      /forward/bind/(scot %p him)/for/(scot %p for)
    =/  com=command
      [%bind for him tar]
    =/  to=ship
      ?:  ?=(%czar (clan:title our.bow))  ~zod
      (sein:title [our now our]:bow)
    (emit [%poke wir [to dap.bow] %dns-command com])
  --
--
