/-  *dns
!:
::
::  moves and state
::
=>  |%
+$  move  (pair bone card)
+$  poke
  $%  [%dns-command command]
      ::  XX find some other notification channel?
      ::
      [%helm-send-hi ship (unit tape)]
  ==
+$  card
  $%  [%flog wire flog:dill]
      [%hiss wire [~ ~] %httr %hiss hiss:eyre]
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
  |=  [wir=wire req=hiss:eyre]
  ^-  card
  [%hiss wir [~ ~] %httr %hiss req]
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
  |=  [wir=wire rep=httr:eyre]
  ^-  (quip move _this)
  ::  at least two segments in every wire
  ::
  ?.  ?=([@ @ *] wir)
    ~&  [%strange-http-response wire=wir response=rep]
    [~ this]
  ?+  i.wir
    ::  print and ignore unrecognized responses
    ::
    ~&  [%strange-http-response wire=wir response=rep]
    [~ this]
  ::  responses for a nameserver
  ::
      %authority
    ?~  nem
      ~&  [%strange-authority wire=wir response=rep]
      [~ this]
    ?+  i.t.wir
      !!
    ::  response confirming a valid nameserver config
    :: 
        %confirm
      ?.  =(200 p.rep)
        ~&  [%authority-confirm-fail rep]
        [~ this(nem ~)]
      abet:(~(update bind u.nem) ~)
    ::  response to a binding creation request
    ::
        %create
      ?>  ?=([@ %for @ ~] t.t.wir)
      ?.  =(200 p.rep)
        ::  XX set a retry timeout?
        ::
        ~&  [%authority-create-fail wire=wir response=rep]
        [~ this]
      =/  him=ship  (slav %p i.t.t.wir)
      =/  for=ship  (slav %p i.t.t.t.t.wir)
      abet:(~(confirm bind u.nem) for him)
    ::  response to an existing-binding retrieval request
    ::
        %update
      ?.  =(200 p.rep)
        ::  XX retry
        ::
        [~ this]
      ?~  r.rep
        [~ this]
      abet:(~(restore bind u.nem) u.r.rep)
    ==
  ::  responses for a relay validating a binding target
  ::
      %check
    =/  him=ship  (slav %p i.t.wir)
    ?:  =(200 p.rep)
      abet:bind:(tell him)
    ::  cttp timeout
    ::  XX backoff, refactor
    ::
    ?:  =(504 p.rep)
      :_  this  :_  ~
      [ost.bow %wait wir (add now.bow ~m10)]
    ::  XX specific messages per status code
    ::
    ~&  %direct-confirm-fail
    abet:(fail:(tell him) %failed-request)
  ::  responses for a relay validating an established binding
  ::
      %check-bond
    =/  him=ship  (slav %p i.t.wir)
    ?:  =(200 p.rep)
      abet:bake:(tell him)
    ::  XX backoff, refactor
    ::
    :_  this  :_  ~
    [ost.bow %wait wir (add now.bow ~m5)]
  ==
::  +sigh-tang: failed to make http request
::
++  sigh-tang
  |=  [wir=wire saw=tang]
  ^-  (quip move _this)
  ~&  [%sigh-tang wir]
  ?+  wir
        [((slog saw) ~) this]
  ::
      [%authority %confirm ~]
    ~&  %authority-confirm-fail
    [((slog saw) ~) this(nem ~)]
  ::
      [%authority %create ~]
    ~&  %authority-create-fail
    ::  XX retry pending bindings
    ::
    [((slog saw) ~) this]
  ::
      [%authority %update ~]
    ~&  %authority-update-fail
    ::  XX retry binding retrieval
    ::
    [((slog saw) ~) this]
  ::
      [%check @ ~]
    ~&  %direct-confirm-fail
    =/  him=ship  (slav %p i.t.wir)
    %-  (slog saw)
    abet:(fail:(tell him) %crash)
  ::
      [%check-bond @ ~]
    ~&  check-bond-fail+wir
    ::  XX backoff, refactor
    ::
    :_  this  :_  ~
    [ost.bow %wait wir (add now.bow ~m10)]
  ==
::  +wake: timer callback
::
++  wake
  |=  [wir=wire ~]
  ^-  (quip move _this)
  ?+    wir
      ~&  [%strange-wake wir]
      [~ this]
  ::
      [%check @ ~]
    =/  him=ship  (slav %p i.t.wir)
    abet:check:(tell him)
  ::
      [%check-bond @ ~]
    =/  him=ship  (slav %p i.t.wir)
    abet:recheck-bond:(tell him)
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
    ~!  com
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
      abet:(check-bond:(tell him.com) dom.com)
    ::
    ?:  =(our.bow him.com)
      =/  msg=tape
        "new dns binding established at {(trip (join '.' dom.com))}"
      :_  this(dom (~(put in dom) dom.com))
      :~  [ost.bow %flog / %text msg]
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
      ~&  %dns-meet-not-sponsored
      [~ this]
    abet:(hear:(tell him.com) ~)
  ==
::  +coup: general poke acknowledgement or error
::
++  coup
  |=  [wir=wire saw=(unit tang)]
  ?~  saw  [~ this]
  ~&  [%coup-fallthru wir]
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
  =|  moz=(list move)
  |_  nam=nameserver
  ++  this  .
  ::  +abet: finalize state changes, produce moves
  ::
  ++  abet
    ^-  (quip move _^this)
    [(flop moz) ^this(nem `nam)]
  ::  +emit: emit a move
  ::
  ++  emit
    |=  car=card
    ^+  this
    this(moz [[ost.bow car] moz])
  ::  +init: establish zone authority (request confirmation)
  ::
  ++  init
    |=  aut=authority
    =/  wir=wire  /authority/confirm
    =/  url=purl:eyre
      %+  endpoint  base:gcloud
      /[project.pro.aut]/['managedZones']/[zone.pro.aut]
    %-  emit(nam [aut ~ ~ ~])
    (request wir url %get ~ ~)
  ::  +update: retrieve existing remote nameserver records
  ::
  ++  update
    |=  page=(unit @t)
    ^+  this
    (emit (request /authority/update (~(list gcloud aut.nam) page)))
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
    =/  wir=wire
      /authority/create/(scot %p him)/for/(scot %p for)
    =/  pre=(unit target)
      =/  bon=(unit bound)  (~(get by bon.nam) him)
      ?~(bon ~ `cur.u.bon)
    =/  req=hiss:eyre
      (~(create gcloud aut.nam) him tar pre)
    ::  XX save :for relay state?
    ::
    =.  pen.nam  (~(put by pen.nam) him tar)
    (emit (request wir req))
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
    =.  rel  `[wen=now.bow addr dom=~ try=0 tar]
    ?:(?=(%indirect -.tar) bind check)
  ::  +check: confirm %direct target is accessible
  ::
  ++  check
    ^+  this
    ?>  ?=(^ rel)
    ?>  ?=(%direct -.tar.u.rel)
    ?:  (reserved:eyre p.tar.u.rel)
      (fail %reserved-ip)
    ::  XX confirm max retries
    ::
    ?:  (gth try.u.rel 2)
      (fail %unreachable)
    =.  try.u.rel  +(try.u.rel)
    =/  wir=wire
      /check/(scot %p him)
    =/  url=purl:eyre
      :-  [sec=| por=~ host=[%| `@if`p.tar.u.rel]]
      [[ext=`~.umd path=/static] query=~]
    (emit (request wir url %get ~ ~))
  ::  +fail: %direct target is invalid or inaccessible
  ::
  ++  fail
    |=  err=@tas
    ^+  this
    ?>  ?=(^ rel)
    ~&  [%fail err him tar.u.rel]
    =/  wir=wire
      /fail/(scot %p him)
    ::  XX add failure-specific messages
    ::  XX use a better notification channel?
    ::
    =/  msg=tape
      ?+  err
          "dns binding failed"
      ::
          %reserved-ip
        ?>  ?=(%direct -.tar.u.rel)
        =/  addr=tape  (scow %if p.tar.u.rel)
        "unable to create dns binding reserved address {addr}"
      ==
    ::  XX save failure state?
    ::
    (emit [%poke wir [our.bow %hood] %helm-send-hi him `msg])
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
  ::  +check-bond: confirm binding propagation
  ::
  ++  check-bond
    |=  dom=turf
    ^+  this
    ?>  ?=(^ rel)
    =/  wir=wire
      /check-bond/(scot %p him)
    =/  url=purl:eyre
      :-  [sec=| por=~ host=[%& dom]]
      [[ext=`~.umd path=/static] query=~]
    ::  XX track bound-state per-domain
    ::
    %-  emit(dom.u.rel `dom)
    (request wir url %get ~ ~)
  ::  +recheck-bond: re-attempt to confirm binding propagation
  ::
  ++  recheck-bond
    ^+  this
    ?>  ?&  ?=(^ rel)
            ?=(^ dom.u.rel)
        ==
    (check-bond u.dom.u.rel)
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
    =/  msg=tape
      "relaying new dns binding: {(trip (join '.' dom))}"
    ::  XX save notification state?
    ::
    %-  emit:(emit %flog / %text msg)
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
