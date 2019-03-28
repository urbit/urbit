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
  $%  [%connect wire =binding:http-server app=term]
      [%http-response =http-event:http]
      [%poke wire dock poke]
      [%request wire request:http outbound-config:http-client]
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
::  +local-uri: XX
::
++  local-uri
  |=  [our=ship =path]
  ^-  @t
  =/  =hart:eyre  .^(hart:eyre %r /(scot %p our)/host/real)
  (crip (en-purl:html [hart [~ path] ~]))
::  +oauth2-config: as one would expect
::
+$  oauth2-config
  $:  auth-url=@t
      exchange-url=@t
      domain=turf
      initial-path=path
      redirect-path=path
      scopes=(list @t)
  ==
::
::  +oauth2: library core
::
++  oauth2
  |_  [our=@p now=@da config=oauth2-config]
  ::
  ++  code
    ^-  @t
    %-  crip
    +:(scow %p .^(@p %j /(scot %p our)/code/(scot %da now)/(scot %p our)))
  ::
  :: to initialize these values: |init-oauth2 /com/googleapis
  ::
  ++  oauth2-secrets
    ^-  [client-id=@t client-secret=@t]
    =;  =wain
      ?>  ?=([@t @t ~] wain)
      [i.wain i.t.wain]
    ::
    %-  to-wain:format
    %-  need
    %+  de:crub:crypto  code
    %+  slav  %uw
    .^(@ %cx :(weld /(scot %p our)/home/(scot %da now)/sec domain.config /atom))
  ::
  ++  initial-uri  (local-uri our initial-path.config)
  ++  redirect-uri  (local-uri our redirect-path.config)
  ::
  ++  redirect-to-provider
    ^-  @t
    =/  url  (need (de-purl:html auth-url.config))
    =.  r.url
      :*  ['access_type' 'offline']
          ['response_type' 'code']
          ['prompt' 'consent']
          ['client_id' client-id:oauth2-secrets]
          ['redirect_uri' redirect-uri]
          ['scope' (join ' ' scopes.config)]
          r.url
      ==
    (crip (en-purl:html url))
  ::
  ++  retrieve-access-token
    |=  code=@t
    ^-  request:http
    =/  hed
      :~  ['Accept' 'application/json']
          ['Content-Type' 'application/x-www-form-urlencoded']
      ==
    =/  bod
      %-  some  %-  as-octt:mimes:html
      %-  tail  %-  tail:en-purl:html
      :~  ['client_id' client-id:oauth2-secrets]
          ::  note: required, unused parameter
          ::
          ['redirect_uri' redirect-uri]
          ['client_secret' client-secret:oauth2-secrets]
          ['grant_type' 'authorization_code']
          ['code' code]
      ==
    [%'POST' exchange-url.config hed bod]
  ::
  ++  parse-token-response
    |=  =octs
    ^-  (unit [access=@t expires=@u refresh=@t])
    %.  q.octs
    ;~  biff
      de-json:html
      =,  dejs-soft:format
      (ot 'access_token'^so 'expires_in'^ni 'refresh_token'^so ~)
    ==
  ::  XX implement
  ::
  ++  refresh-token  !!
  --
--
::
=>  |%
::  +name: fully-qualified domain name for :ship
::
++  name
  |=  [=ship =turf]
  (cat 3 (join '.' (weld turf /(crip +:(scow %p ship)))) '.')
::  +lame: domain name for :ship (without trailing '.')
::
++  lame
  |=  [=ship =turf]
  (join '.' (weld turf /(crip +:(scow %p ship))))
::  +endpoint: append :path to :purl
::
++  endpoint
  |=  [=purl:eyre =path]
  ^+  purl
  purl(q.q (weld q.q.purl path))
::  +params: append :params to :purl
::
++  params
  |=  [=purl:eyre =quay:eyre]
  ^+  purl
  purl(r (weld r.purl quay))
::  +print-path: serialize a +path to a +cord
::
++  print-path
  |=  =path
  (crip ~(ram re (sell !>(path))))
::  +json-octs: deserialize json and apply reparser
::
++  json-octs
  |*  [bod=octs wit=fist:dejs:format]
  =/  jon  (de-json:html q.bod)
  ?~  jon  ~
  (wit u.jon)
::  +ship-turf: parse ship from first subdomain
::
++  ship-turf
  |=  [nam=@t aut-dom=turf]
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
  ?.  =(aut-dom (flop (tail (flop p.u.dom))))
    ~
  ::  galaxies always excluded
  ::
  ?:  ?=(%czar (clan:title u.who))
    ~
  who
--
::
::  service providers
::
=>  |%
::  +provider: initialize provider-specific core
::
++  provider
  |=  aut=authority
  ?-  -.pro.aut
    %fcloud  ~(. fcloud aut)
    %gcloud  ~(. gcloud aut)
  ==
::  |fcloud: Cloudflare provider
::
++  fcloud
  =>  |%
      ++  parse-raw-record
        |=  aut-dom=turf
        ^-  $-  json
            (unit [=ship id=@ta tar=target])
        =,  dejs:format
        %+  cu
          |=  [id=@t typ=@t nam=@t dat=@t]
          ^-  (unit [=ship id=@ta tar=target])
          ::  XX fix this
          ::
          =/  him  (ship-turf (cat 3 nam '.') aut-dom)
          ?:  ?=(~ him)
            ~
          ?+  typ
            ~
          ::
              %'A'
            =/  adr  (rush dat lip:ag)
            ?~  adr  ~
            `[u.him `@ta`id %direct %if u.adr]
          ::
              %'CNAME'
            ::  XX fix this
            ::
            =/  for  (ship-turf (cat 3 dat '.') aut-dom)
            ?~  for  ~
            `[u.him `@ta`id %indirect u.for]
          ==
        ::  XX parse dates, proxied, ttl?
        ::
        %-  ot  :~
          'id'^so
          'type'^so
          'name'^so
          'content'^so
        ==
      --
  ::
  |_  aut=authority
  ::  +base: provider service endpoint
  ::
  ++  base
    ^-  purl:eyre
    (need (de-purl:html 'https://api.cloudflare.com/client/v4'))
  ::  +headers: standard HTTP headers for all |fcloud requests
  ::
  ++  headers
    |=  aut=authority
    ?>  ?=(%fcloud -.pro.aut)
    %-  ~(gas by *math:eyre)
    :~  ['Content-Type' ['application/json' ~]]
        ['X-Auth-Email' [email.auth.pro.aut ~]]
        ['X-Auth-Key' [key.auth.pro.aut ~]]
    ==
  ::  +zone: provider-specific zone info request
  ::
  ++  zone
    ^-  hiss:eyre
    ?>  ?=(%fcloud -.pro.aut)
    [(endpoint base /zones/[zone.pro.aut]) %get (headers aut) ~]
  ::  +record: JSON-formatted provider-specific dns record
  ::
  ++  record
    |=  [him=ship tar=target]
    ^-  json
    ?>  ?=(%fcloud -.pro.aut)
    =/  type
      ?:(?=(%direct -.tar) 'A' 'CNAME')
    =/  data
      ?:  ?=(%direct -.tar)
        (crip +:(scow %if p.tar))
      (lame p.tar dom.aut)
    :-  %o
    %-  ~(gas by *(map @t json))
    :~  ['name' %s (lame him dom.aut)]
        ['type' %s type]
        ['content' %s data]
        ::  XX make configureable?
        ::
        ['ttl' %n ~.1]
        ['proxied' %b %.n]
    ==
  ::  +create: provider-specific record-creation request
  ::
  ++  create
    |=  [him=ship tar=target pre=(unit [id=@ta tar=target])]
    ^-  hiss:eyre
    ?>  ?=(%fcloud -.pro.aut)
    =/  bod=octs
      %-  as-octt:mimes:html
      %-  en-json:html
      (record him tar)
    ?~  pre
      :-  (endpoint base /zones/[zone.pro.aut]/['dns_records'])
      [%post (headers aut) `bod]
    :-  (endpoint base /zones/[zone.pro.aut]/['dns_records']/[id.u.pre])
    [%put (headers aut) `bod]
  ::  +existing: list existing records stored by provider
  ::
  ++  existing
    |=  page=(unit @t)
    ^-  hiss:eyre
    ?>  ?=(%fcloud -.pro.aut)
    ::  XX more url params:
    ::  ?type ?per-page ?order ?direction
    ::
    :-  %+  params
          (endpoint base /zones/[zone.pro.aut]/['dns_records'])
        ?~(page ~ ['page' u.page]~)
    [%get (headers aut) ~]
  ::  +parse-list: existing records stored by provider
  ::
  ++  parse-list
    ^-  $-  json
        (pair (list [=ship id=@ta tar=target]) (unit @t))
    ?>  ?=(%fcloud -.pro.aut)
    =,  dejs:format
    %+  cu
      |=  $:  success=?
              response=(list (unit [=ship id=@ta tar=target]))
              paginate=[page=@ud per-page=@ud count=@ud total-count=@ud]
          ==
      ^-  (pair (list [=ship id=@ta tar=target]) (unit @t))
      ?.  success  [~ ~]
      :-  (murn response same)
      ::  XX calculate next page number if applicable
      ::
      ~
    ::  XX parse errors and messages?
    ::
    %-  ot  :~
      'success'^bo
      'result'^(ar (parse-raw-record dom.aut))
      :-  'result_info'
      %-  ot  :~
        'page'^ni
        'per_page'^ni
        'count'^ni
        'total_count'^ni
      ==
    ==
  ::  +parse-record: single record stored by provider
  ::
  ++  parse-record
    ^-  $-  json
        (unit [=ship id=@ta tar=target])
    ?>  ?=(%fcloud -.pro.aut)
    =,  dejs:format
    %+  cu
      |=  [success=? response=(unit [=ship id=@ta tar=target])]
      ^-  (unit [=ship id=@ta tar=target])
      ?.  success  ~
      response
    ::  XX parse errors and messages?
    ::
    %-  ot  :~
      'success'^bo
      'result'^(parse-raw-record dom.aut)
    ==
  --
::  |gcloud: GCP provider
::
++  gcloud
  |_  aut=authority
  ::  +base: provider service endpoint
  ::
  ++  base
    ^-  purl:eyre
    (need (de-purl:html 'https://www.googleapis.com/dns/v1/projects'))
  ::  +headers: standard HTTP headers for all |gcloud requests
  ::
  ++  headers
    |=  aut=authority
    ?>  ?=(%gcloud -.pro.aut)
    ?.  ?=(^ auth.pro.aut)
      ~|  %gcloud-missing-auth  !!
    %-  ~(gas by *math:eyre)
    :~  ['Content-Type' ['application/json' ~]]
        ['Authorization' [`@t`(cat 3 'Bearer ' access.u.auth.pro.aut) ~]]
    ==
  ::  +zone: provider-specific zone info request
  ::
  ++  zone
    ^-  hiss:eyre
    ?>  ?=(%gcloud -.pro.aut)
    :-  (endpoint base /[project.pro.aut]/['managedZones']/[zone.pro.aut])
    [%get (headers aut) ~]
  ::  +record: JSON-formatted provider-specific dns record
  ::
  ++  record
    |=  [him=ship tar=target]
    ^-  json
    ?>  ?=(%gcloud -.pro.aut)
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
    |=  [him=ship tar=target pre=(unit [id=@ta tar=target])]
    ^-  hiss
    ?>  ?=(%gcloud -.pro.aut)
    =/  url=purl
      %+  endpoint  base
      /[project.pro.aut]/['managedZones']/[zone.pro.aut]/changes
    =/  bod=octs
      %-  as-octt:mimes:html
      %-  en-json:html
      :-  %o
      %-  ~(gas by *(map @t json))
      :-  ['additions' %a (record him tar) ~]
      ?~  pre  ~
      [['deletions' %a (record him tar.u.pre) ~] ~]
    [url %post (headers aut) `bod]
  ::  +existing: list existing records stored by provider
  ::
  ++  existing
    =,  eyre
    |=  page=(unit @t)
    ^-  hiss
    ?>  ?=(%gcloud -.pro.aut)
    =/  url=purl
      %+  endpoint  base
      /[project.pro.aut]/['managedZones']/[zone.pro.aut]/rrsets
    =/  hed=math  (headers aut)
    =?  hed  ?=(^ page)
      (~(put by hed) 'pageToken' [u.page]~)
    [url %get hed ~]
  ::  +parse-list: existing records stored by provider
  ::
  ++  parse-list
    ^-  $-  json
        (pair (list [=ship id=@ta tar=target]) (unit @t))
    ?>  ?=(%gcloud -.pro.aut)
    =,  dejs:format
    =>  |%
        ++  page  (uf ~ (mu so))
        ++  records
          %+  uf  ~
          %+  cu
            |*(a=(list (unit)) (murn a same))
          (ar parse-record)
        --
    ::  XX parse but don't produce
    ::  'kind'^(su (jest "dns#resourceRecordSetsListResponse'))
    ::
    (ou 'rrsets'^records 'nextPageToken'^page ~)
  ::  +parse-record: single record stored by provider
  ::
  ++  parse-record
    ^-  $-  json
        (unit [=ship id=@ta tar=target])
    ?>  ?=(%gcloud -.pro.aut)
    =,  dejs:format
    %+  cu
      |=  [typ=@t nam=@t dat=(list @t)]
      ^-  (unit [=ship id=@ta tar=target])
      ::  gcloud doesn't expose UUIDs for bindings
      ::
      =/  id  %$
      =/  him  (ship-turf nam dom.aut)
      ?:  |(?=(~ him) ?=(~ dat) ?=(^ t.dat))
        ~
      ?+  typ
        ~
      ::
          %'A'
        =/  adr  (rush i.dat lip:ag)
        ?~  adr  ~
        `[u.him id %direct %if u.adr]
      ::
          %'CNAME'
        =/  for  (ship-turf i.dat dom.aut)
        ?~  for  ~
        `[u.him id %indirect u.for]
      ==
    ::
    %-  ot  :~
      ::  'kind'^(su (jest "dns#resourceRecordSet'))
      ::
      'type'^so
      'name'^so
      'rrdatas'^(ar so)
    ==
  --
--
::
::  the app itself
::
|_  [bow=bowl:gall state]
::  +this: is sparta
::
++  this  .
::  |oauth2-core: configured oauth functionality (for |gcloud only)
::
++  oauth2-core
  =/  =oauth2-config
    :*  auth-url='https://accounts.google.com/o/oauth2/v2/auth'
        exchange-url='https://www.googleapis.com/oauth2/v4/token'
        domain=/com/googleapis
        redirect-path=/dns/oauth
        initial-path=/dns/oauth/result
        :~  'https://www.googleapis.com/auth/ndev.clouddns.readwrite'
            'https://www.googleapis.com/auth/cloud-platform.read-only'
    ==  ==
  ~(. oauth2 our.bow now.bow oauth2-config)
::
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
::
++  http-response
  |=  [=wire response=client-response:http-client]
  ^-  (quip move _this)
  ::  ignore progress reports
  ::
  ?:  ?=(%progress -.response)
    [~ this]
  ::
  ?+  wire
    ~&  [%strange-http-response wire response]
    [~ this]
  ::
      [%authority *]
    ?~  nem
      ~&  [%not-an-authority %http-response wire response]
      [~ this]
    =<  abet
    ?:  ?=(%cancel -.response)
      (~(http-cancel bind u.nem) t.wire)
    (~(http-response bind u.nem) t.wire (to-httr:http-client +.response))
  ::
      [%relay %him @ *]
    =/  him=ship  (slav %p i.t.t.wire)
    =<  abet
    ?:  ?=(%cancel -.response)
      (http-cancel:(tell him) t.t.t.wire)
    (http-response:(tell him) t.t.t.wire (to-httr:http-client +.response))
  ==
::
++  poke-handle-http-request
  |=  =inbound-request:http-server
  ^-  (quip move _this)
  ?~  nem
      ~&  :*  %not-an-authority
              %http-request
              =>  inbound-request
              [authenticated secure address [method url]:request]
          ==
    =/  =move
      [ost.bow %http-response %start [%403 ~] ~ %.y]
    [[move ~] this]
  abet:(~(http-request bind u.nem) inbound-request)
::  +wake: timer callback
::
++  wake
  |=  [=wire ~]
  ^-  (quip move _this)
  ?+    wire
      ~&  [%strange-wake wire]
      [~ this]
  ::
      [%authority *]
    ?~  nem
      ~&  [%not-an-authority %wake wire]
      [~ this]
    abet:(~(retry bind u.nem) t.wire)
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
    abet:(pre-init:bind aut.com)
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
  ::  XX this binding should be deleted when we're done
  ::  but that would require tracking bones
  ::
  =|  mov=(list move)
  =.  mov
    :_  mov
    [ost.bow %connect /dns/oauth [~ /dns/oauth] %dns]
  ::  galaxies must already have dns bindings on *all* ames domains
  ::
  =?  mov  ?=(%czar (clan:title our.bow))
    %+  weld  mov
    %+  turn
      .^((list turf) %j /(scot %p our.bow)/turf/(scot %da now.bow))
    |=  =turf
    ^-  move
    =/  full  (weld turf /(crip +:(scow %p our.bow)))
    [ost.bow %rule /bound %turf %put full]
  ::  XX print :dns|ip config instructions for stars?
  ::
  [mov this]
::
++  bound
  |=  [=wire accepted=? =binding:http-server]
  ?:  accepted
    [~ this]
  ::  XX better error message
  ::
  ~&  [%dns-http-path-binding-failed +<]
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
  :: +request: authenticated http request
  ::
  ++  request
    |=  [=wire =hiss:eyre]
    ^-  card
    [%request wire (hiss-to-request:html hiss) *outbound-config:http-client]
  ::  +http-wire: build a wire for a |tell request
  ::
  ++  http-wire
    |=  [try=@ud =wire]
    ^-  ^wire
    (weld /authority/try/(scot %ud try) wire)
  ::  +http-cancel: retry canceled http request
  ::
  ++  http-cancel
    |=  =wire
    ^+  this
    ?>  ?=([%try @ @ *] wire)
    =/  try  (slav %ud i.t.wire)
    ?+  t.t.wire
      ~&([%bind %unknown-crash wire] this)
    ::
        [%oath %access ~]
      ~&  %do-the-oauth-thing-again
      this
    ::
        [%confirm ~]
      =.  try  +(try)
      (emit (wait (http-wire try /confirm) (min ~h1 (backoff try))))
    ::
        [%create @ %for @ ~]
      =.  try  +(try)
      (emit (wait (http-wire try t.t.wire) (min ~h1 (backoff try))))
    ::
        [%update @ ~]
      =.  try  +(try)
      (emit (wait (http-wire try t.t.wire) (min ~h1 (backoff try))))
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
    ::  response providing oauth access token (|gcloud only)
    ::
        [%oauth %access ~]
      ?>  ?=(%gcloud -.pro.aut.nam)
      ::  XX save access/refresh tokens
      ::
      ?.  =(200 p.rep)
        ~&  [%oauth-failed p.rep]
        this
      ?~  r.rep
        ~&  [%oauth-failed %no-body]
        this
      =/  data  (parse-token-response:oauth2 u.r.rep)
      ?~  data
        ~&  [%oauth-failed %invalid-body u.r.rep]
        this
      =.  auth.pro.aut.nam  (some [access refresh]:u.data)
      ::  XX  use expiry to set refresh timer
      ::
      (init 1)
    ::  response confirming a valid nameserver config
    ::
        [%confirm ~]
      ?:  =(200 p.rep)
        (update ~ 1)
      %-  emit(abort &)
      ::  XX include response
      ::
      =/  =tang  [(sell !>(rep)) ~]
      (notify our.bow 'authority confirmation failed' tang)
    ::  response to a binding creation request
    ::
        [%create @ %for @ ~]
      ?.  =(200 p.rep)
        ::  XX any retry-able errors?
        ::
        =/  msg
          (cat 3 'failed to create binding: ' (print-path t.t.wire))
        =/  =tang  [(sell !>(rep)) ~]
        (emit (notify our.bow msg tang))
      ::
      =/  him=ship  (slav %p i.t.t.t.wire)
      =/  for=ship  (slav %p i.t.t.t.t.t.wire)
      =/  id
        ::  XX move into provider interface
        ::
        ?.  ?=(%fcloud -.pro.aut.nam)  ~.
        ~|  [%authority-create-confirm-id rep]
        ?>  ?=(^ r.rep)
        =/  dat=(unit [=ship id=@ta tar=target])
          (json-octs u.r.rep parse-record:(provider aut.nam))
        id:(need dat)
      (confirm for him id)
    ::  response to an existing-binding retrieval request
    ::
        [%update @ ~]
      ?.  =(200 p.rep)
        ?.  (gth try 5)
          =/  =tang  [(sell !>(rep)) ~]
          (emit (notify our.bow 'failed to retrieve bindings' tang))
        =.  try  +(try)
        (emit (wait (http-wire try t.t.wire) (min ~h1 (backoff try))))
      ?~  r.rep
        this
      (restore u.r.rep)
    ==
  ::  +http-request: act as server for oauth redirects (|gcloud only)
  ::
  ++  http-request
    |=  =inbound-request:http-server
    ^+  this
    ?>  ?=(%gcloud -.pro.aut.nam)
    ::
    =/  parsed=(unit (pair pork:eyre quay:eyre))
      %+  rush
        url.request.inbound-request
      ;~(plug ;~(pose apat:de-purl:html (easy *pork:eyre)) yque:de-purl:html)
    ::
    ?.  ?=(^ parsed)
      ~|  [%invalid-url url.request.inbound-request]  !!
    =*  url  q.p.u.parsed
    =*  ext  p.p.u.parsed
    =*  params  q.u.parsed
  ::
    ?+  url
      (emit %http-response %start [%404 ~] ~ %.y)
    ::
        [%dns %oauth ~]
      =/  link  (trip redirect-to-provider:oauth2-core)
      =/  bod=(unit octs)
        %-  some
        %-  as-octt:mimes:html
        %-  en-xml:html
        ;html
          ;head
            ;title:  :dns oauth
          ==
          ;body
            ;p  make sure that the oauth credential is configured
                with a redirect uri of {(trip redirect-uri:oauth2-core)}
            ==
            ;a(href link):  {link}
          ==
        ==
      (emit %http-response %start [%200 ~] bod %.y)
    ::
        [%dns %oauth %result ~]
      =/  code  (~(got by (my params)) %code)
      ::  XX make path configurable
      ::
      =/  hed  [['Location' '/dns/oauth/success'] ~]
      ::  XX y no tisdot tho
      ::
      %-  =<  emit
        %:  emit
          %request
          (http-wire 1 /oauth/access)
          (retrieve-access-token:oauth2-core code)
          *outbound-config:http-client
        ==
      ::  XX don't redirect to success until above request
      ::  comes back successfully
      ::
      [%http-response %start [%301 hed] ~ %.y]
    ::
        [%dns %oauth %success ~]
      =/  bod=(unit octs)
        %-  some
        %-  as-octt:mimes:html
        %-  en-xml:html
        ;html
          ;head
            ;title:  :dns oauth
          ==
          ;body
            ;p: you may close the browser window
            ;p
              ;span:  XX remove me
              ::  XX make path configurable
              ::
              ;a(href "/dns/oauth"):  again
            ==
          ==
        ==
      (emit %http-response %start [%201 ~] bod %.y)
    ==
  ::  +retry: re-attempt http request after timer
  ::
  ++  retry
    |=  =wire
    ^+  this
    ?>  ?=([%try @ @ *] wire)
    =/  try  (slav %ud i.t.wire)
    ?+  t.t.wire
      ~&([%bind %unknown-retry wire] this)
    ::
        [%confirm ~]
      (init try)
    ::
        [%create @ %for @ ~]
      =/  him=ship  (slav %p i.t.t.t.wire)
      =/  for=ship  (slav %p i.t.t.t.t.t.wire)
      (do-create him for try)
    ::
        [%update @ ~]
      =*  page  i.t.t.t.wire
      (update ?~(page ~ `page) try)
    ==
  ::  +pre-init: gross
  ::
  ++  pre-init
    |=  aut=authority
    =.  nam  [aut ~ ~ ~]
    ::  XX move this into the provider interface
    ::
    ?:  ?&  ?=(%gcloud -.pro.aut)
            ?=(~ auth.pro.aut)
        ==
      ~&  %do-the-oauth-thing
      ~&  initial-uri:oauth2-core
      this
    (init 1)
  ::  +init: establish zone authority (request confirmation)
  ::
  ++  init
    |=  try=@ud
    %-  emit
    (request (http-wire try /confirm) zone:(provider aut.nam))
  ::  +update: retrieve existing remote nameserver records
  ::
  ++  update
    |=  [page=(unit @t) try=@ud]
    ^+  this
    =/  =hiss:eyre
      (existing:(provider aut.nam) page)
    =/  =wire
      (http-wire try /update/[?~(page %$ u.page)])
    (emit (request wire hiss))
  ::  +restore: restore existing remote nameserver records
  ::
  ++  restore
    |=  bod=octs
    =+  ^-  [dat=(list [=ship id=@ta tar=target]) page=(unit @t)]
      ::  XX gross
      ::
      =-  ?~(- [~ ~] -)
      (json-octs bod parse-list:(provider aut.nam))
    |-  ^+  this
    ?~  dat
      ?~(page this (update page 1))
    =/  nob=^bound  [now.bow id.i.dat tar.i.dat ~]
    $(dat t.dat, bon.nam (~(put by bon.nam) ship.i.dat nob))
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
    ::  XX save :for relay state?
    ::
    =.  pen.nam  (~(put by pen.nam) him tar)
    (do-create him for 1)
  ::  +do-create: create new or replace existing binding
  ::
  ++  do-create
    |=  [him=ship for=ship try=@ud]
    ^+  this
    =/  pending  (~(get by pen.nam) him)
    ?~  pending
      this
    =*  tar  u.pending
    =/  =wire
      (http-wire try /create/(scot %p him)/for/(scot %p for))
    =/  pre=(unit [id=@ta tar=target])
      =/  bon=(unit ^bound)  (~(get by bon.nam) him)
      ?~(bon ~ `[id.u.bon cur.u.bon])
    =/  req=hiss:eyre
      (create:(provider aut.nam) him tar pre)
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
    |=  [for=ship him=ship id=@ta]
    =/  tar=target  (~(got by pen.nam) him)
    =/  bon=(unit ^bound)
      (~(get by bon.nam) him)
    =/  nob=^bound
      [now.bow id tar ?~(bon ~ [[wen.u.bon cur.u.bon] hit.u.bon])]
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
  :: +request: unauthenticated http request
  ::
  ++  request
    |=  [=wire =hiss:eyre]
    ^-  card
    [%request wire (hiss-to-request:html hiss) *outbound-config:http-client]
  ::  +http-wire: build a wire for a |tell request
  ::
  ++  http-wire
    |=  [try=@ud act=@tas]
    ^-  wire
    /relay/him/(scot %p him)/try/(scot %ud try)/[act]
  ::  +http-cancel: retry canceled http request
  ::
  ++  http-cancel
    |=  =wire
    ^+  this
    ?>  ?=([%try @ @ ~] wire)
    =/  try  (slav %ud i.t.wire)
    =*  act  i.t.t.wire
    ?+  act
      ~&([%tell %unknown-crash act] this)
    ::
        %check-before
      =.  try  +(try)
      (emit (wait (http-wire try %check-before) (min ~h1 (backoff try))))
    ::
        %check-after
      =.  try  +(try)
      (emit (wait (http-wire try %check-after) (min ~h1 (backoff try))))
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
      ?:  (gth try 10)
        (fail %check-before [(sell !>(rep)) ~])
      =.  try  +(try)
      (emit (wait (http-wire try %check-before) (min ~h1 (backoff try))))
    ::  validating an established binding
    ::
        %check-after
      ?:  =(200 p.rep)
        bake
      ::  no max retries, the binding has been created
      ::  XX notify after some number of failures
      ::
      =.  try  +(try)
      (emit (wait (http-wire try %check-after) (min ~h1 (backoff try))))
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
    (check-before 1)
  ::  +check-before: confirm %direct target is accessible
  ::
  ++  check-before
    |=  try=@ud
    ^+  this
    ?>  ?=(^ rel)
    ?>  ?=(%direct -.tar.u.rel)
    ?:  (reserved:eyre p.tar.u.rel)
      (fail %reserved-ip ~)
    =/  =wire  (http-wire try %check-before)
    =/  url=purl:eyre
      :-  [sec=| por=~ host=[%| `@if`p.tar.u.rel]]
      [[ext=`~.udon path=/static] query=~]
    (emit (request wire url %get ~ ~))
  ::  +fail: %direct target is invalid or inaccessible
  ::
  ++  fail
    |=  [err=@tas =tang]
    ^+  this
    ?>  ?=(^ rel)
    ::  XX add failure-specific messages
    ::
    =/  msg
      ?+  err
          'dns binding failed'
      ::
          %check-before
        ?>  ?=(%direct -.tar.u.rel)
        =/  addr  (scot %if p.tar.u.rel)
        %+  rap  3
        :~  'dns binding failed: '
            'unable to reach you at '  addr  ' on port 80, '
            'please confirm or correct your ipv4 address '
            'and re-enter it with :dns|ip'
        ==
      ::
          %reserved-ip
        ?>  ?=(%direct -.tar.u.rel)
        =/  addr  (scot %if p.tar.u.rel)
        (cat 3 'unable to create dns binding for reserved ip address' addr)
      ==
    ::  XX save failed bindings somewhere?
    ::
    %-  =<  emit(rel ~)
        (emit (notify him msg ~))
    (notify our.bow (rap 3 (scot %p him) ' fail: ' err ~) tang)
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
    (check-after(dom.u.rel `dom) 1)
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
      [[ext=`~.udon path=/static] query=~]
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
