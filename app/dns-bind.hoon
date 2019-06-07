/-  *dns-bind, dns, hall
/+  tapp, stdio
::
::  tapp types and boilerplate
::
=>  |%
    ++  collector-app  `dock`[~zod %dns-collector]
    +$  app-state
      $:  %0
          :: nem: authoritative state
          ::
          nem=(unit nameserver)
      ==
    +$  peek-data  _!!
    +$  in-poke-data
      $%  [%dns-authority =authority]
          [%dns-bind =ship =target]
          [%handle-http-request =inbound-request:http-server]
      ==
    +$  out-poke-data
      $%  [%dns-bind =ship =target]
          [%dns-complete =ship =binding:dns]
          [%drum-unlink =dock]
      ==
    +$  in-peer-data
      $%  [%dns-request =request:dns]
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
    ++  tapp-async  tapp-async:tapp
    ++  stdio  (^stdio out-poke-data out-peer-data)
    --
::
::  oauth2 implementation
::
=>  |%
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
              ['scope' (rap 3 (join ' ' scopes.config))]
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
::  helpers
::
=>  |%
    ::  +name: fully-qualified domain name for :ship
    ::
    ++  name
      |=  [=ship =turf]
      (cat 3 (en-turf:html (weld turf /(crip +:(scow %p ship)))) '.')
    ::  +lame: domain name for :ship (without trailing '.')
    ::
    ++  lame
      |=  [=ship =turf]
      (en-turf:html (weld turf /(crip +:(scow %p ship))))
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
    ++  request-retry
      |=  [=hiss:eyre max=@ud limit=@dr]
      =/  m  (async:stdio (unit httr:eyre))
      =/  try=@ud  0
      |-  ^-  form:m
      =*  loop  $
      ?:  =(try max)
        (pure:m ~)
      ;<  ~                     bind:m  (backoff try limit)
      ;<  rep=(unit httr:eyre)  bind:m  (request hiss)
      ::  XX needs a better predicate. LTE will make this easier
      ::
      ?:  &(?=(^ rep) =(200 p.u.rep))
        (pure:m (some u.rep))
      loop(try +(try))
    --
::
::  application actions
::
=>  |%
    ++  confirm-authority
      |=  =authority
      =/  m  (async:stdio ?)
      ^-  form:m
      ;<  rep=(unit httr:eyre)  bind:m
        (request-retry zone:(provider authority) 5 ~m10)
      (pure:m &(?=(^ rep) =(200 p.u.rep)))
    ::
    ++  retrieve-existing
      |=  =authority
      =/  m  (async:stdio (map ship bound))
      ^-  form:m
      =|  existing=(map ship bound)
      =|  next-page=(unit @t)
      ;<  now=@da  bind:m  get-time:stdio
      |-  ^-  form:m
      =*  loop  $
      ;<  rep=(unit httr:eyre)  bind:m
        (request-retry (existing:(provider authority) next-page) 5 ~m10)
      ?:  ?|  ?=(~ rep)
              ?=(~ r.u.rep)
          ==
        (pure:m existing)
      ::
      =*  octs  u.r.u.rep
      =+  ^-  [dat=(list [=ship id=@ta =target]) page=(unit @t)]
        ::  XX gross
        ::
        =-  ?~(- [~ ~] -)
        (json-octs octs parse-list:(provider authority))
      =.  existing
        |-  ^+  existing
        ?~  dat
          existing
        =/  =bound  [now id.i.dat target.i.dat ~]
        $(dat t.dat, existing (~(put by existing) ship.i.dat bound))
      ?~  page
        (pure:m existing)
      loop(next-page page)
    ::
    ++  create-binding
      |=  [=authority =ship =target existing=(unit bound)]
      =/  m  (async:stdio (unit bound))
      ^-  form:m
      ?:  &(?=(^ existing) =(target cur.u.existing))
        ~|  %bind-duplicate-wat-do  !!
      ::
      =/  pre=(unit [@ta ^target])
        ?~(existing ~ (some [id cur]:u.existing))
      ;<  rep=(unit httr:eyre)  bind:m
        (request (create:(provider authority) ship target pre))
      ::  XX retryable?
      ::
      ?.  &(?=(^ rep) =(200 p.u.rep))
        (pure:m ~)
      ::
      =*  httr  u.rep
      =/  id=@ta
        ?.  ?=(%fcloud -.pro.authority)  ~.
        ?.  ?=(^ r.httr)
          ~|  [%authority-create-confirm-id rep]  !!
        =/  dat=(unit [^ship id=@ta ^target])
          (json-octs u.r.httr parse-record:(provider authority))
        ?~(dat ~. id.u.dat)
      ::
      =/  =address:dns
        ?>(?=(%direct -.target) +.target)
      =/  =turf
        (weld dom.authority /(crip +:(scow %p ship)))
      ;<  ~           bind:m  (poke-app:stdio collector-app [%dns-complete ship address turf])
      ;<  now=@da     bind:m  get-time:stdio
      =/  =bound
        [now id target ?~(existing ~ [[wen cur] hit]:u.existing)]
      (pure:m (some bound))
    ::
    ++  initialize-authority
      |=  [aut=authority state=app-state]
      =/  m  tapp-async
      ^-  form:m
      ?>  ?=(^ nem.state)
      =*  nam  u.nem.state
      ;<  good=?  bind:m  (confirm-authority aut)
      ?.  good
        ~&  %dns-authority-failed
        (pure:m state(nem ~))
      ::
      ::  XX wait-effect
      ::
      ;<  existing=(map ship bound)  bind:m  (retrieve-existing aut)
      =.  bon.nam  (~(uni by bon.nam) existing)
      =.  nem.state  (some nam)
      ::
      ::  XX wait-effect
      ::
      ;<  ~       bind:m  (peer-app:stdio collector-app /requests)
      (pure:m state)
    --
::
::  |oauth2-core: configured oauth functionality (for |gcloud only)
::
=>  |%
    ++  oauth2-core
      |=  =bowl:gall
      =/  =oauth2-config
        :*  auth-url='https://accounts.google.com/o/oauth2/v2/auth'
            exchange-url='https://www.googleapis.com/oauth2/v4/token'
            domain=/com/googleapis
            redirect-path=/dns/oauth
            initial-path=/dns/oauth/result
            :~  'https://www.googleapis.com/auth/ndev.clouddns.readwrite'
                'https://www.googleapis.com/auth/cloud-platform.read-only'
        ==  ==
      ~(. oauth2 our.bowl now.bowl oauth2-config)
    --
::
::  the app itself
::
=*  default-tapp  default-tapp:tapp
%-  create-tapp-all:tapp
^-  tapp-core-all:tapp
|_  [=bowl:gall state=app-state]
::
++  handle-peek  handle-peek:default-tapp
++  handle-peer  handle-peer:default-tapp
::
++  handle-init
  =/  m  tapp-async
  ^-  form:m
  ;<  ~  bind:m  (poke-app:stdio [[our %hood] [%drum-unlink our dap]]:bowl)
  (pure:m state)
::
++  handle-poke
  |=  =in-poke-data
  =/  m  tapp-async
  ^-  form:m
  ?.  (team:title [our src]:bowl)
    ~|  %bind-yoself  !!
  ?-  -.in-poke-data
  ::
      %dns-authority
    ?.  =(~ nem.state)
      ~|  %authority-reset-wat-do  !!
    =*  aut  authority.in-poke-data
    =/  nam=nameserver  [aut ~ ~]
    =.  nem.state  (some nam)
    ::  XX move this into the provider interface
    ::
    ?:  ?&  ?=(%gcloud -.pro.aut)
            ?=(~ auth.pro.aut)
        ==
      ~&  %do-the-oauth-thing
      ~&  initial-uri:(oauth2-core bowl)
      (pure:m state)
    ::
    (initialize-authority aut state)
  ::
      %dns-bind
    ?~  nem.state
      ~|  %bind-not-authority  !!
    =*  nam  u.nem.state
    =*  who  ship.in-poke-data
    =*  tar  target.in-poke-data
    ?.  ?=(%indirect -.tar)
      ~|  %indirect-unsupported  !!
    ::  defer %indirect where target isn't yet bound
    ::
    :: ?:  ?&  ?=(%indirect -.tar)
    ::         !(~(has by bon.nam) p.tar)
    ::     ==
    ::   =.  dep.nam  (~(put ju dep.nam) p.tar [who tar])
    ::   =.  nem.state  (some nam)
    ::   (pure:m state)
    =/  existing  (~(get by bon.nam) who)
    ;<  new=(unit bound)  bind:m  (create-binding aut.nam who tar existing)
    ?~  new
      ~&  [%bind-failed in-poke-data]
      (pure:m state)
    =.  bon.nam  (~(put by bon.nam) who u.new)
    =.  nem.state  (some nam)
    ::
    ::  XX wait-effect
    ::
    =/  dep=(list [=ship =target])
      ~(tap in (~(get ju dep.nam) who))
    |-  ^-  form:m
    =*  loop  $
    ?~  dep
      =.  dep.nam  (~(del by dep.nam) who)
      =.  nem.state  (some nam)
      (pure:m state)
    ;<  ~  bind:m  (poke-app:stdio [our dap]:bowl [%dns-bind ship target]:i.dep)
    loop(dep t.dep)
  ::
      %handle-http-request
    ::  XX maybe always (set-raw-contract %request) so transaction failure is captured?
    ::
    =*  inbound-request  inbound-request.in-poke-data
    ?~  nem.state
      ~&  :*  %not-an-authority
              %http-request
              =>  inbound-request
              [authenticated secure address [method url]:request]
          ==
      ;<  ~  bind:m  (send-effect:stdio [%http-response %start [%403 ~] ~ %.y])
      (pure:m state)
    ::
    =*  nam  u.nem.state
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
      ;<  ~  bind:m  (send-effect:stdio [%http-response %start [%404 ~] ~ %.y])
      (pure:m state)
    ::
        [%dns %oauth ~]
      =/  link  (trip redirect-to-provider:(oauth2-core bowl))
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
                with a redirect uri of {(trip redirect-uri:(oauth2-core bowl))}
            ==
            ;a(href link):  {link}
          ==
        ==
      ;<  ~  bind:m  (send-effect:stdio [%http-response %start [%200 ~] bod %.y])
      (pure:m state)
    ::
        [%dns %oauth %result ~]
      =/  code  (~(got by (my params)) %code)
      ::  XX make path configurable
      ::
      =/  hed  [['Location' '/dns/oauth/success'] ~]
      ::
      ;<  ~                                       bind:m
        (send-request:stdio (retrieve-access-token:(oauth2-core bowl) code))
      ;<  rep=(unit client-response:http-client)  bind:m
        take-maybe-response:stdio
      ::  XX  retry
      ::
      ?>  ?&  ?=(^ rep)
              ?=(%finished -.u.rep)
              ?=(^ full-file.u.rep)
          ==
      =/  data  (parse-token-response:oauth2 data.u.full-file.u.rep)
      =.  auth.pro.aut.nam  (some [access refresh]:(need data))
      =.  nem.state  (some nam)
      ::  XX  use expiry to set refresh timer
      ::
      ::  XX may need to send this as a card so we don't wait
      ::
      ;<  ~  bind:m  (send-effect:stdio [%http-response %start [%301 hed] ~ %.y])
      (initialize-authority aut.nam state)
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
      ;<  ~  bind:m  (send-effect:stdio %http-response %start [%201 ~] bod %.y)
      (pure:m state)
    ==
  ==
::
++  handle-diff
  |=  [=dock =path =in-peer-data]
  =/  m  tapp-async
  ^-  form:m
  ?.  =(dock collector-app)
    (pure:m state)
  =*  req  request.in-peer-data
  =/  =target  [%direct address.req]
  ;<  ~  bind:m  (poke-app:stdio [our dap]:bowl [%dns-bind ship.req target])
  (pure:m state)
::
++  handle-take
  |=  =sign:tapp
  =/  m  tapp-async
  ^-  form:m
  ?.  ?=(%quit -.sign)
    ::  XX handle stuff
    ::
    (pure:m state)
  ::
  ?.  ?&  =(dock.sign collector-app)
          =(path.sign /requests)
      ==
    ~&  [%unexpected-quit-wat-do [dock path]:sign]
    (pure:m state)
  ::
  ;<  ~  bind:m  (peer-app:stdio collector-app /requests)
  (pure:m state)
--
