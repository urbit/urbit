::  dbug: debug dashboard server
::
/-  spider
/+  server, default-agent, verb, dbug
::
|%
+$  state-0  [%0 passcode=(unit @t)]
+$  card     card:agent:gall
--
::
=|  state-0
=*  state  -
::
%+  verb  |
%-  agent:dbug
^-  agent:gall
=<
  |_  =bowl:gall
  +*  this  .
      do    ~(. +> bowl)
      def   ~(. (default-agent this %|) bowl)
  ::
  ++  on-init
    ^-  (quip card _this)
    :_  this
    [%pass /connect %arvo %e %connect [~ /'~debug'] dap.bowl]~
  ::
  ++  on-save  !>(state)
  ::
  ++  on-load
    |=  old=vase
    ^-  (quip card _this)
    [~ this(state !<(state-0 old))]
  ::
  ++  on-watch
    |=  =path
    ^-  (quip card _this)
    ?.  ?=([%http-response *] path)
      (on-watch:def path)
    [~ this]
  ::
  ++  on-poke
    |=  [=mark =vase]
    ^-  (quip card _this)
    ?:  ?=(%noun mark)
      ?>  (team:title [our src]:bowl)
      =/  code  !<((unit @t) vase)
      =/  msg=tape
        ?~  code
          "Removing passcode access for debug interface."
        """
        Enabling passcode access for debug interface. Anyone with this code can
         view your applications' state, the people you've talked to, etc. Only
         share with people you trust. To disable, run :dbug ~
        """
      %-  (slog leaf+msg ~)
      [~ this(passcode code)]
    ?.  ?=(%handle-http-request mark)
      (on-poke:def mark vase)
    =+  !<([eyre-id=@ta =inbound-request:eyre] vase)
    :_  this
    %+  give-simple-payload:app:server  eyre-id
    %+  authorize-http-request:do  inbound-request
    handle-http-request:do
  ::
  ++  on-arvo
    |=  [=wire =sign-arvo]
    ^-  (quip card _this)
    ?.  ?=([%eyre %bound *] sign-arvo)
      (on-arvo:def wire sign-arvo)
    ~?  !accepted.sign-arvo
      [dap.bowl "bind rejected!" binding.sign-arvo]
    [~ this]
  ::
  ++  on-peek   on-peek:def
  ++  on-leave  on-leave:def
  ++  on-agent  on-agent:def
  ++  on-fail   on-fail:def
  --
::
|_  =bowl:gall
::
::  serving
::
++  authorize-http-request
  =,  server
  ::  if no passcode configured, only allow host ship to view
  ::
  ?~  passcode  require-authorization:app
  |=  $:  =inbound-request:eyre
          handler=$-(inbound-request:eyre simple-payload:http)
      ==
  ?:  authenticated.inbound-request
    (handler inbound-request)
  ::  else, allow randos access,
  ::  on the condition they provide a correct ?passcode= url parameter
  ::
  =;  pass=(unit @t)
    ?:  =(passcode pass)
      (handler inbound-request)
    (require-authorization:app inbound-request handler)
  =/  from-url=(unit @t)
    =-  (~(get by -) 'passcode')
    %-  ~(gas by *(map @t @t))
    args:(parse-request-line url.request.inbound-request)
  ?^  from-url  from-url
  ::  try the referer field instead
  ::
  =/  ref-url=(unit @t)
    (get-header:http 'referer' header-list.request.inbound-request)
  ?~  ref-url  ~
  ?~  (find "passcode={(trip u.passcode)}" (trip u.ref-url))  ~
  passcode
::
++  handle-http-request
  =,  server
  |=  =inbound-request:eyre
  ^-  simple-payload:http
  =/  =request-line
    %-  parse-request-line
    url.request.inbound-request
  =*  req-head  header-list.request.inbound-request
  ::TODO  handle POST
  ?.  ?=(%'GET' method.request.inbound-request)
    not-found:gen
  (handle-get-request req-head request-line)
::
++  handle-get-request
  =,  server
  |=  [headers=header-list:http request-line]
  ^-  simple-payload:http
  =?  site  ?=([%'~debug' *] site)  t.site
  ?~  ext
    $(ext `%html, site [%index ~]) ::NOTE  hack
  ::  serve dynamic session.js
  ::
  ?:  =([/js/session `%js] [site ext])
    %-  js-response:gen
    %-  as-octt:mimes:html
    "window.ship = '{(slag 1 (scow %p our.bowl))}';"
  ::  if not json, serve static file
  ::
  ?.  ?=([~ %json] ext)
    =/  file=(unit octs)
      (get-file-at /app/debug site u.ext)
    ?~  file  not-found:gen
    ?+  u.ext  not-found:gen
      %html  (html-response:gen u.file)
      %js    (js-response:gen u.file)
      %css   (css-response:gen u.file)
      %png   (png-response:gen u.file)
    ==
  ::  get data matching the json and convert it
  ::
  =;  json=(unit json)
    ?~  json  not-found:gen
    (json-response:gen u.json)
  =,  enjs:format
  ?+  site  ~
    ::  /apps.json: {appname: running?}
    ::
      [%apps ~]
    %-  some
    %-  pairs
    %+  turn  all:apps
    |=  app=term
    [app b+(running:apps app)]
  ::
    ::  /app/[appname]...
    ::
      [%app @ *]
    =*  app  i.t.site
    ::TODO  ?.  (dbugable:apps app)  ~
    =/  rest=^path  t.t.site
    ?+  rest  ~
      ::  /app/[appname].json: {state: }
      ::
        ~
      %-  some
      %-  pairs
      :~  :-  'simpleState'
          %-  tank
          =;  head=(unit ^tank)
            (fall head leaf+"unversioned")
          ::  try to print the state version
          ::
          =/  version=(unit vase)
            (slew 2 (state:apps app))
          ?~  version  ~
          ?.  ?=(%atom -.p.u.version)  ~
          `(sell u.version)
        ::
          :-  'subscriptions'
          %-  pairs
          =+  (subscriptions:apps app)
          |^  ~['in'^(incoming in) 'out'^(outgoing out)]
          ::
          ++  incoming
            |=  =bitt:gall
            ^-  json
            :-  %a
            %+  turn  ~(tap by bitt)
            |=  [d=duct [s=^ship p=^path]]
            %-  pairs
            :~  'duct'^a+(turn d path)
                'ship'^(ship s)
                'path'^(path p)
            ==
          ::
          ++  outgoing
            |=  =boat:gall
            ^-  json
            :-  %a
            %+  turn  ~(tap by boat)
            |=  [[w=wire s=^ship t=term] [a=? p=^path]]
            %-  pairs
            :~  'wire'^(path w)
                'ship'^(ship s)
                'app'^s+t
                'acked'^b+a
                'path'^(path p)
            ==
          --
      ==
    ::
      ::  /app/[appname]/state.json
      ::  /app/[appname]/state/[query].json
      ::
        [%state ?(~ [@ ~])]
      %-  some
      =-  (pairs 'state'^(tank -) ~)
      %+  state-at:apps  app
      ?~  t.rest  ~
      (slaw %t i.t.rest)
    ==
  ::
    ::  /spider.json
    ::
      [%spider %threads ~]
    %-  some
    ::  turn flat stack descriptors into object (tree) representing stacks
    ::
    |^  (tree-to-json build-thread-tree)
    ::
    +$  tree
      $~  ~
      (map tid:spider tree)
    ::
    ++  build-thread-tree
      %+  roll  tree:threads
      |=  [stack=(list tid:spider) =tree]
      ?~  stack  tree
      %+  ~(put by tree)  i.stack
      %_  $
        stack  t.stack
        tree   (~(gut by tree) i.stack ~)
      ==
    ::
    ++  tree-to-json
      |=  =tree
      o+(~(run by tree) tree-to-json)
    --
  ::
    ::  /azimuth/status
  ::
    ::  /ames/peer.json
    ::
      [%ames %peer ~]
    =/  [known=(list [^ship *]) alien=(list [^ship *])]
      %+  skid  ~(tap by peers:v-ames)
      |=  [^ship kind=?(%alien %known)]
      ?=(%known kind)
    %-  some
    %-  pairs
    ::NOTE  would do (cork head ship) but can't get that to compile...
    :~  'known'^a+(turn (turn known head) ship)
        'alien'^a+(turn (turn alien head) ship)
    ==
  ::
    ::  /ames/peer/[shipname].json
    ::
      [%ames %peer @ ~]
    =/  who=^ship
      (rash i.t.t.site fed:ag)
    %-  some
    =,  v-ames
    (peer-to-json (peer who))
  ::
    ::  /behn/timers.json
    ::
      [%behn %timers ~]
    %-  some
    :-  %a
    %+  turn  timers:v-behn
    |=  [date=@da =duct]
    %-  pairs
    :~  'date'^(time date)
        'duct'^a+(turn duct path)
    ==
  ::
    ::  /clay/commits.json
    ::
      [%clay %commits ~]
    (some commits-json:v-clay)
  ::
    ::  /eyre/bindings.json
    ::
      [%eyre %bindings ~]
    %-  some
    :-  %a
    %+  turn  bindings:v-eyre
    =,  eyre
    |=  [binding =duct =action]
    %-  pairs
    :~  'location'^s+(cat 3 (fall site '*') (spat path))
        'action'^(render-action:v-eyre action)
    ==
  ::
    ::  /eyre/connections.json
    ::
      [%eyre %connections ~]
    %-  some
    :-  %a
    %+  turn  ~(tap by connections:v-eyre)
    |=  [=duct outstanding-connection:eyre]
    %-  pairs
    :~  'duct'^a+(turn duct path)
        'action'^(render-action:v-eyre action)
      ::
        :-  'request'
        %-  pairs
        =,  inbound-request
        :~  'authenticated'^b+authenticated
            'secure'^b+secure
            'source'^s+(scot %if +.address)
            :: ?-  -.address
            ::   %ipv4  %if
            ::   %ipv6  %is
            :: ==
        ==
      ::
        :-  'response'
        %-  pairs
        :~  'sent'^(numb bytes-sent)
          ::
            :-  'header'
            ?~  response-header  ~
            =,  u.response-header
            %-  pairs
            :~  'status-code'^(numb status-code)
              ::
                :-  'headers'
                :-  %a
                %+  turn  headers
                |=([k=@t v=@t] s+:((cury cat 3) k ': ' v))
            ==
        ==
    ==
  ::
    ::  /eyre/authentication.json
    ::
      [%eyre %authentication ~]
    %-  some
    =/  auth  auth-state:v-eyre
    %-  pairs
    :~  :-  'sessions'
        :-  %a
        %+  turn
          %+  sort  ~(tap by sessions.auth)
          |=  [[@uv a=session:eyre] [@uv b=session:eyre]]
          (gth expiry-time.a expiry-time.b)
        |=  [cookie=@uv session:eyre]
        %-  pairs
        :~  'cookie'^s+(scot %uv cookie)
            'identity'^(render-identity:v-eyre identity)
            'expiry'^(time expiry-time)
            'channels'^(numb ~(wyt in channels))
        ==
      ::
        :-  'visitors'
        :-  %a
        %+  turn
          %+  sort  ~(tap by visitors.auth)
          |=  [[@uv a=visitor:eyre] [@uv b=visitor:eyre]]
          ?@  +.a  &
          ?@  +.b  |
          (aor (scot %p ship.a) (scot %p ship.b))
        |=  [nonce=@uv v=visitor:eyre]
        %-  pairs
        :+  'nonce'^s+(scot %uv nonce)
          'duct'^?~(duct.v ~ a+(turn u.duct.v path))
        ?@  +.v  ['sesh' s+(scot %uv sesh.v)]~
        :~  'pend'^b+?=(^ pend.v)
            'ship'^(ship ship.v)
            'last'^s+last.v
            'toke'^?~(toke.v ~ s+(scot %uv u.toke.v))
        ==
      ::
        :-  'visiting'
        :-  %a
        %-  zing
        %+  turn
          %+  sort  ~(tap by visiting.auth)
          |=  [[a=@p *] [b=@p *]]
          (aor (scot %p a) (scot %p b))
        |=  [who=@p q=(qeu @uv) m=(map @uv portkey)]
        %+  turn  ~(tap by m)
        |=  [nonce=@uv p=portkey]
        %-  pairs
        :+  'who'^(ship who)
          'nonce'^s+(scot %uv nonce)
        ?@  p  ['made' (time made.p)]~
        :~  ['pend' b+?=(^ pend.p)]
            ['toke' ?~(toke.p ~ s+(scot %uv u.toke.p))]
        ==
    ==
  ::
    ::  /eyre/channels.json
    ::
      [%eyre %channels ~]
    %-  some
    :-  %a
    =+  channel-state:v-eyre
    %+  turn  ~(tap by session)
    |=  [key=@t channel:eyre]
    %-  pairs
    :~  'session'^s+key
        'identity'^(render-identity:v-eyre identity)
        'connected'^b+!-.state
        'expiry'^?-(-.state %& (time date.p.state), %| ~)
        'next-id'^(numb next-id)
        'last-ack'^(time last-ack)
        'unacked'^a+(turn (sort (turn ~(tap in events) head) dor) numb)
      ::
        :-  'subscriptions'
        :-  %a
        %+  turn  ~(tap by subscriptions)
        |=  [id=@ud [=^ship app=term =^path *]]
        %-  pairs
        :~  'id'^(numb id)
            'ship'^(^ship ship)
            'app'^s+app
            'path'^(^path path)
            'unacked'^(numb (~(gut by unacked) id 0))
        ==
    ==
  ==
::
++  get-file-at
  |=  [base=path file=path ext=@ta]
  ^-  (unit octs)
  ?.  ?=(?(%html %css %js %png) ext)
    ~
  =/  =path
    :*  (scot %p our.bowl)
        q.byk.bowl
        (scot %da now.bowl)
        (snoc (weld base file) ext)
    ==
  ?.  .^(? %cu path)  ~
  %-  some
  %-  as-octs:mimes:html
  .^(@ %cx path)
::
::  applications
::
++  apps
  |%
  ++  all
    ^-  (list dude:gall)
    %-  zing
    ^-  (list (list dude:gall))
    %+  turn
      ~(tap in (scry (set desk) %cd %$ /))
    |=  =desk
    ^-  (list dude:gall)
    =-  (turn ~(tap in -) head)
    ;;  (set [dude:gall ?])  ::TODO  for some reason we need this?
    (scry (set [dude:gall ?]) %ge desk /$)
  ::
  ++  running
    |=  app=term
    (scry ? %gu app /$)
  ::
  ++  dbugable
    |=  app=term
    ^-  ?
    !!  ::TODO  how to check if it supports the /dbug scries?
  ::
  ++  state
    |=  app=term
    ^-  vase
    (scry-dbug vase app /state)
  ::
  ++  state-at
    |=  [app=term what=(unit @t)]
    ^-  tank
    =/  state=vase  (state app)
    ?~  what  (sell state)
    =/  result=(each vase tang)
      %-  mule  |.
      %+  slap
        (slop state !>([bowl=bowl ..zuse]))
      (ream u.what)
    ?-  -.result
      %&  (sell p.result)
      %|  (head p.result)
    ==
  ::
  ++  subscriptions
    =,  gall
    |=  app=term
    ^-  [out=boat in=bitt]
    (scry-dbug ,[boat bitt] app /subscriptions)
  ::
  ++  scry-dbug
    |*  [=mold app=term =path]
    (scry mold %gx app (snoc `^path`[%dbug path] %noun))
  ::
  ::TODO  but why? we can't tell if it's on or not
  ++  poke-verb-toggle
    |=  app=term
    ^-  card
    (poke /verb/[app] app %verb !>(%loud))
  --
::
::  threads
::
++  threads
  |%
  ::NOTE  every (list tid:spider) represents a stack,
  ::      with a unique tid at the end
  ++  tree
    (scry (list (list tid:spider)) %gx %spider /tree/noun)
  ::
  ++  poke-kill
    |=  =tid:spider
    ^-  card
    (poke /spider/kill/[tid] %spider %spider-stop !>([tid |]))
  --
::
::  ames
::
++  v-ames
  |%
  ++  peers
    (scry (map ship ?(%alien %known)) %ax %$ /peers)
  ::
  ++  peer
    |=  who=ship
    (scry ship-state:ames %ax %$ /peers/(scot %p who))
  ::
  ++  peer-to-json
    =,  ames
    =,  enjs:format
    |=  =ship-state
    |^  ^-  json
        %+  frond  -.ship-state
        ?-  -.ship-state
          %alien  (alien +.ship-state)
          %known  (known +.ship-state)
        ==
    ::
    ++  alien
      |=  alien-agenda
      %-  pairs
      :~  'messages'^(numb (lent messages))
          'packets'^(numb ~(wyt in packets))
          'heeds'^(set-array heeds from-duct)
          'keens'^(set-array ~(key by keens) path)
      ==
    ::
    ::  json for known peer is structured to closely match the peer-state type.
    ::  where an index is specified, the array is generally sorted by those.
    ::
    ::  { life: 123,
    ::    rift: 0,
    ::    route: { direct: true, lane: 'something' },
    ::    qos: { kind: 'status', last-contact: 123456 },  // ms timestamp
    ::    flows: { forward: [snd, rcv, ...], backward: [snd, rcv, ...] }
    ::    ->  snd:
    ::        { bone: 123,  // index
    ::          duct: ['/paths', ...]
    ::          current: 123,
    ::          next: 123,
    ::          unsent-messages: [123, ...],  // size in bytes
    ::          queued-message-acks: [{
    ::            message-num: 123,  // index
    ::            ack: 'ok'
    ::          }, ...],
    ::          packet-pump-state: {
    ::            next-wake: 123456,  // ms timestamp
    ::            live: [{
    ::              message-num: 123,  // index
    ::              fragment-num: 123,  // index
    ::              num-fragments: 123,
    ::              last-sent: 123456,  //  ms timestamp
    ::              retries: 123,
    ::              skips: 123
    ::            }, ...],
    ::            metrics: {
    ::              rto: 123,  // seconds
    ::              rtt: 123,  // seconds
    ::              rttvar: 123,
    ::              ssthresh: 123,
    ::              num-live: 123,
    ::              cwnd: 123,
    ::              counter: 123
    ::            }
    ::          }
    ::        }
    ::    ->  rcv:
    ::        { bone: 123,  // index
    ::          duct: ['/paths', ...]  // index
    ::          last-acked: 123,
    ::          last-heard: 123,
    ::          pending-vane-ack: [123, ...],
    ::          live-messages: [{
    ::            message-num: 123,  // index
    ::            num-received: 122,
    ::            num-fragments: 123,
    ::            fragments: [123, ...]
    ::          }, ...],
    ::          nax: [123, ...]
    ::        }
    ::    nax: [{
    ::      bone: 123,  // index
    ::      duct: ['/paths', ...],
    ::      message-num: 123
    ::    }, ...],
    ::    closing: [bone, ..., bone],
    ::    corked: [bone, ..., bone],
    ::    heeds: [['/paths', ...] ...]
    ::    scries:
    ::    ->  { =path
    ::          keen-state: {
    ::            wan: [                  //request packets, sent
    ::              { frag: 1234,
    ::                size: 1234,         // size, in bytes
    ::                last-sent: 123456,  //  ms timestamp
    ::                retries: 123,
    ::                skips: 123
    ::              }, ...
    ::            ],
    ::            nex: [                  //  request packets, unsent
    ::              { frag: 1234,
    ::                size: 1234,         // size, in bytes
    ::                last-sent: 123456,  //  ms timestamp
    ::                retries: 123,
    ::                skips: 123
    ::              }, ...
    ::            ],
    ::            hav: [                  //  response packets, backward
    ::              {fra: 1234,
    ::               meow: { num: 1234, size: 1234}
    ::              }, ...
    ::            ],
    ::            num-fragments: 1234,
    ::            num-received: 1234,
    ::            next-wake: 123456,  // ms timestamp
    ::            listeners: [['/paths', ...] ...],
    ::            metrics: {
    ::              rto: 123,  // seconds
    ::              rtt: 123,  // seconds
    ::              rttvar: 123,
    ::              ssthresh: 123,
    ::              num-live: 123,
    ::              cwnd: 123,
    ::              counter: 123
    ::            }
    ::          }
    ::        }
    ::  }
    ::
    ++  known
      |=  peer-state
      %-  pairs
      :~  'life'^(numb life)
          ::  TODO: needs to be updated in /pkg/interface/dbug
          ::
          'rift'^(numb rift)
        ::
          :-  'route'
          %+  maybe  route
          |=  [direct=? =lane]
          %-  pairs
          :~  'direct'^b+direct
            ::
              :-  'lane'
              ?-  -.lane
                %&  (ship p.lane)
              ::
                  %|
                %-  tape
                =/  ip=@if  (end [0 32] p.lane)
                =/  pt=@ud  (cut 0 [32 16] p.lane)
                "{(scow %if ip)}:{((d-co:co 1) pt)} ({(scow %ux p.lane)})"
              ==
          ==
        ::
          :-  'qos'
          %-  pairs
          :~  'kind'^s+-.qos
              'last-contact'^(time last-contact.qos)
          ==
        ::
          :-  'flows'
          |^  =/  mix=(list flow)
                =-  (sort - dor)
                %+  welp
                  (turn ~(tap by snd) (tack %snd closing corked))
                (turn ~(tap by rcv) (tack %rcv closing corked))
              =/  [forward=(list flow) backward=(list flow)]
                %+  skid  mix
                |=  [=bone *]
                =(0 (mod bone 2))
              %-  pairs
              :~  ['forward' a+(turn forward build)]
                  ['backward' a+(turn backward build)]
              ==
          ::
          +$  flow
            $:  =bone
                closing=?
                corked=?
              ::
                $=  state
                $%  [%snd message-pump-state]
                    [%rcv message-sink-state]
                ==
            ==
          ::
          ++  tack
            |*  [=term closing=(set bone) corked=(set bone)]
            |*  [=bone =noun]
            [bone (~(has in closing) bone) (~(has in corked) bone) [term noun]]
          ::
          ++  build
            |=  flow
            ^-  json
            %+  frond  -.state
            ?-  -.state
              %snd  (snd-with-bone ossuary bone closing corked +.state)
              %rcv  (rcv-with-bone ossuary bone closing corked +.state)
            ==
          --
        ::
          :-  'nax'
          :-  %a
          %+  turn  (sort ~(tap in nax) dor)  ::  sort by bone
          |=  [=bone =message-num]
          %-  pairs
          :*  'message-num'^(numb message-num)
              (bone-to-pairs bone ossuary)
          ==
        ::
          'closing'^(set-array closing numb)
        ::
          'corked'^(set-array corked numb)
        ::
          'heeds'^(set-array heeds from-duct)
        ::
          'scries'^(scries ~(tap by keens))
      ==
    ::
    ++  snd-with-bone
      |=  [=ossuary =bone closing=? corked=? message-pump-state]
      ^-  json
      %-  pairs
      :*  'closing'^b+closing
          'corked'^b+corked
          'current'^(numb current)
          'next'^(numb next)
        ::
          :-  'unsent-messages'  ::  as byte sizes
          (set-array unsent-messages (cork (cury met 3) numb))
        ::
          'unsent-fragments'^(numb (lent unsent-fragments))  ::  as lent
        ::
          :-  'queued-message-acks'
          :-  %a
          %+  turn  (sort ~(tap by queued-message-acks) dor)  ::  sort by msg nr
          |=  [=message-num =ack]
          %-  pairs
          :~  'message-num'^(numb message-num)
              'ack'^s+-.ack
          ==
        ::
          :-  'packet-pump-state'
          %-  pairs
          =,  packet-pump-state
          :~  'next-wake'^(maybe next-wake time)
            ::
              :-  'live'
              :-  %a
              %+  turn  (sort ~(tap in live) dor)  ::  sort by msg nr & frg nr
              |=  [live-packet-key live-packet-val]
              %-  pairs
              :~  'message-num'^(numb message-num)
                  'fragment-num'^(numb fragment-num)
                  'num-fragments'^(numb num-fragments)
                  'last-sent'^(time last-sent)
                  'tries'^(numb tries)
                  'skips'^(numb skips)
              ==
            ::
              :-  'metrics'
              %-  pairs
              =,  metrics
              :~  'rto'^(numb (div rto ~s1))  ::TODO  milliseconds?
                  'rtt'^(numb (div rtt ~s1))
                  'rttvar'^(numb (div rttvar ~s1))
                  'ssthresh'^(numb ssthresh)
                  'num-live'^(numb ~(wyt by live))
                  'cwnd'^(numb cwnd)
                  'counter'^(numb counter)
              ==
          ==
        ::
          (bone-to-pairs bone ossuary)
      ==
    ::
    ++  rcv-with-bone
      |=  [=ossuary =bone closing=? corked=? message-sink-state]
      ^-  json
      %-  pairs
      :*  'closing'^b+closing
          'corked'^b+corked
          'last-acked'^(numb last-acked)
          'last-heard'^(numb last-heard)
        ::
          :-  'pending-vane-ack'
          =-  a+(turn - numb)
          (sort (turn ~(tap in pending-vane-ack) head) dor)  ::  sort by msg #
        ::
          :-  'live-messages'
          :-  %a
          %+  turn  (sort ~(tap by live-messages) dor)  ::  sort by msg #
          |=  [=message-num partial-rcv-message]
          %-  pairs
          :~  'message-num'^(numb message-num)
              'num-received'^(numb num-received)
              'num-fragments'^(numb num-fragments)
              'fragments'^(set-array ~(key by fragments) numb)
          ==
        ::
          'nax'^a+(turn (sort ~(tap in nax) dor) numb)
        ::
          (bone-to-pairs bone ossuary)
      ==
    ::
    ++  bone-to-pairs
      |=  [=bone ossuary]
      ^-  (list [@t json])
      :~  'bone'^(numb bone)
          'duct'^(from-duct (~(gut by by-bone) bone ~))
      ==
    ::
    ++  maybe
      |*  [unit=(unit) enjs=$-(* json)]
      ^-  json
      ?~  unit  ~
      (enjs u.unit)
    ::
    ++  set-array
      |*  [set=(set) enjs=$-(* json)]
      ^-  json
      a+(turn ~(tap in set) enjs)
    ::
    ++  from-duct
      |=  =duct
      a+(turn duct path)
    ::
    ++  scries
      |=  keens=(list [^path keen-state])
      ^-  json
      :-  %a
      %+  turn  keens
      |=  [=^path keen=keen-state]
      %-  pairs
      :~  'scry-path'^(^path path)
          'keen-state'^(parse-keens keen)
      ==
    ::
    ++  parse-keens
      |=  keen-state
      |^  ^-  json
      %-  pairs
      :~  :-  %wan
          a/(turn (tap:((on @ud want) lte) wan) |=([@ a=_+6:wants] (wants a)))
        ::
          'nex'^a/(turn nex wants)
        ::
          :-  'hav'
          :-  %a
          %+  turn  hav
          |=  [fra=@ud meow]
          %-  pairs
          :~  'fra'^(numb fra)
            ::
              :-  'meow'
              %-  pairs
              :~  'num'^(numb num)
                  'size'^(numb (met 3 dat))
          ==  ==
        ::
          'num-fragments'^(numb num-fragments)
          'num-received'^(numb num-received)
          'next-wake'^(maybe next-wake time)
          'listeners'^(set-array listeners from-duct)
        ::
          ::  XX  refactor (see metric in snd-with-bone)
          :-  'metrics'
          %-  pairs
          =,  metrics
          :~  'rto'^(numb (div rto ~s1))  ::TODO  milliseconds?
              'rtt'^(numb (div rtt ~s1))
              'rttvar'^(numb (div rttvar ~s1))
              'ssthresh'^(numb ssthresh)
              'cwnd'^(numb cwnd)
              'counter'^(numb counter)
      ==  ==
      ::
      ++  wants
        |=  [fra=@ud =hoot packet-state]
        %-  pairs
        :~  'frag'^(numb fra)
            'size'^(numb (met 3 hoot))
            'last-sent'^(time last-sent)
            'tries'^(numb tries)
            'skips'^(numb skips)
        ==
      --
    --
  --
::
::  behn
::
++  v-behn
  |%
  ++  timers
    (scry ,(list [date=@da =duct]) %bx %$ /debug/timers)
  --
::
::  clay
::
::TODO  depends on new clay changes (%s care)
++  v-clay
  =,  clay
  |%
  ++  start-path  /(scot %p our.bowl)/base/(scot %da now.bowl)
  ::
  +$  commit
    [=tako parents=(list tako) children=(list tako) wen=@da content-hash=@uvI]
  ::
  ++  commits-json
    ^-  json
    =+  .^(desks=(set desk) %cd start-path)
    =/  heads=(list [tako desk])
      %+  turn  ~(tap in desks)
      |=  =desk
      =+  .^(=domo %cv /(scot %p our.bowl)/[desk]/(scot %da now.bowl))
      =/  =tako  (~(got by hit.domo) let.domo)
      [tako desk]
    =/  yakis=(set yaki)
      %-  silt
      ^-  (list yaki)
      %-  zing
      %+  turn  heads
      |=  [=tako =desk]
      (trace-tako tako)
    =/  commits=(list commit)  (yakis-to-commits ~(tap in yakis))
    =,  enjs:format
    %:  pairs
      head+(pairs (turn heads |=([=tako =desk] (scot %uv tako)^s+desk)))
      commits+(commits-to-json commits)
      ~
    ==
  ::
  ++  yakis-to-commits
    |=  yakis=(list yaki)
    ^-  (list commit)
    %+  turn  yakis
    |=  =yaki
    :*  r.yaki  p.yaki
        =/  candidates
          %+  turn
            (skim yakis |=(can=^yaki (lien p.can |=(=tako =(r.yaki tako)))))
          |=  can=^yaki
          r.can
        ~(tap in (silt candidates))
        t.yaki
        .^(@uvI %cs (weld start-path /hash/(scot %uv r.yaki)))
    ==
  ::
  ++  trace-tako
    |=  =tako
    ~+
    ^-  (list yaki)
    =+  .^(=yaki %cs (weld start-path /yaki/(scot %uv tako)))
    :-  yaki
    (zing (turn p.yaki trace-tako))
  ::
  ++  commits-to-json
    |=  commits=(list commit)
    ^-  json
    :-  %a
    %+  turn
      %+  sort  commits
      |=  [a=commit b=commit]
      (gte wen.a wen.b)
    |=  =commit
    (commit-to-json commit)
  ::
  ++  commit-to-json
    |=  =commit
    ^-  json
    =,  enjs:format
    %:  pairs
      'commitHash'^(tako-to-json tako.commit)
      parents+a+(turn parents.commit tako-to-json)
      children+a+(turn children.commit tako-to-json)
      'contentHash'^(tako-to-json content-hash.commit)
      ~
    ==
  ::
  ++  tako-to-json
    |=  =tako
    ^-  json
    s+(scot %uv tako)
  --
::
::  eyre
::
++  v-eyre
  =,  eyre
  |%
  ++  bindings
    (scry ,(list [=binding =duct =action]) %e %bindings ~)
  ::
  ++  connections
    (scry ,(map duct outstanding-connection) %e %connections ~)
  ::
  ++  auth-state
    (scry authentication-state %e %authentication-state ~)
  ::
  ++  channel-state
    (scry ^channel-state %e %channel-state ~)
  ::
  ++  render-identity
    |=  =identity
    ^-  json
    %-  ship:enjs:format
    ?-  -.identity
      %ours  our.bowl
      %fake  who.identity
      %real  who.identity
    ==
  ::
  ++  render-action
    |=  =action
    ^-  json
    :-  %s
    ?+  -.action  -.action
      %gen  :((cury cat 3) '+' (spat [desk path]:generator.action))
      %app  (cat 3 ':' app.action)
    ==
  --
::
::  helpers
::
++  poke
  |=  [=wire app=term =mark =vase]
  ^-  card
  [%pass wire %agent [our.bowl app] %poke mark vase]
::
++  scry
  |*  [=mold care=term =desk =path]
  .^(mold care (scot %p our.bowl) desk (scot %da now.bowl) path)
--
