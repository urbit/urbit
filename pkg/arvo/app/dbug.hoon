::  dbug: debug dashboard server
::
/-  spider
/+  server, default-agent, verb, dbug
::
|%
+$  state-0  [%0 passcode=(unit @t)]
+$  card  card:agent:gall
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
    ?.  ?=([%e %bound *] sign-arvo)
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
    %-  json-response:gen
    =,  html
    (as-octt:mimes (en-json u.json))
  =,  enjs:format
  ?+  site  ~
    ::  /app.json: {appname: running, ...}
    ::
      [%app ~]
    %-  some
    %-  pairs
    %+  turn  all:apps
    |=  app=term
    [app b+(running:apps app)]
  ::
    ::  /app/[appname].json: {state: }
    ::
      [%app @ ~]
    =*  app  i.t.site
    ::TODO  ?.  (dbugable:apps app)  ~
    %-  some
    %-  pairs
    :~  :-  'state'
        (tank (sell (state:apps app)))
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
          ~!  (ship s)
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
      ::
        :-  'action'
        :-  %s
        ?+  -.action  -.action
          %gen  :((cury cat 3) '+' (spat [desk path]:generator.action))
          %app  (cat 3 ':' app.action)
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
    ^-  (list term)
    %+  murn
      (scry (list path) %ct %home /app)
    |=  =path
    ^-  (unit term)
    ?.  ?=([%app @ %hoon ~] path)  ~
    `i.t.path
  ::
  ++  running
    |=  app=term
    (scry ? %gu app ~)
  ::
  ++  dbugable
    |=  app=term
    ^-  ?
    !!  ::TODO  how to check if it supports the /dbug scries?
  ::
  ++  state
    |=  app=term
    (scry-dbug vase app /dbug/state)
  ::
  ++  subscriptions
    =,  gall
    |=  app=term
    ^-  [out=boat in=bitt]
    (scry-dbug ,[boat bitt] app /dbug/subscriptions)
  ::
  ++  scry-dbug
    |*  [=mold app=term =path]
    (scry mold %gx app (snoc `^path`path %noun))
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
    (scry (map ship ?(%alien %known)) %a %peers ~)
  ::
  ++  peer
    |=  who=ship
    (scry ship-state:ames %a %peer /(scot %p who))
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
      ==
    ::
    ::  json for known peer is structured to closely match the peer-state type.
    ::  where an index is specified, the array is generally sorted by those.
    ::
    ::  { life: 123,
    ::    route: { direct: true, lane: 'something' },
    ::    qos: { kind: 'status', last-contact: 123456 },  // ms timestamp
    ::    snd: [
    ::      { bone: 123,  // index
    ::        duct: ['/paths', ...]
    ::        current: 123,
    ::        next: 123,
    ::        unsent-messages: [123, ...],  // size in bytes
    ::        queued-message-acks: [{
    ::          message-num: 123,  // index
    ::          ack: 'ok'
    ::        }, ...],
    ::        packet-pump-state: {
    ::          next-wake: 123456,  // ms timestamp
    ::          live: [{
    ::            message-num: 123,  // index
    ::            fragment-num: 123,  // index
    ::            num-fragments: 123,
    ::            last-sent: 123456,  //  ms timestamp
    ::            retries: 123,
    ::            skips: 123
    ::          }, ...],
    ::          metrics: {
    ::            rto: 123,  // seconds
    ::            rtt: 123,  // seconds
    ::            rttvar: 123,
    ::            ssthresh: 123,
    ::            num-live: 123,
    ::            cwnd: 123,
    ::            counter: 123
    ::          }
    ::        }
    ::      },
    ::    ...],
    ::    rcv: [
    ::      { bone: 123,  // index
    ::        duct: ['/paths', ...]  // index
    ::        last-acked: 123,
    ::        last-heard: 123,
    ::        pending-vane-ack: [123, ...],
    ::        live-messages: [{
    ::          message-num: 123,  // index
    ::          num-received: 122,
    ::          num-fragments: 123,
    ::          fragments: [123, ...]
    ::        }, ...],
    ::        nax: [123, ...]
    ::      },
    ::    ...],
    ::    nax: [{
    ::      bone: 123,  // index
    ::      duct: ['/paths', ...],
    ::      message-num: 123
    ::    }, ...],
    ::    heeds: [['/paths', ...] ...]
    ::  }
    ::
    ++  known
      |=  peer-state
      %-  pairs
      :~  'life'^(numb life)
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
                ?~  l=((soft ,[=@tas =@if =@ud]) (cue p.lane))
                  s+(scot %x p.lane)
                =,  u.l
                (tape "%{(trip tas)}, {(scow %if if)}, {(scow %ud ud)}")
              ==
          ==
        ::
          :-  'qos'
          %-  pairs
          :~  'kind'^s+-.qos
              'last-contact'^(time last-contact.qos)
          ==
        ::
          :-  'snd'
          :-  %a
          %+  turn  (sort ~(tap by snd) vor)  ::  sort by bone
          (cury snd-with-bone ossuary)
        ::
          :-  'rcv'
          :-  %a
          %+  turn  (sort ~(tap by rcv) vor)  ::  sort by bone
          (cury rcv-with-bone ossuary)
        ::
          :-  'nax'
          :-  %a
          %+  turn  (sort ~(tap in nax) vor)  ::  sort by bone
          |=  [=bone =message-num]
          %-  pairs
          :*  'message-num'^(numb message-num)
              (bone-to-pairs bone ossuary)
          ==
        ::
          'heeds'^(set-array heeds from-duct)
      ==
    ::
    ++  snd-with-bone
      |=  [=ossuary =bone message-pump-state]
      ^-  json
      %-  pairs
      :*  'current'^(numb current)
          'next'^(numb next)
        ::
          :-  'unsent-messages'  ::  as byte sizes
          (set-array unsent-messages (cork (cury met 3) numb))
        ::
          'unsent-fragments'^(numb (lent unsent-fragments))  ::  as lent
        ::
          :-  'queued-message-acks'
          :-  %a
          %+  turn  (sort ~(tap by queued-message-acks) vor)  ::  sort by msg nr
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
              %+  turn  (sort ~(tap in live) vor)  ::  sort by msg nr & frg nr
              |=  [live-packet-key live-packet-val]
              %-  pairs
              :~  'message-num'^(numb message-num)
                  'fragment-num'^(numb fragment-num)
                  'num-fragments'^(numb num-fragments)
                  'last-sent'^(time last-sent)
                  'retries'^(numb retries)
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
                  'num-live'^(numb num-live)
                  'cwnd'^(numb cwnd)
                  'counter'^(numb counter)
              ==
          ==
        ::
          (bone-to-pairs bone ossuary)
      ==
    ::
    ++  rcv-with-bone
      |=  [=ossuary =bone message-sink-state]
      ^-  json
      %-  pairs
      :*  'last-acked'^(numb last-acked)
          'last-heard'^(numb last-heard)
        ::
          :-  'pending-vane-ack'
          =-  a+(turn - numb)
          (sort (turn ~(tap in pending-vane-ack) head) vor)  ::  sort by msg #
        ::
          :-  'live-messages'
          :-  %a
          %+  turn  (sort ~(tap by live-messages) vor)  ::  sort by msg #
          |=  [=message-num partial-rcv-message]
          %-  pairs
          :~  'message-num'^(numb message-num)
              'num-received'^(numb num-received)
              'num-fragments'^(numb num-fragments)
              'fragments'^(set-array ~(key by fragments) numb)
          ==
        ::
          'nax'^a+(turn (sort ~(tap in nax) vor) numb)
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
    --
  --
::
::  behn
::
++  v-behn
  |%
  ++  timers
    (scry ,(list [date=@da =duct]) %b %timers ~)
  --
::
::  clay
::
::TODO  depends on new clay changes (%s care)
++  v-clay
  =,  clay
  |%
  ++  start-path  /(scot %p our.bowl)/home/(scot %da now.bowl)
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
      =+  .^(=dome %cv /(scot %p our.bowl)/[desk]/(scot %da now.bowl))
      =/  =tako  (~(got by hit.dome) let.dome)
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
  |%
  ++  bindings
    =,  eyre
    (scry ,(list [=binding =duct =action]) %e %bindings ~)
  --
::
::  helpers
::
::  +vor: value order
::
::    Orders atoms before cells, and atoms in ascending order.
::
++  vor
  |=  [a=* b=*]
  ^-  ?
  ?:  =(a b)  &
  ?.  ?=(@ a)
    ?:  ?=(@ b)  |
    ?:  =(-.a -.b)
      $(a +.a, b +.b)
    $(a -.a, b -.b)
  ?.  ?=(@ b)  &
  (lth a b)
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
