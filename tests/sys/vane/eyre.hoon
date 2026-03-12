/+  *test
/=  eyre-raw  /sys/vane/eyre
::
::NOTE  some points about the testing style/structure here
::
::  the tests are written in monadic continuation-passing style with the help
::  of the ;< rune. this tests file is the first place to use that pattern for
::  tests, and it has grown/evolved organically since then. please forgive the
::  rough edges.
::
::  in addition to tracking the time and full state of the eyre core, we also
::  track the active cookie string (sesh.state). +call and +take put their
::  resulting moves through +read-moves in order to figure out if/how to update
::  the sesh. we try to roughly mimick the browser behavior, where a cookie
::  either gets set, or is made to expire. (we don't pro-actively expire the
::  sesh value based on +wait calls.)
::
::  both simulated requests, and tests for responses are augmented based on the
::  sesh. if we have it, we'll inject it as a 'cookie' header into simulated
::  requests, and expect it to be re-set in a 'set-cookie' header for the
::  responses that we test. (if you manually provide 'cookie' or 'set-cookie'
::  headers, the utilities will respect those instead.)
::  (for response testers, this architecture unfortunately means we need to
::  construct them inside ;< blocks, because we need to grab the sesh value
::  from the continuation. less ergonomic than desired, to be improved...)
::
::  note that we only ever track one active cookie at a time, because we assume
::  the tests only ever interact with the one eyre as its one client. for
::  complicated multi-party tests, you would have to manually juggle the
::  cookies, or further complicate the mechanism here.
::
::  when invoking +make-ex- arms, we do so _before_ running the call that will
::  produce the moves you want to test. otherwise, the +make-ex- arm may
::  construct the test based on the information it learned from the moves
::  emitted by the call, leading to always-passing tests.
::
::  because we have a fair amount of things about test details that change only
::  very rarely, we use the "optional args in the gate context" pattern in a
::  number of places. (see, for example, +ex-start-response.)
::  unfortunately, for indirect calls/constructors, this means we have to take
::  care to thread even the optional args all the way through.
::
!:
=/  eyre-gate  (eyre-raw ~nul)
=/  eyre-id  '~.eyre_0v4.elsnk.20412.0h04v.50lom.5lq0o'
::
::  mare: a monad for testing eyre
::
|%
::
++  form-raw
  |$  [a]
  $-(state (output-raw a))
::
++  state
  $:  gate=_eyre-gate
      now=@da
      guest=_&        ::  false if we intenionally authed as a real ship
      sesh=(unit @t)  ::  last-heard cookie string, as 'urbauth-~xxx=0vyyy'
  ==
::
++  output-raw
  |$  [a]
  (each [out=a =state] tang)
::
++  mare
  |*  a=mold
  |%
  ++  ouptut  (output-raw a)
  ++  form  (form-raw a)
  ++  easy
    |=  g=$-(state a)
    ^-  form
    |=  =state
    [%& (g state) state]
  ++  pure
    |=  arg=a
    ^-  form
    |=  =state
    [%& arg state]
  ::
  ++  bind
    |*  b=mold
    |=  [m-b=(form-raw b) fun=$-(b form)]
    ^-  form
    |=  =state
    =/  b-res=(output-raw b)  (m-b state)
    ?-  -.b-res
      %&  ((fun out.p.b-res) state.p.b-res)
      %|  [%| p.b-res]
    ==
  --
::
::  mario: helpers for mare
::
++  move  move:eyre-gate
::  advance time
::
++  wait
  |=  =@dr
  =/  m  (mare ,~)
  ^-  form:m
  |=  =state
  [%& ~ state(now (add now.state dr))]
::
++  get-now
  =/  m  (mare ,@da)
  ^-  form:m
  |=  =state
  [%& now.state state]
::  raise failure
::
++  fail
  |=  =tang
  |=  =state
  [%| tang]
::  fail if tang is non-null
::
++  try
  |=  =tang
  =/  m  (mare ,~)
  ^-  form:m
  ?~  tang
    (pure:m ~)
  (fail tang)
::
::  stupid way to normalize header order while maintaining ordering for
::  headers whose field-name occurs multiple times
++  header-sort
  |=  heads=header-list:http
  %+  roll  heads
  |=  [head=[k=@t v=@t] heads=header-list:http]
  ?~  heads  [head]~
  ?:  =(k.head key.i.heads)     [i.heads $(heads t.heads)]
  ?.  (aor k.head key.i.heads)  [i.heads $(heads t.heads)]
  [head heads]
::
++  set-guest
  |=  guest=?
  =/  m  (mare ,~)
  ^-  form:m
  |=  =state
  [%& ~ state(guest guest)]
::
++  get-token
  %-  easy:(mare ,@t)
  |=  =state
  (rsh 3^13 (need sesh.state))
::
++  get-cookie
  %-  easy:(mare ,@t)
  |=  =state
  (need sesh.state)
::
++  guest-time  '604800'
++  auth-time   '2592000'
::
++  expected-cookie
  =/  next=@t  g-cook
  |=  live=?
  %-  easy:(mare ,@t)
  |=  =state
  %+  rap  3
  :~  (fall sesh.state next)
      '; Path=/; Max-Age='
      ?:(live ?:(guest.state guest-time auth-time) '0')
  ==
::
++  read-moves
  |=  [moves=(list move) =state]
  ^+  state
  =-  state(sesh -)
  %+  roll  moves
  |=  [=move =_sesh.state]
  ?.  ?=([* %give %response %start *] move)
    sesh
  =/  soot=(unit @t)
    %+  get-header:http  'set-cookie'
    headers.response-header.http-event.p.card.move
  ?~  soot  sesh
  =;  [who=@p ses=@uv end=?]
    ?:  end  ~
    `(rap 3 'urbauth-' (scot %p who) '=' (scot %uv ses) ~)
  %+  rash  u.soot
  ;~  plug
    ;~(pfix (jest 'urbauth-~') fed:ag)
    ;~(pfix (jest '=0v') viz:ag)
    ;~  pfix  (jest '; Path=/; Max-Age=')
      ;~  pose
        (cold & (just '0'))
        (cold | dum:ag)
      ==
    ==
  ==
::
++  call
  |=  [=duct wrapped-task=(hobo task:eyre-gate)]
  =/  m  (mare ,(list move))
  ^-  form:m
  |=  =state
  =/  eyre-core
    %:  gate.state
        now=now.state
        eny=`@uvJ`0xdead.beef
        scry=scry-provides-code
    ==
  =^  moves  gate.state
    (call:eyre-core duct ~ wrapped-task)
  [%& moves (read-moves moves state)]
::
++  take
  |=  [=wire =duct =sign:eyre-gate]
  =/  m  (mare ,(list move))
  ^-  form:m
  |=  =state
  =/  eyre-core
    %:  gate.state
        now=now.state
        eny=`@uvJ`0xdead.beef
        scry=scry-provides-code
    ==
  =^  moves  gate.state
    (take:eyre-core wire duct ~ sign)
  [%& moves (read-moves moves state)]
::
++  call-request
  |=  [=duct =method:http url=@t =header-list:http body=(unit @t)]
  =/  m  (mare ,(list move))
  ^-  form:m
  |=  =state
  %.  state
  ::  if there is no cookie in the request, but we have one in state,
  ::  inject it.
  ::
  =?  header-list
      ?&  ?=(^ sesh.state)
          ?=(~ (get-header:http 'cookie' header-list))
      ==
    [['cookie' u.sesh.state] header-list]
  =/  body  (bind body as-octs:mimes:html)
  %+  call  duct
  [%request %.n [%ipv4 .192.168.1.1] [method url header-list body]]
::
++  get
  |=  [url=@t =header-list:http]
  =/  m  (mare ,(list move))
  ^-  form:m
  %+  call-request  ~[/http-blah]
  [%'GET' url header-list ~]
::
++  post
  |=  [url=@t =header-list:http body=@t]
  =/  m  (mare ,(list move))
  ^-  form:m
  %+  call-request  ~[/http-blah]
  [%'POST' url header-list `body]
::
++  put
  |=  [url=@t =header-list:http body=@t]
  =/  m  (mare ,(list move))
  ^-  form:m
  %+  call-request  ~[/http-blah]
  [%'PUT' url header-list `body]
::  use different wire
::
++  put-2
  |=  [url=@t =header-list:http body=@t]
  =/  m  (mare ,(list move))
  ^-  form:m
  %+  call-request  ~[/http-put-request]
  [%'PUT' url header-list `body]
::
++  connect
  |=  [app=@t pax=path]
  =/  m  (mare ,~)
  ^-  form:m
  ;<  mos=(list move)  bind:m  (call ~[/[app]] [%connect [~ pax] app])
  (expect-moves mos (ex ~[/[app]] %give %bound %.y [~ pax]) ~)
::
++  request
  |=  [app=@t pax=path]
  =/  m  (mare ,~)
  =/  target  (crip (spud pax))
  ;<  mos=(list move)  bind:m  (get target ~)
  =/  mov-1
    %^  ex-gall-deal  /watch-response/[eyre-id]  g-name
    [app %watch /http-response/[eyre-id]]
  =/  mov-2
    =/  response  !>([eyre-id %.n %.n [%ipv4 .192.168.1.1] [%'GET' target ~ ~]])
    %^  ex-gall-deal  /run-app-request/[eyre-id]  g-name
    [app %poke %handle-http-request response]
  (expect-moves mos mov-1 mov-2 ~)
::
++  expect-moves
  |=  [mos=(list move) exes=(list $-(move tang))]
  =/  m  (mare ,~)
  ^-  form:m
  |=  =state
  =/  =tang
    |-  ^-  tang
    ?~  exes
      ?~  mos
        ~
      ['got more moves than expected' ~]
    ?~  mos
      ['expected more moves than got' ~]
    %+  weld
      (i.exes i.mos)
    $(exes t.exes, mos t.mos)
  ?~  tang
    [%& ~ state]
  [%| tang]
::
++  ex-set-config
  |=  =http-config:eyre
  |=  mov=move
  ^-  tang
  (expect-eq !>([duct=~[/unix] %give %set-config http-config]) !>(mov))
::
++  ex
  |=  mow=move
  |=  mov=move
  (expect-eq !>(mow) !>(mov))
::
++  ex-rest
  |=  [=wire =@da]
  (ex ~[/http-blah] %pass wire %b %rest da)
::
++  ex-wait
  |=  [=wire =@da]
  (ex ~[/http-blah] %pass wire %b %wait da)
::
++  ex-sessions
  |=  tokens=(set @t)
  |=  mov=move
  ^-  tang
  (expect-eq !>([duct=~[/unix] %give %sessions tokens]) !>(mov))
::
++  make-ex-resp  ::  auto-fill 'set-cookie' header from observed value
  =/  =duct  [/http-blah ~]
  |=  [status=@ud headers=header-list:http body=(unit octs)]
  %-  easy:(mare ,$-(move tang))
  |=  =state
  =?  headers  ?=(~ (get-header:http 'set-cookie' headers))
    :_  headers
    ['set-cookie' =<(?>(?=(%& -) out.p) ((expected-cookie &) state))]
  (%*(. ex-response duct duct) status headers body)
::
++  ex-response
  =/  =duct  [/http-blah ~]
  |=  [status=@ud headers=header-list:http body=(unit octs)]
  |=  mov=move
  ^-  tang
  ?.  ?=([* %give %response %start * * %.y] mov)
    [leaf+"expected %response, got: {<mov>}" ~]
  =?  headers  &(?=(^ body) ?=(~ (get-header:http 'content-length' headers)))
    :_  headers
    ['content-length' (crip ((d-co:co 1) p.u.body))]
  ;:  weld
    (expect-eq !>(duct) !>(duct.mov))
    (expect-eq !>(status) !>(status-code.response-header.http-event.p.card.mov))
    (expect-eq !>(body) !>(data.http-event.p.card.mov))
  ::
    %+  expect-eq  !>((header-sort headers))
    !>((header-sort headers.response-header.http-event.p.card.mov))
  ==
::
++  make-ex-start-resp  ::  auto-fill 'set-cookie' header from observed value
  =/  next=@t  g-cook
  =/  auto-cl=?  &
  |=  [status=@ud headers=header-list:http body=(unit octs)]
  %-  easy:(mare ,$-(move tang))
  |=  =state
  =.  headers
    :_  headers
    ['set-cookie' =<(?>(?=(%& -) out.p) ((expected-cookie &) state))]
  (%*(. ex-start-response auto-cl auto-cl) status headers body)
::
++  ex-start-response
  =/  =duct  [/http-blah ~]
  =/  auto-cl=?  &
  |=  [status=@ud headers=header-list:http body=(unit octs)]
  |=  mov=move
  ^-  tang
  ?.  ?=([* %give %response %start * * %.n] mov)
    [leaf+"expected start %response, got: {<mov>}" ~]
  =?  headers
      ?&  auto-cl
          ?=(^ body)
          ?=(~ (get-header:http 'content-length' headers))
      ==
    [['content-length' (crip ((d-co:co 1) p.u.body))] headers]
  ;:  weld
    (expect-eq !>(duct) !>(duct.mov))
    (expect-eq !>(status) !>(status-code.response-header.http-event.p.card.mov))
    (expect-eq !>(body) !>(data.http-event.p.card.mov))
  ::
    %+  expect-eq  !>((header-sort headers))
    !>((header-sort headers.response-header.http-event.p.card.mov))
  ==
::
++  ex-continue-response
  |=  [body=(unit octs) complete=?]
  |=  mov=move
  ^-  tang
  ?.  ?=([[[%http-blah ~] ~] %give %response %continue * *] mov)
    [leaf+"expected continue %response, got: {<mov>}" ~]
  ;:  weld
    (expect-eq !>(body) !>(data.http-event.p.card.mov))
    (expect-eq !>(complete) !>(complete.http-event.p.card.mov))
  ==
::
++  make-ex-channel-resp
  |=  body=(unit @t)
  =/  m  (mare ,$-(move tang))
  ^-  form:m
  =/  headers
    :~  ['content-type' 'text/event-stream']
        ['cache-control' 'no-cache']
        ['connection' 'keep-alive']
    ==
  (%*(. make-ex-start-resp auto-cl |) 200 headers (bind body as-octs:mimes:html))
::
++  ex-gall-deal
  |=  [=wire our=@p app=term =deal:gall]
  |=  mov=move
  ^-  tang
  %+  weld  (expect-eq !>(~[/http-blah]) !>(duct.mov))
  (expect-gall-deal [wire [our ~nul /eyre] app deal] card.mov)
::
++  expect-gall-deal
  |=  $:  expected=[wire=path id=sack app=term =deal:gall]
          actual=(wind note:eyre-gate gift:eyre-gate)
      ==
  ^-  tang
  ::
  ?.  ?=(%pass -.actual)
    [%leaf "bad move, not a %pass: {<actual>}"]~
  ::
  %+  weld
    (expect-eq !>(wire.expected) !>(p.actual))
  ::
  =/  note=note:eyre-gate  q.actual
  ?.  ?=([%g %deal *] note)
    [%leaf "bad move, not a %deal: {<actual>}"]~
  ::
  %+  weld
    (expect-eq !>(id.expected) !>(p.note))
  ::
  %+  weld
    (expect-eq !>(app.expected) !>(q.note))
  ::
  ?:  ?=([%poke *] deal.expected)
    ?.  ?=([%poke *] r.note)
      [%leaf "expected %poke, actual {<r.note>}"]~
    ::
    %+  weld
      (expect-eq !>(p.cage.deal.expected) !>(p.cage.r.note))
    ::  compare the payload vases
    ::
    (expect-eq q.cage.deal.expected q.cage.r.note)
  ::
  ?:  ?=([%poke-as *] deal.expected)
    ?.  ?=([%poke-as *] r.note)
      [%leaf "expected %poke-as, actual {<r.note>}"]~
    ::  compare the mark type
    ::
    %+  weld
      (expect-eq !>(mark.deal.expected) !>(mark.r.note))
    ::  compare the cage mark
    ::
    %+  weld
      (expect-eq !>(p.cage.deal.expected) !>(p.cage.r.note))
    ::  compare the payload vases
    ::
    (expect-eq q.cage.deal.expected q.cage.r.note)
  ::
  ?:  ?=([%watch *] deal.expected)
    ?.  ?=([%watch *] r.note)
      [%leaf "expected %watch, actual {<r.note>}"]~
    ::  compare the path
    ::
    (expect-eq !>(path.deal.expected) !>(path.r.note))
  ::
  ?:  ?=([%watch-as *] deal.expected)
    ?.  ?=([%watch-as *] r.note)
      [%leaf "expected %watch-as, actual {<r.note>}"]~
    ::  compare the result mark
    ::
    %+  weld
      (expect-eq !>(mark.deal.expected) !>(mark.r.note))
    ::  compare the path
    ::
    (expect-eq !>(path.deal.expected) !>(path.r.note))
  ::
  ?:  ?=([%leave *] deal.expected)
    ?.  ?=([%leave *] r.note)
      [%leaf "expected %leave, actual {<r.note>}"]~
    ::
    ~
  ::  todo: handle other deals
  ::
  [%leaf "unexpected %deal type"]~
::
++  eval-mare
  =/  m  (mare ,~)
  |=  computation=form:m
  ^-  tang
  =/  res  (computation eyre-gate ~1111.1.1 guest=& sesh=~)
  ?-  -.res
    %&  ~
    %|  p.res
  ==
::
++  scry-provides-code  ^-  roof
  |=  [gang pov=path =view =beam]
  ^-  (unit (unit cage))
  ?:  =(%gd view)  ``noun+!>(%base)
  ?:  =(%gu view)  ``noun+!>(=(%app1 q.beam))
  ?:  &(=(%ca view) =(/gen/handler/hoon s.beam))
    :+  ~  ~
    vase+!>(!>(|=(* |=(* [[%404 ~] ~]))))
  ?:  &(=(%cb view) =(/json s.beam))
    :^  ~  ~  %dais
    !>  ^-  dais:clay
    |_  sam=vase
    ++  diff  !!
    ++  form  !!
    ++  join  !!
    ++  mash  !!
    ++  pact  !!
    ++  vale  |=(=noun !>(;;(json noun)))
    --
  ::
  ?>  =(%j view)
  ?>  =(~nul p.beam)
  ?>  =(%code q.beam)
  ?>  =(%da -.r.beam)
  ?>  =(/~nul s.beam)
  ::  This is the default code for a fakeship.
  ::
  [~ ~ %noun !>(.~lidlut-tabwed-savheb-loslux)]
::
++  bake-cookie
  |=  [guest=? sesh=@t]
  %+  rap  3
  :~  sesh
      '; Path=/; Max-Age='
      ?:(guest guest-time auth-time)
  ==
::
++  g-name  ~fitbur-togtep-ladnup-ronbus--tanmer-sibsyn-lavryg-ramnul
++  g-cook  'urbauth-~nul=0v5.gbhev.sbeh0.3rov1.o6ibh.a3t9r'
--
::  Tests
::
|%
++  test-init
  (eval-mare perform-init)
::
++  test-born
  (eval-mare perform-born)
::
++  test-overwrite-bindings
  %-  eval-mare
  =/  m  (mare ,~)
  ;<  ~  bind:m  perform-init
  ;<  ~  bind:m  (wait ~d1)
  ::  app1 binds successfully
  ::
  ;<  ~  bind:m  (connect %app1 /)
  ;<  ~  bind:m  (wait ~d1)
  ::  app2 tries to bind to the same path and succeeds
  ::
  (connect %app2 /)
::
++  test-remove-binding
  %-  eval-mare
  =/  m  (mare ,~)
  ;<  ~  bind:m  perform-init
  ;<  ~  bind:m  (wait ~d1)
  ::  app1 binds successfully
  ::
  ;<  ~  bind:m  (connect %app1 /)
  ;<  ~  bind:m  (wait ~d1)
  ::  app1 unbinds
  ::
  ;<  mos=(list move)  bind:m  (call ~[/app1] [%disconnect `/])
  ;<  ~  bind:m  (expect-moves mos ~)
  ;<  ~  bind:m  (wait ~d1)
  ::  app2 binds successfully
  ::
  (connect %app2 /)
::
++  test-host-matching
  ;:  weld
    (expect !>((host-matches:eyre-gate ~ `'example.com')))
    (expect !>((host-matches:eyre-gate ~ ~)))
    (expect !>(!(host-matches:eyre-gate `'example.com' ~)))
    (expect !>((host-matches:eyre-gate `'example.com' `'example.com')))
    (expect !>(!(host-matches:eyre-gate `'example.com' `'blah.com')))
  ==
::  tests that when we have no match, that we fall back to the built-in 404
::
++  test-builtin-four-oh-four
  %-  eval-mare
  =/  m  (mare ,~)
  ;<  ~  bind:m  perform-init-wo-timer
  ;<  ex-rs=$-(move tang)  bind:m
    =/  headers  ['content-type' 'text/html']~
    =/  body  `(error-page:eyre-gate 404 %.n '/' ~)
    (make-ex-resp 404 headers body)
  ;<  mos=(list move)  bind:m  (get '/' ~)
  (expect-moves mos ex-rs ~)
::
++  test-basic-app-request
  %-  eval-mare
  =/  m  (mare ,~)
  ;<  ~  bind:m  perform-init-wo-timer
  ;<  ~  bind:m  (wait ~d1)
  ::  app1 binds successfully
  ::
  ;<  ~  bind:m  (connect %app1 /)
  ;<  ~  bind:m  (wait ~d1)
  ::  outside requests a path that app1 has bound to
  ::
  ;<  ~  bind:m  (request %app1 /)
  ;<  ~  bind:m  (wait ~d1)
  ::  theoretical outside response
  ::
  ;<  ex-sr=$-(move tang)  bind:m
    =/  headers  ['content-type' 'text/html']~
    (make-ex-start-resp 200 headers ~)
  ;<  mos=(list move)  bind:m
    =/  response  !>([200 ['content-type' 'text/html']~])
    =/  sign=sign:eyre-gate
      [%gall %unto %fact %http-response-header response]
    (take /watch-response/[eyre-id] ~[/http-blah] sign)
  (expect-moves mos ex-sr ~)
::
++  test-app-error
  %-  eval-mare
  =/  m  (mare ,~)
  ;<  ~  bind:m  perform-init-wo-timer
  ;<  ~  bind:m  (wait ~d1)
  ::  app1 binds successfully
  ::
  ;<  ~  bind:m  (connect %app1 /)
  ;<  ~  bind:m  (wait ~d1)
  ::  outside requests a path that app1 has bound to
  ::
  ;<  ~  bind:m  (request %app1 /)
  ;<  ~  bind:m  (wait ~d1)
  ::  the poke fails. we should relay this to the client
  ::
  ;<  mov-2=$-(move tang)  bind:m
    =/  response  `(internal-server-error:eyre-gate %.n '/' ~)
    (make-ex-resp 500 ['content-type' 'text/html']~ response)
  ;<  mos=(list move)  bind:m
    =/  sign=sign:eyre-gate
      [%gall %unto %poke-ack ~ [%leaf "/~zod/...../app1:<[1 1].[1 20]>"]~]
    (take /run-app-request/[eyre-id] ~[/http-blah] sign)
  =/  mov-1  (ex-gall-deal /watch-response/[eyre-id] g-name %app1 [%leave ~])
  (expect-moves mos mov-1 mov-2 ~)
::
++  test-multipart-app-request
  %-  eval-mare
  =/  m  (mare ,~)
  ;<  ~  bind:m  perform-init-wo-timer
  ;<  ~  bind:m  (wait ~d1)
  ::  app1 binds successfully
  ::
  ;<  ~  bind:m  (connect %app1 /)
  ;<  ~  bind:m  (wait ~d1)
  ::  outside requests a path that app1 has bound to
  ::
  ;<  ~  bind:m  (request %app1 /)
  ;<  ~  bind:m  (wait ~d1)
  ::  theoretical outside response
  ::
  ;<  ex-sr=$-(move tang)  bind:m
    =/  headers  ['content-type' 'text/html']~
    (make-ex-start-resp 200 headers ~)
  ;<  mos=(list move)  bind:m
    =/  response  !>([200 ['content-type' 'text/html']~])
    =/  sign=sign:eyre-gate
      [%gall %unto %fact %http-response-header response]
    (take /watch-response/[eyre-id] ~[/http-blah] sign)
  ;<  ~  bind:m
    (expect-moves mos ex-sr ~)
  ;<  ~  bind:m  (wait ~s1)
  ::  2nd response
  ::
  ;<  mos=(list move)  bind:m
    =/  response  !>(`(as-octs:mimes:html 'ya!'))
    =/  sign=sign:eyre-gate
      [%gall %unto %fact %http-response-data response]
    (take /watch-response/[eyre-id] ~[/http-blah] sign)
  =/  headers  ['content-type' 'text/html']~
  (expect-moves mos (ex-continue-response `[3 'ya!'] %.n) ~)
::
++  test-dead-app-request
  %-  eval-mare
  =/  m  (mare ,~)
  ;<  ~  bind:m  perform-init-wo-timer
  ;<  ~  bind:m  (wait ~d1)
  ::  dead-app binds successfully
  ::
  ;<  ~  bind:m  (connect %dead-app /)
  ;<  ~  bind:m  (wait ~d1)
  ::  outside requests a path that dead-app has bound to
  ::
  ;<  ex-rs=$-(move tang)  bind:m
    =/  body  `(error-page:eyre-gate 503 %.n '/' "%dead-app not running")
    (make-ex-resp 503 ['content-type' 'text/html']~ body)
  ;<  mos=(list move)  bind:m  (get '/' ~)
  (expect-moves mos ex-rs ~)
::  tests an app redirecting to the login handler, which then receives a post
::  and redirects back to app
::
++  test-login-handler-full-path
  %-  eval-mare
  =/  m  (mare ,~)
  ;<  ~  bind:m  perform-init-wo-timer
  ;<  ~  bind:m  perform-born
  ;<  ~  bind:m  (wait ~d1)
  ::  app1 binds successfully
  ::
  ;<  ~  bind:m  (connect %app1 /'~landscape')
  ;<  ~  bind:m  (wait ~d1)
  ::  outside requests a path that app1 has bound to
  ::
  ;<  ~  bind:m  (request %app1 /'~landscape')
  ;<  ~  bind:m  (wait ~d1)
  ::  app then gives a redirect to Eyre, which then includes a tmp guest cookie
  ::
  =/  headers  ['location' '/~/login?redirect=/~landscape/inner-path']~
  ;<  ex-sr=$-(move tang)  bind:m
    (make-ex-start-resp 303 headers ~)
  ;<  mos=(list move)  bind:m
    =/  sign=sign:eyre-gate
      [%gall %unto %fact %http-response-header !>([303 headers])]
    (take /watch-response/[eyre-id] ~[/http-blah] sign)
  ;<  ~  bind:m  (expect-moves mos ex-sr ~)
  ;<  ~  bind:m  (wait ~d1)
  ::  the browser then fetches the login page
  ::
  ;<  ~  bind:m  perform-authentication-2
  ;<  ~  bind:m  (wait ~h1)
  ::  going back to the original url will acknowledge the authentication cookie
  ::
  ;<  c=@t  bind:m  get-cookie
  ;<  mos=(list move)  bind:m
    (get '/~landscape/inner-path' ~)
  =/  mov-1
    %^  ex-gall-deal  /watch-response/[eyre-id]  ~nul
    [%app1 %watch /http-response/[eyre-id]]
  =/  mov-2
    =/  request  [%'GET' '/~landscape/inner-path' ['cookie' c]~ ~]
    =/  response  !>([eyre-id %.y %.n [%ipv4 .192.168.1.1] request])
    %^  ex-gall-deal  /run-app-request/[eyre-id]  ~nul
    [%app1 %poke %handle-http-request response]
  (expect-moves mos mov-1 mov-2 ~)
::
++  test-bad-auth-401
  %-  eval-mare
  =/  m  (mare ,~)
  ;<  ~  bind:m  perform-init-wo-timer
  ::  request made with unrecognized session should 401 & redirect
  ::
  ;<  ~  bind:m  |=(=state [%& ~ state(sesh `'urbauth-~nul=0v0')])
  ;<  mos=(list move)  bind:m  (get '/' ~)
  ;<  ex-rs=$-(move tang)  bind:m
    %^  make-ex-resp  401
      ['set-cookie' 'urbauth-~nul=0v0; Path=/; Max-Age=0']~
    `(as-octs:mimes:html 'bad session auth')
  (expect-moves mos ex-rs ~)
::
++  test-generator
  %-  eval-mare
  =/  m  (mare ,~)
  ;<  ~  bind:m  perform-init-wo-timer
  ;<  ~  bind:m  (wait ~d1)
  ::  gen1 binds successfully
  ::
  ;<  mos=(list move)  bind:m
    (call ~[/gen1] [%serve [~ /] %base /gen/handler/hoon ~])
  ;<  ~  bind:m  (expect-moves mos (ex ~[/gen1] %give %bound %.y [~ /]) ~)
  ;<  ~  bind:m  (wait ~d1)
  ::  outside requests a path that app1 has bound to
  ::
  ;<  ex-rs=$-(move tang)  bind:m  (make-ex-resp 404 ~ ~)
  ;<  mos=(list move)  bind:m  (get '/' ~)
  (expect-moves mos ex-rs ~)
::
++  test-simplified-url-parser
  ;:  weld
    %+  expect-eq
      !>  `[[%site 'localhost'] [~ 8.000]]
      !>  (rush 'localhost:8000' simplified-url-parser:eyre-gate)
  ::
    %+  expect-eq
      !>  `[[%ip .192.168.1.1] ~]
      !>  (rush '192.168.1.1' simplified-url-parser:eyre-gate)
  ==
::
++  test-parse-channel-request-jam
  ;:  weld
    %+  expect-eq
      !>  &+[%ack 5]~
      !>  %+  parse-channel-request:eyre-gate  %jam
          (as-octs:mimes:html (scot %uw (jam [%ack 5]~)))
  ::
    %+  expect-eq
      !>  |+'invalid request data'
      !>  %+  parse-channel-request:eyre-gate  %jam
          (as-octs:mimes:html (scot %uw (jam [%not %a %chanreq %list])))
  ==
::
++  test-parse-channel-request-json
  ;:  weld
    %+  expect-eq
      !>  &+[%ack 5]~
      !>  %+  parse-channel-request:eyre-gate  %json
          (as-octs:mimes:html '[{"action": "ack", "event-id": 5}]')
  ::
    %+  expect-eq
      !>  &+[%poke-json 0 ~nec %app1 %app-type [%n '5']]~
      !>  %+  parse-channel-request:eyre-gate  %json
          %-  as-octs:mimes:html
          '''
          [{"action": "poke",
            "id": 0,
            "ship": "nec",
            "app": "app1",
            "mark": "app-type",
            "json": 5}]
          '''
  ::
    %+  expect-eq
      !>  &+[%subscribe 1 ~sampyl-sipnym %hall /this/path]~
      !>  %+  parse-channel-request:eyre-gate  %json
          %-  as-octs:mimes:html
          '''
          [{"action": "subscribe",
            "id": 1,
            "ship": "sampyl-sipnym",
            "app": "hall",
            "path": "/this/path"}]
          '''
  ::
    %+  expect-eq
      !>  &+[%unsubscribe 2 1]~
      !>  %+  parse-channel-request:eyre-gate  %json
          %-  as-octs:mimes:html
          '''
          [{"action": "unsubscribe",
            "id": 2,
            "subscription": 1}]
          '''
  ::
      %+  expect-eq
        !>  |+'invalid channel json'
        !>  %+  parse-channel-request:eyre-gate  %json
            %-  as-octs:mimes:html
            '[{"noaction": "noaction"}]'
  ::
      %+  expect-eq
        !>  |+'invalid channel json'
        !>  %+  parse-channel-request:eyre-gate  %json
            %-  as-octs:mimes:html
            '[{"action": "bad-action"}]'
  ::
      %+  expect-eq
        !>  |+'invalid channel json'
        !>  %+  parse-channel-request:eyre-gate  %json
            %-  as-octs:mimes:html
            '[{"action": "ack", "event-id": 5}, {"action": "bad-action"}]'
  ::
      %+  expect-eq
        !>  :-  %&
            :~  [%ack 9]
                [%poke-json 3 ~bud %wut %wut-type [%a [%n '2'] [%n '1'] ~]]
            ==
        !>  %+  parse-channel-request:eyre-gate  %json
            %-  as-octs:mimes:html
            '''
            [{"action": "ack", "event-id": 9},
             {"action": "poke",
              "id": 3,
              "ship": "bud",
              "app": "wut",
              "mark": "wut-type",
              "json": [2, 1]}]
            '''
  ==
::
++  test-channel-open-never-used-expire
  %-  eval-mare
  =/  m  (mare ,~)
  ;<  ~  bind:m  perform-init-start-channel-2
  ::  the behn timer wakes us up; we cancel our subscription
  ::
  ;<  ~  bind:m  (wait ~h12)
  =/  wire  /channel/timeout/'0123456789abcdef'
  ;<  mos=(list move)  bind:m  (take wire ~[/http-blah] %behn %wake ~)
  =/  wire  /channel/subscription/'0123456789abcdef'/1/~nul/two/~nul
  (expect-moves mos (ex-gall-deal wire ~nul %two %leave ~) ~)
::
++  test-channel-open-with-get
  %-  eval-mare
  =/  m  (mare ,~)
  ;<  ~  bind:m  perform-init-wo-timer
  ;<  ~  bind:m  perform-born
  ;<  ~  bind:m  (wait ~d1)
  ;<  ex-rs=$-(move tang)  bind:m
    =/  headers  ['content-type' 'text/html']~
    =/  body  `(error-page:eyre-gate 404 %.n '/~/channel/0123456789abcdef' ~)
    (make-ex-resp 404 headers body)
  ;<  mos=(list move)  bind:m
    (get '/~/channel/0123456789abcdef' ~)
  ;<  now=@da  bind:m  get-now
  (expect-moves mos ex-rs ~)
::
++  test-channel-put-zero-requests
  %-  eval-mare
  =/  m  (mare ,~)
  ;<  ~  bind:m  perform-init-start-channel-2
  ;<  ~  bind:m  (wait ~m1)
  ;<  mos=(list move)  bind:m
    (put '/~/channel/0123456789abcdef' ~ '[]')
  ;<  mov-1=$-(move tang)  bind:m  (make-ex-resp 204 ~ ~)
  =/  mov-2  (ex-rest /channel/timeout/'0123456789abcdef' ~1111.1.2..12.00.00)
  =/  mov-3  (ex-wait /channel/timeout/'0123456789abcdef' ~1111.1.2..12.01.00)
  (expect-moves mos mov-1 mov-2 mov-3 ~)
::
++  test-channel-results-before-open
  %-  eval-mare
  =/  m  (mare ,~)
  ;<  ~  bind:m  perform-init-start-channel-2
  ;<  ~  bind:m  (wait ~m1)
  ::  poke gets a success message
  ::
  =/  wire  /channel/poke/'0123456789abcdef'/'0'
  ;<  mos=(list move)  bind:m  (take wire ~[/http-blah] %gall %unto %poke-ack ~)
  ;<  ~  bind:m  (expect-moves mos ~)
  ::  subscription gets a success message
  ::
  =/  wire  /channel/subscription/'0123456789abcdef'/'1'/~nul/two/~nul
  ;<  mos=(list move)  bind:m
    (take wire ~[/http-blah] %gall %unto %watch-ack ~)
  ;<  ~  bind:m  (expect-moves mos ~)
  ::  subscription gets a result
  ::
  ;<  ~  bind:m  (wait ~m1)
  ;<  mos=(list move)  bind:m
    =/  =cage  [%json !>(`json`[%a [%n '1'] [%n '2'] ~])]
    (take wire ~[/http-blah] %gall %unto %fact cage)
  ;<  ~  bind:m  (expect-moves mos ~)
  ::  open up the channel
  ::
  ::  send the channel a poke and a subscription request
  ::
  ;<  ~  bind:m  (wait ~m1)
  ;<  now=@da  bind:m  get-now
  =/  mov-1  (ex-wait /channel/heartbeat/'0123456789abcdef' (add now ~s20))
  ;<  mov-2=$-(move tang)  bind:m
    %+  make-ex-channel-resp  ~
    '''
    id: 0
    data: {"ok":"ok","id":0,"response":"poke"}

    id: 1
    data: {"ok":"ok","id":1,"response":"subscribe"}

    id: 2
    data: {"json":[1,2],"id":1,"response":"diff"}


    '''
  ::  opening the channel cancels the timeout timer
  ::
  =/  mov-3  (ex-rest /channel/timeout/'0123456789abcdef' ~1111.1.2..12.00.00)
  ;<  mos=(list move)  bind:m
    (get '/~/channel/0123456789abcdef' ~)
  ;<  ~  bind:m  (expect-moves mos mov-1 mov-2 mov-3 ~)
  ::  we get a cancel when we notice the client has disconnected
  ::
  ;<  ~  bind:m  (wait ~m1)
  ;<  mos=(list move)  bind:m  (call ~[/http-blah] %cancel-request ~)
  =/  mov-1
    (ex-rest /channel/heartbeat/'0123456789abcdef' :(add ~1111.1.2 ~m3 ~s20))
  =/  mov-2
    (ex-wait /channel/timeout/'0123456789abcdef' :(add ~1111.1.2 ~m4 ~h12))
  (expect-moves mos mov-1 mov-2 ~)
::
++  test-channel-second-get-updates-timer
  %-  eval-mare
  =/  m  (mare ,~)
  ;<  ~  bind:m  perform-init-start-channel-2
  ;<  ~  bind:m  (wait ~m1)
  ::  perform another poke to a different app
  ::
  ::    Since we haven't connected with a GET, the old timer should be canceled
  ::    and a new one should be set.
  ::
  ;<  mos=(list move)  bind:m
    %^  put  '/~/channel/0123456789abcdef'  ~
    '''
    [{"action": "poke",
      "id": 2,
      "ship": "nul",
      "app": "eight",
      "mark": "a",
      "json": 9}]
    '''
  =/  wire  /channel/poke/'0123456789abcdef'/'2'
  =/  mov-1  (ex-gall-deal wire ~nul %eight %poke-as %a %json !>([%n '9']))
  ;<  mov-2=$-(move tang)  bind:m  (make-ex-resp 204 ~ ~)
  =/  mov-3  (ex-rest /channel/timeout/'0123456789abcdef' ~1111.1.2..12.00.00)
  =/  mov-4  (ex-wait /channel/timeout/'0123456789abcdef' ~1111.1.2..12.01.00)
  (expect-moves mos mov-1 mov-2 mov-3 mov-4 ~)
::
++  test-channel-unsubscribe-stops-events
  %-  eval-mare
  =/  m  (mare ,~)
  ;<  ~  bind:m  perform-init-start-channel-2
  ;<  ~  bind:m  (wait ~m1)
  ::  poke gets a success message
  ::
  =/  wire  /channel/poke/'0123456789abcdef'/'0'
  ;<  mos=(list move)  bind:m  (take wire ~[/http-blah] %gall %unto %poke-ack ~)
  ;<  ~  bind:m  (expect-moves mos ~)
  ;<  ~  bind:m  (wait ~m1)
  ::  subscription gets a success message
  ::
  =/  wire  /channel/subscription/'0123456789abcdef'/'1'/~nul/two/~nul
  ;<  mos=(list move)  bind:m
    (take wire ~[/http-blah] %gall %unto %watch-ack ~)
  ;<  ~  bind:m  (expect-moves mos ~)
  ;<  ~  bind:m  (wait ~m1)
  ::  sending an unsubscribe sends an unsubscribe to gall
  ::
  ;<  mos=(list move)  bind:m
    %^  put  '/~/channel/0123456789abcdef'  ~
    '''
    [{"action": "unsubscribe",
      "id": 2,
      "subscription": 1}
    ]
    '''
  =/  wire  /channel/subscription/'0123456789abcdef'/'1'/~nul/two/~nul
  =/  mov-1  (ex-gall-deal wire ~nul %two %leave ~)
  ;<  mov-2=$-(move tang)  bind:m  (make-ex-resp 204 ~ ~)
  =/  mov-3  (ex-rest /channel/timeout/'0123456789abcdef' ~1111.1.2..12.00.00)
  =/  mov-4  (ex-wait /channel/timeout/'0123456789abcdef' ~1111.1.2..12.03.00)
  (expect-moves mos mov-1 mov-2 mov-3 mov-4 ~)
::
++  test-channel-double-subscription-works
  %-  eval-mare
  =/  m  (mare ,~)
  ;<  ~  bind:m  perform-init-start-channel-2
  ;<  ~  bind:m  (wait ~m1)
  ::  poke gets a success message
  ::
  =/  wire  /channel/poke/'0123456789abcdef'/'0'
  ;<  mos=(list move)  bind:m  (take wire ~[/http-blah] %gall %unto %poke-ack ~)
  ;<  ~  bind:m  (expect-moves mos ~)
  ;<  ~  bind:m  (wait ~m1)
  ::  subscription gets a success message
  ::
  =/  wire  /channel/subscription/'0123456789abcdef'/'1'/~nul/two/~nul
  ;<  mos=(list move)  bind:m
    (take wire ~[/http-blah] %gall %unto %watch-ack ~)
  ;<  ~  bind:m  (expect-moves mos ~)
  ;<  ~  bind:m  (wait ~m1)
  ::  now make a second subscription from the client on the same path
  ::
  ;<  mos=(list move)  bind:m
    %^  put  '/~/channel/0123456789abcdef'  ~
    '''
    [{"action": "subscribe",
      "id": 2,
      "ship": "nul",
      "app": "two",
      "path": "/one/two/three"}
    ]
    '''
  =/  wire  /channel/subscription/'0123456789abcdef'/'2'/~nul/two/~nul
  =/  mov-1  (ex-gall-deal wire ~nul %two %watch /one/two/three)
  ;<  mov-2=$-(move tang)  bind:m  (make-ex-resp 204 ~ ~)
  =/  mov-3  (ex-rest /channel/timeout/'0123456789abcdef' ~1111.1.2..12.00.00)
  =/  mov-4  (ex-wait /channel/timeout/'0123456789abcdef' ~1111.1.2..12.03.00)
  ::  subscription gets 2 results
  ::
  ;<  mos=(list move)  bind:m
    =/  wire  /channel/subscription/'0123456789abcdef'/'1'/~nul/two/~nul
    =/  =cage  [%json !>(`json`[%a [%n '1'] [%n '2'] ~])]
    (take wire ~[/http-blah] %gall %unto %fact cage)
  ;<  ~  bind:m  (expect-moves mos ~)
  ::
  ;<  mos=(list move)  bind:m
    =/  wire  /channel/subscription/'0123456789abcdef'/'2'/~nul/two/~nul
    =/  =cage  [%json !>(`json`[%a [%n '1'] [%n '2'] ~])]
    (take wire ~[/http-blah] %gall %unto %fact cage)
  ;<  ~  bind:m  (expect-moves mos ~)
  ::  open up the channel
  ::
  ;<  now=@da  bind:m  get-now
  =/  mov-1  (ex-wait /channel/heartbeat/'0123456789abcdef' (add now ~s20))
  ;<  mov-2=$-(move tang)  bind:m
    %+  make-ex-channel-resp  ~
    '''
    id: 0
    data: {"ok":"ok","id":0,"response":"poke"}

    id: 1
    data: {"ok":"ok","id":1,"response":"subscribe"}

    id: 2
    data: {"json":[1,2],"id":1,"response":"diff"}

    id: 3
    data: {"json":[1,2],"id":2,"response":"diff"}


    '''
  ::  opening the channel cancels the timeout timer
  ::
  =/  mov-3  (ex-rest /channel/timeout/'0123456789abcdef' ~1111.1.2..12.03.00)
  ;<  mos=(list move)  bind:m
    (get '/~/channel/0123456789abcdef' ~)
  ;<  ~  bind:m  (expect-moves mos mov-1 mov-2 mov-3 ~)
  ::  we can close the first channel without closing the second
  ::
  ;<  mos=(list move)  bind:m
    %^  put  '/~/channel/0123456789abcdef'  ~
    '''
    [{"action": "unsubscribe",
      "id": 3,
      "subscription": 1}
    ]
    '''
  =/  wire  /channel/subscription/'0123456789abcdef'/'1'/~nul/two/~nul
  =/  mov-1  (ex-gall-deal wire ~nul %two %leave ~)
  ;<  mov-2=$-(move tang)  bind:m  (make-ex-resp 204 ~ ~)
  ;<  ~  bind:m  (expect-moves mos mov-1 mov-2 ~)
  ::  gall responds on the second subscription.
  ::
  ::    This just tests that closing one of the two subscriptions doesn't
  ::    unsubscribe to the other.
  ::
  ;<  mos=(list move)  bind:m
    =/  wire  /channel/subscription/'0123456789abcdef'/'2'/~nul/two/~nul
    =/  =cage  [%json !>(`json`[%a [%n '1'] [%n '2'] ~])]
    (take wire ~[/http-blah] %gall %unto %fact cage)
  =/  mov-1
    %-  ex-continue-response  :_  %.n  :-  ~
    %-  as-octs:mimes:html
    '''
    id: 4
    data: {"json":[1,2],"id":2,"response":"diff"}


    '''
  (expect-moves mos mov-1 ~)
::
++  test-prune-events
  =/  q=(qeu [id=@ud @ud channel-event:eyre])  ~
  =.  q  (~(put to q) [0 0 *channel-event:eyre])
  =.  q  (~(put to q) [1 0 *channel-event:eyre])
  =.  q  (~(put to q) [2 0 *channel-event:eyre])
  =.  q  (~(put to q) [3 1 *channel-event:eyre])
  =.  q  (~(put to q) [4 1 *channel-event:eyre])
  ::
  =^  a  q  (prune-events:eyre-gate q 3)
  ::
  %+  expect-eq
    !>
    :-  (~(gas by *(map @ud @ud)) ~[0^3 1^1])
    [~ [4 1 *channel-event:eyre]]
  !>([a ~(top to q)])
::
++  test-subtract-acked-events
  =/  a  (~(gas by *(map @ud @ud)) ~[0^3 1^1])
  =/  u  (~(gas by *(map @ud @ud)) ~[0^4 2^1])
  =/  e  (~(gas by *(map @ud @ud)) ~[0^1 2^1])
  =/  r  (subtract-acked-events:eyre-gate a u)
  (expect-eq !>(e) !>(r))
::
++  test-channel-sends-unacknowledged-events-on-reconnection
  %-  eval-mare
  =/  m  (mare ,~)
  ;<  ~  bind:m  perform-init-start-channel-2
  ;<  ~  bind:m  (wait ~m1)
  ::  poke gets a success message
  ::
  =/  wire  /channel/poke/'0123456789abcdef'/'0'
  ;<  mos=(list move)  bind:m  (take wire ~[/http-blah] %gall %unto %poke-ack ~)
  ;<  ~  bind:m  (expect-moves mos ~)
  ::  subscription gets a success message
  ::
  =/  wire  /channel/subscription/'0123456789abcdef'/'1'/~nul/two/~nul
  ;<  mos=(list move)  bind:m
    (take wire ~[/http-blah] %gall %unto %watch-ack ~)
  ;<  ~  bind:m  (expect-moves mos ~)
  ::  open the http channel
  ::
  ;<  ~  bind:m  (wait ~m2)
  ;<  now=@da  bind:m  get-now
  =/  heartbeat  (add now ~s20)
  =/  mov-1  (ex-wait /channel/heartbeat/'0123456789abcdef' heartbeat)
  ;<  mov-2=$-(move tang)  bind:m
    %+  make-ex-channel-resp  ~
    '''
    id: 0
    data: {"ok":"ok","id":0,"response":"poke"}

    id: 1
    data: {"ok":"ok","id":1,"response":"subscribe"}


    '''
  ::  opening the channel cancels the timeout timer
  ::
  =/  mov-3  (ex-rest /channel/timeout/'0123456789abcdef' ~1111.1.2..12.00.00)
  ;<  mos=(list move)  bind:m
    (get '/~/channel/0123456789abcdef' ~)
  ;<  ~  bind:m  (expect-moves mos mov-1 mov-2 mov-3 ~)
  ;<  ~  bind:m  (wait ~m1)
  ::  first subscription result gets sent to the user
  ::
  ;<  mos=(list move)  bind:m
    =/  wire  /channel/subscription/'0123456789abcdef'/'1'/~nul/two/~nul
    =/  =cage  [%json !>(`json`[%a [%n '1'] ~])]
    (take wire ~[/http-blah] %gall %unto %fact cage)
  =/  mov-1
    %-  ex-continue-response  :_  %.n  :-  ~
    %-  as-octs:mimes:html
    '''
    id: 2
    data: {"json":[1],"id":1,"response":"diff"}


    '''
  ;<  ~  bind:m  (expect-moves mos mov-1 ~)
  ;<  ~  bind:m  (wait ~m1)
  ::  the client now acknowledges up to event 1
  ::
  ;<  mos=(list move)  bind:m
    %^  put-2  '/~/channel/0123456789abcdef'  ~
    '''
    [{"action": "ack",
      "event-id": 1}
    ]
    '''
  ;<  ex-rs=$-(move tang)  bind:m  (%*(. make-ex-resp duct [/http-put-request]~) 204 ~ ~)
  ;<  ~  bind:m  (expect-moves mos ex-rs ~)
  ;<  ~  bind:m  (wait ~m1)
  ::  the client connection is detected to be broken
  ::
  ;<  mos=(list move)  bind:m  (call ~[/http-blah] %cancel-request ~)
  =/  mov-1  (ex-rest /channel/heartbeat/'0123456789abcdef' heartbeat)
  =/  mov-2
    (ex-wait /channel/timeout/'0123456789abcdef' :(add ~1111.1.2 ~m6 ~h12))
  ;<  ~  bind:m  (expect-moves mos mov-1 mov-2 ~)
  ;<  ~  bind:m  (wait ~m1)
  ::  another subscription result while the user is disconnected
  ::
  ;<  mos=(list move)  bind:m
    =/  wire  /channel/subscription/'0123456789abcdef'/'1'/~nul/two/~nul
    =/  =cage  [%json !>(`json`[%a [%n '2'] ~])]
    (take wire ~[/http-blah] %gall %unto %fact cage)
  ;<  ~  bind:m  (expect-moves mos ~)
  ;<  ~  bind:m  (wait ~m1)
  ::  the client now retries to connect
  ::
  ::    Because the client has acknowledged up to event 1, we should start the connection by
  ::    resending events 2 and 3.
  ::
  ;<  now=@da  bind:m  get-now
  =/  heartbeat  (add now ~s20)
  =/  mov-1  (ex-wait /channel/heartbeat/'0123456789abcdef' heartbeat)
  ;<  mov-2=$-(move tang)  bind:m
    %+  make-ex-channel-resp  ~
    '''
    id: 2
    data: {"json":[1],"id":1,"response":"diff"}

    id: 3
    data: {"json":[2],"id":1,"response":"diff"}


    '''
  =/  mov-3
    (ex-rest /channel/timeout/'0123456789abcdef' :(add ~1111.1.2 ~m6 ~h12))
  ;<  mos=(list move)  bind:m
    (get '/~/channel/0123456789abcdef' ~)
  (expect-moves mos mov-1 mov-2 mov-3 ~)
::
++  test-channel-subscription-clogged
  %-  eval-mare
  =/  m  (mare ,~)
  ;<  ~  bind:m  perform-init-start-channel-2
  ::TODO  also test that if we get a bunch within +clog-timeout, it doesn't
  ::      clog the channel yet
  ;<  ~  bind:m  (wait (add ~s1 clog-timeout:eyre-gate))
  ::  subscription gets a success message
  ::
  =/  wire  /channel/subscription/'0123456789abcdef'/'1'/~nul/two/~nul
  ;<  mos=(list move)  bind:m
    (take wire ~[/http-blah] %gall %unto %watch-ack ~)
  ;<  ~  bind:m  (expect-moves mos ~)
  ::  opens the http channel
  ::
  ;<  tested-elsewhere=(list move)  bind:m
    (get '/~/channel/0123456789abcdef' ~)
  ::  user gets sent multiple subscription results
  ::
  =/  max=@ud  clog-threshold:eyre-gate
  =/  cur=@ud  0
  |-  ^-  form:m
  =*  loop-fact  $
  ?.  =(cur max)
    ;<  tested-elsewhere=(list move)  bind:m
      =/  wire  /channel/subscription/'0123456789abcdef'/'1'/~nul/two/~nul
      =/  =cage  [%json !>(`json`[%a [%n '1'] ~])]
      (take wire ~[/http-blah] %gall %unto %fact cage)
    loop-fact(cur +(cur))
  ::  the next subscription result should trigger a clog
  ::
  ;<  mos=(list move)  bind:m
    =/  wire  /channel/subscription/'0123456789abcdef'/'1'/~nul/two/~nul
    =/  =cage  [%json !>(`json`[%a [%n '1'] ~])]
    (take wire ~[/http-blah] %gall %unto %fact cage)
  =/  mov-1
    %-  ex-continue-response  :_  %.n  :-  ~
    %-  as-octt:mimes:html
    """
    id: {((d-co:co 1) +(clog-threshold:eyre-gate))}
    data: \{"json":[1],"id":1,"response":"diff"}


    """
  =/  wire  /channel/subscription/'0123456789abcdef'/'1'/~nul/two/~nul
  =/  mov-2  (ex-gall-deal wire ~nul %two %leave ~)
  =/  mov-3
    %-  ex-continue-response  :_  %.n  :-  ~
    %-  as-octt:mimes:html
    """
    id: {((d-co:co 1) (add 2 clog-threshold:eyre-gate))}
    data: \{"id":1,"response":"quit"}


    """
  ;<  ~  bind:m  (expect-moves mos mov-1 mov-2 mov-3 ~)
  ::  subsequent subscription updates, which might have gotten sent out during
  ::  the same event in which a clog triggered, should be silently ignored
  ::
  ;<  mos=(list move)  bind:m
    =/  wire  /channel/subscription/'0123456789abcdef'/'1'/~nul/two/~nul
    =/  =cage  [%json !>(`json`[%a [%n '1'] ~])]
    (take wire ~[/http-blah] %gall %unto %fact cage)
  (expect-moves mos ~)
::
++  test-born-sends-pending-cancels
  %-  eval-mare
  =/  m  (mare ,~)
  ;<  ~  bind:m  perform-init-wo-timer
  ;<  ~  bind:m  (wait ~d1)
  ::  app1 binds successfully
  ::
  ;<  ~  bind:m  (connect %app1 /)
  ;<  ~  bind:m  (wait ~d1)
  ::  outside requests a path that app1 has bound to
  ::
  ;<  ~  bind:m  (request %app1 /)
  ;<  ~  bind:m  (wait ~d1)
  ::  but app1 doesn't respond before our urbit gets shut down. ensure we send
  ::  cancels on open connections.
  ::
  ;<  mos=(list move)  bind:m  (call ~[/born] [%born ~])
  =/  mov-3  (ex-gall-deal /watch-response/[eyre-id] g-name %app1 [%leave ~])
  (expect-moves mos _*tang _*tang mov-3 ~)
::
::  +perform-init: %init a new eyre-gate
::
++  perform-init
  =/  m  (mare ,~)
  ;<  mos=(list move)  bind:m  (call ~[/init] [%init ~])
  (expect-moves mos ~)
::  +perform-init-wo-timer: init, then add a guest session
::
::    so that we don't have to include the session expiry timer move
::    in every single request handling test
::
++  perform-init-wo-timer
  =/  m  (mare ,~)
  ^-  form:m
  ;<  ~  bind:m  perform-init
  |=  =state
  :+  %&  ~
  =-  state(sessions.auth.server-state.ax.gate -)
  %+  ~(put by sessions.auth.server-state.ax.gate.state)
    0vguest
  [fake+~sampel-sampel-sampel-sampel--sampel-sampel-sampel-sampel ~2222.2.2 ~]
::
++  setup-for-eauth
  |=  base=@t
  =/  m  (mare ,~)
  ^-  form:m
  ;<  ~  bind:m  perform-init-wo-timer
  ;<  ~  bind:m  perform-born
  ;<  ~  bind:m  perform-authentication-2
  ;<  ~  bind:m
    ::  make sure there is an eauth endpoint set
    ::
    |=  =state
    &+`state(user.endpoint.auth.server-state.ax.gate `base)
  (pure:m ~)
::  +perform-born: %born an eyre-gate
::
++  perform-born
  =/  m  (mare ,~)
  ^-  form:m
  ;<  mos=(list move)  bind:m  (call ~[/unix] [%born ~])
  (expect-moves mos (ex-set-config *http-config:eyre) (ex-sessions ~) ~)
::
++  request-name
  =/  m  (mare ,@p)
  ^-  form:m
  ;<  mos=(list move)  bind:m  (get '/~/name' ~)
  ?>  ?=([[* %give %response %start * * %.y] ~] mos)
  (pure:m (slav %p q:(need data.http-event.p.card.i.mos)))
::
++  test-perform-authentication
  %-  eval-mare
  =/  m  (mare ,~)
  ^-  form:m
  ;<  ~  bind:m  perform-init-wo-timer
  ;<  ~  bind:m  perform-born
  ;<  name=@p  bind:m  request-name
  ;<  ~  bind:m  (try (expect-eq !>(g-name) !>(name)))
  ::
  ;<  ~  bind:m  perform-authentication-2
  ;<  name=@p  bind:m  request-name
  (try (expect-eq !>(~nul) !>(name)))
::
++  test-header-authentication
  %-  eval-mare
  =/  m  (mare ,~)
  ^-  form:m
  ;<  ~  bind:m  perform-init-wo-timer
  ;<  ~  bind:m  perform-born
  ;<  ~  bind:m  perform-authentication-2
  ::  clear out the sesh so that we don't send a cookie,
  ::  then request our name while passing in header auth with the same token
  ::
  ;<  t=@t  bind:m  get-token
  ;<  ~  bind:m  |=(=state [%& ~ state(sesh ~)])
  ;<  name=@p  bind:m
    =/  m  (mare ,@p)
    ^-  form:m
    ;<  mos=(list move)  bind:m  (get '/~/name' ['authorization' (cat 3 'Bearer ' t)]~)
    ?>  ?=([[* %give %response %start * * %.y] ~] mos)
    (pure:m (slav %p q:(need data.http-event.p.card.i.mos)))
  ;<  ~  bind:m
    (try (expect-eq !>(~nul) !>(name)))
  ::  that should've also given us the corresponding cookie in case we want it
  ::
  ;<  tt=@t  bind:m  get-token
  (try (expect-eq !>(t) !>(tt)))
::  +perform-authentication: goes through the authentication flow
::
++  perform-authentication-2
  =/  m  (mare ,~)
  ^-  form:m
  ;<  ex-rs=$-(move tang)  bind:m
    =/  headers  ['content-type' 'text/html']~
    =/  body  `(login-page:eyre-gate `'/~landscape/inner-path' ~nul fake+g-name ~ %.n)
    (make-ex-resp 200 headers body)
  ;<  mos=(list move)  bind:m
    (get '/~/login?redirect=/~landscape/inner-path' ~)
  ;<  ~  bind:m
    (expect-moves mos ex-rs ~)
  ::
  ;<  mos=(list move)  bind:m
    =/  body  'password=lidlut-tabwed-pillex-ridrup&redirect=/~landscape'
    (post '/~/login' ~ body)
  ;<  ~  bind:m  (set-guest |)  ::  authed as self, no longer guest
  ;<  ~  bind:m
    ::NOTE  post-hoc token rationalization, but this tests the mechanism used
    ::      in these tests for capturing it from moves coming out of eyre.
    ;<  t=@t  bind:m  get-token
    =/  headers
      :~  ['location' '/~landscape']
          ['set-cookie' (bake-cookie | (cat 3 'urbauth-~nul=' t))]
      ==
    =/  token  t
    (expect-moves mos (ex-sessions token ~ ~) (ex-response 303 headers `(as-octs:mimes:html t)) ~)
  (pure:m ~)
::
++  eauth
  |%
  ++  nonce  0vcn5.qlgj3.hpopf
  ++  server
    |%
    ++  wire  `^wire`/eauth/keen/(scot %p ~sampel)/(scot %uv nonce)
    ::
    ++  start
      =/  body  'eauth&name=~sampel&redirect=/final'
      (post '/~/login' ~ body)
    ::
    ++  sage
      %^  take  /eauth/keen/(scot %p ~sampel)/(scot %uv nonce)
        ~[/http-blah]
      ::NOTE  path and signature don't matter here, eyre doesn't look at them
      [%ames %sage [~sampel *path] %noun `'http://sampel.com/~/eauth']
    ::
    ++  grant
      %+  call  ~[/http-blah]
      [%plea ~sampel %e /eauth/0 `eauth-plea:eyre`[%0 %open nonce `0vtoken]]
    ::
    ++  final
      =;  url=@t  (get url ~)
      (cat 3 '/~/eauth?token=0vtoken&nonce=' (scot %uv nonce))
    ::
    ++  ex-keen
      |=  =time
      %+  ex  ~[/http-blah]
      ::TODO  want to access eauth-cache-rounding:eyre-gate here but can't..?
      =.  time  (sub time (mod time ~m5))
      [%pass wire %a %keen ~ ~sampel /e/x/(scot %da time)//eauth/url]
    ::
    ++  ex-yawn
      |=  =time
      %+  ex  ~[/http-blah]
      =.  time  (sub time (mod time ~h1))
      [%pass wire %a %yawn ~sampel /e/x/(scot %da time)//eauth/url]
    ::
    ++  ex-done
      (ex ~[/http-blah] %give %done ~)
    ::
    ++  ex-boon
      |=  boon=eauth-boon:eyre
      (ex ~[/http-blah] %give %boon boon)
    --
  ::
  ++  client
    |%
    ++  wire   /eauth/plea/(scot %p ~hoster)
    ++  duct   [/eyre/eauth/synthetic]~
    ::
    ++  grant
      =/  body  'server=~hoster&nonce=0vnonce&grant=grant'
      (post '/~/eauth' ~ body)
    ::
    ++  okay
      ::NOTE  eyre doesn't do anything with the %done ack,
      ::      so we dont simulate it
      %^  take  wire
        duct
      [%ames %boon %0 %okay 0vnonce 'http://hoster.com/~/eauth']
    ::
    ::NOTE  expects a version %0 plea for :ship
    ++  ex-plea
      |=  [=ship plea=eauth-plea:eyre]
      (ex duct %pass wire %a %plea ship %e /eauth/(scot %ud %0) plea)
    --
  --
::
::TODO  would be good to test with different ducts so we know they get
::      stored & re-used correctly
++  test-eauth-incoming
  %-  eval-mare
  =/  m  (mare ,~)
  ^-  form:m
  =,  server:eauth
  ;<  ~  bind:m  (setup-for-eauth 'http://hoster.com')
  ::  eauth login attempt starts the flow: send a scry, set timeout timer
  ::
  ;<  mos=(list move)  bind:m  start
  ;<  now=@da  bind:m  get-now
  ;<  ~  bind:m
    %+  expect-moves  mos
    :~  (ex-keen now)
        (ex-wait /eauth/expire/visitors/(scot %uv nonce) (add now eauth-timeout:eyre-gate))
    ==
  ::  ~sampel gets back to us with a url, we redirect the requester
  ::
  ;<  ex-rs=$-(move tang)  bind:m
    =/  loc=@t
      %^  cat  3
        'http://sampel.com/~/eauth?server=~nul&nonce='
      (scot %uv nonce)
    (make-ex-resp 303 ['location' loc]~ ~)
  ;<  mos=(list move)  bind:m  sage
  ;<  ~  bind:m
    (expect-moves mos ex-rs ~)
  ::  requester approves, we get an %open plea, must give an %okay boon
  ::
  ;<  mos=(list move)  bind:m  grant
  ;<  ~  bind:m
    %+  expect-moves  mos
    :~  ex-done
        (ex-boon %0 %okay nonce 'http://hoster.com/~/eauth')
    ==
  ::  requester returns for the final request
  ::
  ;<  ex-rs=$-(move tang)  bind:m
    (make-ex-resp 303 ['location' '/final']~ ~)
  ;<  mos=(list move)  bind:m  final
  ;<  ~  bind:m
    =/  nook
      ::  a new cookie gets generated with the proper expiry time.
      ::  the fact that the token here matches the +g-cook one is a side-effect
      ::  of eyre killing the old session first, and the tests passing a static
      ::  entropy value.
      ::
      %^  cat  3
        'urbauth-~nul=0v5.gbhev.sbeh0.3rov1.o6ibh.a3t9r; Path=/; Max-Age='
      auth-time
    %+  expect-moves  mos
    :~  (ex-response 303 ~['location'^'/final' 'set-cookie'^nook] ~)
    ==
  (pure:m ~)
::
++  test-eauth-incoming-bad-token
  %-  eval-mare
  =/  m  (mare ,~)
  ^-  form:m
  =,  server:eauth
  ;<  ~  bind:m  (setup-for-eauth 'http://hoster.com')
  ;<  *  bind:m  start
  ;<  *  bind:m  sage
  ;<  *  bind:m  grant
  ::  requester GETs a url with a non-matching token
  ::
  ;<  ex-rs=$-(move tang)  bind:m
    =/  body  `(eauth-error-page:eyre-gate %server '/final')
    (make-ex-resp 400 ['content-type' 'text/html']~ body)
  ;<  mos=(list move)  bind:m
    =;  url=@t  (get url ~)
    (cat 3 '/~/eauth?token=0vbad&nonce=' (scot %uv nonce))
  ;<  ~  bind:m
    (expect-moves mos ex-rs ~)
  (pure:m ~)
::
++  test-eauth-incoming-expired
  %-  eval-mare
  =/  m  (mare ,~)
  ^-  form:m
  =,  server:eauth
  ;<  ~  bind:m  (setup-for-eauth 'http://hoster.com')
  ;<  *  bind:m  start
  ;<  =time  bind:m  get-now
  ::  expiry timer fires, we serve a response and delete the attempt
  ::
  ;<  ~  bind:m  (wait eauth-timeout:eyre-gate)
  ;<  ex-rs=$-(move tang)  bind:m
    =/  body  `(eauth-error-page:eyre-gate %server '/final')
    (make-ex-resp 503 ['content-type' 'text/html']~ body)
  ;<  mos=(list move)  bind:m
    =/  =^wire  /eauth/expire/visitors/(scot %uv nonce)
    (take wire ~[/http-blah] %behn %wake ~)
  ;<  ~  bind:m
    (expect-moves mos (ex-yawn time) ex-rs ~)
  (pure:m ~)
::
++  test-eauth-incoming-aborted
  %-  eval-mare
  =/  m  (mare ,~)
  ^-  form:m
  =,  server:eauth
  ;<  ~  bind:m  (setup-for-eauth 'http://hoster.com')
  ;<  *  bind:m  start
  ;<  *  bind:m  sage
  ::  visitor returns, saying the attempt was aborted. we delete it
  ::
  ;<  ex-rs=$-(move tang)  bind:m
    (make-ex-resp 303 ['location' '/~/login?eauth&redirect=%2Ffinal']~ ~)
  ;<  mos=(list move)  bind:m
    =;  url=@t  (get url ~)
    (cat 3 '/~/eauth?abort&nonce=' (scot %uv nonce))
  ;<  ~  bind:m
    (expect-moves mos ex-rs ~)
  (pure:m ~)
::
++  test-eauth-incoming-aborted-with-duct
  %-  eval-mare
  =/  m  (mare ,~)
  ^-  form:m
  =,  server:eauth
  ;<  ~  bind:m  (setup-for-eauth 'http://hoster.com')
  ;<  *  bind:m  start
  ;<  *  bind:m  sage
  ;<  *  bind:m  grant
  ::  visitor returns, saying the attempt was aborted. we delete it
  ::
  ;<  ex-rs=$-(move tang)  bind:m
    (make-ex-resp 303 ['location' '/~/login?eauth&redirect=%2Ffinal']~ ~)
  ;<  mos=(list move)  bind:m
    =;  url=@t  (get url ~)
    (cat 3 '/~/eauth?abort&nonce=' (scot %uv nonce))
  ;<  ~  bind:m
    (expect-moves mos ex-rs (ex-boon %0 %shut nonce) ~)
  (pure:m ~)
::
++  test-eauth-incoming-delete
  %-  eval-mare
  =/  m  (mare ,~)
  ^-  form:m
  =,  server:eauth
  ;<  ~  bind:m  (setup-for-eauth 'http://hoster.com')
  ;<  *  bind:m  start
  ;<  *  bind:m  sage
  ;<  *  bind:m  grant
  ;<  *  bind:m  final
  ::  visitor tells us they want the session deleted
  ::
  ;<  mos=(list move)  bind:m
    %+  call  ~[/http-blah]
    [%plea ~sampel %e /eauth/0 %0 %shut nonce]
  ;<  ~  bind:m
    %+  expect-moves  mos
    :~  ex-done
        (ex-boon %0 %shut nonce)
    ==
  (pure:m ~)
::
++  test-eauth-outgoing
  %-  eval-mare
  =/  m  (mare ,~)
  ^-  form:m
  =,  client:eauth
  ;<  ~  bind:m  (setup-for-eauth 'http://client.com')
  ::  visitor uses eauth page to approve a login attempt,
  ::  we send ~hoster the token and await its url
  ::
  ;<  mos=(list move)  bind:m  grant
  ;<  now=@da  bind:m  get-now
  ;<  ~  bind:m
    %+  expect-moves  mos
    :~  (ex-plea ~hoster %0 %open 0vnonce `0v4.qkgot.d07e3.pi1qd.m1bhj.ti8bo)
        (ex-wait /eauth/expire/visiting/~hoster/0vnonce (add now eauth-timeout:eyre-gate))
    ==
  ::  upon receiving an %okay from ~hoster, redirect the user
  ::
  ;<  ex-rs=$-(move tang)  bind:m
    =/  loc=@t
      'http://hoster.com/~/eauth?nonce=0vnonce&token=0v4.qkgot.d07e3.pi1qd.m1bhj.ti8bo'
    (make-ex-resp 303 ['location' loc]~ ~)
  ;<  mos=(list move)  bind:m  okay
  ;<  ~  bind:m
    (expect-moves mos ex-rs ~)
  (pure:m ~)
::
++  test-eauth-unauthenticated-approval
  %-  eval-mare
  =/  m  (mare ,~)
  ^-  form:m
  =,  client:eauth
  ;<  ~  bind:m  (setup-for-eauth 'http://client.com')
  ::  visitor attempts to approve an eauth attempt without being authenticated
  ::
  ;<  ~  bind:m  |=(=state [%& ~ state(guest &, sesh ~)])
  ;<  mos=(list move)  bind:m
    =/  body  'server=~hoster&nonce=0vnonce'
    (post '/~/eauth' ~ body)
  ::  eyre must not comply, instead redirect to login page
  ::
  ;<  ex-rs=$-(move tang)  bind:m
    (make-ex-resp 303 ~['location'^'/~/login?redirect=%2F~%2Feauth'] ~)
  ;<  ~  bind:m
    %+  expect-moves  mos
    :~  ex-rs
    ==
  (pure:m ~)
::
++  test-perform-init-start-channel
  %-  eval-mare
  perform-init-start-channel-2
::
++  perform-init-start-channel-2
  =/  m  (mare ,~)
  ^-  form:m
  ;<  ~  bind:m  perform-init-wo-timer
  ;<  ~  bind:m  perform-born
  ;<  ~  bind:m  (wait ~d1)
  ;<  ~  bind:m  perform-authentication-2
  ::  send the channel a poke and a subscription request
  ::
  ;<  mos=(list move)  bind:m
    %^  put  '/~/channel/0123456789abcdef'  ~
    '''
    [{"action": "poke",
      "id": 0,
      "ship": "nul",
      "app": "one",
      "mark": "a",
      "json": 5},
     {"action": "subscribe",
      "id": 1,
      "ship": "nul",
      "app": "two",
      "path": "/one/two/three"}
    ]
    '''
  ;<  now=@da  bind:m  get-now
  =/  mov-1
    %^  ex-gall-deal  /channel/poke/'0123456789abcdef'/'0'  ~nul
    [%one %poke-as %a %json !>([%n '5'])]
  =/  mov-2
    %^  ex-gall-deal  /channel/subscription/'0123456789abcdef'/'1'/~nul/two/~nul
      ~nul
    [%two %watch /one/two/three]
  ;<  mov-3=$-(move tang)  bind:m
    (make-ex-resp 204 ~ ~)
  =/  mov-4
    %+  ex  ~[/http-blah]
    [%pass /channel/timeout/'0123456789abcdef' %b %wait (add now ~h12)]
  (expect-moves mos mov-1 mov-2 mov-3 mov-4 ~)
--
