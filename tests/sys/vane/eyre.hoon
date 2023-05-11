/+  *test
/=  eyre-raw  /sys/vane/eyre
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
  [%& moves state]
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
  [%& moves state]
::
++  get
  |=  [url=@t =header-list:http]
  =/  m  (mare ,(list move))
  ^-  form:m
  %+  call  ~[/http-blah]
  [%request %.n [%ipv4 .192.168.1.1] [%'GET' url header-list ~]]
::
++  post
  |=  [url=@t =header-list:http body=@t]
  =/  m  (mare ,(list move))
  ^-  form:m
  =/  body  (as-octs:mimes:html body)
  %+  call  ~[/http-blah]
  [%request %.n [%ipv4 .192.168.1.1] [%'POST' url header-list `body]]
::
++  put
  |=  [url=@t =header-list:http body=@t]
  =/  m  (mare ,(list move))
  ^-  form:m
  =/  body  (as-octs:mimes:html body)
  %+  call  ~[/http-blah]
  [%request %.n [%ipv4 .192.168.1.1] [%'PUT' url header-list `body]]
::  use different wire
::
++  put-2
  |=  [url=@t =header-list:http body=@t]
  =/  m  (mare ,(list move))
  ^-  form:m
  =/  body  (as-octs:mimes:html body)
  %+  call  ~[/http-put-request]
  [%request %.n [%ipv4 .192.168.1.1] [%'PUT' url header-list `body]]
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
++  ex-response
  |=  [status=@ud headers=header-list:http body=(unit octs)]
  |=  mov=move
  ^-  tang
  ?.  ?=([[[%http-blah ~] ~] %give %response %start * * %.y] mov)
    [leaf+"expected %response, got: {<mov>}" ~]
  =?  headers  ?=(^ body)
    %+  weld  headers
    :~  ['content-length' (crip ((d-co:co 1) p.u.body))]
        ['set-cookie' g-sook]
    ==
  ;:  weld
    (expect-eq !>(status) !>(status-code.response-header.http-event.p.card.mov))
    (expect-eq !>(body) !>(data.http-event.p.card.mov))
    (expect-eq !>(headers) !>(headers.response-header.http-event.p.card.mov))
  ==
++  ex-start-response
  |=  [status=@ud headers=header-list:http body=(unit octs)]
  |=  mov=move
  ^-  tang
  ?.  ?=([[[%http-blah ~] ~] %give %response %start * * %.n] mov)
    [leaf+"expected start %response, got: {<mov>}" ~]
  =.  headers  (weld headers ~[g-head])
  ;:  weld
    (expect-eq !>(status) !>(status-code.response-header.http-event.p.card.mov))
    (expect-eq !>(body) !>(data.http-event.p.card.mov))
    (expect-eq !>(headers) !>(headers.response-header.http-event.p.card.mov))
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
::  produce the 204 response to a put request
::
++  ex-204
  (ex-response 204 ['set-cookie' cookie-string]~ ~)
::
++  ex-204-2
  |=  mov=move
  ?.  ?=([[[%http-put-request ~] ~] %give %response %start * * %.y] mov)
    [leaf+"expected %response, got: {<mov>}" ~]
  =/  headers  ['set-cookie' cookie-string]~
  ;:  weld
    (expect-eq !>(204) !>(status-code.response-header.http-event.p.card.mov))
    (expect-eq !>(~) !>(data.http-event.p.card.mov))
    (expect-eq !>(headers) !>(headers.response-header.http-event.p.card.mov))
  ==
::
++  ex-channel-response
  |=  body=@t
  |=  mov=move
  ^-  tang
  ?.  ?=([[[%http-blah ~] ~] %give %response %start * * %.n] mov)
    [leaf+"expected start %response, got: {<mov>}" ~]
  =/  headers
    :~  ['content-type' 'text/event-stream']
        ['cache-control' 'no-cache']
        ['connection' 'keep-alive']
        ['set-cookie' cookie-string]
    ==
  =/  body  `(as-octs:mimes:html body)
  ;:  weld
    (expect-eq !>(200) !>(status-code.response-header.http-event.p.card.mov))
    (expect-eq !>(body) !>(data.http-event.p.card.mov))
    (expect-eq !>(headers) !>(headers.response-header.http-event.p.card.mov))
  ==
::
++  ex-gall-deal
  |=  [=wire our=@p app=term =deal:gall]
  |=  mov=move
  ^-  tang
  %+  weld  (expect-eq !>(~[/http-blah]) !>(duct.mov))
  (expect-gall-deal [wire [our ~nul] app deal] card.mov)
::
++  expect-gall-deal
  |=  $:  expected=[wire=path id=sock app=term =deal:gall]
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
  =/  res  (computation eyre-gate ~1111.1.1)
  ?-  -.res
    %&  ~
    %|  p.res
  ==
::
++  scry-provides-code  ^-  roof
  |=  [gang =view =beam]
  ^-  (unit (unit cage))
  ?:  =(%gd view)  ``noun+!>(%base)
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
++  cookie-value
  'urbauth-~nul=0v2.v5g1m.rr6kg.bjj3k.59t1m.qp48h'
::
++  cookie-string
  %^  cat  3  cookie-value
  '; Path=/; Max-Age=604800'
::
++  cookie  ['cookie' cookie-value]~
::
++  g-name  ~rocfyn-bistyv-tadlux-modsel--bittex-patsun-sitpec-ravnul
++  g-auth  ['cookie' g-cook]
++  g-cook  'urbauth-~nul=0v5.gbhev.sbeh0.3rov1.o6ibh.a3t9r'
++  g-sook  (cat 3 g-cook '; Path=/; Max-Age=604800')
++  g-head  ['set-cookie' g-sook]
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
  ;<  mos=(list move)  bind:m  (get '/' ~)
  =/  headers  ['content-type' 'text/html']~
  =/  body  `(error-page:eyre-gate 404 %.n '/' ~)
  (expect-moves mos (ex-response 404 headers body) ~)
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
  ;<  mos=(list move)  bind:m
    =/  response  !>([200 ['content-type' 'text/html']~])
    =/  sign=sign:eyre-gate
      [%gall %unto %fact %http-response-header response]
    (take /watch-response/[eyre-id] ~[/http-blah] sign)
  =/  headers  ['content-type' 'text/html']~
  (expect-moves mos (ex-start-response 200 headers ~) ~)
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
  ;<  mos=(list move)  bind:m
    =/  sign=sign:eyre-gate
      [%gall %unto %poke-ack ~ [%leaf "/~zod/...../app1:<[1 1].[1 20]>"]~]
    (take /run-app-request/[eyre-id] ~[/http-blah] sign)
  =/  mov-1  (ex-gall-deal /watch-response/[eyre-id] g-name %app1 [%leave ~])
  =/  response  `(internal-server-error:eyre-gate %.n '/' ~)
  =/  mov-2  (ex-response 500 ['content-type' 'text/html']~ response)
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
  ;<  mos=(list move)  bind:m
    =/  response  !>([200 ['content-type' 'text/html']~])
    =/  sign=sign:eyre-gate
      [%gall %unto %fact %http-response-header response]
    (take /watch-response/[eyre-id] ~[/http-blah] sign)
  ;<  ~  bind:m
    =/  headers  ['content-type' 'text/html']~
    (expect-moves mos (ex-start-response 200 headers ~) ~)
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
  ::  app then gives a redirect to Eyre
  ::
  =/  headers  ['location' '/~/login?redirect=/~landscape/inner-path']~
  ;<  mos=(list move)  bind:m
    =/  sign=sign:eyre-gate
      [%gall %unto %fact %http-response-header !>([303 headers])]
    (take /watch-response/[eyre-id] ~[/http-blah] sign)
  ;<  ~  bind:m  (expect-moves mos (ex-start-response 303 headers ~) ~)
  ;<  ~  bind:m  (wait ~d1)
  ::  the browser then fetches the login page
  ::
  ;<  ~  bind:m  perform-authentication-2
  ;<  ~  bind:m  (wait ~h1)
  ::  going back to the original url will acknowledge the authentication cookie
  ::
  ;<  mos=(list move)  bind:m
    (get '/~landscape/inner-path' ['cookie' cookie-value]~)
  =/  mov-1
    %^  ex-gall-deal  /watch-response/[eyre-id]  ~nul
    [%app1 %watch /http-response/[eyre-id]]
  =/  mov-2
    =/  request  [%'GET' '/~landscape/inner-path' ['cookie' cookie-value]~ ~]
    =/  response  !>([eyre-id %.y %.n [%ipv4 .192.168.1.1] request])
    %^  ex-gall-deal  /run-app-request/[eyre-id]  ~nul
    [%app1 %poke %handle-http-request response]
  (expect-moves mos mov-1 mov-2 ~)
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
  ;<  mos=(list move)  bind:m  (get '/' ~)
  (expect-moves mos (ex-response 404 [g-head]~ ~) ~)
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
  =/  wire  /channel/subscription/'0123456789abcdef'/1/~nul/two
  (expect-moves mos (ex-gall-deal wire ~nul %two %leave ~) ~)
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
  =/  wire  /channel/subscription/'0123456789abcdef'/'1'/~nul/two
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
  ;<  mos=(list move)  bind:m
    (get '/~/channel/0123456789abcdef' cookie)
  ;<  now=@da  bind:m  get-now
  =/  mov-1  (ex-wait /channel/heartbeat/'0123456789abcdef' (add now ~s20))
  =/  mov-2
    %-  ex-channel-response
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
    %^  put  '/~/channel/0123456789abcdef'  cookie
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
  =/  mov-2  ex-204
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
  =/  wire  /channel/subscription/'0123456789abcdef'/'1'/~nul/two
  ;<  mos=(list move)  bind:m
    (take wire ~[/http-blah] %gall %unto %watch-ack ~)
  ;<  ~  bind:m  (expect-moves mos ~)
  ;<  ~  bind:m  (wait ~m1)
  ::  sending an unsubscribe sends an unsubscribe to gall
  ::
  ;<  mos=(list move)  bind:m
    %^  put  '/~/channel/0123456789abcdef'  cookie
    '''
    [{"action": "unsubscribe",
      "id": 2,
      "subscription": 1}
    ]
    '''
  =/  wire  /channel/subscription/'0123456789abcdef'/'1'/~nul/two
  =/  mov-1  (ex-gall-deal wire ~nul %two %leave ~)
  =/  mov-2  ex-204
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
  =/  wire  /channel/subscription/'0123456789abcdef'/'1'/~nul/two
  ;<  mos=(list move)  bind:m
    (take wire ~[/http-blah] %gall %unto %watch-ack ~)
  ;<  ~  bind:m  (expect-moves mos ~)
  ;<  ~  bind:m  (wait ~m1)
  ::  now make a second subscription from the client on the same path
  ::
  ;<  mos=(list move)  bind:m
    %^  put  '/~/channel/0123456789abcdef'  cookie
    '''
    [{"action": "subscribe",
      "id": 2,
      "ship": "nul",
      "app": "two",
      "path": "/one/two/three"}
    ]
    '''
  =/  wire  /channel/subscription/'0123456789abcdef'/'2'/~nul/two
  =/  mov-1  (ex-gall-deal wire ~nul %two %watch /one/two/three)
  =/  mov-2  ex-204
  =/  mov-3  (ex-rest /channel/timeout/'0123456789abcdef' ~1111.1.2..12.00.00)
  =/  mov-4  (ex-wait /channel/timeout/'0123456789abcdef' ~1111.1.2..12.03.00)
  ::  subscription gets 2 results
  ::
  ;<  mos=(list move)  bind:m
    =/  wire  /channel/subscription/'0123456789abcdef'/'1'/~nul/two
    =/  =cage  [%json !>(`json`[%a [%n '1'] [%n '2'] ~])]
    (take wire ~[/http-blah] %gall %unto %fact cage)
  ;<  ~  bind:m  (expect-moves mos ~)
  ::
  ;<  mos=(list move)  bind:m
    =/  wire  /channel/subscription/'0123456789abcdef'/'2'/~nul/two
    =/  =cage  [%json !>(`json`[%a [%n '1'] [%n '2'] ~])]
    (take wire ~[/http-blah] %gall %unto %fact cage)
  ;<  ~  bind:m  (expect-moves mos ~)
  ::  open up the channel
  ::
  ;<  mos=(list move)  bind:m
    (get '/~/channel/0123456789abcdef' cookie)
  ;<  now=@da  bind:m  get-now
  =/  mov-1  (ex-wait /channel/heartbeat/'0123456789abcdef' (add now ~s20))
  =/  mov-2
    %-  ex-channel-response
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
  ;<  ~  bind:m  (expect-moves mos mov-1 mov-2 mov-3 ~)
  ::  we can close the first channel without closing the second
  ::
  ;<  mos=(list move)  bind:m
    %^  put  '/~/channel/0123456789abcdef'  cookie
    '''
    [{"action": "unsubscribe",
      "id": 3,
      "subscription": 1}
    ]
    '''
  =/  wire  /channel/subscription/'0123456789abcdef'/'1'/~nul/two
  =/  mov-1  (ex-gall-deal wire ~nul %two %leave ~)
  =/  mov-2  ex-204
  ;<  ~  bind:m  (expect-moves mos mov-1 mov-2 ~)
  ::  gall responds on the second subscription.
  ::
  ::    This just tests that closing one of the two subscriptions doesn't
  ::    unsubscribe to the other.
  ::
  ;<  mos=(list move)  bind:m
    =/  wire  /channel/subscription/'0123456789abcdef'/'2'/~nul/two
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
  =/  wire  /channel/subscription/'0123456789abcdef'/'1'/~nul/two
  ;<  mos=(list move)  bind:m
    (take wire ~[/http-blah] %gall %unto %watch-ack ~)
  ;<  ~  bind:m  (expect-moves mos ~)
  ::  open the http channel
  ::
  ;<  ~  bind:m  (wait ~m2)
  ;<  mos=(list move)  bind:m
    (get '/~/channel/0123456789abcdef' cookie)
  ;<  now=@da  bind:m  get-now
  =/  heartbeat  (add now ~s20)
  =/  mov-1  (ex-wait /channel/heartbeat/'0123456789abcdef' heartbeat)
  =/  mov-2
    %-  ex-channel-response
    '''
    id: 0
    data: {"ok":"ok","id":0,"response":"poke"}

    id: 1
    data: {"ok":"ok","id":1,"response":"subscribe"}


    '''
  ::  opening the channel cancels the timeout timer
  ::
  =/  mov-3  (ex-rest /channel/timeout/'0123456789abcdef' ~1111.1.2..12.00.00)
  ;<  ~  bind:m  (expect-moves mos mov-1 mov-2 mov-3 ~)
  ;<  ~  bind:m  (wait ~m1)
  ::  first subscription result gets sent to the user
  ::
  ;<  mos=(list move)  bind:m
    =/  wire  /channel/subscription/'0123456789abcdef'/'1'/~nul/two
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
    %^  put-2  '/~/channel/0123456789abcdef'  cookie
    '''
    [{"action": "ack",
      "event-id": 1}
    ]
    '''
  ;<  ~  bind:m  (expect-moves mos ex-204-2 ~)
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
    =/  wire  /channel/subscription/'0123456789abcdef'/'1'/~nul/two
    =/  =cage  [%json !>(`json`[%a [%n '2'] ~])]
    (take wire ~[/http-blah] %gall %unto %fact cage)
  ;<  ~  bind:m  (expect-moves mos ~)
  ;<  ~  bind:m  (wait ~m1)
  ::  the client now retries to connect
  ::
  ::    Because the client has acknowledged up to event 1, we should start the connection by
  ::    resending events 2 and 3.
  ::
  ;<  mos=(list move)  bind:m
    (get '/~/channel/0123456789abcdef' cookie)
  ;<  now=@da  bind:m  get-now
  =/  heartbeat  (add now ~s20)
  =/  mov-1  (ex-wait /channel/heartbeat/'0123456789abcdef' heartbeat)
  =/  mov-2
    %-  ex-channel-response
    '''
    id: 2
    data: {"json":[1],"id":1,"response":"diff"}

    id: 3
    data: {"json":[2],"id":1,"response":"diff"}


    '''
  =/  mov-3
    (ex-rest /channel/timeout/'0123456789abcdef' :(add ~1111.1.2 ~m6 ~h12))
  (expect-moves mos mov-1 mov-2 mov-3 ~)
::
++  test-channel-subscription-clogged
  %-  eval-mare
  =/  m  (mare ,~)
  ;<  ~  bind:m  perform-init-start-channel-2
  ;<  ~  bind:m  (wait (add ~s1 clog-timeout:eyre-gate))
  ::  subscription gets a success message
  ::
  =/  wire  /channel/subscription/'0123456789abcdef'/'1'/~nul/two
  ;<  mos=(list move)  bind:m
    (take wire ~[/http-blah] %gall %unto %watch-ack ~)
  ;<  ~  bind:m  (expect-moves mos ~)
  ::  opens the http channel
  ::
  ;<  tested-elsewhere=(list move)  bind:m
    (get '/~/channel/0123456789abcdef' cookie)
  ::  user gets sent multiple subscription results
  ::
  =/  max=@ud  clog-threshold:eyre-gate
  =/  cur=@ud  0
  |-  ^-  form:m
  =*  loop-fact  $
  ?.  =(cur max)
    ;<  tested-elsewhere=(list move)  bind:m
      =/  wire  /channel/subscription/'0123456789abcdef'/'1'/~nul/two
      =/  =cage  [%json !>(`json`[%a [%n '1'] ~])]
      (take wire ~[/http-blah] %gall %unto %fact cage)
    loop-fact(cur +(cur))
  ::  the next subscription result should trigger a clog
  ::
  ;<  mos=(list move)  bind:m
    =/  wire  /channel/subscription/'0123456789abcdef'/'1'/~nul/two
    =/  =cage  [%json !>(`json`[%a [%n '1'] ~])]
    (take wire ~[/http-blah] %gall %unto %fact cage)
  =/  mov-1
    %-  ex-continue-response  :_  %.n  :-  ~
    %-  as-octt:mimes:html
    """
    id: {((d-co:co 1) +(clog-threshold:eyre-gate))}
    data: \{"json":[1],"id":1,"response":"diff"}


    """
  =/  wire  /channel/subscription/'0123456789abcdef'/'1'/~nul/two
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
    =/  wire  /channel/subscription/'0123456789abcdef'/'1'/~nul/two
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
  =-  state(sessions.authentication-state.server-state.ax.gate -)
  %+  ~(put by sessions.authentication-state.server-state.ax.gate.state)
    0vguest
  [fake+~sampel-sampel-sampel-sampel--sampel-sampel-sampel-sampel ~2222.2.2 ~]
::  +perform-born: %born an eyre-gate
::
++  perform-born
  =/  m  (mare ,~)
  ^-  form:m
  ;<  mos=(list move)  bind:m  (call ~[/unix] [%born ~])
  (expect-moves mos (ex-set-config *http-config:eyre) (ex-sessions ~) ~)
::
++  test-perform-authentication
  %-  eval-mare
  =/  m  (mare ,~)
  ^-  form:m
  ;<  ~  bind:m  perform-init-wo-timer
  ;<  ~  bind:m  perform-born
  perform-authentication-2

::  +perform-authentication: goes through the authentication flow
::
++  perform-authentication-2
  =/  m  (mare ,~)
  ^-  form:m
  ;<  mos=(list move)  bind:m 
    (get '/~/login?redirect=/~landscape/inner-path' g-auth ~)
  ;<  ~  bind:m
    =/  headers  ['content-type' 'text/html']~
    =/  body  `(login-page:eyre-gate `'/~landscape/inner-path' ~nul fake+g-name %.n)
    (expect-moves mos (ex-response 200 headers body) ~)
  ;<  mos=(list move)  bind:m
    =/  body  'password=lidlut-tabwed-pillex-ridrup&redirect=/~landscape'
    (post '/~/login' ~ body)
  ;<  ~  bind:m
    =/  headers  ~[['location' '/~landscape'] ['set-cookie' cookie-string]]
    =/  token  '0v2.v5g1m.rr6kg.bjj3k.59t1m.qp48h'
    (expect-moves mos (ex-sessions token ~ ~) (ex-response 303 headers ~) ~)
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
    %^  put  '/~/channel/0123456789abcdef'  cookie
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
    %^  ex-gall-deal  /channel/subscription/'0123456789abcdef'/'1'/~nul/two
      ~nul
    [%two %watch /one/two/three]
  =/  mov-3  (ex-response 204 ['set-cookie' cookie-string]~ ~)
  =/  mov-4
    %+  ex  ~[/http-blah]
    [%pass /channel/timeout/'0123456789abcdef' %b %wait (add now ~h12)]
  (expect-moves mos mov-1 mov-2 mov-3 mov-4 ~)
--
