/+  *test, *test-ford
::
/=  http-server-raw  /:  /===/sys/vane/rver  /!noun/
::
!:
::
=/  test-pit=vase  !>(..zuse)
=/  http-server-gate  (http-server-raw test-pit)
::
|%
++  test-init
  =^  results1  http-server-gate
    %-  http-server-call  :*
      http-server-gate
      now=~1111.1.1
      scry=scry-provides-code
      call-args=[duct=~[/init] ~ [%init ~nul]]
      expected-moves=~
    ==
  ::
  results1
::
++  test-duplicate-bindings
  ::
  =^  results1  http-server-gate
    %-  http-server-call  :*
      http-server-gate
      now=~1111.1.1
      scry=scry-provides-code
      call-args=[duct=~[/init] ~ [%init ~nul]]
      expected-moves=~
    ==
  ::  app1 binds successfully
  ::
  =^  results2  http-server-gate
    %-  http-server-call  :*
      http-server-gate
      now=~1111.1.2
      scry=scry-provides-code
      call-args=[duct=~[/app1] ~ [%connect [~ /] %app1]]
      expected-moves=[duct=~[/app1] %give %bound %.y [~ /]]~
    ==
  ::  app2 tries to bind to the same path and fails
  ::
  =^  results3  http-server-gate
    %-  http-server-call  :*
      http-server-gate
      now=~1111.1.3
      scry=scry-provides-code
      call-args=[duct=~[/app2] ~ [%connect [~ /] %app2]]
      expected-moves=[duct=~[/app2] %give %bound %.n [~ /]]~
    ==
  ::
  ;:  weld
    results1
    results2
    results3
  ==
::
++  test-remove-binding
  ::
  =^  results1  http-server-gate
    %-  http-server-call  :*
      http-server-gate
      now=~1111.1.1
      scry=scry-provides-code
      call-args=[duct=~[/init] ~ [%init ~nul]]
      expected-moves=~
    ==
  ::  app1 binds successfully
  ::
  =^  results2  http-server-gate
    %-  http-server-call  :*
      http-server-gate
      now=~1111.1.2
      scry=scry-provides-code
      call-args=[duct=~[/app1] ~ [%connect [~ /] %app1]]
      expected-moves=[duct=~[/app1] %give %bound %.y [~ /]]~
    ==
  ::  app1 unbinds
  ::
  =^  results3  http-server-gate
    %-  http-server-call  :*
      http-server-gate
      now=~1111.1.3
      scry=scry-provides-code
      call-args=[duct=~[/app1] ~ [%disconnect [~ /]]]
      expected-moves=~
    ==
  ::  app2 binds successfully
  ::
  =^  results4  http-server-gate
    %-  http-server-call  :*
      http-server-gate
      now=~1111.1.4
      scry=scry-provides-code
      call-args=[duct=~[/app2] ~ [%connect [~ /] %app2]]
      expected-moves=[duct=~[/app2] %give %bound %.y [~ /]]~
    ==
  ::
  ;:  weld
    results1
    results2
    results3
    results4
  ==
::
++  test-cant-remove-other-ducts-binding
  ::
  =^  results1  http-server-gate
    %-  http-server-call  :*
      http-server-gate
      now=~1111.1.1
      scry=scry-provides-code
      call-args=[duct=~[/init] ~ [%init ~nul]]
      expected-moves=~
    ==
  ::  app1 binds successfully
  ::
  =^  results2  http-server-gate
    %-  http-server-call  :*
      http-server-gate
      now=~1111.1.2
      scry=scry-provides-code
      call-args=[duct=~[/app1] ~ [%connect [~ /] %app1]]
      expected-moves=[duct=~[/app1] %give %bound %.y [~ /]]~
    ==
  ::  app2 tries to steal the binding by disconnecting the path
  ::
  =^  results3  http-server-gate
    %-  http-server-call  :*
      http-server-gate
      now=~1111.1.3
      scry=scry-provides-code
      call-args=[duct=~[/app2] ~ [%disconnect [~ /]]]
      expected-moves=~
    ==
  ::  app2 doesn't bind successfully because it couldn't remove app1's binding
  ::
  =^  results4  http-server-gate
    %-  http-server-call  :*
      http-server-gate
      now=~1111.1.4
      scry=scry-provides-code
      call-args=[duct=~[/app2] ~ [%connect [~ /] %app2]]
      expected-moves=[duct=~[/app2] %give %bound %.n [~ /]]~
    ==
  ::
  ;:  weld
    results1
    results2
    results3
    results4
  ==
::  tests that when we have no match, that we fall back to the built-in 404
::
++  test-builtin-four-oh-four
  ::
  =^  results1  http-server-gate
    %-  http-server-call  :*
      http-server-gate
      now=~1111.1.1
      scry=scry-provides-code
      call-args=[duct=~[/init] ~ [%init ~nul]]
      expected-moves=~
    ==
  ::  when there's no configuration and nothing matches, expect 404
  ::
  =^  results2  http-server-gate
    %-  http-server-call  :*
      http-server-gate
      now=~1111.1.1
      scry=scry-provides-code
      ^=  call-args
        :*  duct=~[/http-blah]  ~
            %request
            %.n
            [%ipv4 .192.168.1.1]
            [%'GET' '/' ~ ~]
        ==
      ^=  expectec-moves
        ^-  (list move:http-server-gate)
        :~  :*  duct=~[/http-blah]
                %give
                %response
                %start
                :-  404
                :~  ['content-type' 'text/html']
                    ['content-length' '156']
                ==
                [~ (error-page:http-server-gate 404 %.n '/' ~)]
                complete=%.y
        ==  ==
    ==
  ::
  ;:  weld
    results1
    results2
  ==
::
++  test-basic-app-request
  ::
  =^  results1  http-server-gate
    %-  http-server-call  :*
      http-server-gate
      now=~1111.1.1
      scry=scry-provides-code
      call-args=[duct=~[/init] ~ [%init ~nul]]
      expected-moves=~
    ==
  ::  app1 binds successfully
  ::
  =^  results2  http-server-gate
    %-  http-server-call  :*
      http-server-gate
      now=~1111.1.2
      scry=scry-provides-code
      call-args=[duct=~[/app1] ~ [%connect [~ /] %app1]]
      expected-moves=[duct=~[/app1] %give %bound %.y [~ /]]~
    ==
  ::  outside requests a path that app1 has bound to
  ::
  =^  results3  http-server-gate
    %-  http-server-call-with-comparator  :*
      http-server-gate
      now=~1111.1.3
      scry=scry-provides-code
      ^=  call-args
        :*  duct=~[/http-blah]  ~
            %request
            %.n
            [%ipv4 .192.168.1.1]
            [%'GET' '/' ~ ~]
        ==
      ^=  comparator
        |=  moves=(list move:http-server-gate)
        ^-  tang
        ::
        ?.  ?=([* ~] moves)
          [%leaf "wrong number of moves: {<(lent moves)>}"]~
        ::
        ::
        =/  move=move:http-server-gate                              i.moves
        =/  =duct                                             duct.move
        =/  card=(wind note:http-server-gate gift:able:http-server-gate)  card.move
        ::
        %+  weld
          (expect-eq !>(~[/http-blah]) !>(duct))
        ::
        %+  expect-gall-deal
          :+  /run-app/app1  [~nul ~nul]
              ^-  cush:gall
              :*  %app1  %poke  %handle-http-request
                  !>([%.n %.n [%ipv4 .192.168.1.1] [%'GET' '/' ~ ~]])
              ==
          card
    ==
  ::  theoretical outside response
  ::
  =^  results4  http-server-gate
    %-  http-server-take  :*
      http-server-gate
      now=~1111.1.4
      scry=scry-provides-code
      ^=  take-args
        :*  wire=/run-app/app1  duct=~[/http-blah]
            ^-  (hypo sign:http-server-gate)
            :-  *type
            :*  %g  %unto  %http-response
                %start
                [200 ['content-type' 'text/html']~]
                [~ (as-octs:mimes:html 'Hiya!')]
                %.y
            ==
         ==
      ^=  expected-move
        :~  :*  duct=~[/http-blah]  %give  %response
                [%start [200 ['content-type' 'text/html']~] `[5 'Hiya!'] %.y]
    ==  ==  ==
  ::
  ;:  weld
    results1
    results2
    results3
    results4
  ==
::
++  test-app-error
  ::
  =^  results1  http-server-gate
    %-  http-server-call  :*
      http-server-gate
      now=~1111.1.1
      scry=scry-provides-code
      call-args=[duct=~[/init] ~ [%init ~nul]]
      expected-moves=~
    ==
  ::  app1 binds successfully
  ::
  =^  results2  http-server-gate
    %-  http-server-call  :*
      http-server-gate
      now=~1111.1.2
      scry=scry-provides-code
      call-args=[duct=~[/app1] ~ [%connect [~ /] %app1]]
      expected-moves=[duct=~[/app1] %give %bound %.y [~ /]]~
    ==
  ::  outside requests a path that app1 has bound to
  ::
  =^  results3  http-server-gate
    %-  http-server-call-with-comparator  :*
      http-server-gate
      now=~1111.1.3
      scry=scry-provides-code
      ^=  call-args
        :*  duct=~[/http-blah]  ~
            %request
            %.n
            [%ipv4 .192.168.1.1]
            [%'GET' '/' ~ ~]
        ==
      ^=  comparator
        |=  moves=(list move:http-server-gate)
        ^-  tang
        ::
        ?.  ?=([* ~] moves)
          [%leaf "wrong number of moves: {<(lent moves)>}"]~
        ::
        ::
        =/  move=move:http-server-gate                              i.moves
        =/  =duct                                             duct.move
        =/  card=(wind note:http-server-gate gift:able:http-server-gate)  card.move
        ::
        %+  weld
          (expect-eq !>(~[/http-blah]) !>(duct))
        ::
        %+  expect-gall-deal
          :+  /run-app/app1  [~nul ~nul]
              ^-  cush:gall
              :*  %app1  %poke  %handle-http-request
                  !>([%.n %.n [%ipv4 .192.168.1.1] [%'GET' '/' ~ ~]])
              ==
          card
    ==
  ::  the poke fails. we should relay this to the client
  ::
  =^  results4  http-server-gate
    %-  http-server-take  :*
      http-server-gate
      now=~1111.1.4
      scry=scry-provides-code
      ^=  take-args
        :*  wire=/run-app/app1  duct=~[/http-blah]
            ^-  (hypo sign:http-server-gate)
            :-  *type
            :*  %g  %unto  %coup  ~
                :~  [%leaf "/~zod/...../app1:<[1 1].[1 20]>"]
            ==  ==
         ==
      ^=  expected-move
        :~  :*  duct=~[/http-blah]  %give  %response
                %start
                :-  500
                :~  ['content-type' 'text/html']
                    ['content-length' '180']
                ==
                [~ (internal-server-error:http-server-gate %.n '/' ~)]
                complete=%.y
    ==  ==  ==
  ::
  ;:  weld
    results1
    results2
    results3
    results4
  ==
::
++  test-multipart-app-request
  ::
  =^  results1  http-server-gate
    %-  http-server-call  :*
      http-server-gate
      now=~1111.1.1
      scry=scry-provides-code
      call-args=[duct=~[/init] ~ [%init ~nul]]
      expected-moves=~
    ==
  ::  app1 binds successfully
  ::
  =^  results2  http-server-gate
    %-  http-server-call  :*
      http-server-gate
      now=~1111.1.2
      scry=scry-provides-code
      call-args=[duct=~[/app1] ~ [%connect [~ /] %app1]]
      expected-moves=[duct=~[/app1] %give %bound %.y [~ /]]~
    ==
  ::  outside requests a path that app1 has bound to
  ::
  =^  results3  http-server-gate
    %-  http-server-call-with-comparator  :*
      http-server-gate
      now=~1111.1.3
      scry=scry-provides-code
      ^=  call-args
        :*  duct=~[/http-blah]  ~
            %request
            %.n
            [%ipv4 .192.168.1.1]
            [%'GET' '/' ~ ~]
        ==
      ^=  comparator
        |=  moves=(list move:http-server-gate)
        ^-  tang
        ::
        ?.  ?=([* ~] moves)
          [%leaf "wrong number of moves: {<(lent moves)>}"]~
        ::
        ::
        =/  move=move:http-server-gate                              i.moves
        =/  =duct                                             duct.move
        =/  card=(wind note:http-server-gate gift:able:http-server-gate)  card.move
        ::
        %+  weld
          (expect-eq !>(~[/http-blah]) !>(duct))
        ::
        %+  expect-gall-deal
          :+  /run-app/app1  [~nul ~nul]
              ^-  cush:gall
              :*  %app1  %poke  %handle-http-request
                  !>([%.n %.n [%ipv4 .192.168.1.1] [%'GET' '/' ~ ~]])
              ==
          card
    ==
  ::  theoretical outside response
  ::
  =^  results4  http-server-gate
    %-  http-server-take  :*
      http-server-gate
      now=~1111.1.4
      scry=scry-provides-code
      ^=  take-args
        :*  wire=/run-app/app1  duct=~[/http-blah]
            ^-  (hypo sign:http-server-gate)  :-  *type
            :*  %g  %unto  %http-response
                %start
                [200 ['content-type' 'text/html']~]
                [~ (as-octs:mimes:html 'Hi')]
                %.n
            ==
         ==
      ^=  expected-move
        :~  :*  duct=~[/http-blah]  %give  %response
                [%start [200 ['content-type' 'text/html']~] `[2 'Hi'] %.n]
    ==  ==  ==
  ::  theoretical outside response
  ::
  =^  results5  http-server-gate
    %-  http-server-take  :*
      http-server-gate
      now=~1111.1.4
      scry=scry-provides-code
      ^=  take-args
        :*  wire=/run-app/app1  duct=~[/http-blah]
            ^-  (hypo sign:http-server-gate)  :-  *type
            :*  %g  %unto  %http-response
                [%continue [~ (as-octs:mimes:html 'ya!')] %.y]
            ==
         ==
      ^=  expected-move
        :~  :*  duct=~[/http-blah]  %give  %response
                [%continue `[3 'ya!'] %.y]
    ==  ==  ==
  ::
  ;:  weld
    results1
    results2
    results3
    results4
    results5
  ==
::  tests an app redirecting to the login handler, which then receives a post
::  and redirects back to app
::
++  test-login-handler-full-path
  ::
  =^  results1  http-server-gate
    %-  http-server-call  :*
      http-server-gate
      now=~1111.1.1
      scry=scry-provides-code
      call-args=[duct=~[/init] ~ [%init ~nul]]
      expected-moves=~
    ==
  ::  app1 binds successfully
  ::
  =^  results2  http-server-gate
    %-  http-server-call  :*
      http-server-gate
      now=~1111.1.2
      scry=scry-provides-code
      call-args=[duct=~[/app1] ~ [%connect [~ /'~landscape'] %app1]]
      expected-moves=[duct=~[/app1] %give %bound %.y [~ /'~landscape']]~
    ==
  ::  outside requests a path that app1 has bound to
  ::
  =^  results3  http-server-gate
    %-  http-server-call-with-comparator  :*
      http-server-gate
      now=~1111.1.3
      scry=scry-provides-code
      ^=  call-args
        :*  duct=~[/http-blah]  ~
            %request
            %.n
            [%ipv4 .192.168.1.1]
            [%'GET' '/~landscape/inner-path' ~ ~]
        ==
      ^=  comparator
        |=  moves=(list move:http-server-gate)
        ^-  tang
        ::
        ?.  ?=([* ~] moves)
          [%leaf "wrong number of moves: {<(lent moves)>}"]~
        ::
        ::
        =/  move=move:http-server-gate                              i.moves
        =/  =duct                                             duct.move
        =/  card=(wind note:http-server-gate gift:able:http-server-gate)  card.move
        ::
        %+  weld
          (expect-eq !>(~[/http-blah]) !>(duct))
        ::
        %+  expect-gall-deal
          :+  /run-app/app1  [~nul ~nul]
              ^-  cush:gall
              :*  %app1  %poke  %handle-http-request
                  !>([%.n %.n [%ipv4 .192.168.1.1] [%'GET' '/~landscape/inner-path' ~ ~]])
              ==
          card
    ==
  ::  app then gives a redirect to Eyre
  ::
  =^  results4  http-server-gate
    %-  http-server-take  :*
      http-server-gate
      now=~1111.1.4
      scry=scry-provides-code
      ^=  take-args
        :*  wire=/run-app/app1  duct=~[/http-blah]
            ^-  (hypo sign:http-server-gate)  :-  *type
            :*  %g  %unto  %http-response
                [%start [307 ['location' '/~/login?redirect=/~landscape/inner-path']~] ~ %.y]
            ==
         ==
      ^=  expected-move
        :~  :*  duct=~[/http-blah]  %give  %response
                [%start [307 ['location' '/~/login?redirect=/~landscape/inner-path']~] ~ %.y]
    ==  ==  ==
  ::  the browser then fetches the login page
  ::
  =^  results5  http-server-gate
    %-  perform-authentication  :*
      http-server-gate
      now=~1111.1.5
      scry=scry-provides-code
    ==
  ::  going back to the original url will acknowledge the authentication cookie
  ::
  =^  results6  http-server-gate
    %-  http-server-call-with-comparator  :*
      http-server-gate
      now=~1111.1.5..1.0.0
      scry=scry-provides-code
      ^=  call-args
        ^-  [=duct type=* wrapped-task=(hobo task:able:http-server-gate)]
        :*  duct=~[/http-blah]  ~
            %request
            %.n
            [%ipv4 .192.168.1.1]
            %'GET'
            '/~landscape/inner-path'
            ['cookie' 'urbauth=0v3.q0p7t.mlkkq.cqtto.p0nvi.2ieea']~
            ~
        ==
      ^=  comparator
        |=  moves=(list move:http-server-gate)
        ^-  tang
        ::
        ?.  ?=([* ~] moves)
          [%leaf "wrong number of moves: {<(lent moves)>}"]~
        ::
        ::
        =/  move=move:http-server-gate                              i.moves
        =/  =duct                                             duct.move
        =/  card=(wind note:http-server-gate gift:able:http-server-gate)  card.move
        ::
        %+  weld
          (expect-eq !>(~[/http-blah]) !>(duct))
        ::  expect authenticated=%.y in the handle below
        ::
        %+  expect-gall-deal
          :+  /run-app/app1  [~nul ~nul]
              ^-  cush:gall
              :*  %app1  %poke  %handle-http-request
                  !>  :*
                    %.y
                    %.n
                    [%ipv4 .192.168.1.1]
                    :*  %'GET'
                        '/~landscape/inner-path'
                        ['cookie' 'urbauth=0v3.q0p7t.mlkkq.cqtto.p0nvi.2ieea']~
                        ~
                  ==  ==
              ==
          card
    ==
  ::
  ;:  weld
    results1
    results2
    results3
    results4
    results5
    results6
  ==
::
++  test-generator
  ::
  =^  results1  http-server-gate
    %-  http-server-call  :*
      http-server-gate
      now=~1111.1.1
      scry=scry-provides-code
      call-args=[duct=~[/init] ~ [%init ~nul]]
      expected-moves=~
    ==
  ::  gen1 binds successfully
  ::
  =^  results2  http-server-gate
    %-  http-server-call  :*
      http-server-gate
      now=~1111.1.2
      scry=scry-provides-code
      call-args=[duct=~[/gen1] ~ [%serve [~ /] [%home /gen/handler/hoon ~]]]
      expected-moves=[duct=~[/gen1] %give %bound %.y [~ /]]~
    ==
  ::  outside requests a path that app1 has bound to
  ::
  =^  results3  http-server-gate
    %-  http-server-call-with-comparator  :*
      http-server-gate
      now=~1111.1.3
      scry=scry-provides-code
      ^=  call-args
        :*  duct=~[/http-blah]  ~
            %request
            %.n
            [%ipv4 .192.168.1.1]
            [%'GET' '/' ~ ~]
        ==
      ^=  comparator
        |=  moves=(list move:http-server-gate)
        ^-  tang
        ::
        ?.  ?=([* ~] moves)
          [%leaf "wrong number of moves: {<(lent moves)>}"]~
        ::
        ::
        =/  move=move:http-server-gate                              i.moves
        =/  =duct                                             duct.move
        =/  card=(wind note:http-server-gate gift:able:http-server-gate)  card.move
        ::
        ?.  ?=(%pass -.card)
          [%leaf "not a %pass"]~
        ?.  ?=([%f %build *] q.card)
          [%leaf "not a ford build"]~
        ::
        %+  weld
          %+  expect-eq
            !>  /run-build
            !>  p.card
        ::
        %+  expect-schematic
          :+  %call
            :+  %call
              [%core [[~nul %home] /hoon/handler/gen]]
            [%$ %noun !>([[~1111.1.3 0xdead.beef [~nul %home [%da ~1111.1.3]]] ~ ~])]
          [%$ %noun !>([%.n [%'GET' '/' ~ ~]])]
        ::
          schematic.q.card
    ==
  ::  ford response (time assumes nothing blocked)
  ::
  =^  results4  http-server-gate
    %-  http-server-take  :*
      http-server-gate
      now=~1111.1.3
      scry=scry-provides-code
      ^=  take-args
        :*  wire=/run-build  duct=~[/http-blah]
            ^-  (hypo sign:http-server-gate)
            :-  *type
            :^  %f  %made  ~1111.1.3
            ^-  made-result:ford
            :-  %complete
            ^-  build-result:ford
            :^  %success  %cast  %mime
            !>
            :-  [200 ['content-type' 'text/plain']~]
            `(as-octs:mimes:html 'one two three')
         ==
      ^=  expected-move
        :~  :*  duct=~[/http-blah]  %give  %response
                :*  %start
                    :-  200
                    :~  ['content-type' 'text/plain']
                        ['content-length' '13']
                    ==
                    `[13 'one two three']
                    %.y
    ==  ==  ==  ==
  ::
  ;:  weld
    results1
    results2
    results3
    results4
  ==
::
++  test-simplified-url-parser
  ;:  weld
    %+  expect-eq
      !>  `[[%site 'localhost'] [~ 8.000]]
      !>  (rush 'localhost:8000' simplified-url-parser:http-server-gate)
  ::
    %+  expect-eq
      !>  `[[%ip .192.168.1.1] ~]
      !>  (rush '192.168.1.1' simplified-url-parser:http-server-gate)
  ==
::
++  test-parse-channel-request
  ;:  weld
    %+  expect-eq
      !>  `[%ack 5]~
      !>  %-  parse-channel-request:http-server-gate
          (need (de-json:html '[{"action": "ack", "event-id": 5}]'))
  ::
    %+  expect-eq
      !>  `[%poke 0 ~nec %app1 %app-type [%n '5']]~
      !>  %-  parse-channel-request:http-server-gate
          %-  need  %-  de-json:html
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
      !>  `[%subscribe 1 ~sampyl-sipnym %hall /this/path]~
      !>  %-  parse-channel-request:http-server-gate
          %-  need  %-  de-json:html
          '''
          [{"action": "subscribe",
            "id": 1,
            "ship": "sampyl-sipnym",
            "app": "hall",
            "path": "/this/path"}]
          '''
  ::
    %+  expect-eq
      !>  `[%unsubscribe 2 ~marlyt %thing /other]~
      !>  %-  parse-channel-request:http-server-gate
          %-  need  %-  de-json:html
          '''
          [{"action": "unsubscribe",
            "id": 2,
            "ship": "marlyt",
            "app": "thing",
            "path": "/other"}]
          '''
  ::
      %+  expect-eq
        !>  ~
        !>  %-  parse-channel-request:http-server-gate
            %-  need  %-  de-json:html
            '[{"noaction": "noaction"}]'
  ::
      %+  expect-eq
        !>  ~
        !>  %-  parse-channel-request:http-server-gate
            %-  need  %-  de-json:html
            '[{"action": "bad-action"}]'
  ::
      %+  expect-eq
        !>  ~
        !>  %-  parse-channel-request:http-server-gate
            %-  need  %-  de-json:html
            '[{"action": "ack", "event-id": 5}, {"action": "bad-action"}]'
  ::
      %+  expect-eq
        !>  :-  ~
            :~  [%ack 9]
                [%poke 3 ~bud %wut %wut-type [%a [%n '2'] [%n '1'] ~]]
            ==
        !>  %-  parse-channel-request:http-server-gate
            %-  need  %-  de-json:html
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
++  test-channel-reject-unauthenticated
  ::
  =^  results1  http-server-gate
    %-  http-server-call  :*
      http-server-gate
      now=~1111.1.1
      scry=scry-provides-code
      call-args=[duct=~[/init] ~ [%init ~nul]]
      expected-moves=~
    ==
  ::
  =^  results2  http-server-gate
    %-  http-server-call  :*
      http-server-gate
      now=~1111.1.2
      scry=scry-provides-code
      ^=  call-args
        :*  duct=~[/http-blah]  ~
            %request
            %.n
            [%ipv4 .192.168.1.1]
            [%'PUT' '/~/channel/1234567890abcdef' ~ ~]
        ==
      ^=  expected-moves
        ^-  (list move:http-server-gate)
        :~  :*  duct=~[/http-blah]
                %give
                %response
                %start
                :-  403
                :~  ['content-type' 'text/html']
                    ['content-length' '182']
                ==
              ::
                :-  ~
                %-  error-page:http-server-gate  :*
                  403
                  %.n
                  '/~/channel/1234567890abcdef'
                  ~
                ==
              ::
                complete=%.y
        ==  ==
    ==
  ::
  ;:  weld
    results1
    results2
  ==
::
++  test-channel-open-never-used-expire
  =^  results1  http-server-gate  (perform-init-start-channel http-server-gate *sley)
  ::  the behn timer wakes us up; we cancel our subscription
  ::
  =^  results2  http-server-gate
    %-  http-server-take-with-comparator  :*
      http-server-gate
      now=(add ~1111.1.2 ~h12)
      scry=scry-provides-code
      ^=  take-args
        :*  wire=/channel/timeout/'0123456789abcdef'  duct=~[/http-blah]
            ^-  (hypo sign:http-server-gate)
            :-  *type
            [%b %wake ~]
         ==
      ^=  comparator
        |=  moves=(list move:http-server-gate)
        ^-  tang
        ::
        ?.  ?=([^ ~] moves)
          [%leaf "wrong number of moves: {<(lent moves)>}"]~
        ::
        %+  expect-gall-deal
          :*  /channel/subscription/'0123456789abcdef'
              [~nul ~nul]  %two  %pull  ~
          ==
          card.i.moves
    ==
  ::
  ;:  weld
    results1
    results2
  ==
::
++  test-channel-results-before-open
  ::  common initialization
  ::
  =^  results1  http-server-gate  (perform-init-start-channel http-server-gate *sley)
  ::  poke gets a success message
  ::
  =^  results2  http-server-gate
    %-  http-server-take  :*
      http-server-gate
      now=(add ~1111.1.2 ~m1)
      scry=scry-provides-code
      ^=  take-args
        :*  wire=/channel/poke/'0123456789abcdef'/'0'  duct=~[/http-put-request]
            ^-  (hypo sign:http-server-gate)
            :-  *type
            [%g %unto %coup ~]
         ==
      moves=~
    ==
  ::  subscription gets a success message
  ::
  =^  results3  http-server-gate
    %-  http-server-take  :*
      http-server-gate
      now=(add ~1111.1.2 ~m1)
      scry=scry-provides-code
      ^=  take-args
        :*  wire=/channel/subscription/'0123456789abcdef'/'1'  duct=~[/http-put-request]
            ^-  (hypo sign:http-server-gate)
            :-  *type
            [%g %unto %reap ~]
         ==
      moves=~
    ==
  ::  subscription gets a result
  ::
  =^  results4  http-server-gate
    %-  http-server-take  :*
      http-server-gate
      now=(add ~1111.1.2 ~m2)
      scry=scry-provides-code
      ^=  take-args
        :*  wire=/channel/subscription/'0123456789abcdef'/'1'  duct=~[/http-put-request]
            ^-  (hypo sign:http-server-gate)
            :-  *type
            [%g %unto %diff %json !>(`json`[%a [%n '1'] [%n '2'] ~])]
         ==
      moves=~
    ==
  ::  open up the channel
  ::
  ::  send the channel a poke and a subscription request
  ::
  =^  results5  http-server-gate
    %-  http-server-call  :*
      http-server-gate
      now=(add ~1111.1.2 ~m3)
      scry=scry-provides-code
      ^=  call-args
        :*  duct=~[/http-get-open]  ~
            %request
            %.n
            [%ipv4 .192.168.1.1]
            %'GET'
            '/~/channel/0123456789abcdef'
            ['cookie' 'urbauth=0v3.q0p7t.mlkkq.cqtto.p0nvi.2ieea']~
            ~
        ==
      ^=  expected-moves
        ^-  (list move:http-server-gate)
        :~  :*  duct=~[/http-get-open]
                %give
                %response
                %start
                :-  200
                :~  ['content-type' 'text/event-stream']
                    ['cache-control' 'no-cache']
                    ['connection' 'keep-alive']
                ==
              ::
                :-  ~
                %-  as-octs:mimes:html
                '''
                id: 0
                data: {"ok":"ok","id":0,"response":"poke"}

                id: 1
                data: {"ok":"ok","id":1,"response":"subscribe"}

                id: 2
                data: {"json":[1,2],"id":1,"response":"diff"}


                '''
              ::
                complete=%.n
            ==
            ::  opening the channel cancels the timeout timer
            ::
            :*  duct=~[/http-put-request]  %pass
                /channel/timeout/'0123456789abcdef'
                [%b %rest ~1111.1.2..12.00.00]
    ==  ==  ==
  ::  we get a cancel when we notice the client has disconnected
  ::
  =^  results6  http-server-gate
    %-  http-server-call  :*
      http-server-gate
      now=(add ~1111.1.2 ~m4)
      scry=scry-provides-code
      call-args=[duct=~[/http-get-open] ~ %cancel-request ~]
      ^=  expected-moves
        ^-  (list move:http-server-gate)
        ::  closing the channel restarts the timeout timer
        ::
        :~  :*  duct=~[/http-get-open]  %pass
                /channel/timeout/'0123456789abcdef'
                %b  %wait  :(add ~1111.1.2 ~h12 ~m4)
        ==  ==
    ==
  ::
  ;:  weld
    results1
    results2
    results3
    results4
    results5
    results6
  ==
::
::
++  test-channel-second-get-updates-timer
  ::  common initialization
  ::
  =^  results1  http-server-gate  (perform-init-start-channel http-server-gate *sley)
  ::  perform another poke to a different app
  ::
  ::    Since we haven't connected with a GET, the old timer should be canceled
  ::    and a new one should be set.
  ::  send the channel a poke and a subscription request
  ::
  =^  results2  http-server-gate
    %-  http-server-call-with-comparator  :*
      http-server-gate
      now=(add ~1111.1.2 ~m1)
      scry=scry-provides-code
      ^=  call-args
        :*  duct=~[/http-put-request]  ~
            %request
            %.n
            [%ipv4 .192.168.1.1]
            %'PUT'
            '/~/channel/0123456789abcdef'
            ['cookie' 'urbauth=0v3.q0p7t.mlkkq.cqtto.p0nvi.2ieea']~
        ::
            :-  ~
            %-  as-octs:mimes:html
            '''
            [{"action": "poke",
              "id": 2,
              "ship": "nul",
              "app": "eight",
              "mark": "a",
              "json": 9}]
            '''
        ==
      ^=  comparator
        |=  moves=(list move:http-server-gate)
        ^-  tang
        ::
        ?.  ?=([^ ^ ^ ^ ~] moves)
          [%leaf "wrong number of moves: {<(lent moves)>}"]~
        ::
        ;:  weld
          %+  expect-gall-deal
            :*  /channel/poke/'0123456789abcdef'/'2'
                [~nul ~nul]  %eight
                %punk  %a  %json  !>([%n '9'])
            ==
            card.i.moves
        ::
          %+  expect-eq
            !>  [~[/http-put-request] %give %response %start [200 ~] ~ %.y]
            !>  i.t.moves
        ::
          %+  expect-eq
            !>  :*  ~[/http-put-request]  %pass
                    /channel/timeout/'0123456789abcdef'
                    %b  %rest  (add ~1111.1.2 ~h12)
                ==
            !>  i.t.t.moves
        ::
          %+  expect-eq
            !>  :*  ~[/http-put-request]  %pass
                    /channel/timeout/'0123456789abcdef'
                    %b  %wait  :(add ~1111.1.2 ~h12 ~m1)
                ==
            !>  i.t.t.t.moves
    ==  ==
  ::
  ;:  weld
    results1
    results2
  ==
::
++  test-channel-unsubscribe-stops-events
  ::  common initialization
  ::
  =^  results1  http-server-gate  (perform-init-start-channel http-server-gate *sley)
  ::  poke gets a success message
  ::
  =^  results2  http-server-gate
    %-  http-server-take  :*
      http-server-gate
      now=(add ~1111.1.2 ~m1)
      scry=scry-provides-code
      ^=  take-args
        :*  wire=/channel/poke/'0123456789abcdef'/'0'  duct=~[/http-put-request]
            ^-  (hypo sign:http-server-gate)
            :-  *type
            [%g %unto %coup ~]
         ==
      moves=~
    ==
  ::  subscription gets a success message
  ::
  =^  results3  http-server-gate
    %-  http-server-take  :*
      http-server-gate
      now=(add ~1111.1.2 ~m2)
      scry=scry-provides-code
      ^=  take-args
        :*  wire=/channel/subscription/'0123456789abcdef'/'1'  duct=~[/http-put-request]
            ^-  (hypo sign:http-server-gate)
            :-  *type
            [%g %unto %reap ~]
         ==
      moves=~
    ==
  ::  sending an unsubscribe sends an unsubscribe to gall
  ::
  =^  results4  http-server-gate
    %-  http-server-call-with-comparator  :*
      http-server-gate
      now=(add ~1111.1.2 ~m3)
      scry=scry-provides-code
      ^=  call-args
        :*  duct=~[/http-put-request]  ~
            %request
            %.n
            [%ipv4 .192.168.1.1]
            %'PUT'
            '/~/channel/0123456789abcdef'
            ['cookie' 'urbauth=0v3.q0p7t.mlkkq.cqtto.p0nvi.2ieea']~
        ::
            :-  ~
            %-  as-octs:mimes:html
            '''
            [{"action": "unsubscribe",
              "id": 2,
              "ship": "nul",
              "app": "two",
              "path": "/one/two/three"}
            ]
            '''
        ==
      ^=  comparator
        |=  moves=(list move:http-server-gate)
        ^-  tang
        ::
        ?.  ?=([^ ^ ^ ^ ~] moves)
          [%leaf "wrong number of moves: {<(lent moves)>}"]~
        ::
        ;:  weld
          %+  expect-gall-deal
            :*  /channel/subscription/'0123456789abcdef'/'2'
                [~nul ~nul]  %two  %pull  ~
            ==
            card.i.moves
        ::
          %+  expect-eq
            !>  [~[/http-put-request] %give %response %start [200 ~] ~ %.y]
            !>  i.t.moves
        ::
          %+  expect-eq
            !>  :*  ~[/http-put-request]  %pass
                    /channel/timeout/'0123456789abcdef'
                    %b  %rest  (add ~1111.1.2 ~h12)
                ==
            !>  i.t.t.moves
        ::
          %+  expect-eq
            !>  :*  ~[/http-put-request]  %pass
                    /channel/timeout/'0123456789abcdef'
                    %b  %wait  :(add ~1111.1.2 ~h12 ~m3)
                ==
            !>  i.t.t.t.moves
    ==  ==
  ::
  ;:  weld
    results1
    results2
    results3
    results4
  ==
::
++  test-prune-events
  =/  q=(qeu [id=@ud lines=wall])  ~
  =.  q  (~(put to q) [0 ~])
  =.  q  (~(put to q) [1 ~])
  =.  q  (~(put to q) [2 ~])
  =.  q  (~(put to q) [3 ~])
  =.  q  (~(put to q) [4 ~])
  ::
  =.  q  (prune-events:http-server-gate q 3)
  ::
  (expect-eq !>([~ [4 ~]]) !>(~(top to q)))
::
++  test-channel-sends-unacknowledged-events-on-reconnection
  ::  common initialization
  ::
  =^  results1  http-server-gate  (perform-init-start-channel http-server-gate *sley)
  ::  poke gets a success message
  ::
  =^  results2  http-server-gate
    %-  http-server-take  :*
      http-server-gate
      now=(add ~1111.1.2 ~m1)
      scry=scry-provides-code
      ^=  take-args
        :*  wire=/channel/poke/'0123456789abcdef'/'0'  duct=~[/http-put-request]
            ^-  (hypo sign:http-server-gate)
            :-  *type
            [%g %unto %coup ~]
         ==
      moves=~
    ==
  ::  subscription gets a success message
  ::
  =^  results3  http-server-gate
    %-  http-server-take  :*
      http-server-gate
      now=(add ~1111.1.2 ~m2)
      scry=scry-provides-code
      ^=  take-args
        :*  wire=/channel/subscription/'0123456789abcdef'/'1'  duct=~[/http-put-request]
            ^-  (hypo sign:http-server-gate)
            :-  *type
            [%g %unto %reap ~]
         ==
      moves=~
    ==
  ::  opens the http channel
  ::
  =^  results4  http-server-gate
    %-  http-server-call  :*
      http-server-gate
      now=(add ~1111.1.2 ~m3)
      scry=scry-provides-code
      ^=  call-args
        :*  duct=~[/http-get-open]  ~
            %request
            %.n
            [%ipv4 .192.168.1.1]
            %'GET'
            '/~/channel/0123456789abcdef'
            ['cookie' 'urbauth=0v3.q0p7t.mlkkq.cqtto.p0nvi.2ieea']~
            ~
        ==
      ^=  expected-moves
        ^-  (list move:http-server-gate)
        :~  :*  duct=~[/http-get-open]
                %give
                %response
                %start
                :-  200
                :~  ['content-type' 'text/event-stream']
                    ['cache-control' 'no-cache']
                    ['connection' 'keep-alive']
                ==
              ::
                :-  ~
                %-  as-octs:mimes:html
                '''
                id: 0
                data: {"ok":"ok","id":0,"response":"poke"}

                id: 1
                data: {"ok":"ok","id":1,"response":"subscribe"}


                '''
              ::
                complete=%.n
            ==
            ::  opening the channel cancels the timeout timer
            ::
            :*  duct=~[/http-put-request]  %pass
                /channel/timeout/'0123456789abcdef'
                [%b %rest :(add ~1111.1.2 ~h12)]
    ==  ==  ==
  ::  first subscription result gets sent to the user
  ::
  =^  results5  http-server-gate
    %-  http-server-take  :*
      http-server-gate
      now=(add ~1111.1.2 ~m4)
      scry=scry-provides-code
      ^=  take-args
        :*  wire=/channel/subscription/'0123456789abcdef'/'1'  duct=~[/http-put-request]
            ^-  (hypo sign:http-server-gate)
            :-  *type
            [%g %unto %diff %json !>(`json`[%a [%n '1'] ~])]
         ==
      ^=  moves
        ^-  (list move:http-server-gate)
        :~  :*  duct=~[/http-get-open]
                %give
                %response
                %continue
                :-  ~
                %-  as-octs:mimes:html
                '''
                id: 2
                data: {"json":[1],"id":1,"response":"diff"}


                '''
                complete=%.n
    ==  ==  ==
  ::  the client now acknowledges up to event 1
  ::
  ::  send the channel a poke and a subscription request
  ::
  =^  results6  http-server-gate
    %-  http-server-call-with-comparator  :*
      http-server-gate
      now=(add ~1111.1.2 ~m5)
      scry=scry-provides-code
      ^=  call-args
        :*  duct=~[/http-put-request]  ~
            %request
            %.n
            [%ipv4 .192.168.1.1]
            %'PUT'
            '/~/channel/0123456789abcdef'
            ['cookie' 'urbauth=0v3.q0p7t.mlkkq.cqtto.p0nvi.2ieea']~
        ::
            :-  ~
            %-  as-octs:mimes:html
            '''
            [{"action": "ack",
              "event-id": 1}
            ]
            '''
        ==
      ^=  comparator
        |=  moves=(list move:http-server-gate)
        ^-  tang
        ::
        ?.  ?=([^ ~] moves)
          [%leaf "wrong number of moves: {<(lent moves)>}"]~
        ::
        %+  expect-eq
          !>  [~[/http-put-request] %give %response %start [200 ~] ~ %.y]
          !>  i.moves
    ==
  ::  the client connection is detected to be broken
  ::
  =^  results7  http-server-gate
    %-  http-server-call  :*
      http-server-gate
      now=(add ~1111.1.2 ~m6)
      scry=scry-provides-code
      call-args=[duct=~[/http-get-open] ~ %cancel-request ~]
      ^=  expected-moves
        ^-  (list move:http-server-gate)
        ::  closing the channel restarts the timeout timer
        ::
        :~  :*  duct=~[/http-get-open]  %pass
                /channel/timeout/'0123456789abcdef'
                %b  %wait  :(add ~1111.1.2 ~h12 ~m6)
        ==  ==
    ==
  ::  another subscription result while the user is disconnected
  ::
  =^  results8  http-server-gate
    %-  http-server-take  :*
      http-server-gate
      now=(add ~1111.1.2 ~m7)
      scry=scry-provides-code
      ^=  take-args
        :*  wire=/channel/subscription/'0123456789abcdef'/'1'  duct=~[/http-put-request]
            ^-  (hypo sign:http-server-gate)
            :-  *type
            [%g %unto %diff %json !>(`json`[%a [%n '2'] ~])]
         ==
      moves=~
    ==
  ::  the client now retries to connect
  ::
  ::    Because the client has acknowledged up to event 1, we should start the connection by
  ::    resending events 2 and 3.
  ::
  =^  results9  http-server-gate
    %-  http-server-call  :*
      http-server-gate
      now=(add ~1111.1.2 ~m8)
      scry=scry-provides-code
      ^=  call-args
        :*  duct=~[/http-get-open]  ~
            %request
            %.n
            [%ipv4 .192.168.1.1]
            %'GET'
            '/~/channel/0123456789abcdef'
            ['cookie' 'urbauth=0v3.q0p7t.mlkkq.cqtto.p0nvi.2ieea']~
            ~
        ==
      ^=  expected-moves
        ^-  (list move:http-server-gate)
        :~  :*  duct=~[/http-get-open]
                %give
                %response
                %start
                :-  200
                :~  ['content-type' 'text/event-stream']
                    ['cache-control' 'no-cache']
                    ['connection' 'keep-alive']
                ==
              ::
                :-  ~
                %-  as-octs:mimes:html
                '''
                id: 2
                data: {"json":[1],"id":1,"response":"diff"}

                id: 3
                data: {"json":[2],"id":1,"response":"diff"}


                '''
              ::
                complete=%.n
            ==
            ::  opening the channel cancels the timeout timer
            ::
            :*  duct=~[/http-get-open]  %pass
                /channel/timeout/'0123456789abcdef'
                ::  add ~m6 because that was the time of the last GET
                ::
                [%b %rest :(add ~1111.1.2 ~m6 ~h12)]
    ==  ==  ==
  ::
  ;:  weld
    results1
    results2
    results3
    results4
    results5
    results6
    results7
    results8
    results9
  ==
::
++  test-born-sends-pending-cancels
  ::
  =^  results1  http-server-gate
    %-  http-server-call  :*
      http-server-gate
      now=~1111.1.1
      scry=scry-provides-code
      call-args=[duct=~[/init] ~ [%init ~nul]]
      expected-moves=~
    ==
  ::  app1 binds successfully
  ::
  =^  results2  http-server-gate
    %-  http-server-call  :*
      http-server-gate
      now=~1111.1.2
      scry=scry-provides-code
      call-args=[duct=~[/app1] ~ [%connect [~ /] %app1]]
      expected-moves=[duct=~[/app1] %give %bound %.y [~ /]]~
    ==
  ::  start a request to app1
  ::
  ::  outside requests a path that app1 has bound to
  ::
  =^  results3  http-server-gate
    %-  http-server-call-with-comparator  :*
      http-server-gate
      now=~1111.1.3
      scry=scry-provides-code
      ^=  call-args
        :*  duct=~[/http-blah]  ~
            %request
            %.n
            [%ipv4 .192.168.1.1]
            [%'GET' '/' ~ ~]
        ==
      ^=  comparator
        |=  moves=(list move:http-server-gate)
        ^-  tang
        ::
        ?.  ?=([* ~] moves)
          [%leaf "wrong number of moves: {<(lent moves)>}"]~
        ::
        ::
        =/  move=move:http-server-gate                              i.moves
        =/  =duct                                             duct.move
        =/  card=(wind note:http-server-gate gift:able:http-server-gate)  card.move
        ::
        %+  weld
          (expect-eq !>(~[/http-blah]) !>(duct))
        ::
        %+  expect-gall-deal
          :+  /run-app/app1  [~nul ~nul]
              ^-  cush:gall
              :*  %app1  %poke  %handle-http-request
                  !>([%.n %.n [%ipv4 .192.168.1.1] [%'GET' '/' ~ ~]])
              ==
          card
    ==
  ::  but app1 doesn't respond before our urbit gets shut down. ensure we send
  ::  cancels on open connections.
  ::
  =^  results4  http-server-gate
    %-  http-server-call-with-comparator  :*
      http-server-gate
      now=~1111.1.4
      scry=scry-provides-code
      call-args=[duct=~[/born] ~ [%born ~]]
      ^=  expected-moves
        |=  moves=(list move:http-server-gate)
        ^-  tang
        ::
        ?.  ?=([^ ^ ~] moves)
          [%leaf "wrong number of moves: {<(lent moves)>}"]~
        ::
        ::  we don't care about the first one, which is just a static
        ::  configuration move.
        ::
        =/  move=move:http-server-gate                              i.t.moves
        =/  =duct                                             duct.move
        =/  card=(wind note:http-server-gate gift:able:http-server-gate)  card.move
        ::
        %+  weld
          (expect-eq !>(~[/http-blah]) !>(duct))
        ::
        %+  expect-gall-deal
          :+  /run-app/app1  [~nul ~nul]
              ^-  cush:gall
              :*  %app1  %poke  %handle-http-cancel
                  !>([%.n %.n [%ipv4 .192.168.1.1] [%'GET' '/' ~ ~]])
              ==
          card
    ==
  ::
  ;:  weld
    results1
    results2
    results3
    results4
  ==
::
++  http-server-call
  |=  $:  http-server-gate=_http-server-gate
          now=@da
          scry=sley
          call-args=[=duct type=* wrapped-task=(hobo task:able:http-server-gate)]
          expected-moves=(list move:http-server-gate)
      ==
  ^-  [tang _http-server-gate]
  ::
  =/  http-server-core  (http-server-gate our=~nul now=now eny=`@uvJ`0xdead.beef scry=scry)
  ::
  =^  moves  http-server-gate  (call:http-server-core call-args)
  ::
  =/  output=tang
    %+  expect-eq
      !>  expected-moves
      !>  moves
  ::
  [output http-server-gate]
::
++  http-server-call-with-comparator
  |=  $:  http-server-gate=_http-server-gate
          now=@da
          scry=sley
          call-args=[=duct type=* wrapped-task=(hobo task:able:http-server-gate)]
          move-comparator=$-((list move:http-server-gate) tang)
      ==
  ^-  [tang _http-server-gate]
  ::
  =/  http-server-core  (http-server-gate our=~nul now=now eny=`@uvJ`0xdead.beef scry=scry)
  ::
  =^  moves  http-server-gate  (call:http-server-core call-args)
  ::
  =/  output=tang  (move-comparator moves)
  ::
  [output http-server-gate]
::
++  http-server-take
  |=  $:  http-server-gate=_http-server-gate
          now=@da
          scry=sley
          take-args=[=wire =duct wrapped-task=(hypo sign:http-server-gate)]
          expected-moves=(list move:http-server-gate)
      ==
  ^-  [tang _http-server-gate]
  ::
  =/  http-server-core  (http-server-gate our=~nul now=now eny=`@uvJ`0xdead.beef scry=scry)
  ::
  =^  moves  http-server-gate  (take:http-server-core take-args)
  ::
  =/  output=tang
    %+  expect-eq
      !>  expected-moves
      !>  moves
  ::
  [output http-server-gate]
::
++  http-server-take-with-comparator
  |=  $:  http-server-gate=_http-server-gate
          now=@da
          scry=sley
          take-args=[=wire =duct wrapped-task=(hypo sign:http-server-gate)]
          move-comparator=$-((list move:http-server-gate) tang)
      ==
  ^-  [tang _http-server-gate]
  ::
  =/  http-server-core  (http-server-gate our=~nul now=now eny=`@uvJ`0xdead.beef scry=scry)
  ::
  =^  moves  http-server-gate  (take:http-server-core take-args)
  ::
  =/  output=tang  (move-comparator moves)
  ::
  [output http-server-gate]
::
++  expect-gall-deal
  |=  $:  expected=[wire=path id=sock data=cush:gall]
          actual=(wind note:http-server-gate gift:able:http-server-gate)
      ==
  ^-  tang
  ::
  ?.  ?=(%pass -.actual)
    [%leaf "bad move, not a %pass: {<actual>}"]~
  ::
  %+  weld
    (expect-eq !>(wire.expected) !>(p.actual))
  ::
  =/  note=note:http-server-gate  q.actual
  ?.  ?=([%g %deal *] note)
    [%leaf "bad move, not a %deal: {<actual>}"]~
  ::
  %+  weld
    (expect-eq !>(id.expected) !>(id.note))
  ::
  %+  weld
    (expect-eq !>(p.data.expected) !>(p.data.note))
  ::
  ?:  ?=([%poke *] q.data.expected)
    ?.  ?=([%poke *] q.data.note)
      [%leaf "expected %poke, actual {<q.data.note>}"]~
    ::
    %+  weld
      (expect-eq !>(p.p.q.data.expected) !>(p.p.q.data.note))
    ::  compare the payload vases
    ::
    (expect-eq q.p.q.data.expected q.p.q.data.note)
  ::
  ?:  ?=([%punk *] q.data.expected)
    ?.  ?=([%punk *] q.data.note)
      [%leaf "expected %punk, actual {<q.data.note>}"]~
    ::  compare the mark type
    ::
    %+  weld
      (expect-eq !>(p.q.data.expected) !>(p.q.data.note))
    ::  compare the cage mark
    ::
    %+  weld
      (expect-eq !>(p.q.q.data.expected) !>(p.q.q.data.note))
    ::  compare the payload vases
    ::
    (expect-eq q.q.q.data.expected q.q.q.data.note)
  ::
  ?:  ?=([%peel *] q.data.expected)
    ?.  ?=([%peel *] q.data.note)
      [%leaf "expected %peel, actual {<q.data.note>}"]~
    ::  compare the result mark
    ::
    %+  weld
      (expect-eq !>(p.q.data.expected) !>(p.q.data.note))
    ::  compare the path
    ::
    (expect-eq !>(q.q.data.expected) !>(q.q.data.note))
  ::
  ?:  ?=([%pull *] q.data.expected)
    ?.  ?=([%pull *] q.data.note)
      [%leaf "expected %pull, actual {<q.data.note>}"]~
    ::
    ~
  ::  todo: handle other deals
  ::
  [%leaf "unexpected %deal type"]~
::  +perform-authentication: goes through the authentication flow
::
++  perform-authentication
  |=  $:  http-server-gate=_http-server-gate
          start-now=@da
          scry=sley
      ==
  ^-  [tang _http-server-gate]
  ::  the browser then fetches the login page
  ::
  =^  results1  http-server-gate
    %-  http-server-call  :*
      http-server-gate
      now=start-now
      scry=scry-provides-code
      ^=  call-args
        :*  duct=~[/http-blah]  ~
            %request
            %.n
            [%ipv4 .192.168.1.1]
            [%'GET' '/~/login?redirect=/~landscape/inner-path' ~ ~]
        ==
      ^=  expected-moves
        ^-  (list move:http-server-gate)
        :~  :*  duct=~[/http-blah]
                %give
                %response
                %start
                :-  200
                :~  ['content-type' 'text/html']
                    ['content-length' '348']
                ==
                [~ (login-page:http-server-gate `'/~landscape/inner-path')]
                complete=%.y
        ==  ==
    ==
  ::  a response post redirects back to the application, setting cookie
  ::
  =^  results2  http-server-gate
    %-  http-server-call  :*
      http-server-gate
      now=(add start-now ~m1)
      scry=scry-provides-code
      ^=  call-args
        :*  duct=~[/http-blah]  ~
            %request
            %.n
            [%ipv4 .192.168.1.1]
            %'POST'
            '/~/login'
            ~
            :-  ~
            %-  as-octs:mimes:html
            'password=lidlut-tabwed-pillex-ridrup&redirect=/~landscape'
        ==
      ^=  expected-moves
        ^-  (list move:http-server-gate)
        :~  :*  duct=~[/http-blah]
                %give
                %response
                %start
                :-  307
                :~  ['location' '/~landscape']
                    :-  'set-cookie'
                    'urbauth=0v3.q0p7t.mlkkq.cqtto.p0nvi.2ieea; Path=/; Max-Age=86400'
                ==
                ~
                complete=%.y
        ==  ==
    ==
  ::
  :_  http-server-gate
  (weld results1 results2)
::  performs all initialization and an initial PUT.
::
++  perform-init-start-channel
  |=  $:  http-server-gate=_http-server-gate
          scry=sley
      ==
  ^-  [tang _http-server-gate]
  ::
  =^  results1  http-server-gate
    %-  http-server-call  :*
      http-server-gate
      now=~1111.1.1
      scry=scry-provides-code
      call-args=[duct=~[/init] ~ [%init ~nul]]
      expected-moves=~
    ==
  ::  ensure there's an authenticated session
  ::
  =^  results2  http-server-gate
    %-  perform-authentication  :*
      http-server-gate
      now=~1111.1.2
      scry=scry-provides-code
    ==
  ::  send the channel a poke and a subscription request
  ::
  =^  results3  http-server-gate
    %-  http-server-call-with-comparator  :*
      http-server-gate
      now=~1111.1.2
      scry=scry-provides-code
      ^=  call-args
        :*  duct=~[/http-put-request]  ~
            %request
            %.n
            [%ipv4 .192.168.1.1]
            %'PUT'
            '/~/channel/0123456789abcdef'
            ['cookie' 'urbauth=0v3.q0p7t.mlkkq.cqtto.p0nvi.2ieea']~
        ::
            :-  ~
            %-  as-octs:mimes:html
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
        ==
      ^=  comparator
        |=  moves=(list move:http-server-gate)
        ^-  tang
        ::
        ?.  ?=([^ ^ ^ ^ ~] moves)
          [%leaf "wrong number of moves: {<(lent moves)>}"]~
        ::
        ;:  weld
          %+  expect-gall-deal
            :*  /channel/poke/'0123456789abcdef'/'0'
                [~nul ~nul]  %one
                %punk  %a  %json  !>([%n '5'])
            ==
            card.i.moves
        ::
          %+  expect-gall-deal
            :*  /channel/subscription/'0123456789abcdef'/'1'
                [~nul ~nul]  %two
                %peel  %json  /one/two/three
            ==
            card.i.t.moves
        ::
          %+  expect-eq
            !>  [~[/http-put-request] %give %response %start [200 ~] ~ %.y]
            !>  i.t.t.moves
        ::
          %+  expect-eq
            !>  :*  ~[/http-put-request]  %pass
                    /channel/timeout/'0123456789abcdef'
                    %b  %wait  (add ~1111.1.2 ~h12)
                ==
            !>  i.t.t.t.moves
    ==  ==
  ::
  :_  http-server-gate
  :(weld results1 results2 results3)
::
++  scry-provides-code  ^-  sley
  |=  [* (unit (set monk)) =term =beam]
  ^-  (unit (unit cage))
  ::
  ?>  =(%j term)
  ?>  =(~nul p.beam)
  ?>  =(%code q.beam)
  ?>  =(%da -.r.beam)
  ?>  =(/~nul s.beam)
  ::  This is the default code for a fakeship.
  ::
  [~ ~ %noun !>(.~lidlut-tabwed-savheb-loslux)]
--
