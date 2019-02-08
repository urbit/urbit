/+  *test
::
/=  http-client-raw  /:  /===/sys/vane/http-client  /!noun/
::
!:
::
=/  test-pit=vase  !>(..zuse)
=/  http-client-gate  (http-client-raw test-pit)
::
|%
::  +test-client-request-basic: tests a single request, single reply style http request
::
++  test-client-request-basic
  ::  send a %born event to use /initial-born-duct for requests
  ::
  =^  results1  http-client-gate
    %-  http-client-call  :*
      http-client-gate
      now=~1111.1.1
      scry=*sley
      call-args=[duct=~[/initial-born-duct] ~ [%born ~]]
      expected-moves=~
    ==
  ::
  =/  request=request:http
    :*  %'GET'
        'http://www.example.com'
        ~
        ~
    ==
  ::  opens the http channel
  ::
  =^  results2  http-client-gate
    %-  http-client-call  :*
      http-client-gate
      now=(add ~1111.1.1 ~s1)
      scry=*sley
      ^=  call-args
        :*  duct=~[/http-get-request]  ~
            %request
            request
            *outbound-config:http-client
        ==
      ^=  expected-moves
        ^-  (list move:http-client-gate)
        :~  :*  duct=~[/initial-born-duct]
                %give
                %request
                id=0
                method=%'GET'
                url='http://www.example.com'
                ~
                ~
    ==  ==  ==
  ::  returns the entire payload in one response
  ::
  =^  results3  http-client-gate
    %-  http-client-call  :*
      http-client-gate
      now=(add ~1111.1.1 ~s2)
      scry=*sley
      ^=  call-args
        :+  duct=~[/initial-born-duct]  ~
        ^-  task:able:http-client
        :*  %receive
            id=0
            ^-  http-event:http
            :*  %start
                :-  200
                :~  ['content-type' 'text/html']
                    ['content-length' '34']
                ==
            ::
                :-  ~
                %-  as-octs:mimes:html
                '''
                <html><body>Response</body></html>
                '''
            ::
                complete=%.y
        ==  ==
      ^=  expected-moves
        ^-  (list move:http-client-gate)
        :~  :*  duct=~[/http-get-request]
                %give
                %finished
            ::
                :-  200
                :~  ['content-type' 'text/html']
                    ['content-length' '34']
                ==
            ::
                :-  ~
                :-  'text/html'
                %-  as-octs:mimes:html
                '''
                <html><body>Response</body></html>
                '''
    ==  ==  ==
  ::
  ;:  weld
    results1
    results2
    results3
  ==
::  +test-client-request-multiple-cards: tests when complete=%.n
::
++  test-client-request-multiple-cards
  ::  send a %born event to use /initial-born-duct for requests
  ::
  =^  results1  http-client-gate
    %-  http-client-call  :*
      http-client-gate
      now=~1111.1.1
      scry=*sley
      call-args=[duct=~[/initial-born-duct] ~ [%born ~]]
      expected-moves=~
    ==
  ::
  =/  request=request:http
    :*  %'GET'
        'http://www.example.com'
        ~
        ~
    ==
  ::  opens the http channel
  ::
  =^  results2  http-client-gate
    %-  http-client-call  :*
      http-client-gate
      now=(add ~1111.1.1 ~s1)
      scry=*sley
      ^=  call-args
        :*  duct=~[/http-get-request]  ~
            %request
            request
            *outbound-config:http-client
        ==
      ^=  expected-moves
        ^-  (list move:http-client-gate)
        :~  :*  duct=~[/initial-born-duct]
                %give
                %request
                id=0
                method=%'GET'
                url='http://www.example.com'
                ~
                ~
    ==  ==  ==
  ::  returns the first 1/3 of the payload in the first response
  ::
  =^  results3  http-client-gate
    %-  http-client-call  :*
      http-client-gate
      now=(add ~1111.1.1 ~s2)
      scry=*sley
      ^=  call-args
        :+  duct=~[/initial-born-duct]  ~
        ^-  task:able:http-client
        :*  %receive
            id=0
            ^-  http-event:http
            :*  %start
                :-  200
                :~  ['content-type' 'text/html']
                    ['content-length' '34']
                ==
                [~ (as-octs:mimes:html '<html><body>')]
                complete=%.n
        ==  ==
      ^=  expected-moves
        ^-  (list move:http-client-gate)
        :~  :*  duct=~[/http-get-request]
                %give
                %progress
            ::
                :-  200
                :~  ['content-type' 'text/html']
                    ['content-length' '34']
                ==
            ::
                bytes-read=12
                expected-size=`34
                [~ (as-octs:mimes:html '<html><body>')]
    ==  ==  ==
  ::  returns the second 1/3 of the payload
  ::
  =^  results4  http-client-gate
    %-  http-client-call  :*
      http-client-gate
      now=(add ~1111.1.1 ~s3)
      scry=*sley
      ^=  call-args
        :+  duct=~[/initial-born-duct]  ~
        ^-  task:able:http-client
        :*  %receive
            id=0
            ^-  http-event:http
            :*  %continue
                [~ (as-octs:mimes:html 'Response')]
                complete=%.n
        ==  ==
      ^=  expected-moves
        ^-  (list move:http-client-gate)
        :~  :*  duct=~[/http-get-request]
                %give
                %progress
            ::
                :-  200
                :~  ['content-type' 'text/html']
                    ['content-length' '34']
                ==
            ::
                bytes-read=20
                expected-size=`34
                [~ (as-octs:mimes:html 'Response')]
    ==  ==  ==
  ::  returns the last part
  ::
  =^  results5  http-client-gate
    %-  http-client-call  :*
      http-client-gate
      now=(add ~1111.1.1 ~s4)
      scry=*sley
      ^=  call-args
        :+  duct=~[/initial-born-duct]  ~
        ^-  task:able:http-client
        :*  %receive
            id=0
            ^-  http-event:http
            :*  %continue
                [~ (as-octs:mimes:html '</body></html>')]
                complete=%.y
        ==  ==
      ^=  expected-moves
        ^-  (list move:http-client-gate)
        :~  :*  duct=~[/http-get-request]
                %give
                %finished
            ::
                :-  200
                :~  ['content-type' 'text/html']
                    ['content-length' '34']
                ==
            ::
                :-  ~
                :-  'text/html'
                %-  as-octs:mimes:html
                '''
                <html><body>Response</body></html>
                '''
    ==  ==  ==
  ::
  ;:  weld
    results1
    results2
    results3
    results4
    results5
  ==
::
++  http-client-call
  |=  $:  http-client-gate=_http-client-gate
          now=@da
          scry=sley
          call-args=[=duct type=* wrapped-task=(hobo task:able:http-client)]
          expected-moves=(list move:http-client-gate)
      ==
  ^-  [tang _http-client-gate]
  ::
  =/  http-client-core
    (http-client-gate our=~nul now=now eny=`@uvJ`0xdead.beef scry=scry)
  ::
  =^  moves  http-client-gate  (call:http-client-core call-args)
  ::
  =/  output=tang
    %+  expect-eq
      !>  expected-moves
      !>  moves
  ::
  [output http-client-gate]
--

