|%
:: +move: output effect
::
+$  move  [bone card]
:: +card: output effect payload
::
+$  card
  $%  [%connect wire [(unit @t) (list @t)] %server]
      [%wait wire @da]
      [%http-response =raw-http-response:light]
  ==
--
::  utilities:
::
|%
::
++  parse-request-line
  |=  url=@t
  ^-  [[(unit @ta) site=(list @t)] args=(list [key=@t value=@t])]
  (fall (rush url ;~(plug apat:de-purl:html yque:de-purl:html)) [[~ ~] ~])
::  +hello:
::
++  hello
  |=  name=@t
  ^-  octs
  %-  as-octs:mimes:html
  %-  crip
  %-  en-xml:html
  ;html
    ;head
      ;title:"Hello, {<(trip name)>}"
    ==
    ;body
      ;h1:"Hello, {<(trip name)>}"
    ==
  ==
::  helper library that lets an app handle an EventSource.
::
::    TODO: This doesn't even attempt to deal with sequence numbers.
::
++  event-source
  |_  m=(map =bone last-id=@ud)
  ++  abet  m
  ::  +start-session: called by app to start a session and send first event
  ::
  ::    This creates a new session where we 
  ::
  ++  start-session
    |=  [session=@ud =bone data=wall]
    ^-  [(list move) _m]
    ::
    :-  :~  :*  bone  %http-response
                %start  200
                :~  ['content-type' 'text/event-stream']
                    ['cache-control' 'no-cache']
                ==
                (wall-to-output data)
                complete=%.n
        ==  ==
    (~(put by m) bone 0)
  ::  +session-stopped: external notification that a session ended
  ::
  ++  session-stopped
    |=  =bone
    ^-  _m
    ::
    (~(del by m) bone)
  ::  +send-message: sends a message based on the continuation
  ::
  ++  send-message
    |=  [=bone data=wall]
    ^-  [(list move) _m]
    :-  [bone %http-response %continue (wall-to-output data) complete=%.n]~
    (~(jab by m) bone |=(a=@ud +(a)))
  ::  +wall-to-output: changes our raw text lines to a text/event-stream
  ::
  ++  wall-to-output
    |=  =wall
    ^-  (unit octs)
    :-  ~
    %-  as-octs:mimes:html
    %-  crip
    %-  zing
    %+  weld
      %+  turn  wall
      |=  t=tape
      "data: {t}\0a"
    ::
    [`tape`['\0a' ~] ~]
  --
::
++  part1
  ^-  octs
  %-  as-octs:mimes:html
  %-  crip
  "<html><head><title>Hello, &quot;"
::
++  part2
  |=  name=@t
  ^-  octs
  %-  as-octs:mimes:html
  %-  crip
  ;:  weld
    (trip name)
    "&quot;</title></head><body><h1>Hello, &quot;"
    (trip name)
    "&quot;</h1>"
    "<p>Time is <span id=time></span></p>"
  ::
    %-  trip
    '''
    <script>
      var evtSource = new EventSource("/~server/stream", { withCredentials: true } );

      evtSource.onmessage = function(e) {
        var message = document.getElementById("time");
        message.innerHTML = e.data;
      }
    </script>
    '''
  ::
    "</body></html>"
  ==
::  +require-authorization: redirect to the login page when unauthenticated
::
++  require-authorization
  |*  [=bone move=mold this=*]
  |=  handler=$-(inbound-request:light (quip move _this))
  |=  =inbound-request:light
  ^-  (quip move _this)
  ::
  ?:  authenticated.inbound-request
    (handler inbound-request)
  ::
  :_  this
  ^-  (list move)
  =/  redirect=cord
    %-  crip
    "/~/login?redirect={(trip url.http-request.inbound-request)}"
  [bone [%http-response %start 307 ['location' redirect]~ ~ %.y]]~
--
|%
::
+$  state
  $:  events=(map =bone last-id=@ud)
  ==
--
::
|_  [bow=bowl:gall state]
::
++  this  .
::
++  prep
  |=  old=(unit state)
  ^-  (quip move _this)
  ~&  %prep
  :-  [`move`[ost.bow [%connect / [~ /'~server'] %server]] ~]
  ?~  old
    this
  this(+<+ u.old)
::  alerts us that we were bound. we need this because the vane calls back.
::
++  bound
  |=  [wir=wire success=? binding=binding:light]
  ~&  [%bound success]
  [~ this]
::
++  handle-start-stream
  |=  =inbound-request:light
  ^-  (quip move _this)
  ::  Start a session sending the current time
  ::
  =^  moves  events
    (~(start-session event-source events) 0 ost.bow ["{<now.bow>}" ~])
  ::
  :_  this
  :-  ^-  move
      [ost.bow %wait /timer (add now.bow ~s1)]
  ::
  moves
::  +wake: responds to a %wait send from +handle-start-stream
::
++  wake
  |=  [wir=wire ~]
  ^-  (quip move _this)
  ?.  (~(has by events) ost.bow)
    ~&  [%closed wir now.bow]
    [~ this]
  ::
  ~&  [%timer-tick wir now.bow]
  ::
  =^  moves  events
    (~(send-message event-source events) ost.bow ["{<now.bow>}" ~])
  ::
  :_  this
  :-  ^-  move
      [ost.bow %wait /timer (add now.bow ~s1)]
  moves
::  +poke-handle-http-request: received on a new connection established
::
++  poke-handle-http-request
  %-  (require-authorization ost.bow move this)
  |=  =inbound-request:light
  ^-  (quip move _this)
  ::
  =+  request-line=(parse-request-line url.http-request.inbound-request)
  ~&  [%request-line request-line]
  =/  name=@t
    =+  back-path=(flop site.request-line)
    ?~  back-path
      'World'
    i.back-path
  ?:  =(name 'stream')
    (handle-start-stream inbound-request)
  ~&  [%name name]
  ::
  :_  this
  :~  ^-  move
      :-  ost.bow
      :*  %http-response
          [%start 200 ['content-type' 'text/html']~ [~ part1] %.n]
      ==
  ::
      ^-  move
      :-  ost.bow
      :*  %http-response
          [%continue [~ (part2 name)] %.y]
      ==
  ==
::  +poke-handle-http-cancel: received when a connection was killed
::
++  poke-handle-http-cancel
  |=  =inbound-request:light
  ^-  (quip move _this)
  ::  the only long lived connections we keep state about are the stream ones.
  ::
  =.  events
    (~(session-stopped event-source events) ost.bow)
  ::
  [~ this]
--
