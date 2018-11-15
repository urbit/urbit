!:
::  lighter than eyre
::
|=  pit=vase
=,  light
::  internal data structures
::
=>  =~
::
::  internal data structures that won't go in zuse
::
|%
+$  move
  ::
  $:  ::  duct: request identifier
      ::
      =duct
      ::
      ::
      card=(wind note gift:able)
  ==
::  +note: private request from light to another vane
::
+$  note
  $%  ::  %b: to behn
      ::
      $:  %b
          ::
          ::
          $%  [%wait p=@da]
      ==  ==
      ::  %f: to ford
      ::
      $:  %f
          ::
          ::
          $%  [%build our=@p live=? schematic=schematic:ford]
              [%kill our=@p]
      ==  ==
      ::  %g: to gall
      ::
      $:  %g
          ::
          ::
          $%  [%deal id=sock data=cush:gall]
  ==  ==  ==
::  +sign: private response from another vane to ford
::
+$  sign
  $%  ::  %f: from ford
      ::
      $:  %f
          ::
          ::
          $%  [%made date=@da result=made-result:ford]
      ==  ==
      ::  %g: from gall
      ::
      $:  %g
          ::
          ::
          $%  [%unto p=cuft:gall]
  ==  ==  ==
--
::  more structures
::
|%
++  axle
  $:  ::  date: date at which light's state was updated to this data structure
      ::
      date=%~2018.9.12
      ::  ship: the ship name.
      ::
      ::    TODO: Remove when we single home.
      ::
      ship=(unit ship)
      ::  client-state: state of outbound requests
      ::
      client-state=state:client
      ::  server-state: state of inbound requests
      ::
      =server-state
  ==
::  +client: light as an http client
::
++  client
  |%
  ::  +state:client: state relating to open outbound HTTP connections
  ::
  +$  state
    $:  ::  next-id: monotonically increasing id number for the next connection
        ::
        next-id=@ud
        ::  connection-by-id: open connections to the
        ::
        connection-by-id=(map @ud [=duct =in-progress-http-request])
    ==
  ::  +in-progress-http-request: state around an outbound http
  ::
  +$  in-progress-http-request
    $:  ::  remaining-redirects: http limit of number of redirects before error
        ::
        remaining-redirects=@ud
        ::  remaining-retries: number of times to retry the request
        ::
        remaining-retries=@ud
        ::  chunks: a list of partial results returned from unix
        ::
        chunks=(list octs)
        ::  bytes-read: the sum of the size of the :chunks
        ::
        bytes-read=@ud
        ::  expected-size: the expected content-length of the http request
        ::
        expected-size=(unit @ud)
    ==
  --
::  +server-state: state relating to open inbound HTTP connections
::
+$  server-state
  $:  ::  bindings: actions to dispatch to when a binding matches
      ::
      ::    Eyre is responsible for keeping its bindings sorted so that it
      ::    will trigger on the most specific binding first. Eyre should send
      ::    back an error response if an already bound binding exists.
      ::
      ::    TODO: It would be nice if we had a path trie. We could decompose
      ::    the :binding into a (map (unit @t) (trie knot =action)).
      ::
      bindings=(list [=binding =duct =action])
      ::  connections: open http connections not fully complete
      ::
      connections=(map duct outstanding-connection)
      ::  authentication-state: state managed by the +authentication core
      ::
      =authentication-state
      ::  channel-state: state managed by the +channel core
      ::
      =channel-state
  ==
::  +outstanding-connection: open http connections not fully complete:
::
::    This refers to outstanding connections where the connection to
::    outside is opened and we are currently waiting on ford or an app to
::    produce the results.
::
+$  outstanding-connection
  $:  ::  action: the action that had matched
      ::
      =action
      ::  inbound-request: the original request which caused this connection
      ::
      =inbound-request
      ::  code: the status code, if sent
      ::
      code=(unit @ud)
      ::  headers: the headers, if sent
      ::
      headers=(unit header-list)
      ::  bytes-sent: the total bytes sent in response
      ::
      bytes-sent=@ud
  ==
::  +action: the action to take when a binding matches an incoming request
::
+$  action
  $%  ::  dispatch to a generator
      ::
      [%gen =generator]
      ::  dispatch to an application
      ::
      [%app app=term]
      ::  internal authentication page
      ::
      [%authentication ~]
      ::  gall channel system
      ::
      [%channel ~]
  ==
::  +authentication-state: state used in the login system
::
+$  authentication-state
  $:  ::  sessions: a mapping of session cookies to session information
      ::
      sessions=(map @uv session)
  ==
::  +session: server side data about a session
::
+$  session
  $:  ::  expiry-time: when this session expires
      ::
      ::    We check this server side, too, so we aren't relying on the browser
      ::    to properly handle cookie expiration as a security mechanism.
      ::
      expiry-time=@da
      ::
      ::  TODO: We should add a system for individual capabilities; we should
      ::  mint some sort of long lived cookie for mobile apps which only has
      ::  access to a single application path.
  ==
::  channel-state: state used in the channel system
::
+$  channel-state
  $:  ::  session: mapping between an arbitrary key to a channel
      ::
      session=(map @t channel)
  ==
::  channel: connection to the browser
::
::    Channels are the main method where a webpage communicates with Gall
::    apps. Subscriptions and pokes are issues with PUT requests on a path,
::    while GET requests on that same path open a persistent EventSource
::    channel.
::
::    The EventSource API is a sequence number based API that browser provide
::    which allow the server to push individual events to the browser over a
::    connection held open. In case of reconnection, the browser will send a
::    'Last-Event-Id: ' header to the server; the server then resends all
::    events since then.
::
+$  channel
  $:  ::  expiration-time: when this channel will expire
      ::
      ::    In case of an EventSource disconnect, we set a timer to reap the
      ::    subscriptions. This timer shouldn't be too short because the
      ::
      expiration-time=(unit @da)
      ::  next-id: next sequence number to use
      ::
      next-id=@ud
      ::  events: unacknowledged events
      ::
      ::    We keep track of all events where we haven't received a
      ::    'Last-Event-Id: ' response from the client or a per-poke {'ack':
      ::    ...} call. When there's an active EventSource connection on this
      ::    channel, we send the event but we still add it to events because we
      ::    can't assume it got received until we get an acknowledgment.
      ::
      events=(qeu [id=@ud type=term data=wall])
      ::  subscriptions: gall subscriptions
      ::
      ::    We maintain a list of subscriptions so if a channel times out, we
      ::    can cancel all the subscriptions we've made.
      ::
      subscriptions=(list [ship=@p app=term =wire =path])
      ::  duct: the open http sessions which we must %continue on new events.
      ::
      ::    For each channel, there is at most one open EventSource
      ::    connection. A 400 is issues on duplicate attempts to connect to the
      ::    same channel.
      ::
      duct=(unit duct)
  ==
::  channel-request: an action requested on a channel
::
+$  channel-request
  $%  ::  %ack: acknowledges that the client has received events up to :id
      ::
      [%ack id=@ud]
      ::  %poke: pokes an application, translating :json to :mark.
      ::
      [%poke ship=@p app=term mark=term =json]
      ::  %subscribe: subscribes to an application path
      ::
      [%subscribe ship=@p app=term =path]
      ::  %unsubscribe: unsubscribes from an application path
      ::
      [%unsubscribe ship=@p app=term =path]
  ==
--
::  utilities
::
|%
::  +parse-channel-request: parses a list of channel-requests
::
::    Parses a json array into a list of +channel-request. If any of the items
::    in the list fail to parse, the entire thing fails so we can 400 properly
::    to the client.
::
++  parse-channel-request
  |=  request-list=json
  ^-  (unit (list channel-request))
  ::  parse top
  ::
  =,  dejs-soft:format
  =-  ((ar -) request-list)
  ::
  |=  item=json
  ^-  (unit channel-request)
  ::
  ?~  maybe-key=((ot action+so ~) item)
    ~
  ?:  =('ack' u.maybe-key)
    ((pe %ack (ot id+ni ~)) item)
  ?:  =('poke' u.maybe-key)
    ((pe %poke (ot ship+(su fed:ag) app+so mark+so json+some ~)) item)
  ?:  =('subscribe' u.maybe-key)
    %.  item
    %+  pe  %subscribe
    (ot ship+(su fed:ag) app+so path+(su ;~(pfix fas (more fas urs:ab))) ~)
  ?:  =('unsubscribe' u.maybe-key)
    %.  item
    %+  pe  %unsubscribe
    (ot ship+(su fed:ag) app+so path+(su ;~(pfix fas (more fas urs:ab))) ~)
  ::  if we reached this, we have an invalid action key. fail parsing.
  ::
  ~
::  +file-not-found-page: 404 page for when all other options failed
::
++  file-not-found-page
  |=  url=@t
  ^-  octs
  %-  as-octs:mimes:html
  %-  crip
  %-  en-xml:html
  ;html
    ;head
      ;title:"404 Not Found"
    ==
    ;body
      ;h1:"Not Found"
      ;p:"The requested URL {<(trip url)>} was not found on this server."
    ==
  ==
::  +login-page: internal page to login to an Urbit
::
++  login-page
  |=  redirect-url=(unit @t)
  ^-  octs
  =+  redirect-str=?~(redirect-url "" (trip u.redirect-url))
  %-  as-octs:mimes:html
  %-  crip
  %-  en-xml:html
  ;html
    ;head
      ;title:"Sign in"
    ==
    ;body
      ;h1:"Sign in"
      ;form(action "/~/login", method "post", enctype "application/x-www-form-urlencoded")
        ;input(type "password", name "password", placeholder "passcode");
        ;input(type "hidden", name "redirect", value redirect-str);
        ;button(type "submit"):"Login"
      ==
    ==
  ==
::  +render-tang: renders a tang and adds <br/> tags between each line
::
++  render-tang
  |=  {wid/@u tan/tang}
  ^-  marl
  =/  raw=(list tape)  (zing (turn tan |=(a/tank (wash 0^wid a))))
  ::
  |-  ^-  marl
  ?~  raw  ~
  [;/(i.raw) ;br; $(raw t.raw)]
::  +internal-server-error: 500 page, with a tang
::
++  internal-server-error
  |=  [authorized=? url=@t t=tang]
  ^-  octs
  %-  as-octs:mimes:html
  %-  crip
  %-  en-xml:html
  ;html
    ;head
      ;title:"500 Internal Server Error"
    ==
    ;body
      ;h1:"Internal Server Error"
      ;p:"There was an error while handling the request for {<(trip url)>}."
      ;*  ?:  authorized
            ;=
              ;code:"*{(render-tang 80 t)}"
            ==
          ~
    ==
  ==
::  +format-ud-as-integer: prints a number for consumption outside urbit
::
++  format-ud-as-integer
  |=  a=@ud
  ^-  @t
  ?:  =(0 a)  '0'
  %-  crip
  %-  flop
  |-  ^-  tape
  ?:(=(0 a) ~ [(add '0' (mod a 10)) $(a (div a 10))])
::  +path-matches: returns %.y if :prefix is a prefix of :full
::
++  path-matches
  |=  [prefix=path full=path]
  ^-  ?
  ?~  prefix
    %.y
  ?~  full
    %.n
  ?.  =(i.prefix i.full)
    %.n
  $(prefix t.prefix, full t.full)
::  +get-header: returns the value for :header, if it exists in :header-list
::
++  get-header
  |=  [header=@t =header-list]
  ^-  (unit @t)
  ::
  ?~  header-list
    ~
  ::
  ?:  =(key.i.header-list header)
    `value.i.header-list
  ::
  $(header-list t.header-list)
::  +simplified-url-parser: returns [(each @if @t) (unit port=@ud)]
::
++  simplified-url-parser
  ;~  plug
    ;~  pose
      %+  stag  %ip
      =+  tod=(ape:ag ted:ab)
      %+  bass  256
      ;~(plug tod (stun [3 3] ;~(pfix dot tod)))
    ::
      (stag %site (cook crip (star ;~(pose dot alp))))
    ==
    ;~  pose
      (stag ~ ;~(pfix col dim:ag))
      (easy ~)
    ==
  ==
::  +per-client-event: per-event client core
::
++  per-client-event
  |=  [[our=@p eny=@ =duct now=@da scry=sley] state=state:client]
  |%
  ++  fetch
    |=  [=http-request =outbound-config]
    ^-  [(list move) state:client]
    ::  get the next id for this request
    ::
    =^  id  next-id.state  [next-id.state +(next-id.state)]
    ::  add a new open session
    ::
    =.  connection-by-id.state
      %+  ~(put by connection-by-id.state)  id
      =,  outbound-config
      [duct [redirects retries ~ 0 ~]]
    ::  start the download 
    ::
    ::  the original eyre keeps track of the duct on %born and then sends a
    ::  %give on that duct. this seems like a weird inversion of
    ::  responsibility, where we should instead be doing a pass to unix. the
    ::  reason we need to manually build ids is because we aren't using the
    ::  built in duct system.
    ::
    ::  email discussions make it sound like fixing that might be hard, so
    ::  maybe i should just live with the way it is now?
    ::
    ::  :-  [duct %pass /fetch 
    [~ state]
  --
::  +per-server-event: per-event server core
::
++  per-server-event
  ::  gate that produces the +per-server-event core from event information
  ::
  |=  [[our=@p eny=@ =duct now=@da scry=sley] state=server-state]
  |%
  ::  +request: starts handling an inbound http request
  ::
  ++  request
    |=  [secure=? =address =http-request]
    ^-  [(list move) server-state]
    ::
    =+  host=(get-header 'host' header-list.http-request)
    =+  action=(get-action-for-binding host url.http-request)
    ::  if no action matches, send the built in 404 page.
    ::
    ?~  action
      %^  return-static-data-on-duct  404  'text/html'
      (file-not-found-page url.http-request)
    ::
    =/  authenticated  (request-is-logged-in:authentication http-request)
    ::  record that we started an asynchronous response
    ::
    =/  connection=outstanding-connection
      [u.action [authenticated secure address http-request] ~ ~ 0]
    =.  connections.state
      (~(put by connections.state) duct connection)
    ::
    ?-    -.u.action
    ::
        %gen
      ::
      =-  [[duct %pass /run-build %f %build our live=%.n schematic=-]~ state]
      ::
      =-  [%cast [our desk.generator.u.action] %mime -]
      ::
      :+  %call
        :+  %call
          [%core [[our desk.generator.u.action] (flop path.generator.u.action)]]
        ::  TODO: Figure out what goes in generators. We need to slop the
        ::  prelude with the arguments passed in.
        ::
        [%$ %noun !>([[now=now eny=eny bek=[our desk.generator.u.action [%da now]]] ~ ~])]
      [%$ %noun !>([authenticated http-request])]
    ::
        %app
      :_  state
      :_  ~
      :^  duct  %pass  /run-app/[app.u.action]
      ^-  note
      :^  %g  %deal  [our our]
      ::  todo: i don't entirely understand gall; there's a way to make a gall
      ::  use a %handle arm instead of a sub-%poke with the
      ::  %handle-http-request type.
      ::
      ^-  cush:gall
      :*  app.u.action
          %poke
          %handle-http-request
          !>(inbound-request.connection)
      ==
    ::
        %authentication
      (handle-request:authentication secure address http-request)
    ::
        %channel
      (handle-request:channel secure authenticated address http-request)
    ==
  ::  +cancel-request: handles a request being externally aborted
  ::
  ++  cancel-request
    ^-  [(list move) server-state]
    ::
    ~&  [%cancel-request duct]
    ::
    ?~  connection=(~(get by connections.state) duct)
      ::  nothing has handled this connection
      ::
      [~ state]
    ::
    =.   connections.state  (~(del by connections.state) duct)
    ::
    ?-    -.action.u.connection
    ::
        %gen
      :_  state
      [duct %pass /run-build %f %kill our]~
    ::
        %app
      :_  state
      :_  ~
      :^  duct  %pass  /run-app/[app.action.u.connection]
      ^-  note
      :^  %g  %deal  [our our]
      ::  todo: i don't entirely understand gall; there's a way to make a gall
      ::  use a %handle arm instead of a sub-%poke with the
      ::  %handle-http-request type.
      ::
      ^-  cush:gall
      :*  app.action.u.connection
          %poke
          %handle-http-cancel
          !>(inbound-request.u.connection)
      ==
    ::
        %authentication
      [~ state]
    ::
        %channel
      ::  todo: this part actually matters.
      ::
      [~ state]
    ==
  ::  +return-static-data-on-duct: returns one piece of data all at once
  ::
  ++  return-static-data-on-duct
    |=  [code=@ content-type=@t data=octs]
    ^-  [(list move) server-state]
    ::
    :_  state
    :_  ~
    :+  duct  %give
    :*  %http-response  %start
        status-code=code
        ^=  headers
          :~  ['content-type' content-type]
              ['content-length' (format-ud-as-integer p.data)]
          ==
        data=[~ data]
        complete=%.y
    ==
  ::  +authentication: per-event authentication as this Urbit's owner
  ::
  ::    Right now this hard codes the authentication page using the old +code
  ::    system, but in the future should be pluggable so we can use U2F or
  ::    WebAuthn or whatever is more secure than passwords.
  ::
  ++  authentication
    |%
    ::  +handle-request: handles an http request for the 
    ::
    ++  handle-request
      |=  [secure=? =address =http-request]
      ^-  [(list move) server-state]
      ::
      ::  if we received a simple get, just return the page
      ::
      ?:  =('GET' method.http-request)
        ::  parse the arguments out of request uri
        ::
        =+  request-line=(parse-request-line url.http-request)
        %^  return-static-data-on-duct  200  'text/html'
        (login-page (get-header 'redirect' args.request-line))
      ::  if we are not a post, return an error
      ::
      ?.  =('POST' method.http-request)
        ~&  [%something-other-than-get-post-on-login method.http-request]
        (return-static-data-on-duct 400 'text/html' (login-page ~))
      ::  we are a post, and must process the body type as form data
      ::
      ?~  body.http-request
        (return-static-data-on-duct 400 'text/html' (login-page ~))
      ::
      =/  parsed=(unit (list [key=@t value=@t]))
        (rush q.u.body.http-request yquy:de-purl:html)
      ?~  parsed
        (return-static-data-on-duct 400 'text/html' (login-page ~))
      ::
      ?~  password=(get-header 'password' u.parsed)
        (return-static-data-on-duct 400 'text/html' (login-page ~))
      ::  check that the password is correct
      ::
      ?.  =(u.password code)
        (return-static-data-on-duct 400 'text/html' (login-page ~))
      ::  mint a unique session cookie
      ::
      =/  session=@uv
        |-
        =/  candidate=@uv  (~(raw og eny) 128)
        ?.  (~(has by sessions.authentication-state.state) candidate)
          candidate
        $(eny (shas %try-again candidate))
      ::  record cookie and record expiry time
      ::
      =.  sessions.authentication-state.state
        (~(put by sessions.authentication-state.state) session (add now ~h24))
      ::
      =/  cookie-line
        %-  crip
        "urbauth={<session>}; Path=/; Max-Age=86400"
      ::
      =/  new-location=@t
        ?~  redirect=(get-header 'redirect' u.parsed)
          '/'
        u.redirect
      ~&  [%minting http-request]
      ::
      :_  state
      :_  ~
      :+  duct  %give
      :*  %http-response  %start
          status-code=307
          ^=  headers
            :~  ['location' new-location]
                ['set-cookie' cookie-line]
            ==
          data=~
          complete=%.y
      ==
    ::  +request-is-logged-in: checks to see if the request is authenticated
    ::
    ::    We are considered logged in if this http-request has an urbauth
    ::    Cookie which is not expired.
    ::
    ++  request-is-logged-in
      |=  =http-request
      ^-  ?
      ::  are there cookies passed with this request?
      ::
      ::    TODO: In HTTP2, the client is allowed to put multiple 'Cookie'
      ::    headers.
      ::
      ?~  cookie-header=(get-header 'cookie' header-list.http-request)
        %.n
      ::  is the cookie line is valid?
      ::
      ?~  cookies=(rush u.cookie-header cock:de-purl:html)
        %.n
      ::  is there an urbauth cookie?
      ::
      ?~  urbauth=(get-header 'urbauth' u.cookies)
        %.n
      ::  is this formatted like a valid session cookie?
      ::
      ?~  session-id=(rush u.urbauth ;~(pfix (jest '0v') viz:ag))
        %.n
      ::  is this a session that we know about?
      ::
      ?~  session=(~(get by sessions.authentication-state.state) u.session-id)
        %.n
      ::  is this session still valid?
      ::
      (lte now expiry-time.u.session)
    ::  +code: returns the same as |code
    ::
    ::    This has the problem where the signature for sky vs sley.
    ::
    ++  code
      ^-  @ta
      'lidlut-tabwed-pillex-ridrup'
      ::  =+  pax=/(scot %p our)/code/(scot %da now)/(scot %p our)
      ::  %^  rsh  3  1
      ::  (scot %p (@ (need ((sloy scry) [151 %noun] %a pax))))
    --
  ::  +channel: per-event handling of requests to the channel system
  ::
  ::    Eyre offers a remote interface to your Urbit through channels, which
  ::    are persistent connections on the server which 
  ::
  ++  channel
    ::  moves: the moves to be sent out at the end of this event, reversed
    ::
    =|  moves=(list move)
    |%
    ::  channel-timeout: the delay before a channel should be reaped
    ::
    ++  channel-timeout  ~h12
    ::  +handle-request: handles an http request for the subscription system
    ::
    ++  handle-request
      |=  [secure=? authenticated=? =address =http-request]
      ^-  [(list move) server-state]
      ::  if we're not authenticated error, but don't redirect.
      ::
      ::    We don't redirect because subscription stuff is never the toplevel
      ::    page; issuing a redirect won't help.
      ::
      ?.  authenticated
        ::  TODO: Real 400 page.
        ::
        %^  return-static-data-on-duct  400  'text/html'
        (internal-server-error authenticated url.http-request ~)
      ::  parse out the path key the subscription is on
      ::
      =+  request-line=(parse-request-line url.http-request)
      ?.  ?=([@t @t @t ~] site.request-line)
        ::  url is not of the form '/~/subscription/uid'
        ::
        %^  return-static-data-on-duct  400  'text/html'
        (internal-server-error authenticated url.http-request ~)
      ::  uid: unique channel id parsed out of url
      ::
      =+  uid=i.t.t.site.request-line
      ::
      ?:  =('PUT' method.http-request)
        ::  PUT methods starts/modifies a channel, and returns a result immediately
        ::
        (on-put-request uid http-request)
      ::
      ~&  %session-not-a-put
      [~ state]
    ::  +handle-cancel: cancels an ongoing subscription
    ::
    ::++  handle-cancel
    ::  +on-put-request: handles a PUT request
    ::
    ::    
    ::
    ++  on-put-request
      |=  [uid=@t =http-request]
      ^-  [(list move) server-state]
      ::  error when there's no body
      ::
      ?~  body.http-request
        %^  return-static-data-on-duct  400  'text/html'
        (internal-server-error %.y url.http-request ~)
      ::  parse the incoming body as a json array of +channel-request items
      ::
      =/  request-json=(unit json)  ~
        ::(de-json:html u.body.http-request)
      ::  if the json doesn't parse, this is a bad request, 400.
      ::
      ?~  request-json
        %^  return-static-data-on-duct  400  'text/html'
        (internal-server-error %.y url.http-request ~)
      ::
      ::
      =/  requests=(list channel-request)
        ~
        ::
        ::  (parse-channel-request request-json)
      ?:  =(~ requests)
        %^  return-static-data-on-duct  400  'text/html'
        (internal-server-error %.y url.http-request ~)
      ::  check for the existence of the uid
      ::
      ::    if we have no session, create a new one set to expire in
      ::    :channel-timeout from now.
      ::
      =?  ..on-put-request  (~(has by session.channel-state.state) uid)
        ::
        =/  expiration-time=@da  (add now channel-timeout)
        %_    ..on-put-request
          ::    session.channel-state.state
          ::  %+  ~(put by session.channel-state.state)  uid
          ::  [`expiration-time 0 ~ ~ ~]
        ::
            moves
          :_  moves
          ^-  move
          [duct %pass /timeout/[uid] %b %wait expiration-time]
        ==
      ::  for each request, execute the action passed in
      ::
      |-
      ?~  requests
        [moves state]
      ::
      ?-    -.i.requests
          %ack
        !!
      ::
          %poke
        ::
        ::  =.  moves
        ::    :_  moves
        ::    :^  duct  %pass  /channel-poke/[uid]
        ::    =,  i.requests
        ::    [%g %deal [our ship] app %peel mark %json !>(json)]
        ::
        $(requests t.requests)
      ::
          %subscribe
        !!
      ::
          %unsubscribe
        !!
      ==
    ::  +on-channel-timeout: we received a wake to clear an old session
    ::
    ++  on-channel-timeout
      |=  uid=@t
      ^-  [(list move) server-state]
      ::
      =/  session
        (~(got by session.channel-state.state) uid)
      ::
      :_  state
          ::  %_    state
          ::      session.channel-state
          ::    (~(del by session.channel-state) uid)
          ::  ==
      ::  produce a list of moves which cancels every gall subscription
      ::
      ::  %+  turn  subscriptions.session
      ::  |=  [ship=@p app=term =wire =path]
      ::  ^-  move
      ::  ::  todo: double check this; which duct should we be canceling on? does
      ::  ::  gall strongly bind to a duct as a cause like ford does?
      ::  ::
      ::  [duct %pass [%g %deal [our ship] app %pull ~]]
      ~
    --
  ::  +handle-ford-response: translates a ford response for the outside world
  ::
  ::    TODO: Get the authentication state and source url here.
  ::
  ++  handle-ford-response
    |=  made-result=made-result:ford
    ^-  [(list move) server-state]
    ::
    ?:  ?=(%incomplete -.made-result)
      %^  return-static-data-on-duct  500  'text/html'
      ::  TODO: Thread original URL and authentication state here.
      (internal-server-error %.y 'http://' tang.made-result)
    ::
    ?:  ?=(%error -.build-result.made-result)
      %^  return-static-data-on-duct  500  'text/html'
      (internal-server-error %.y 'http://' message.build-result.made-result)
    ::
    =/  =cage  (result-to-cage:ford build-result.made-result)
    ::
    %-  handle-response
    =/  result=mime  ((hard mime) q.q.cage)
    ::
    ^-  raw-http-response
    :*  %start
        200
        ^-  header-list
        :~  ['content-type' (en-mite:mimes:html p.result)]
            ['content-length' (format-ud-as-integer p.q.result)]
        ==
        `(unit octs)`[~ q.result]
        complete=%.y
    ==
  ::  +handle-response: check a response for correctness and send to earth
  ::
  ++  handle-response
    |=  =raw-http-response
    ^-  [(list move) server-state]
    ::  verify that this is a valid response on the duct
    ::
    ?~  connection-state=(~(get by connections.state) duct)
      ~&  [%invalid-outstanding-connection duct]
      [~ state]
    ::
    |^  ^-  [(list move) server-state]
        ::
        ?-    -.raw-http-response
        ::
            %start
          ?^  code.u.connection-state
            ~&  [%http-multiple-start duct]
            error-connection
          ::
          =.  connections.state
            %+  ~(jab by connections.state)  duct
            |=  connection=outstanding-connection
            %_  connection
              code        `status-code.raw-http-response
              headers     `headers.raw-http-response
              bytes-sent  ?~(data.raw-http-response 0 p.u.data.raw-http-response)
            ==
          ::
          =?  state  complete.raw-http-response
            log-complete-request
          ::
          pass-response
        ::
            %continue
          ?~  code.u.connection-state
            ~&  [%http-continue-without-start duct]
            error-connection
          ::
          =.  connections.state
            %+  ~(jab by connections.state)  duct
            |=  connection=outstanding-connection
            =+  size=?~(data.raw-http-response 0 p.u.data.raw-http-response)
            connection(bytes-sent (add bytes-sent.connection size))
          ::
          =?  state  complete.raw-http-response
            log-complete-request
          ::
          pass-response
        ::
            %cancel
          ::  todo: log this differently from an ise.
          ::
          error-connection
        ==
    ::
    ++  pass-response
      ^-  [(list move) server-state]
      [[duct %give %http-response raw-http-response]~ state]
    ::
    ++  log-complete-request
      ::  todo: log the complete request
      ::
      ::  remove all outstanding state for this connection
      ::
      =.  connections.state
        (~(del by connections.state) duct)
      state
    ::
    ++  error-connection
      ::  todo: log application error
      ::
      ::  remove all outstanding state for this connection
      ::
      =.  connections.state
        (~(del by connections.state) duct)
      ::  respond to outside with %error
      ::
      ^-  [(list move) server-state]
      [[duct %give %http-response %cancel ~]~ state]
    --
  ::  +add-binding: conditionally add a pairing between binding and action
  ::
  ::    Adds =binding =action if there is no conflicting bindings.
  ::
  ++  add-binding
    |=  [=binding =action]
    ::
    =/  to-search  bindings.state
    |-
    ^-  [(list move) server-state]
    ?~  to-search
      :-  [duct %give %bound %.y binding]~
      =.  bindings.state
        ::  store in reverse alphabetical order so that longer paths are first
        ::
        %-  flop
        %+  sort  [[binding duct action] bindings.state]
        |=  [[a=^binding *] [b=^binding *]]
        ::
        ?:  =(site.a site.b)
          (aor path.a path.b)
        ::  alphabetize based on site
        ::
        (aor ?~(site.a '' u.site.a) ?~(site.b '' u.site.b))
      state
    ::
    ?:  =(binding binding.i.to-search)
      :-  [duct %give %bound %.n binding]~
      state
    ::
    $(to-search t.to-search)
  ::  +remove-binding: removes a binding if it exists and is owned by this duct
  ::
  ++  remove-binding
    |=  =binding
    ::
    ^-  server-state
    %_    state
        bindings
      %+  skip  bindings.state
      |=  [item-binding=^binding item-duct=^duct =action]
      ^-  ?
      &(=(item-binding binding) =(item-duct duct))
    ==
  ::  +get-action-for-binding: finds an action for an incoming web request
  ::
  ++  get-action-for-binding
    |=  [raw-host=(unit @t) url=@t]
    ^-  (unit action)
    ::  process :raw-host
    ::
    ::    If we are missing a 'Host:' header, if that header is a raw IP
    ::    address, or if the 'Host:' header refers to [our].urbit.org, we want
    ::    to return ~ which is the binding for our Urbit identity.
    ::
    ::    Otherwise, return the site given.
    ::
    =/  host=(unit @t)
      ?~  raw-host
        ~
      ::  Parse the raw-host so that we can ignore ports, usernames, etc.
      ::
      =+  parsed=(rush u.raw-host simplified-url-parser)
      ?~  parsed
        ~
      ::  if the url is a raw IP, assume default site.
      ::
      ?:  ?=([%ip *] -.u.parsed)
        ~
      ::  if the url is "localhost", assume default site.
      ::
      ?:  =([%site 'localhost'] -.u.parsed)
        ~
      ::  render our as a tape, and cut off the sig in front.
      ::
      =/  with-sig=tape  (scow %p our)
      ?>  ?=(^ with-sig)
      ?:  =(u.raw-host (crip t.with-sig))
        ::  [our].urbit.org is the default site
        ::
        ~
      ::
      raw-host
    ::  url is the raw thing passed over the 'Request-Line'.
    ::
    ::    todo: this is really input validation, and we should return a 500 to
    ::    the client.
    ::
    =/  request-line  (parse-request-line url)
    =/  parsed-url=(list @t)  site.request-line
    ::
    =/  bindings  bindings.state
    |-
    ::
    ?~  bindings
      ~
    ::
    ?:  (path-matches path.binding.i.bindings parsed-url)
      `action.i.bindings
    ::
    $(bindings t.bindings)
  --
  ::
  ::
  ++  parse-request-line
    |=  url=@t
    ^-  [[(unit @ta) site=(list @t)] args=(list [key=@t value=@t])]
    (fall (rush url ;~(plug apat:de-purl:html yque:de-purl:html)) [[~ ~] ~])
--
::  end the =~
::
.  ==
::  begin with a default +axle as a blank slate
::
=|  ax=axle
::  a vane is activated with current date, entropy, and a namespace function
::
|=  [now=@da eny=@ scry-gate=sley]
::  allow jets to be registered within this core
::
~%  %light  ..is  ~
|%
++  call
  |=  [=duct type=* wrapped-task=(hobo task:able)]
  ^-  [p=(list move) q=_light-gate]
  ::
  =/  task=task:able
    ?.  ?=(%soft -.wrapped-task)
      wrapped-task
    ~|  [%p-wrapped-task p.wrapped-task]
    ((hard task:able) p.wrapped-task)
  ::
  ?-    -.task
      ::  %init: tells us what our ship name is
      ::
      %init
    ::
    =.  ship.ax  [~ our.task]
    ::  initial value for the login handler
    ::
    =.  bindings.server-state.ax
      :~  [[~ /~/login] duct [%authentication ~]]
          [[~ /~/channel] duct [%channel ~]]
      ==
    [~ light-gate]
      ::  %born: new unix process
      ::
      %born
    ::
    ~&  [%todo-handle-born p.task]
    ::  TODO: reset the next-id for client state here.
    ::

    ::  close previously open connections
    ::
    ::    When we have a new unix process, every outstanding open connection is
    ::    dead. For every duct, send an implicit close connection.
    ::
    =^  closed-connections=(list move)  server-state.ax
      =/  connections=(list [=^duct *])
        ~(tap by connections.server-state.ax)
      ::
      =|  closed-connections=(list move)
      |-
      ?~  connections
        [closed-connections server-state.ax]
      ::
      =/  event-args
        [[(need ship.ax) eny duct.i.connections now scry-gate] server-state.ax]
      =/  cancel-request  cancel-request:(per-server-event event-args)
      =^  moves  server-state.ax  cancel-request
      ::
      $(closed-connections (weld moves closed-connections), connections t.connections)
    ::
    :_  light-gate
    ;:  weld
      ::  hand back default configuration for now
      ::
      [duct %give %form *http-config]~
    ::
      closed-connections
    ==
  ::
      ::  %live: no idea what this is for
      ::
      %live
    ::
    ~&  [%todo-live p.task q.task]
    ::
    [~ light-gate]
  ::
      ::  %inbound-request: handles an inbound http request
      ::
      %inbound-request
    ::
    ::  TODO: This is uncommit
    ::
    ~|  [%ship ship.ax]
    =/  event-args  [[(need ship.ax) eny duct now scry-gate] server-state.ax]
    =/  request  request:(per-server-event event-args)
    =^  moves  server-state.ax
      (request +.task)
    [moves light-gate]
  ::
      ::
      ::
      %cancel-inbound-request
    =/  event-args  [[(need ship.ax) eny duct now scry-gate] server-state.ax]
    =/  cancel-request  cancel-request:(per-server-event event-args)
    =^  moves  server-state.ax  cancel-request
    [moves light-gate]
  ::
      ::  %fetch
      ::
      %fetch
    ~&  %todo-fetch
    =/  event-args  [[(need ship.ax) eny duct now scry-gate] client-state.ax]
    =/  fetch  fetch:(per-client-event event-args)
    =^  moves  client-state.ax  (fetch +.task)
    [moves light-gate]
  ::
      ::  %cancel-fetch
      ::
      %cancel-fetch
    ~&  %todo-cancel-fetch
    [~ light-gate]
  ::
      ::  %receive: receives http data from unix
      ::
      %receive
    ~&  %todo-receive
    [~ light-gate]
  ::
      ::  %connect / %serve
      ::
      ?(%connect %serve)
    =/  event-args  [[(need ship.ax) eny duct now scry-gate] server-state.ax]
    =/  add-binding  add-binding:(per-server-event event-args)
    =^  moves  server-state.ax
      %+  add-binding  binding.task
      ?-  -.task
        %connect  [%app app.task]
        %serve    [%gen generator.task]
      ==
    [moves light-gate]
  ::
      ::  %disconnect
      ::
      %disconnect
    =/  event-args  [[(need ship.ax) eny duct now scry-gate] server-state.ax]
    =/  remove-binding  remove-binding:(per-server-event event-args)
    =.  server-state.ax  (remove-binding binding.task)
    [~ light-gate]
  ==
::
++  take
  |=  [=wire =duct wrapped-sign=(hypo sign)]
  ^-  [p=(list move) q=_light-gate]
  ::  unwrap :sign, ignoring unneeded +type in :p.wrapped-sign
  ::
  =/  =sign  q.wrapped-sign
  ::  :wire must at least contain two parts, the type and the build
  ::
  ?>  ?=([@ *] wire)
  ::
  |^  ^-  [p=(list move) q=_light-gate]
      ::
      ?+     i.wire
           ~|([%bad-take-wire wire] !!)
      ::
         %run-app    run-app
         %run-build  run-build
      ==
  ::
  ++  run-app
    ::
    ?.  ?=([%g %unto %http-response *] sign)
      ::  entirely normal to get things other than http-response calls, but we
      ::  don't care.
      ::
      [~ light-gate]
    ::
    =/  event-args  [[(need ship.ax) eny duct now scry-gate] server-state.ax]
    =/  handle-response  handle-response:(per-server-event event-args)
    =^  moves  server-state.ax  (handle-response raw-http-response.p.sign)
    [moves light-gate]
  ::
  ++  run-build
    ::
    ?>  ?=([%f %made *] sign)
    ::
    =/  event-args  [[(need ship.ax) eny duct now scry-gate] server-state.ax]
    =/  handle-ford-response  handle-ford-response:(per-server-event event-args)
    =^  moves  server-state.ax  (handle-ford-response result.sign)
    [moves light-gate]
  --
::
++  light-gate  ..$
::  +load: migrate old state to new state (called on vane reload)
::
++  load
  |=  old=axle
  ^+  ..^$
  ::
  ~!  %loading
  ..^$(ax old)
::  +stay: produce current state
::
++  stay  `axle`ax
::  +scry: request a path in the urbit namespace
::
++  scry
  |=  *
  [~ ~]
--
