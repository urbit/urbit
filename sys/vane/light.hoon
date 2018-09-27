!:
::  lighter than eyre
::
|=  pit=vase
::  internal data structures
::
=>  =~
::
|%
--
::  TODO: this becomes the +eyre interface arm in zuse
::
|%
+|  %vane-interface
++  able
  |%
  ++  gift
    $%  ::  http-response: response from urbit to earth
        ::
        [%http-response =raw-http-response]
        ::  response to a %connect or %serve
        ::
        ::    :accepted is whether :binding was valid. Duplicate bindings are not allowed.
        ::
        [%bound accepted=? =binding]
    ==
  ::
  ++  task
    $%  ::  initializes ourselves with an identity
        ::
        ::    TODO: Remove this once we single home.
        ::
        [%init our=@p]
        ::  starts handling an inbound http request
        ::
        [%inbound-request secure=? =address =http-request]
        ::  connects a binding to an app
        ::
        [%connect =binding app=term]
        ::  connects a binding to a generator
        ::
        [%serve =binding generator=[=desk path=(list @t)] arguments=*]
        ::  disconnects a binding
        ::
        ::    This must be called with the same duct that made the binding in
        ::    the first place.
        ::
        [%disconnect =binding]
    ==
  --
::
+|  %bindings
::  +binding: A rule to match a path.
::
::    A +binding is a system unique mapping for a path to match. A +binding
::    must be system unique because we don't want two handlers for a path;
::    what happens if there are two different actions for [~ /]?
::
+$  binding
  $:  ::  site: the site to match.
      ::
      ::    A ~ will match the Urbit's identity site (your.urbit.org). Any
      ::    other value will match a domain literal.
      ::
      site=(unit @t)
      ::  path: matches this prefix path
      ::
      ::    /~myapp will match /~myapp or /~myapp/longer/path
      ::
      path=(list @t)
  ==
::
+|  %http
::  +header-list: an ordered list of http headers
::
+$  header-list
  (list [key=@t value=@t])
::  +http-method: exhaustive list of http verbs
::
+$  http-method
  $?  _'CONNECT'
      _'DELETE'
      _'GET'
      _'HEAD'
      _'OPTIONS'
      _'POST'
      _'PUT'
      _'TRACE'
  ==
::  +http-request: a single http-request
::
+$  http-request
  $:  ::  http-method:
      ::
      method=http-method
      ::  url: the url requested
      ::
      ::    The url is not escaped. There is no escape.
      ::
      url=@t
      ::  header-list: headers to pass with this request
      ::
      =header-list
      ::  body: optionally, data to send with this request
      ::
      body=(unit octs)
  ==
::  +raw-http-response: http-response to sent to earth
::
::    Urbit treats Earth's HTTP servers as pipes, where Urbit sends one or
::    more %http-response replies on the wire. The first of these will
::    always be a %start or an %error, and the last will always be %error
::    or will have :complete set to %.y to finish the connection.
::
::    Calculation of control headers such as 'Content-Length' or
::    'Transfer-Encoding' should be performed at a higher level; this structure
::    is merely for what gets sent to Earth.
::
+$  raw-http-response
  $%  ::  %start: the first packet in a response
      ::
      $:  %start
          ::  status: http status code
          ::
          status-code=@ud
          ::  headers: http headers
          ::
          headers=header-list
          ::  data: data to pass to the pipe
          ::
          data=(unit octs)
          ::  whether this completes the request
          ::
          complete=?
      ==
      ::  %continue: every subsequent packet
      ::
      $:  %continue
          ::  data: data to pass to the pipe
          ::
          data=(unit octs)
          ::  complete: whether this completes the request
          ::
          complete=?
      ==
      ::  %cancel: whether the connection should terminate unsuccessfully
      ::
      [%cancel ~]
  ==
::  +address: client IP address
::
+$  address
  $%  [%ipv4 @if]
      [%ipv6 @is]
      ::  [%ames @p]
  ==
--
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
  $%  ::  %f: to ford
      ::
      $:  %f
          ::
          ::
          $%  [%build our=@p live=? schematic=schematic:ford]
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
  $%  ::  %g: from gall
      ::
      $:  %g
          ::  %response: http-response from a gall app
          ::
          [%response =raw-http-response]
  ==  ==
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
      ::=client-state
      ::  server-state: state of inbound requests
      ::
      =server-state
  ==
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
      [%gen generator=[=desk path=(list @t)] args=*]
      ::  dispatch to an application
      ::
      [%app app=term]
      ::  internal authentication page
      ::
      [%login-handler ~]
  ==
::
::
+$  authentication-state
  $%  ::  sessions: a mapping of session cookies to session information
      ::
      sessions=(map @uv session)
  ==
::  +session: server side data about a session
::
+$  session
  $%  ::  expiry-time: when this session expires
      ::
      expiry-time=@da
  ==
--
::  utilities
::
|%
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
::  +per-server-event: per-event client core
::
++  per-server-event
  |=  [[our=@p eny=@ =duct now=@da scry=sley] state=server-state]
  |%
  ::  +request: starts handling an inbound http request
  ::
  ++  request
    |=  [secure=? =address =http-request]
    ^-  [(list move) server-state]
    ::
    =+  host=(get-header 'Host' header-list.http-request)
    =+  action=(get-action-for-binding host url.http-request)
    ::  if no action matches, send the built in 404 page.
    ::
    ?~  action
      %^  return-static-data-on-duct  404  'text/html'
      (file-not-found-page url.http-request)
    ::  record that we started an asynchronous response
    ::
    =|  record=outstanding-connection
    =.  action.record  u.action
    =.  connections.state  (~(put by connections.state) duct record)
    ::
    =/  authenticated  (request-is-logged-in:authentication http-request)
    ::
    ?-    -.u.action
    ::
        %gen
      ::  TODO: when we get here, we need to make sure that the generator has
      ::  been compiled.
      ::
      ~&  [%i-should-run-a-generator generator.u.action]
      [~ state]
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
      [app.u.action %poke %handle-http-request !>([authenticated secure address http-request])]
    ::
        %login-handler
      (handle-request:authentication secure address http-request)
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
          :~  ['Content-Type' content-type]
              ::  todo: how do I print a number? +scot adds '.' for hoon style.
              ::
              ::  ['Content-Length' p.data]
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
        ~!  q.u.body.http-request
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
        "urbauth={<session>}; Max-Age: 86400"
      ::
      =/  new-location=@t
        ?~  redirect=(get-header 'redirect' u.parsed)
          '/'
        u.redirect
      ::
      :_  state
      :_  ~
      :+  duct  %give
      :*  %http-response  %start
          status-code=307
          ^=  headers
            :~  ['Location' new-location]
                ['Set-Cookie' cookie-line]
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
      ?~  cookie-header=(get-header 'Cookie' header-list.http-request)
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
  ::  +handle-response: check a response for correctness and send to earth
  ::
  ::    TODO: I don't actually know how this gets hooked up. The app response
  ::    should really be a +take since it is a response to the +call poke, but
  ::    the gall interface seems to be mismatched to that.
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
            ~!  data.raw-http-response
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
      ::  TODO: Check IP addresses. I can't just check the
      ::  `\d{0-3}\.\d{0-3}...` regex here.
      ::
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
    ?:  ?&  =(site.binding.i.bindings host)
            ?|  ::  root directory
                ::
                &(=(~ parsed-url) =(~ path.binding.i.bindings))
                ::  everything else
                ::
                =(`0 (find path.binding.i.bindings parsed-url))
        ==  ==
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
      :~  [[~ /~/login] duct [%login-handler ~]]
      ==
    [~ light-gate]
  ::
      ::  %inbound-request: handles an inbound http request
      ::
      %inbound-request
    =/  event-args  [[(need ship.ax) eny duct now scry-gate] server-state.ax]
    =/  request  request:(per-server-event event-args)
    =^  moves  server-state.ax
      (request +.task)
    [moves light-gate]
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
        %serve    [%gen generator.task arguments.task]
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
  ?>  ?=([@ @ *] wire)
  ::
  |^  ^-  [p=(list move) q=_light-gate]
      ::
      ?:  =(%run-app i.wire)
        run-app
      ::
      ~|([%bad-take-wire wire] !!)
  ::
  ++  run-app
    ?>  ?=([%g %response *] sign)
    ::
    =/  event-args  [[(need ship.ax) eny duct now scry-gate] server-state.ax]
    =/  handle-response  handle-response:(per-server-event event-args)
    =^  moves  server-state.ax  (handle-response raw-http-response.sign)
    [moves light-gate]
  --
::
++  light-gate  ..$
--
