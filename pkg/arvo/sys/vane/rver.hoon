!:
::  lighter than eyre
::
|=  pit=vase
=,  http-server
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
::  +note: private request from http-server to another vane
::
+$  note
  $%  ::  %b: to behn
      ::
      $:  %b
          ::
          ::
          $%  [%rest p=@da]
              [%wait p=@da]
      ==  ==
      ::  %d: to dill
      ::
      $:  %d
          ::
          ::
      $%  [%flog =flog:dill]
      ==  ==
      ::  %f: to ford
      ::
      $:  %f
          ::
          ::
          $%  [%build live=? schematic=schematic:ford]
              [%kill ~]
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
  $%  ::  %b: from behn
      ::
      $:  %b
          ::
          ::
          $%  [%wake error=(unit tang)]
      ==  ==
      ::  %f: from ford
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
  $:  ::  date: date at which http-server's state was updated to this data structure
      ::
      date=%~2019.1.7
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
      ::  channel-state: state managed by the +channel core
      ::
      =channel-state
      ::  domains: domain-names that resolve to us
      ::
      domains=(set turf)
      ::  http-config: our server configuration
      ::
      =http-config
      ::  ports: live servers
      ::
      ports=[insecure=@ud secure=(unit @ud)]
      ::  outgoing-duct: to unix
      ::
      outgoing-duct=duct
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
      ::  response-header: set when we get our first %start
      ::
      response-header=(unit response-header:http)
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
      ::  respond with the default file not found page
      ::
      [%four-oh-four ~]
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
      ::  by-duct: mapping from ducts to session key
      ::
      duct-to-key=(map duct @t)
  ==
::  +timer: a reference to a timer so we can cancel or update it.
::
+$  timer
  $:  ::  date: time when the timer will fire
      ::
      date=@da
      ::  duct: duct that set the timer so we can cancel
      ::
      =duct
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
::    TODO: Send \n as a heartbeat every 20 seconds.
::
+$  channel
  $:  ::  channel-state: expiration time or the duct currently listening
      ::
      ::    For each channel, there is at most one open EventSource
      ::    connection. A 400 is issues on duplicate attempts to connect to the
      ::    same channel. When an EventSource isn't connected, we set a timer
      ::    to reap the subscriptions. This timer shouldn't be too short
      ::    because the
      ::
      state=(each timer duct)
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
      events=(qeu [id=@ud lines=wall])
      ::  subscriptions: gall subscriptions
      ::
      ::    We maintain a list of subscriptions so if a channel times out, we
      ::    can cancel all the subscriptions we've made.
      ::
      subscriptions=(map wire [ship=@p app=term =path duc=duct])
  ==
::  channel-request: an action requested on a channel
::
+$  channel-request
  $%  ::  %ack: acknowledges that the client has received events up to :id
      ::
      [%ack event-id=@ud]
      ::  %poke: pokes an application, translating :json to :mark.
      ::
      [%poke request-id=@ud ship=@p app=term mark=@tas =json]
      ::  %subscribe: subscribes to an application path
      ::
      [%subscribe request-id=@ud ship=@p app=term =path]
      ::  %unsubscribe: unsubscribes from an application path
      ::
      [%unsubscribe request-id=@ud subscription-id=@ud]
      ::  %delete: kills a channel
      ::
      [%delete ~]
  ==
::  channel-timeout: the delay before a channel should be reaped
::
++  channel-timeout  ~h12
--
::  utilities
::
|%
::  +combine-octs: combine multiple octs into one
::
++  combine-octs
  |=  a=(list octs)
  ^-  octs
  :-  %+  roll  a
      |=  [=octs sum=@ud]
      (add sum p.octs)
  (can 3 a)
::  +prune-events: removes all items from the front of the queue up to :id
::
++  prune-events
  |=  [q=(qeu [id=@ud lines=wall]) id=@ud]
  ^+  q
  ::  if the queue is now empty, that's fine
  ::
  ?:  =(~ q)
    ~
  ::
  =/  next=[item=[id=@ud lines=wall] _q]  ~(get to q)
  ::  if the head of the queue is newer than the acknowledged id, we're done
  ::
  ?:  (gth id.item.next id)
    q
  ::  otherwise, check next item
  ::
  $(q +:next)
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
    ((pe %ack (ot event-id+ni ~)) item)
  ?:  =('poke' u.maybe-key)
    ((pe %poke (ot id+ni ship+(su fed:ag) app+so mark+(su sym) json+some ~)) item)
  ?:  =('subscribe' u.maybe-key)
    %.  item
    %+  pe  %subscribe
    (ot id+ni ship+(su fed:ag) app+so path+(su ;~(pfix fas (more fas urs:ab))) ~)
  ?:  =('unsubscribe' u.maybe-key)
    %.  item
    %+  pe  %unsubscribe
    (ot id+ni subscription+ni ~)
  ?:  =('delete' u.maybe-key)
    `[%delete ~]
  ::  if we reached this, we have an invalid action key. fail parsing.
  ::
  ~
::  +login-page: internal page to login to an Urbit
::
++  login-page
  |=  [redirect-url=(unit @t) our=@p]
  ^-  octs
  =+  redirect-str=?~(redirect-url "" (trip u.redirect-url))
  %-  as-octs:mimes:html
  %-  crip
  %-  en-xml:html
  ;html
    ;head
      ;meta(charset "utf-8");
      ;title:"Sign in"
      ;style:'''
             @import url("https://rsms.me/inter/inter.css");
             @font-face {
                 font-family: "Source Code Pro";
                 src: url("https://storage.googleapis.com/media.urbit.org/fonts/scp-medium.woff");
                 font-weight: 500
             }
             html, body {
               font-family: Inter, sans-serif;
               height: 100%;
               margin: 0 !important;
               width: 100%;
               background: #000;
               color: #fff;
               display: table;
             }
             form {
               margin: 0 !important;
               display: flex;
               flex-direction: column;
               flex: 2;
             }
             #topborder {
               border-top: 3px #fff solid;
             }
             h1 {
               line-height: 77px;
               font-size: 64px;
               -webkit-margin-before: 0;
               -webkit-margin-after: 0;
               -webkit-margin-start: 0;
               -webkit-margin-end: 0;
               font-weight: 500;
               flex: 1;
               padding-top: 22px;
               margin-bottom: 66px;
             }
             #main {
               vertical-align: middle;
               display: table-cell;
             }
             input {
               display: block;
               width: 100%;
               font-size: 64px;
               line-height: 77px;
               color: #fff;
               background: #000;
               border: none;
               flex: 1;
               margin-bottom: 66px;
               font-weight: 500;
               font-family: 'Source Code Pro', monospace, sans-serif;
             }
             button {
               background: #000;
               border: none;
               color: #fff;
               line-height: 77px;
               font-size: 64px;
               text-align: left;
               flex: 1;
               font-weight: 500;
             }
             #inner {
               width: 75%;
               margin: 0 auto;
               display: flex;
               flex-direction: column;
             }
             '''
    ==
    ;body
      ;div#main
        ;div#inner
          ;h1#topborder:"Modulo"
          ;h1:"{(scow %p our)}"
          ;form(action "/~/login", method "post", enctype "application/x-www-form-urlencoded")
            ;input(type "password", name "password", placeholder "passcode", autofocus "true");
            ;input(type "hidden", name "redirect", value redirect-str);
            ;button(type "submit"):"→ Authenticate"
          ==
        ==
      ==
    ==
  ==
::  +render-tang-to-marl: renders a tang and adds <br/> tags between each line
::
++  render-tang-to-marl
  |=  {wid/@u tan/tang}
  ^-  marl
  =/  raw=(list tape)  (zing (turn tan |=(a/tank (wash 0^wid a))))
  ::
  |-  ^-  marl
  ?~  raw  ~
  [;/(i.raw) ;br; $(raw t.raw)]
::  +render-tang-to-wall: renders tang as text lines
::
++  render-tang-to-wall
  |=  {wid/@u tan/tang}
  ^-  wall
  (zing (turn tan |=(a=tank (wash 0^wid a))))
::  +wall-to-octs: text to binary output
::
++  wall-to-octs
  |=  =wall
  ^-  (unit octs)
  ::
  ?:  =(~ wall)
    ~
  ::
  :-  ~
  %-  as-octs:mimes:html
  %-  crip
  %-  zing
  %+  turn  wall
  |=  t=tape
  "{t}\0a"
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
              ;code:"*{(render-tang-to-marl 80 t)}"
            ==
          ~
    ==
  ==
::  +error-page: error page, with an error string if logged in
::
++  error-page
  |=  [code=@ud authorized=? url=@t t=tape]
  ^-  octs
  ::
  =/  code-as-tape=tape  (format-ud-as-integer code)
  =/  message=tape
    ?:  =(code 400)
      "Bad Request"
    ?:  =(code 403)
      "Forbidden"
    ?:  =(code 404)
      "Not Found"
    "Unknown Error"
  ::
  %-  as-octs:mimes:html
  %-  crip
  %-  en-xml:html
  ;html
    ;head
      ;title:"{code-as-tape} {message}"
    ==
    ;body
      ;h1:"{message}"
      ;p:"There was an error while handling the request for {<(trip url)>}."
      ;*  ?:  authorized
            ;=
              ;code:"{t}"
            ==
          ~
    ==
  ==
::  +channel-js: the urbit javascript interface
::
::    TODO: Must send 'acks' to the server.
::
++  channel-js
  ^-  octs
  %-  as-octs:mimes:html
  '''
  class Channel {
    constructor() {
      //  unique identifier: current time and random number
      //
      this.uid =
        new Date().getTime().toString() +
        "-" +
        Math.random().toString(16).slice(-6);

      this.requestId = 1;

      //  the currently connected EventSource
      //
      this.eventSource = null;

      //  the id of the last EventSource event we received
      //
      this.lastEventId = 0;

      //  this last event id acknowledgment sent to the server
      //
      this.lastAcknowledgedEventId = 0;

      //  a registry of requestId to successFunc/failureFunc
      //
      //    These functions are registered during a +poke and are executed
      //    in the onServerEvent()/onServerError() callbacks. Only one of
      //    the functions will be called, and the outstanding poke will be
      //    removed after calling the success or failure function.
      //
      
      this.outstandingPokes = new Map();

      //  a registry of requestId to subscription functions.
      //
      //    These functions are registered during a +subscribe and are
      //    executed in the onServerEvent()/onServerError() callbacks. The
      //    event function will be called whenever a new piece of data on this
      //    subscription is available, which may be 0, 1, or many times. The
      //    disconnect function may be called exactly once.
      //
      this.outstandingSubscriptions = new Map();

      this.deleteOnUnload();
    }

    deleteOnUnload() {
      window.addEventListener('beforeunload', (event) => {
        this.delete();
      });
    }

    //  sends a poke to an app on an urbit ship
    //
    poke(ship, app, mark, json, successFunc, failureFunc) {
      var id = this.nextId();
      this.outstandingPokes.set(
          id, {"success": successFunc, "fail": failureFunc});

      this.sendJSONToChannel({
          "id": id,
          "action": "poke",
          "ship": ship,
          "app": app,
          "mark": mark,
          "json": json
        });
    }

    //  subscribes to a path on an specific app and ship.
    //
    //    Returns a subscription id, which is the same as the same internal id
    //    passed to your Urbit.
    subscribe(ship, app, path, connectionErrFunc, eventFunc, quitFunc) {
      var id = this.nextId();
      this.outstandingSubscriptions.set(
          id, {"err": connectionErrFunc, "event": eventFunc, "quit": quitFunc});

      this.sendJSONToChannel({
          "id": id,
          "action": "subscribe",
          "ship": ship,
          "app": app,
          "path": path
        });

      return id;
    }

    //  quit the channel
    //
    delete() {
      var id = this.nextId();
      navigator.sendBeacon(this.channelURL(), JSON.stringify([{
        "id": id,
        "action": "delete"
      }]));
    }

    //  unsubscribe to a specific subscription
    //
    unsubscribe(subscriptionId) {
      var id = this.nextId();
      this.sendJSONToChannel({
          "id": id,
          "action": "unsubscribe",
          "subscription": subscriptionId
      });
    }

    //  sends a JSON command command to the server.
    //
    sendJSONToChannel(j) {
      var req = new XMLHttpRequest();
      req.open("PUT", this.channelURL());
      req.setRequestHeader("Content-Type", "application/json");

      if (this.lastEventId == this.lastAcknowledgedEventId) {
        var x = JSON.stringify([j]);
        req.send(x);
      } else {
        //  we add an acknowledgment to clear the server side queue
        //
        //    The server side puts messages it sends us in a queue until we
        //    acknowledge that we received it.
        //
        var x = JSON.stringify(
          [{"action": "ack", "event-id": parseInt(this.lastEventId)}, j])
        req.send(x);

        this.lastEventId = this.lastAcknowledgedEventId;
      }

      this.connectIfDisconnected();
    }

    //  connects to the EventSource if we are not currently connected
    //
    connectIfDisconnected() {
      if (this.eventSource) {
        return;
      }

      this.eventSource = new EventSource(this.channelURL(), {withCredentials:true});
      this.eventSource.onmessage = e => {
        this.lastEventId = e.lastEventId;

        var obj = JSON.parse(e.data);
        if (obj.response == "poke") {
          var funcs = this.outstandingPokes.get(obj.id);
          if (obj.hasOwnProperty("ok")) {
            funcs["success"]()
          } else if (obj.hasOwnProperty("err")) {
            funcs["fail"](obj.err)
          } else {
            console.log("Invalid poke response: ", obj);
          }
          this.outstandingPokes.delete(obj.id);

        } else if (obj.response == "subscribe") {
          //  on a response to a subscribe, we only notify the caller on err
          //
          var funcs = this.outstandingSubscriptions.get(obj.id);
          if (obj.hasOwnProperty("err")) {
            funcs["err"](obj.err);
            this.outstandingSubscriptions.delete(obj.id);
          }
        } else if (obj.response == "diff") {
          var funcs = this.outstandingSubscriptions.get(obj.id);
          funcs["event"](obj.json);
        } else if (obj.response == "quit") {
          var funcs = this.outstandingSubscriptions.get(obj.id);
          funcs["quit"](obj.err);
          this.outstandingSubscriptions.delete(obj.id);
        } else {
          console.log("Unrecognized response: ", e);
        }
      }

      this.eventSource.onerror = e => {
        //  TODO: The server broke the connection. Call every poke cancel and every
        //  subscription disconnect.
        console.log(e);
      }
    }

    channelURL() {
      return "/~/channel/" + this.uid;
    }

    nextId() {
      return this.requestId++;
    }
  };
  '''
::  +format-ud-as-integer: prints a number for consumption outside urbit
::
++  format-ud-as-integer
  |=  a=@ud
  ^-  tape
  ?:  =(0 a)  ['0' ~]
  %-  flop
  |-  ^-  tape
  ?:(=(0 a) ~ [(add '0' (mod a 10)) $(a (div a 10))])
::  +host-matches: %.y if the site :binding should be used to handle :host
::
++  host-matches
  |=  [binding=(unit @t) host=(unit @t)]
  ^-  ?
  ::  if the binding allows for matching anything, match
  ::
  ?~  binding
    %.y
  ::  if the host is ~, that means we're trying to bind nothing to a real
  ::  binding. fail.
  ::
  ?~  host
    %.n
  ::  otherwise, do a straight comparison
  ::
  =(u.binding u.host)
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
::  +per-server-event: per-event server core
::
++  per-server-event
  ::  gate that produces the +per-server-event core from event information
  ::
  |=  [[our=@p eny=@ =duct now=@da scry=sley] state=server-state]
  |%
  ::  +request-local: bypass authentication for local lens connections
  ::
  ++  request-local
    |=  [secure=? =address =request:http]
    ^-  [(list move) server-state]
    ::
    =/  act  [%app app=%lens]
    =/  connection=outstanding-connection
      [act [& secure address request] ~ 0]
    ::
    =.  connections.state
      (~(put by connections.state) duct connection)
    ::
    :_  state
    :_  ~
    :^  duct  %pass  /run-app/[app.act]
    ^-  note
    :^  %g  %deal  [our our]
    ::
    ^-  cush:gall
    :*  app.act
        %poke
        %handle-http-request
        !>(inbound-request.connection)
    ==
  ::  +request: starts handling an inbound http request
  ::
  ++  request
    |=  [secure=? =address =request:http]
    ^-  [(list move) server-state]
    ::
    =+  host=(get-header:http 'host' header-list.request)
    =+  action=(get-action-for-binding host url.request)
    ::
    =/  authenticated  (request-is-logged-in:authentication request)
    ::  record that we started an asynchronous response
    ::
    =/  connection=outstanding-connection
      [action [authenticated secure address request] ~ 0]
    =.  connections.state
      (~(put by connections.state) duct connection)
    ::
    ?-    -.action
    ::
        %gen
      ::
      =-  [[duct %pass /run-build %f %build live=%.n schematic=-]~ state]
      ::
      :+  %call
        :+  %call
          [%core [[our desk.generator.action] (flop path.generator.action)]]
        ::  TODO: Figure out what goes in generators. We need to slop the
        ::  prelude with the arguments passed in.
        ::
        [%$ %noun !>([[now=now eny=eny bek=[our desk.generator.action [%da now]]] ~ ~])]
      [%$ %noun !>([authenticated request])]
    ::
        %app
      :_  state
      :_  ~
      :^  duct  %pass  /run-app/[app.action]
      ^-  note
      :^  %g  %deal  [our our]
      ::  todo: i don't entirely understand gall; there's a way to make a gall
      ::  use a %handle arm instead of a sub-%poke with the
      ::  %handle-http-request type.
      ::
      ^-  cush:gall
      :*  app.action
          %poke
          %handle-http-request
          !>(inbound-request.connection)
      ==
    ::
        %authentication
      (handle-request:authentication secure address request)
    ::
        %channel
      (handle-request:by-channel secure authenticated address request)
    ::
        %four-oh-four
      %^  return-static-data-on-duct  404  'text/html'
      (error-page 404 authenticated url.request ~)
    ==
  ::  +cancel-request: handles a request being externally aborted
  ::
  ++  cancel-request
    ^-  [(list move) server-state]
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
      [duct %pass /run-build %f %kill ~]~
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
      on-cancel-request:by-channel
    ::
        %four-oh-four
      ::  it should be impossible for a 404 page to be asynchronous
      ::
      !!
    ==
  ::  +return-static-data-on-duct: returns one piece of data all at once
  ::
  ++  return-static-data-on-duct
    |=  [code=@ content-type=@t data=octs]
    ^-  [(list move) server-state]
    ::
    %-  handle-response
    :*  %start
        :-  status-code=code
        ^=  headers
          :~  ['content-type' content-type]
              ['content-length' (crip (format-ud-as-integer p.data))]
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
      |=  [secure=? =address =request:http]
      ^-  [(list move) server-state]
      ::
      ::  if we received a simple get, just return the page
      ::
      ?:  =('GET' method.request)
        ::  parse the arguments out of request uri
        ::
        =+  request-line=(parse-request-line url.request)
        %^  return-static-data-on-duct  200  'text/html'
        (login-page (get-header:http 'redirect' args.request-line) our)
      ::  if we are not a post, return an error
      ::
      ?.  =('POST' method.request)
        (return-static-data-on-duct 400 'text/html' (login-page ~ our))
      ::  we are a post, and must process the body type as form data
      ::
      ?~  body.request
        (return-static-data-on-duct 400 'text/html' (login-page ~ our))
      ::
      =/  parsed=(unit (list [key=@t value=@t]))
        (rush q.u.body.request yquy:de-purl:html)
      ?~  parsed
        (return-static-data-on-duct 400 'text/html' (login-page ~ our))
      ::
      ?~  password=(get-header:http 'password' u.parsed)
        (return-static-data-on-duct 400 'text/html' (login-page ~ our))
      ::  check that the password is correct
      ::
      ?.  =(u.password code)
        (return-static-data-on-duct 400 'text/html' (login-page ~ our))
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
      ?~  redirect=(get-header:http 'redirect' u.parsed)
        %-  handle-response
        :*  %start
            :-  status-code=200
            ^=  headers
              :~  ['set-cookie' cookie-line]
              ==
            data=~
            complete=%.y
        ==
      ::
      %-  handle-response
      :*  %start
          :-  status-code=307
          ^=  headers
            :~  ['location' u.redirect]
                ['set-cookie' cookie-line]
            ==
          data=~
          complete=%.y
      ==
    ::  +request-is-logged-in: checks to see if the request is authenticated
    ::
    ::    We are considered logged in if this request has an urbauth
    ::    Cookie which is not expired.
    ::
    ++  request-is-logged-in
      |=  =request:http
      ^-  ?
      ::  are there cookies passed with this request?
      ::
      ::    TODO: In HTTP2, the client is allowed to put multiple 'Cookie'
      ::    headers.
      ::
      ?~  cookie-header=(get-header:http 'cookie' header-list.request)
        %.n
      ::  is the cookie line is valid?
      ::
      ?~  cookies=(rush u.cookie-header cock:de-purl:html)
        %.n
      ::  is there an urbauth cookie?
      ::
      ?~  urbauth=(get-header:http 'urbauth' u.cookies)
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
    ++  code
      ^-  @ta
      ::
      =+  pax=/(scot %p our)/code/(scot %da now)/(scot %p our)
      =+  res=((sloy scry) [151 %noun] %j pax)
      ::
      (rsh 3 1 (scot %p (@ (need (need res)))))
    --
  ::  +channel: per-event handling of requests to the channel system
  ::
  ::    Eyre offers a remote interface to your Urbit through channels, which
  ::    are persistent connections on the server which can be disconnected and
  ::    reconnected on the client.
  ::
  ++  by-channel
    ::  moves: the moves to be sent out at the end of this event, reversed
    ::
    =|  moves=(list move)
    |%
    ::  +handle-request: handles an http request for the subscription system
    ::
    ++  handle-request
      |=  [secure=? authenticated=? =address =request:http]
      ^-  [(list move) server-state]
      ::  if we're not authenticated error, but don't redirect.
      ::
      ::    We don't redirect because subscription stuff is never the toplevel
      ::    page; issuing a redirect won't help.
      ::
      ?.  authenticated
        %^  return-static-data-on-duct  403  'text/html'
        (error-page 403 authenticated url.request "unauthenticated channel usage")
      ::  parse out the path key the subscription is on
      ::
      =+  request-line=(parse-request-line url.request)
      ?.  ?=([@t @t @t ~] site.request-line)
        ::  url is not of the form '/~/channel/'
        ::
        %^  return-static-data-on-duct  400  'text/html'
        (error-page 400 authenticated url.request "malformed channel url")
      ::  channel-id: unique channel id parsed out of url
      ::
      =+  channel-id=i.t.t.site.request-line
      ::
      ?:  ?&  =('channel' channel-id)
              =([~ ~.js] ext.request-line)
          ==
        ::  client is requesting the javascript shim
        ::
        (return-static-data-on-duct 200 'application/javascript' channel-js)
      ::
      ?:  =('PUT' method.request)
        ::  PUT methods starts/modifies a channel, and returns a result immediately
        ::
        (on-put-request channel-id request)
      ::
      ?:  =('GET' method.request)
        (on-get-request channel-id request)
      ?:  =('POST' method.request)
        ::  POST methods are used solely for deleting channels
        (on-put-request channel-id request)
      ::
      ~&  %session-not-a-put
      [~ state]
    ::  +on-cancel-request: cancels an ongoing subscription
    ::
    ::    One of our long lived sessions just got closed. We put the associated
    ::    session back into the waiting state.
    ::
    ++  on-cancel-request
      ^-  [(list move) server-state]
      ::  lookup the session id by duct
      ::
      ?~  maybe-channel-id=(~(get by duct-to-key.channel-state.state) duct)
        ~&  [%canceling-nonexistant-channel duct]
        [~ state]
      ::
      ~&  [%canceling-cancel duct]
      ::
      =/  expiration-time=@da  (add now channel-timeout)
      ::
      :-  [(set-timeout-move u.maybe-channel-id expiration-time) moves]
      %_    state
          session.channel-state
        %+  ~(jab by session.channel-state.state)  u.maybe-channel-id
        |=  =channel
        ::  if we are canceling a known channel, it should have a listener
        ::
        ?>  ?=([%| *] state.channel)
        channel(state [%& [expiration-time duct]])
      ::
          duct-to-key.channel-state
        (~(del by duct-to-key.channel-state.state) duct)
      ==
    ::  +set-timeout-timer-for: sets a timeout timer on a channel
    ::
    ::    This creates a channel if it doesn't exist, cancels existing timers
    ::    if they're already set (we cannot have duplicate timers), and (if
    ::    necessary) moves channels from the listening state to the expiration
    ::    state.
    ::
    ++  update-timeout-timer-for
      |=  channel-id=@t
      ^+  ..update-timeout-timer-for
      ::  when our callback should fire
      ::
      =/  expiration-time=@da  (add now channel-timeout)
      ::  if the channel doesn't exist, create it and set a timer
      ::
      ?~  maybe-channel=(~(get by session.channel-state.state) channel-id)
        ::
        %_    ..update-timeout-timer-for
            session.channel-state.state
          %+  ~(put by session.channel-state.state)  channel-id
          [[%& expiration-time duct] 0 ~ ~]
        ::
            moves
          [(set-timeout-move channel-id expiration-time) moves]
        ==
      ::  if the channel has an active listener, we aren't setting any timers
      ::
      ?:  ?=([%| *] state.u.maybe-channel)
        ..update-timeout-timer-for
      ::  we have a previous timer; cancel the old one and set the new one
      ::
      %_    ..update-timeout-timer-for
          session.channel-state.state
        %+  ~(jab by session.channel-state.state)  channel-id
        |=  =channel
        channel(state [%& [expiration-time duct]])
      ::
          moves
        :*  (cancel-timeout-move channel-id p.state.u.maybe-channel)
            (set-timeout-move channel-id expiration-time)
            moves
        ==
      ==
    ::
    ++  set-timeout-move
      |=  [channel-id=@t expiration-time=@da]
      ^-  move
      [duct %pass /channel/timeout/[channel-id] %b %wait expiration-time]
    ::
    ++  cancel-timeout-move
      |=  [channel-id=@t expiration-time=@da =^duct]
      ^-  move
      :^  duct  %pass  /channel/timeout/[channel-id]
      [%b %rest expiration-time]
    ::  +on-get-request: handles a GET request
    ::
    ::    GET requests open a channel for the server to send events to the
    ::    client in text/event-stream format.
    ::
    ++  on-get-request
      |=  [channel-id=@t =request:http]
      ^-  [(list move) server-state]
      ::  if there's no channel-id, we must 404
      ::
      ?~  maybe-channel=(~(get by session.channel-state.state) channel-id)
        %^  return-static-data-on-duct  404  'text/html'
        (error-page 404 %.y url.request ~)
      ::  if there's already a duct listening to this channel, we must 400
      ::
      ?:  ?=([%| *] state.u.maybe-channel)
        %^  return-static-data-on-duct  400  'text/html'
        (error-page 400 %.y url.request "channel already bound")
      ::  when opening an event-stream, we must cancel our timeout timer
      ::
      =.  moves
        [(cancel-timeout-move channel-id p.state.u.maybe-channel) moves]
      ::  the request may include a 'Last-Event-Id' header
      ::
      =/  maybe-last-event-id=(unit @ud)
        ?~  maybe-raw-header=(get-header:http 'Last-Event-ID' header-list.request)
          ~
        (rush u.maybe-raw-header dum:ag)
      ::  flush events older than the passed in 'Last-Event-ID'
      ::
      =?  state  ?=(^ maybe-last-event-id)
        (acknowledge-events channel-id u.maybe-last-event-id)
      ::  combine the remaining queued events to send to the client
      ::
      =/  event-replay=wall
        %-  zing
        %-  flop
        =/  queue  events.u.maybe-channel
        =|  events=(list wall)
        |-
        ^+  events
        ?:  =(~ queue)
          events
        =^  head  queue  ~(get to queue)
        $(events [lines.p.head events])
      ::  send the start event to the client
      ::
      =^  http-moves  state
        %-  handle-response
        :*  %start
            :-  200
            :~  ['content-type' 'text/event-stream']
                ['cache-control' 'no-cache']
                ['connection' 'keep-alive']
            ==
            (wall-to-octs event-replay)
            complete=%.n
        ==
      ::  associate this duct with this session key
      ::
      =.  duct-to-key.channel-state.state
        (~(put by duct-to-key.channel-state.state) duct channel-id)
      ::  clear the event queue and record the duct for future output
      ::
      =.  session.channel-state.state
        %+  ~(jab by session.channel-state.state)  channel-id
        |=  =channel
        channel(events ~, state [%| duct])
      ::
      [(weld http-moves moves) state]
    ::  +acknowledge-events: removes events before :last-event-id on :channel-id
    ::
    ++  acknowledge-events
      |=  [channel-id=@t last-event-id=@u]
      ^-  server-state
      %_    state
          session.channel-state
        %+  ~(jab by session.channel-state.state)  channel-id
        |=  =channel
        ^+  channel
        channel(events (prune-events events.channel last-event-id))
      ==
    ::  +on-put-request: handles a PUT request
    ::
    ::    PUT requests send commands from the client to the server. We receive
    ::    a set of commands in JSON format in the body of the message.
    ::
    ++  on-put-request
      |=  [channel-id=@t =request:http]
      ^-  [(list move) server-state]
      ::  error when there's no body
      ::
      ?~  body.request
        %^  return-static-data-on-duct  400  'text/html'
        (error-page 400 %.y url.request "no put body")
      ::  if the incoming body isn't json, this is a bad request, 400.
      ::
      ?~  maybe-json=(de-json:html q.u.body.request)
        %^  return-static-data-on-duct  400  'text/html'
        (error-page 400 %.y url.request "put body not json")
      ::  parse the json into an array of +channel-request items
      ::
      ?~  maybe-requests=(parse-channel-request u.maybe-json)
        %^  return-static-data-on-duct  400  'text/html'
        (error-page 400 %.y url.request "invalid channel json")
      ::  while weird, the request list could be empty
      ::
      ?:  =(~ u.maybe-requests)
        %^  return-static-data-on-duct  400  'text/html'
        (error-page 400 %.y url.request "empty list of actions")
      ::  check for the existence of the channel-id
      ::
      ::    if we have no session, create a new one set to expire in
      ::    :channel-timeout from now. if we have one which has a timer, update
      ::    that timer.
      ::
      =.  ..on-put-request  (update-timeout-timer-for channel-id)
      ::  for each request, execute the action passed in
      ::
      =+  requests=u.maybe-requests
      ::  gall-moves: put moves here first so we can flop for ordering
      ::
      ::    TODO: Have an error state where any invalid duplicate subscriptions
      ::    or other errors cause the entire thing to fail with a 400 and a tang.
      ::
      =|  gall-moves=(list move)
      |-
      ::
      ?~  requests
        ::  this is a PUT request; we must mark it as complete
        ::
        =^  http-moves  state
          %-  handle-response
          :*  %start
              [status-code=200 headers=~]
              data=~
              complete=%.y
          ==
        ::
        [:(weld (flop gall-moves) http-moves moves) state]
      ::
      ?-    -.i.requests
          %ack
        ::  client acknowledges that they have received up to event-id
        ::
        %_  $
          state     (acknowledge-events channel-id event-id.i.requests)
          requests  t.requests
        ==
      ::
          %poke
        ::
        =.  gall-moves
          :_  gall-moves
          ^-  move
          :^  duct  %pass  /channel/poke/[channel-id]/(scot %ud request-id.i.requests)
          =,  i.requests
          [%g %deal `sock`[our ship] `cush:gall`[app %punk mark %json !>(json)]]
        ::
        $(requests t.requests)
      ::
          %subscribe
        ::
        =/  channel-wire=path
          /channel/subscription/[channel-id]/(scot %ud request-id.i.requests)
        ::
        =.  gall-moves
          :_  gall-moves
          ^-  move
          :^  duct  %pass  channel-wire
          =,  i.requests
          [%g %deal [our ship] `cush:gall`[app %peel %json path]]
        ::
        =.  session.channel-state.state
          %+  ~(jab by session.channel-state.state)  channel-id
          |=  =channel
          =,  i.requests
          channel(subscriptions (~(put by subscriptions.channel) channel-wire [ship app path duct]))
        ::
        $(requests t.requests)
      ::
          %unsubscribe
        =/  channel-wire=path
          /channel/subscription/[channel-id]/(scot %ud subscription-id.i.requests)
        ::
        =/  subscriptions
          subscriptions:(~(got by session.channel-state.state) channel-id)
        ::
        ?~  maybe-subscription=(~(get by subscriptions) channel-wire)
          ::  the client sent us a weird request referring to a subscription
          ::  which isn't active.
          ::
          ~&  [%missing-subscription-in-unsubscribe channel-wire]
          $(requests t.requests)
        ::
        =.  gall-moves
          :_  gall-moves
          ^-  move
          :^  duc.u.maybe-subscription  %pass  channel-wire
          =,  u.maybe-subscription
          [%g %deal [our ship] `cush:gall`[app %pull ~]]
        ::
        =.  session.channel-state.state
          %+  ~(jab by session.channel-state.state)  channel-id
          |=  =channel
          channel(subscriptions (~(del by subscriptions.channel) channel-wire))
        ::
        $(requests t.requests)
      ::
          %delete
        =/  unitsession
          (~(get by session.channel-state.state) channel-id)
        ::
        ?~  unitsession
          $(requests t.requests)
        ::
        =/  session  u.unitsession
        =.  session.channel-state.state
          (~(del by session.channel-state.state) channel-id)
        ::
        =.  gall-moves
          %+  weld  gall-moves
            ::
            ::  produce a list of moves which cancels every gall subscription
            ::
            %+  turn  ~(tap by subscriptions.session)
            |=  [channel-wire=path ship=@p app=term =path duc=^duct]
            ^-  move
            ::
            [duc %pass channel-wire [%g %deal [our ship] app %pull ~]]
        ::
        ?:  ?=([%& *] state.session)
          =.  gall-moves
            :_  gall-moves
            ::
            ^-  move
            ?>  ?=([%& *] state.session)
            :^  duct.p.state.session  %pass  /channel/timeout/[channel-id]
            [%b %rest date.p.state.session]
          ::
          $(requests t.requests)
          ::
        ?>  ?=([%| *] state.session)
        =.  duct-to-key.channel-state.state
          (~(del by duct-to-key.channel-state.state) p.state.session)
        ::
        $(requests t.requests)
      ::
      ==
    ::  +on-gall-response: turns a gall response into an event
    ::
    ++  on-gall-response
      |=  [channel-id=@t request-id=@ud =cuft:gall]
      ^-  [(list move) server-state]
      ::
      ?+    -.cuft  ~|([%invalid-gall-response -.cuft] !!)
          %coup
        =/  =json
          =,  enjs:format
          %-  pairs  :~
            ['response' [%s 'poke']]
            ['id' (numb request-id)]
            ?~  p.cuft
              ['ok' [%s 'ok']]
            ['err' (wall (render-tang-to-wall 100 u.p.cuft))]
          ==
        ::
        (emit-event channel-id [(en-json:html json)]~)
      ::
          %diff
        =/  =json
          =,  enjs:format
          %-  pairs  :~
            ['response' [%s 'diff']]
            ['id' (numb request-id)]
            :-  'json'
            ?>  =(%json p.p.cuft)
            ;;(json q.q.p.cuft)
          ==
        ::
        (emit-event channel-id [(en-json:html json)]~)
      ::
          %quit
        ~&  [%recieved-quit-from-gall channel-id]
        =/  =json
          =,  enjs:format
          %-  pairs  :~
            ['response' [%s 'quit']]
            ['id' (numb request-id)]
          ==
        ::
        (emit-event channel-id [(en-json:html json)]~)
      ::
          %reap
        =/  =json
          =,  enjs:format
          %-  pairs  :~
            ['response' [%s 'subscribe']]
            ['id' (numb request-id)]
            ?~  p.cuft
              ['ok' [%s 'ok']]
            ['err' (wall (render-tang-to-wall 100 u.p.cuft))]
          ==
        ::
        (emit-event channel-id [(en-json:html json)]~)
      ==
    ::  +emit-event: records an event occurred, possibly sending to client
    ::
    ::    When an event occurs, we need to record it, even if we immediately
    ::    send it to a connected browser so in case of disconnection, we can
    ::    resend it.
    ::
    ::    This function is responsible for taking the raw json lines and
    ::    converting them into a text/event-stream. The :event-stream-lines
    ::    then may get sent, and are stored for later resending until
    ::    acknowledged by the client.
    ::
    ++  emit-event
      |=  [channel-id=@t json-text=wall]
      ^-  [(list move) server-state]
      ::
      =/  channel=(unit channel)
        (~(get by session.channel-state.state) channel-id)
      ?~  channel
        ~&  [%received-event-for-nonexistent-channel channel-id]
        [~ state]
      ::
      =/  event-id  next-id.u.channel
      ::
      =/  event-stream-lines=wall
        %-  weld  :_  [""]~
        :-  (weld "id: " (format-ud-as-integer event-id))
        %+  turn  json-text
        |=  =tape
        (weld "data: " tape)
      ::  if a client is connected, send this event to them.
      ::
      =?  moves  ?=([%| *] state.u.channel)
        ^-  (list move)
        :_  moves
        :+  p.state.u.channel  %give
        ^-  gift:able:http-server
        :*  %response  %continue
        ::
            ^=  data
            :-  ~
            %-  as-octs:mimes:html
            (crip (of-wall:format event-stream-lines))
        ::
            complete=%.n
        ==
      ::
      :-  moves
      %_    state
          session.channel-state
        %+  ~(jab by session.channel-state.state)  channel-id
        |=  =^channel
        ^+  channel
        ::
        %_  channel
          next-id  +(next-id.channel)
          events  (~(put to events.channel) [event-id event-stream-lines])
        ==
      ==
    ::  +on-channel-timeout: we received a wake to clear an old session
    ::
    ++  on-channel-timeout
      |=  channel-id=@t
      ^-  [(list move) server-state]
      ::
      =/  session
        (~(got by session.channel-state.state) channel-id)
      ::
      :_  %_    state
              session.channel-state
            (~(del by session.channel-state.state) channel-id)
          ==
      ::  produce a list of moves which cancels every gall subscription
      ::
      %+  turn  ~(tap by subscriptions.session)
      |=  [channel-wire=path ship=@p app=term =path duc=^duct]
      ^-  move
      ::
      [duc %pass channel-wire [%g %deal [our ship] app %pull ~]]
    --
  ::  +handle-ford-response: translates a ford response for the outside world
  ::
  ++  handle-ford-response
    |=  made-result=made-result:ford
    ^-  [(list move) server-state]
    ::
    =+  connection=(~(got by connections.state) duct)
    ::
    ?:  ?=(%incomplete -.made-result)
      %^  return-static-data-on-duct  500  'text/html'
      ::
      %-  internal-server-error  :*
          authenticated.inbound-request.connection
          url.request.inbound-request.connection
          tang.made-result
      ==
    ::
    ?:  ?=(%error -.build-result.made-result)
      %^  return-static-data-on-duct  500  'text/html'
      ::
      %-  internal-server-error  :*
          authenticated.inbound-request.connection
          url.request.inbound-request.connection
          message.build-result.made-result
      ==
    ::
    =/  =cage  (result-to-cage:ford build-result.made-result)
    ::
    =/  result=simple-payload:http  ;;(simple-payload:http q.q.cage)
    ::  ensure we have a valid content-length header
    ::
    ::    We pass on the response and the headers the generator produces, but
    ::    ensure that we have a single content-length header set correctly in
    ::    the returned if this has a body, and has no content-length if there
    ::    is no body returned to the client.
    ::
    =.  headers.response-header.result
      ?~  data.result
        (delete-header:http 'content-length' headers.response-header.result)
      ::
      %^  set-header:http  'content-length'
        (crip (format-ud-as-integer p.u.data.result))
      headers.response-header.result
    ::
    %-  handle-response
    ::
    ^-  http-event:http
    :*  %start
        response-header.result
        data.result
        complete=%.y
    ==
  ::  +handle-gall-error: a call to +poke-http-response resulted in a %coup
  ::
  ++  handle-gall-error
    |=  =tang
    ^-  [(list move) server-state]
    ::
    =+  connection=(~(got by connections.state) duct)
    ::
    %^  return-static-data-on-duct  500  'text/html'
    ::
    %-  internal-server-error  :*
        authenticated.inbound-request.connection
        url.request.inbound-request.connection
        tang
    ==
  ::  +handle-response: check a response for correctness and send to earth
  ::
  ::    All outbound responses including %http-server generated responses need to go
  ::    through this interface because we want to have one centralized place
  ::    where we perform logging and state cleanup for connections that we're
  ::    done with.
  ::
  ++  handle-response
    |=  =http-event:http
    ^-  [(list move) server-state]
    ::  verify that this is a valid response on the duct
    ::
    ?~  connection-state=(~(get by connections.state) duct)
      ~&  [%invalid-outstanding-connection duct]
      [~ state]
    ::
    |^  ^-  [(list move) server-state]
        ::
        ?-    -.http-event
        ::
            %start
          ?^  response-header.u.connection-state
            ~&  [%http-multiple-start duct]
            error-connection
          ::
          =.  connections.state
            %+  ~(jab by connections.state)  duct
            |=  connection=outstanding-connection
            %_  connection
              response-header  `response-header.http-event
              bytes-sent  ?~(data.http-event 0 p.u.data.http-event)
            ==
          ::
          =?  state  complete.http-event
            log-complete-request
          ::
          pass-response
        ::
            %continue
          ?~  response-header.u.connection-state
            ~&  [%http-continue-without-start duct]
            error-connection
          ::
          =.  connections.state
            %+  ~(jab by connections.state)  duct
            |=  connection=outstanding-connection
            =+  size=?~(data.http-event 0 p.u.data.http-event)
            connection(bytes-sent (add bytes-sent.connection size))
          ::
          =?  state  complete.http-event
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
      [[duct %give %response http-event]~ state]
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
      [[duct %give %response %cancel ~]~ state]
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
    ^-  action
    ::  process :raw-host
    ::
    ::    If we are missing a 'Host:' header, if that header is a raw IP
    ::    address, or if the 'Host:' header refers to [our].urbit.org, we want
    ::    to return ~ which means we're unidentified and will match against any
    ::    wildcard matching.
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
      [%four-oh-four ~]
    ::
    ?:  ?&  (host-matches site.binding.i.bindings raw-host)
            (path-matches path.binding.i.bindings parsed-url)
        ==
      action.i.bindings
    ::
    $(bindings t.bindings)
  --
::
::
++  parse-request-line
  |=  url=@t
  ^-  [[ext=(unit @ta) site=(list @t)] args=(list [key=@t value=@t])]
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
|=  [our=ship now=@da eny=@uvJ scry-gate=sley]
::  allow jets to be registered within this core
::
~%  %http-server  ..is  ~
|%
++  call
  |=  [=duct type=* wrapped-task=(hobo task:able)]
  ^-  [(list move) _http-server-gate]
  ::
  =/  task=task:able
    ?.  ?=(%soft -.wrapped-task)
      wrapped-task
    ~|  [%p-wrapped-task p.wrapped-task]
    ;;(task:able p.wrapped-task)
  ::  %crud: notifies us of an event failure
  ::
  ?:  ?=(%crud -.task)
    =/  moves=(list move)
      [[duct %slip %d %flog task] ~]
    [moves http-server-gate]
  ::  %init: tells us what our ship name is
  ::
  ?:  ?=(%init -.task)
    ::  initial value for the login handler
    ::
    =.  bindings.server-state.ax
      :~  [[~ /~/login] duct [%authentication ~]]
          [[~ /~/channel] duct [%channel ~]]
      ==
    [~ http-server-gate]
  ::  %vega: notifies us of a completed kernel upgrade
  ::
  ?:  ?=(%vega -.task)
    [~ http-server-gate]
  ::  %born: new unix process
  ::
  ?:  ?=(%born -.task)
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
        [[our eny duct.i.connections now scry-gate] server-state.ax]
      =/  cancel-request  cancel-request:(per-server-event event-args)
      =^  moves  server-state.ax  cancel-request
      ::
      $(closed-connections (weld moves closed-connections), connections t.connections)
    ::  save duct for future %give to unix
    ::
    =.  outgoing-duct.server-state.ax  duct
    ::
    :_  http-server-gate
    ;:  weld
      ::  hand back default configuration for now
      ::
      [duct %give %set-config http-config.server-state.ax]~
    ::
      closed-connections
    ==
  ::  all other commands operate on a per-server-event
  ::
  =/  event-args  [[our eny duct now scry-gate] server-state.ax]
  =/  server  (per-server-event event-args)
  ::
  ?-    -.task
      ::  %live: notifies us of the ports of our live http servers
      ::
      %live
    =.  ports.server-state.ax  +.task
    [~ http-server-gate]
      ::  %rule: updates our http configuration
      ::
      %rule
    ?-  -.http-rule.task
        ::  %cert: install tls certificate
        ::
        %cert
      =*  config  http-config.server-state.ax
      ?:  =(secure.config cert.http-rule.task)
        [~ http-server-gate]
      =.  secure.config  cert.http-rule.task
      :_  http-server-gate
      =*  out-duct  outgoing-duct.server-state.ax
      ?~  out-duct  ~
      [out-duct %give %set-config config]~
        ::  %turf: add or remove domain name
        ::
        %turf
      =*  domains  domains.server-state.ax
      =/  mod/(set turf)
        ?:  ?=(%put action.http-rule.task)
          (~(put in domains) turf.http-rule.task)
        (~(del in domains) turf.http-rule.task)
      ?:  =(domains mod)
        [~ http-server-gate]
      =.  domains  mod
      :_  http-server-gate
      =/  cmd
        [%acme %poke `cage`[%acme-order !>(mod)]]
      [duct %pass /acme/order %g %deal [our our] cmd]~
    ==
  ::
      %request
    =^  moves  server-state.ax  (request:server +.task)
    [moves http-server-gate]
  ::
      %request-local
    =^  moves  server-state.ax  (request-local:server +.task)
    [moves http-server-gate]
  ::
      %cancel-request
    =^  moves  server-state.ax  cancel-request:server
    [moves http-server-gate]
  ::
      %connect
    =^  moves  server-state.ax
      %+  add-binding:server  binding.task
      [%app app.task]
    [moves http-server-gate]
  ::
      %serve
    =^  moves  server-state.ax
      %+  add-binding:server  binding.task
      [%gen generator.task]
    [moves http-server-gate]
  ::
      %disconnect
    =.  server-state.ax  (remove-binding:server binding.task)
    [~ http-server-gate]
  ::
      %wegh
    :_  http-server-gate
    :~  :*  duct
            %give
            %mass
            ^-  mass
            :+  %rver  %|
            :~  bindings+&+bindings.server-state.ax
                auth+&+authentication-state.server-state.ax
                connections+&+connections.server-state.ax
                channels+&+channel-state.server-state.ax
                axle+&+ax
    ==  ==  ==
  ==
::
++  take
  |=  [=wire =duct wrapped-sign=(hypo sign)]
  ^-  [(list move) _http-server-gate]
  ::  unwrap :sign, ignoring unneeded +type in :p.wrapped-sign
  ::
  =/  =sign  q.wrapped-sign
  ::  :wire must at least contain two parts, the type and the build
  ::
  ?>  ?=([@ *] wire)
  ::
  |^  ^-  [(list move) _http-server-gate]
      ::
      ?+     i.wire
           ~|([%bad-take-wire wire] !!)
      ::
         %run-app    run-app
         %run-build  run-build
         %channel    channel
         %acme       acme-ack
      ==
  ::
  ++  run-app
    ::
    ?>  ?=([%g %unto *] sign)
    ::
    ::
    ?:  ?=([%coup *] p.sign)
      ?~  p.p.sign
        ::  received a positive acknowledgment: take no action
        ::
        [~ http-server-gate]
      ::  we have an error; propagate it to the client
      ::
      =/  event-args  [[our eny duct now scry-gate] server-state.ax]
      =/  handle-gall-error
        handle-gall-error:(per-server-event event-args)
      =^  moves  server-state.ax  (handle-gall-error u.p.p.sign)
      [moves http-server-gate]
    ::
    ?>  ?=([%g %unto %http-response *] sign)
    ::
    =/  event-args  [[our eny duct now scry-gate] server-state.ax]
    =/  handle-response  handle-response:(per-server-event event-args)
    =^  moves  server-state.ax  (handle-response http-event.p.sign)
    [moves http-server-gate]
  ::
  ++  run-build
    ::
    ?>  ?=([%f %made *] sign)
    ::
    =/  event-args  [[our eny duct now scry-gate] server-state.ax]
    =/  handle-ford-response  handle-ford-response:(per-server-event event-args)
    =^  moves  server-state.ax  (handle-ford-response result.sign)
    [moves http-server-gate]
  ::
  ++  channel
    ::
    =/  event-args  [[our eny duct now scry-gate] server-state.ax]
    ::  channel callback wires are triples.
    ::
    ?>  ?=([@ @ @t *] wire)
    ::
    ?+    i.t.wire
        ~|([%bad-channel-wire wire] !!)
    ::
        %timeout
      ?>  ?=([%b %wake *] sign)
      ?^  error.sign
        [[duct %slip %d %flog %crud %wake u.error.sign]~ http-server-gate]
      =/  on-channel-timeout
        on-channel-timeout:by-channel:(per-server-event event-args)
      =^  moves  server-state.ax
        (on-channel-timeout i.t.t.wire)
      [moves http-server-gate]
    ::
        ?(%poke %subscription)
      ?>  ?=([%g %unto *] sign)
      ?>  ?=([@ @ @t @ *] wire)
      =/  on-gall-response
        on-gall-response:by-channel:(per-server-event event-args)
      ::  ~&  [%gall-response sign]
      =^  moves  server-state.ax
        (on-gall-response i.t.t.wire `@ud`(slav %ud i.t.t.t.wire) p.sign)
      [moves http-server-gate]
    ==
  ::
  ++  acme-ack
    ?>  ?=([%g %unto *] sign)
    ::
    ?>  ?=([%coup *] p.sign)
    ?~  p.p.sign
      ::  received a positive acknowledgment: take no action
      ::
      [~ http-server-gate]
    ::  received a negative acknowledgment: XX do something
    ::
    [((slog u.p.p.sign) ~) http-server-gate]
  --
::
++  http-server-gate  ..$
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
  |=  [fur=(unit (set monk)) ren=@tas why=shop syd=desk lot=coin tyl=path]
  ^-  (unit (unit cage))
  ?.  ?=(%& -.why)
    ~
  =*  who  p.why
  ?.  ?=(%$ ren)
    [~ ~]
  ?.  ?=(%$ -.lot)
    [~ ~]
  ?.  ?=(%host syd)
    [~ ~]
  %-  (lift (lift |=(a=hart:eyre [%hart !>(a)])))
  ^-  (unit (unit hart:eyre))
  ?.  =(our who)
    ?.  =([%da now] p.lot)
      [~ ~]
    ~&  [%r %scry-foreign-host who]
    ~
  =.  p.lot  ?.(=([%da now] p.lot) p.lot [%tas %real])
  ?+  p.lot
    [~ ~]
  ::
      [%tas %fake]
    ``[& [~ 8.443] %& /localhost]
  ::
      [%tas %real]
    =*  domains  domains.server-state.ax
    =*  ports  ports.server-state.ax
    =/  =host:eyre  [%& ?^(domains n.domains /localhost)]
    =/  secure=?  &(?=(^ secure.ports) !?=(hoke:eyre host))
    =/  port=(unit @ud)
      ?.  secure
        ?:(=(80 insecure.ports) ~ `insecure.ports)
      ?>  ?=(^ secure.ports)
      ?:(=(443 u.secure.ports) ~ secure.ports)
    ``[secure port host]
  ==
--
