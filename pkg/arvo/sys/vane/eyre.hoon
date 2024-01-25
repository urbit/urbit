!:
::  lighter than eyre
::
|=  our=ship
=,  eyre
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
      card=(wind note gift)
  ==
::  +note: private request from eyre to another vane
::
+$  note
  $%  [%a $>(?(%plea %keen %yawn) task:ames)]
      [%b $>(?(%rest %wait) task:behn)]
      [%c $>(%warp task:clay)]
      [%d $>(%flog task:dill)]
      [%g $>(%deal task:gall)]
  ==
::  +sign: private response from another vane to eyre
::
+$  sign
  $%  [%ames $>(?(%done %boon %lost %tune) gift:ames)]
      [%behn $>(%wake gift:behn)]
      [%gall gift:gall]
      [%clay gift:clay]
  ==
--
::  more structures
::
|%
++  axle
  $:  ::  date: date at which http-server's state was updated to this data structure
      ::
      date=%~2023.5.15
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
      ::  cache: mapping from url to versioned entry
      ::
      cache=(map url=@t [aeon=@ud val=(unit cache-entry)])
      ::  cors-registry: state used and managed by the +cors core
      ::
      =cors-registry
      ::  connections: open http connections not fully complete
      ::
      connections=(map duct outstanding-connection)
      ::  auth: state managed by the +authentication core
      ::
      auth=authentication-state
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
      ::  verb: verbosity
      ::
      verb=@
  ==
::  channel-request: an action requested on a channel
::
+$  channel-request
  $%  ::  %ack: acknowledges that the client has received events up to :id
      ::
      [%ack event-id=@ud]
      ::  %poke: pokes an application, validating :noun against :mark
      ::
      [%poke request-id=@ud ship=@p app=term mark=@tas =noun]
      ::  %poke-json: pokes an application, translating :json to :mark
      ::
      [%poke-json request-id=@ud ship=@p app=term mark=@tas =json]
      ::  %watch: subscribes to an application path
      ::
      [%subscribe request-id=@ud ship=@p app=term =path]
      ::  %leave: unsubscribes from an application path
      ::
      [%unsubscribe request-id=@ud subscription-id=@ud]
      ::  %delete: kills a channel
      ::
      [%delete ~]
  ==
::  clog-timeout: the delay between acks after which clog-threshold kicks in
::
++  clog-timeout     ~s30
::  clog-threshold: maximum per-subscription event buildup, after clog-timeout
::
++  clog-threshold   50
::  channel-timeout: the delay before a channel should be reaped
::
++  channel-timeout  ~h12
::  session-timeout: the delay before an idle session expires
::
++  session-timeout  ~d7
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
::    also produces, per request-id, the amount of events that have got acked,
::    for use with +subtract-acked-events.
::
++  prune-events
  =|  acked=(map @ud @ud)
  |=  [q=(qeu [id=@ud @ud channel-event]) id=@ud]
  ^+  [acked q]
  ::  if the queue is now empty, that's fine
  ::
  ?:  =(~ q)
    [acked ~]
  ::
  =/  next=[item=[id=@ud request-id=@ud channel-event] _q]  ~(get to q)
  ::  if the head of the queue is newer than the acknowledged id, we're done
  ::
  ?:  (gth id.item.next id)
    [acked q]
  ::  otherwise, note the ack, and check next item
  ::
  %_  $
    q  +:next
  ::
      acked
    =,  item.next
    %+  ~(put by acked)  request-id
    +((~(gut by acked) request-id 0))
  ==
::  +subtract-acked-events: update the subscription map's pending ack counts
::
++  subtract-acked-events
  |=  [acked=(map @ud @ud) unacked=(map @ud @ud)]
  ^+  unacked
  %+  roll  ~(tap by acked)
  |=  [[rid=@ud ack=@ud] unacked=_unacked]
  ?~  sus=(~(get by unacked) rid)
    unacked
  %+  ~(put by unacked)  rid
  ?:  (lte u.sus ack)  0
  (sub u.sus ack)
::  +find-channel-mode: deduce requested mode from headers
::
++  find-channel-mode
  |=  [met=method:http hes=header-list:http]
  ^-  ?(%json %jam)
  =+  ^-  [hed=@t jam=@t]
    ?:  ?=(%'GET' met)  ['x-channel-format' 'application/x-urb-jam']
    ['content-type' 'application/x-urb-jam']
  =+  typ=(bind (get-header:http hed hes) :(cork trip cass crip))
  ?:(=(`jam typ) %jam %json)
::  +parse-channel-request: parses a list of channel-requests
::
++  parse-channel-request
  |=  [mode=?(%json %jam) body=octs]
  ^-  (each (list channel-request) @t)
  ?-  mode
      %json
    ?~  maybe-json=(de:json:html q.body)
      |+'put body not json'
    ?~  maybe-requests=(parse-channel-request-json u.maybe-json)
      |+'invalid channel json'
    &+u.maybe-requests
  ::
      %jam
    ?~  maybe-noun=(bind (slaw %uw q.body) cue)
      |+'invalid request format'
    ?~  maybe-reqs=((soft (list channel-request)) u.maybe-noun)
      ~&  [%miss u.maybe-noun]
      |+'invalid request data'
    &+u.maybe-reqs
  ==
::  +parse-channel-request-json: parses a json list of channel-requests
::
::    Parses a json array into a list of +channel-request. If any of the items
::    in the list fail to parse, the entire thing fails so we can 400 properly
::    to the client.
::
++  parse-channel-request-json
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
    %.  item
    %+  pe  %poke-json
    (ot id+ni ship+(su fed:ag) app+so mark+(su sym) json+some ~)
  ?:  =('subscribe' u.maybe-key)
    %.  item
    %+  pe  %subscribe
    (ot id+ni ship+(su fed:ag) app+so path+(su stap) ~)
  ?:  =('unsubscribe' u.maybe-key)
    %.  item
    %+  pe  %unsubscribe
    (ot id+ni subscription+ni ~)
  ?:  =('delete' u.maybe-key)
    `[%delete ~]
  ::  if we reached this, we have an invalid action key. fail parsing.
  ::
  ~
::  +auth-styling: css for login and eauth pages
::
++  auth-styling
  '''
  @import url("https://rsms.me/inter/inter.css");
  @font-face {
      font-family: "Source Code Pro";
      src: url("https://storage.googleapis.com/media.urbit.org/fonts/scp-regular.woff");
      font-weight: 400;
      font-display: swap;
  }
  :root {
    --red-soft: #FFEFEC;
    --red: #FF6240;
    --gray-100: #E5E5E5;
    --gray-400: #999999;
    --gray-800: #333333;
    --white: #FFFFFF;
  }
  html {
    font-family: Inter, sans-serif;
    height: 100%;
    margin: 0;
    width: 100%;
    background: var(--white);
    color: var(--gray-800);
    -webkit-font-smoothing: antialiased;
    line-height: 1.5;
    font-size: 16px;
    font-weight: 600;
    display: flex;
    flex-flow: row nowrap;
    justify-content: center;
  }
  body {
    display: flex;
    flex-flow: column nowrap;
    justify-content: center;
    max-width: 300px;
    padding: 1rem;
    width: 100%;
  }
  body.local #eauth,
  body.eauth #local {
    display: none;
    min-height: 100%;
  }
  #eauth input {
    /*NOTE dumb hack to get approx equal height with #local */
    margin-bottom: 15px;
  }
  body nav {
    background: var(--gray-100);
    border-radius: 2rem;
    display: flex;
    justify-content: space-around;
    overflow: hidden;
    margin-bottom: 1rem;
  }
  body nav div {
    width: 50%;
    padding: 0.5rem 1rem;
    text-align: center;
    cursor: pointer;
  }
  body.local nav div.local,
  body.eauth nav div.eauth {
    background: var(--gray-800);
    color: var(--white);
    cursor: default;
  }
  nav div.local {
    border-right: none;
    border-top-right-radius: 0;
    border-bottom-right-radius: 0;
  }
  nav div.eauth {
    border-left: none;
    border-top-left-radius: 0;
    border-bottom-left-radius: 0;
  }
  body > *,
  form > input {
    width: 100%;
  }
  form {
    display: flex;
    flex-flow: column;
    align-items: flex-start;
  }
  input {
    background: var(--gray-100);
    border: 2px solid transparent;
    padding: 0.5rem;
    border-radius: 0.5rem;
    font-size: inherit;
    color: var(--gray-800);
    box-shadow: none;
    width: 100%;
  }
  input:disabled {
    background: var(--gray-100);
    color: var(--gray-400);
  }
  input:focus {
    outline: none;
    background: var(--white);
    border-color: var(--gray-400);
  }
  input:invalid:not(:focus) {
    background: var(--red-soft);
    border-color: var(--red);
    outline: none;
    color: var(--red);
  }
  button[type=submit] {
    margin-top: 1rem;
  }
  button[type=submit], a.button {
    font-size: 1rem;
    padding: 0.5rem 1rem;
    border-radius: 0.5rem;
    background: var(--gray-800);
    color: var(--white);
    border: none;
    font-weight: 600;
    text-decoration: none;
  }
  input:invalid ~ button[type=submit] {
    border-color: currentColor;
    background: var(--gray-100);
    color: var(--gray-400);
    pointer-events: none;
  }
  span.guest, span.guest a {
    color: var(--gray-400);
  }
  span.failed {
    display: flex;
    flex-flow: row nowrap;
    height: 1rem;
    align-items: center;
    margin-top: 0.875rem;
    color: var(--red);
  }
  span.failed svg {
    height: 1rem;
    margin-right: 0.25rem;
  }
  span.failed path {
    fill: transparent;
    stroke-width: 2px;
    stroke-linecap: round;
    stroke: currentColor;
  }
  .mono {
    font-family: 'Source Code Pro', monospace;
  }
  @media all and (prefers-color-scheme: dark) {
  :root {
    --white: #000000;
    --gray-800: #E5E5E5;
    --gray-400: #808080;
    --gray-100: #333333;
    --red-soft: #7F1D1D;
  }
  }
  @media screen and (min-width: 30em) {
    html {
      font-size: 14px;
    }
  }
  '''
::  +login-page: internal page to login to an Urbit
::
++  login-page
  |=  [redirect-url=(unit @t) our=@p =identity eauth=(unit ?) failed=?]
  ^-  octs
  =+  redirect-str=?~(redirect-url "" (trip u.redirect-url))
  %-  as-octs:mimes:html
  %-  crip
  %-  en-xml:html
  =/  favicon  %+
    weld  "<svg width='10' height='10' viewBox='0 0 10 10' xmlns='http://www.w3.org/2000/svg'>"
          "<circle r='3.09' cx='5' cy='5' /></svg>"
  ;html
    ;head
      ;meta(charset "utf-8");
      ;meta(name "viewport", content "width=device-width, initial-scale=1, shrink-to-fit=no");
      ;link(rel "icon", type "image/svg+xml", href (weld "data:image/svg+xml;utf8," favicon));
      ;title:"Urbit"
      ;style:"{(trip auth-styling)}"
      ;style:"{?^(eauth "" "nav \{ display: none; }")}"
      ;script:"our = '{(scow %p our)}';"
      ;script:'''
              let name, pass;
              function setup(isEauth) {
                name = document.getElementById('name');
                pass = document.getElementById('pass');
                if (isEauth) goEauth(); else goLocal();
              }
              function goLocal() {
                document.body.className = 'local';
                pass.focus();
              }
              function goEauth() {
                document.body.className = 'eauth';
                name.focus();
              }
              function doEauth() {
                if (name.value == our) {
                  event.preventDefault();
                  goLocal();
                }
              }
              '''
    ==
    ;body
      =class   "{?:(=(`& eauth) "eauth" "local")}"
      =onload  "setup({?:(=(`& eauth) "true" "false")})"
      ;div#local
        ;p:"Urbit ID"
        ;input(value "{(scow %p our)}", disabled "true", class "mono");
        ;+  ?:  =(%ours -.identity)
              ;div
                ;p:"Already authenticated"
                ;a.button/"{(trip (fall redirect-url '/'))}":"Continue"
              ==
        ;form(action "/~/login", method "post", enctype "application/x-www-form-urlencoded")
          ;p:"Access Key"
          ;input
            =type  "password"
            =name  "password"
            =id    "pass"
            =placeholder  "sampel-ticlyt-migfun-falmel"
            =class  "mono"
            =required  "true"
            =minlength  "27"
            =maxlength  "27"
            =pattern  "((?:[a-z]\{6}-)\{3}(?:[a-z]\{6}))";
          ;input(type "hidden", name "redirect", value redirect-str);
          ;+  ?.  failed  ;span;
            ;span.failed
              ;svg(xmlns "http://www.w3.org/2000/svg", viewBox "0 0 16 16")
                ;path(d "m8 8 4-4M8 8 4 4m4 4-4 4m4-4 4 4");
              ==
              Key is incorrect
            ==
          ;button(type "submit"):"Continue"
        ==
      ==
      ;div#eauth
        ;form(action "/~/login", method "post", onsubmit "return doEauth()")
          ;p:"Urbit ID"
          ;input.mono
            =name  "name"
            =id    "name"
            =placeholder  "{(scow %p our)}"
            =required   "true"
            =minlength  "4"
            =maxlength  "57"
            =pattern    "~((([a-z]\{6})\{1,2}-\{0,2})+|[a-z]\{3})";
          ;p
            ; You will be redirected to your own web interface to authorize
            ; logging in to
            ;span.mono:"{(scow %p our)}"
            ; .
          ==
          ;input(type "hidden", name "redirect", value redirect-str);
          ;button(name "eauth", type "submit"):"Continue"
        ==
      ==
      ;*  ?:  ?=(%ours -.identity)  ~
          =+  as="proceed as{?:(?=(%fake -.identity) " guest" "")}"
          ;+  ;span.guest.mono
                ; Or try to
                ;a/"{(trip (fall redirect-url '/'))}":"{as}"
                ; .
              ==
    ==
    ;script:'''
            var failSpan = document.querySelector('.failed');
            if (failSpan) {
              document.querySelector("input[type=password]")
                .addEventListener('keyup', function (event) {
                  failSpan.style.display = 'none';
                });
            }
            '''
  ==
::  +eauth-error-page: render an eauth error reporting page
::
::    optionally redirects the user back to either the login page if we're
::    acting as server, or the host if we're the client.
::
++  eauth-error-page
  |=  $=  return
      $?  ~                  ::  no known return target
          [%server last=@t]  ::  we are the host, return to login
          [%client goal=@t]  ::  we are the client, return to host
      ==
  ^-  octs
  %-  as-octs:mimes:html
  %-  crip
  %-  en-xml:html
  =/  return=(unit @t)
    ?-  return
      ~            ~
      [%server *]  %-  some
                   %^  cat  3  '/~/login?eauth&redirect='
                   (crip (en-urlt:html (trip last.return)))
      [%client *]  `goal.return  ::TODO  plus nonce? or abort?
    ==
  =/  favicon  %+
    weld  "<svg width='10' height='10' viewBox='0 0 10 10' xmlns='http://www.w3.org/2000/svg'>"
          "<circle r='3.09' cx='5' cy='5' /></svg>"
  =/  msg=tape
    ?~  return  "Something went wrong!"
    "Something went wrong! You will be redirected back..."
  ;html
    ;head
      ;*  ?~  return  ~
          :_  ~
          ;meta(http-equiv "Refresh", content "5; url={(trip u.return)}");
      ;meta(charset "utf-8");
      ;meta(name "viewport", content "width=device-width, initial-scale=1, shrink-to-fit=no");
      ;link(rel "icon", type "image/svg+xml", href (weld "data:image/svg+xml;utf8," favicon));
      ;title:"Urbit"
      ;style:'''
             @import url("https://rsms.me/inter/inter.css");
             :root {
               --black60: rgba(0,0,0,0.6);
               --white: rgba(255,255,255,1);
             }
             html {
               font-family: Inter, sans-serif;
               height: 100%;
               margin: 0;
               width: 100%;
               background: var(--white);
               color: var(--black60);
               -webkit-font-smoothing: antialiased;
               line-height: 1.5;
               font-size: 12px;
               display: flex;
               flex-flow: row nowrap;
               justify-content: center;
             }
             body {
               display: flex;
               flex-flow: column nowrap;
               justify-content: center;
               max-width: 300px;
               padding: 1rem;
               width: 100%;
             }
             '''
    ==
    ;body:"{msg}"
  ==
::  +render-tang-to-marl: renders a tang and adds <br/> tags between each line
::
++  render-tang-to-marl
  |=  [wid=@u tan=tang]
  ^-  marl
  =/  raw=(list tape)  (zing (turn tan |=(a=tank (wash 0^wid a))))
  ::
  |-  ^-  marl
  ?~  raw  ~
  [;/(i.raw) ;br; $(raw t.raw)]
::  +render-tang-to-wall: renders tang as text lines
::
++  render-tang-to-wall
  |=  [wid=@u tan=tang]
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
  %-  zing  ^-  ^wall
  %-  zing  ^-  (list ^wall)
  %+  turn  wall
  |=  t=tape
  ^-  ^wall
  ~[t "\0a"]
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
      ;p:"There was an error while handling the request for {(trip url)}."
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
    ?+  code  "{(scow %ud code)} Error"
      %400  "Bad Request"
      %403  "Forbidden"
      %404  "Not Found"
      %405  "Method Not Allowed"
      %500  "Internal Server Error"
    ==
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
      ;p:"There was an error while handling the request for {(trip url)}."
      ;*  ?:  authorized
            ;=
              ;code:"{t}"
            ==
          ~
    ==
  ==
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
::  +find-suffix: returns [~ /tail] if :full is (weld :prefix /tail)
::
++  find-suffix
  |=  [prefix=path full=path]
  ^-  (unit path)
  ?~  prefix
    `full
  ?~  full
    ~
  ?.  =(i.prefix i.full)
    ~
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
::  +host-sans-port: strip the :<port> from a host string
::
++  host-sans-port
  ;~  sfix
    %+  cook  crip
    %-  star
    ;~  less
      ;~(plug col (punt dem) ;~(less next (easy ~)))
      next
    ==
    (star next)
  ==
::  +per-server-event: per-event server core
::
++  per-server-event
  ~%  %eyre-per-server-event  ..part  ~
  ::  gate that produces the +per-server-event core from event information
  ::
  |=  [[eny=@ =duct now=@da rof=roof] state=server-state]
  =/  eyre-id  (scot %ta (cat 3 'eyre_' (scot %uv (sham duct))))
  |%
  ::  +request-local: bypass authentication for local lens connections
  ::
  ++  request-local
    |=  [secure=? =address =request:http]
    ^-  [(list move) server-state]
    ::
    =/  act  [%app app=%lens]
    ::
    =/  connection=outstanding-connection
      [act [& secure address request] [*@uv [%ours ~]] ~ 0]
    ::
    =.  connections.state
      %.  (~(put by connections.state) duct connection)
      (trace 2 |.("{<duct>} creating local"))
    ::
    (request-to-app [%ours ~] app.act inbound-request.connection)
  ::  +request: starts handling an inbound http request
  ::
  ++  request
    |=  [secure=? =address =request:http]
    ^-  [(list move) server-state]
    =*  headers  header-list.request
    ::  for requests from localhost, respect the "forwarded" header
    ::
    =/  [secure=? =^address]
      =*  same  [secure address]
      ?.  =([%ipv4 .127.0.0.1] address)        same
      ?~  forwards=(forwarded-params headers)  same
      :-  (fall (forwarded-secure u.forwards) secure)
      (fall (forwarded-for u.forwards) address)
    ::
    =/  host  (get-header:http 'host' headers)
    =/  [=action suburl=@t]
      (get-action-for-binding host url.request)
    ::
    ::TODO  we might want to mint new identities only for requests that end
    ::      up going into userspace, not the ones that get handled by eyre.
    ::      perhaps that distinction, where userspace requests are async, but
    ::      eyre-handled requests are always synchronous, provides a fruitful
    ::      angle for refactoring...
    =^  [suv=@uv =identity som=(list move)]  state
      (session-for-request:authentication request)
    =;  [moz=(list move) sat=server-state]
      [(weld som moz) sat]
    ::
    =/  authenticated  ?=(%ours -.identity)
    ::  if we have no eauth endpoint yet, and the request is authenticated,
    ::  deduce it from the hostname
    ::
    =?  endpoint.auth.state
        ?&  authenticated
            ?=(^ host)
            ?=(~ auth.endpoint.auth.state)
        ==
      %-  (trace 2 |.("eauth: storing endpoint at {(trip u.host)}"))
      :+  user.endpoint.auth.state
        `(cat 3 ?:(secure 'https://' 'http://') u.host)
      now
    ::  record that we started an asynchronous response
    ::
    =/  connection=outstanding-connection
      [action [authenticated secure address request] [suv identity] ~ 0]
    =.  connections.state
      ::  NB: required by +handle-response and +handle-request:authentication.
      ::  XX optimize, not all requests are asynchronous
      ::
      (~(put by connections.state) duct connection)
    ::  redirect to https if insecure, redirects enabled
    ::  and secure port live
    ::
    ?:  ?&  !secure
            redirect.http-config.state
            ?=(^ secure.ports.state)
        ==
      =/  location=@t
        %+  rap  3
        :~  'https://'
            (rash (fall host '') host-sans-port)
            ?:  =(443 u.secure.ports.state)
              ''
            (crip ":{(a-co:co u.secure.ports.state)}")
            ?:  ?=([[~ ~] ~] (parse-request-line url.request))
              '/'
            url.request
        ==
      %-  handle-response
      :*  %start
          :-  status-code=301
          headers=['location' location]~
          data=~
          complete=%.y
      ==
    ::  figure out whether this is a cors request,
    ::  whether the origin is approved or not,
    ::  and maybe add it to the "pending approval" set
    ::
    =/  origin=(unit origin)
      (get-header:http 'origin' headers)
    =^  cors-approved  requests.cors-registry.state
      =,  cors-registry.state
      ?~  origin                         [| requests]
      ?:  (~(has in approved) u.origin)  [& requests]
      ?:  (~(has in rejected) u.origin)  [| requests]
      [| (~(put in requests) u.origin)]
    ::  if this is a cors preflight request from an approved origin
    ::  handle it synchronously
    ::
    ?:  &(?=(^ origin) cors-approved ?=(%'OPTIONS' method.request))
      %-  handle-response
      =;  =header-list:http
        [%start [204 header-list] ~ &]
      ::  allow the method and headers that were asked for,
      ::  falling back to wildcard if none specified
      ::
      ::NOTE  +handle-response will add the rest of the headers
      ::
      :~  :-  'Access-Control-Allow-Methods'
          =-  (fall - '*')
          (get-header:http 'access-control-request-method' headers)
        ::
          :-  'Access-Control-Allow-Headers'
          =-  (fall - '*')
          (get-header:http 'access-control-request-headers' headers)
      ==
    ::  handle requests to the cache
    ::
    =/  entry  (~(get by cache.state) url.request)
    ?:  &(?=(^ entry) ?=(%'GET' method.request))
      (handle-cache-req authenticated request val.u.entry)
    ::
    ?-    -.action
        %gen
      =/  bek=beak  [our desk.generator.action da+now]
      =/  sup=spur  path.generator.action
      =/  ski       (rof ~ /eyre %ca bek sup)
      =/  cag=cage  (need (need ski))
      ?>  =(%vase p.cag)
      =/  gat=vase  !<(vase q.cag)
      =/  res=toon
        %-  mock  :_  (look rof ~ /eyre)
        :_  [%9 2 %0 1]  |.
        %+  slam
          %+  slam  gat
          !>([[now=now eny=eny bek=bek] ~ ~])
        ::TODO  should get passed the requester's identity
        !>([authenticated request])
      ?:  ?=(%2 -.res)
        =+  connection=(~(got by connections.state) duct)
        %^  return-static-data-on-duct  500  'text/html'
        %:  internal-server-error
            authenticated.inbound-request.connection
            url.request.inbound-request.connection
            leaf+"generator crashed"
            p.res
        ==
      ?:  ?=(%1 -.res)
        =+  connection=(~(got by connections.state) duct)
        %^  return-static-data-on-duct  500  'text/html'
        %:  internal-server-error
            authenticated.inbound-request.connection
            url.request.inbound-request.connection
            leaf+"scry blocked on"
            (fall (bind (bind ((soft path) p.res) smyt) (late ~)) ~)
        ==
      =/  result  ;;(simple-payload:http +.p.res)
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
      ^-  http-event:http
      :*  %start
          response-header.result
          data.result
          complete=%.y
      ==
    ::
        %app
      (request-to-app identity app.action inbound-request.connection)
    ::
        %authentication
      (handle-request:authentication secure host address [suv identity] request)
    ::
        %eauth
      (on-request:eauth:authentication [suv identity] request)
    ::
        %logout
      (handle-logout:authentication [suv identity] request)
    ::
        %channel
      (handle-request:by-channel [suv identity] address request)
    ::
        %scry
      (handle-scry authenticated address request(url suburl))
    ::
        %name
      (handle-name identity request)
    ::
        %host
      %^  return-static-data-on-duct  200  'text/plain'
      (as-octs:mimes:html (scot %p our))
    ::
        %four-oh-four
      %^  return-static-data-on-duct  404  'text/html'
      (error-page 404 authenticated url.request ~)
    ==
  ::  +handle-name: respond with the requester's @p
  ::
  ++  handle-name
    |=  [=identity =request:http]
    ^-  (quip move server-state)
    ?.  =(%'GET' method.request)
      %^  return-static-data-on-duct  405  'text/html'
      (error-page 405 & url.request "may only GET name")
    %^  return-static-data-on-duct  200  'text/plain'
    =/  nom=@p
      ?+(-.identity who.identity %ours our)
    (as-octs:mimes:html (scot %p nom))
  ::  +handle-cache-req: respond with cached value, 404 or 500
  ::
  ++  handle-cache-req
    |=  [authenticated=? =request:http entry=(unit cache-entry)]
    |^  ^-  (quip move server-state)
    ?~  entry
      (error-response 404 "cache entry for that binding was deleted")
    ?:  &(auth.u.entry !authenticated)
      (error-response 403 ~)
    =*  body  body.u.entry
    ?-    -.body
        %payload
      %-  handle-response
      :*  %start
          response-header.simple-payload.body
          data.simple-payload.body
          complete=%.y
      ==
    ==
    ::
    ++  error-response
      |=  [status=@ud =tape]
      ^-  (quip move server-state)
      %^  return-static-data-on-duct  status  'text/html'
      (error-page status authenticated url.request tape)
    --
  ::  +handle-scry: respond with scry result, 404 or 500
  ::
  ++  handle-scry
    |=  [authenticated=? =address =request:http]
    |^  ^-  (quip move server-state)
    ?.  authenticated
      (error-response 403 ~)
    ?.  =(%'GET' method.request)
      (error-response 405 "may only GET scries")
    ::  make sure the path contains an app to scry into
    ::
    =+  req=(parse-request-line url.request)
    ?.  ?=(^ site.req)
      (error-response 400 "scry path must start with app name")
    ::  attempt the scry that was asked for
    ::
    =/  res=(unit (unit cage))
      (do-scry %gx i.site.req (snoc t.site.req (fall ext.req %mime)))
    ?~  res    (error-response 500 "failed scry")
    ?~  u.res  (error-response 404 "no scry result")
    =*  mark   p.u.u.res
    =*  vase   q.u.u.res
    ?:  =(%mime mark)
      =/  =mime  !<(mime vase)
      %^  return-static-data-on-duct  200
        (rsh 3 (spat p.mime))  q.mime
    ::  attempt to find conversion gate to mime
    ::
    =/  tub=(unit [tub=tube:clay mov=move])
      (find-tube i.site.req mark %mime)
    ?~  tub  (error-response 500 "no tube from {(trip mark)} to mime")
    ::  attempt conversion, then send results
    ::
    =/  mym=(each mime tang)
      (mule |.(!<(mime (tub.u.tub vase))))
    =^  cards  state
      ?-  -.mym
        %|  (error-response 500 "failed tube from {(trip mark)} to mime")
        %&  %+  return-static-data-on-duct  200
            [(rsh 3 (spat p.p.mym)) q.p.mym]
      ==
    [[mov.u.tub cards] state]
    ::
    ++  find-tube
      |=  [dap=term from=mark to=mark]
      ^-  (unit [tube:clay move])
      =/  des=(unit (unit cage))
        (do-scry %gd dap /$)
      ?.  ?=([~ ~ *] des)  ~
      =+  !<(=desk q.u.u.des)
      =/  tub=(unit (unit cage))
        (do-scry %cc desk /[from]/[to])
      ?.  ?=([~ ~ %tube *] tub)  ~
      :-  ~
      :-  !<(tube:clay q.u.u.tub)
      :^  duct  %pass  /conversion-cache/[from]
      [%c %warp our desk `[%sing %c da+now /[from]/[to]]]
    ::
    ++  do-scry
      |=  [care=term =desk =path]
      ^-  (unit (unit cage))
      (rof ~ /eyre care [our desk da+now] path)
    ::
    ++  error-response
      |=  [status=@ud =tape]
      ^-  (quip move server-state)
      %^  return-static-data-on-duct  status  'text/html'
      (error-page status authenticated url.request tape)
    --
  ::  +request-to-app: subscribe to app and poke it with request data
  ::
  ++  request-to-app
    |=  [=identity app=term =inbound-request:eyre]
    ^-  (quip move server-state)
    ::  if the agent isn't running, we synchronously serve a 503
    ::
    ?.  !<(? q:(need (need (rof ~ /eyre %gu [our app da+now] /$))))
      %^  return-static-data-on-duct  503  'text/html'
      %:  error-page
        503
        ?=(%ours -.identity)
        url.request.inbound-request
        "%{(trip app)} not running"
      ==
    ::  otherwise, subscribe to the agent and poke it with the request
    ::
    :_  state
    :~  %+  deal-as
          /watch-response/[eyre-id]
        [identity our app %watch /http-response/[eyre-id]]
      ::
        %+  deal-as
          /run-app-request/[eyre-id]
        :^  identity  our  app
        :+  %poke  %handle-http-request
        !>(`[@ta inbound-request:eyre]`[eyre-id inbound-request])
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
        %gen  [~ state]
        %app
      :_  state
      :_  ~
      =,  u.connection
      %-  (trace 1 |.("leaving subscription to {<app.action>}"))
      (deal-as /watch-response/[eyre-id] identity our app.action %leave ~)
    ::
        ?(%authentication %eauth %logout)
      ::NOTE  expiry timer will clean up cancelled eauth attempts
      [~ state]
    ::
        %channel
      on-cancel-request:by-channel
    ::
        ?(%scry %four-oh-four %name %host)
      ::  it should be impossible for these to be asynchronous
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
    ::  +handle-request: handles an http request for the login page
    ::
    ++  handle-request
      |=  [secure=? host=(unit @t) =address [session-id=@uv =identity] =request:http]
      ^-  [(list move) server-state]
      ::  parse the arguments out of request uri
      ::
      =+  request-line=(parse-request-line url.request)
      =/  redirect    (get-header:http 'redirect' args.request-line)
      =/  with-eauth=(unit ?)
        ?:  =(~ eauth-url:eauth)  ~
        `?=(^ (get-header:http 'eauth' args.request-line))
      ::  if we received a simple get: show the login page
      ::
      ::NOTE  we never auto-redirect, to avoid redirect loops with apps that
      ::      send unprivileged users to the login screen
      ::
      ?:  =('GET' method.request)
        %^  return-static-data-on-duct  200  'text/html'
        (login-page redirect our identity with-eauth %.n)
      ::  if we are not a post, return an error
      ::
      ?.  =('POST' method.request)
        %^  return-static-data-on-duct  405  'text/html'
        (login-page ~ our identity with-eauth %.n)
      ::  we are a post, and must process the body type as form data
      ::
      ?~  body.request
        %^  return-static-data-on-duct  400  'text/html'
        (login-page ~ our identity with-eauth %.n)
      ::
      =/  parsed=(unit (list [key=@t value=@t]))
        (rush q.u.body.request yquy:de-purl:html)
      ?~  parsed
        %^  return-static-data-on-duct  400  'text/html'
        (login-page ~ our identity with-eauth %.n)
      ::
      =/  redirect=(unit @t)  (get-header:http 'redirect' u.parsed)
      ?^  (get-header:http 'eauth' u.parsed)
        ?~  ship=(biff (get-header:http 'name' u.parsed) (cury slaw %p))
          %^  return-static-data-on-duct  400  'text/html'
          (login-page redirect our identity `& %.n)
        ::TODO  redirect logic here and elsewhere is ugly
        =/  redirect  (fall redirect '')
        =/  base=(unit @t)
          ?~  host  ~
          `(cat 3 ?:(secure 'https://' 'http://') u.host)
        (start:server:eauth u.ship base ?:(=(redirect '') '/' redirect))
      ::
      =.  with-eauth  (bind with-eauth |=(? |))
      ?~  password=(get-header:http 'password' u.parsed)
        %^  return-static-data-on-duct  400  'text/html'
        (login-page redirect our identity with-eauth %.n)
      ::  check that the password is correct
      ::
      ?.  =(u.password code)
        %^  return-static-data-on-duct  400  'text/html'
        (login-page redirect our identity with-eauth %.y)
      ::  clean up the session they're changing out from
      ::
      =^  moz  state
        (close-session session-id |)
      ::  initialize the new session
      ::
      =^  fex  state  (start-session %local)
      ::  associate the new session with the request that caused the login
      ::
      ::    if we don't do this here, +handle-response will include the old
      ::    session's cookie, confusing the client.
      ::
      =.  connections.state
        %+  ~(jab by connections.state)  duct
        |=  o=outstanding-connection
        o(session-id session.fex)
      ::  store the hostname used for this login, later reuse it for eauth
      ::
      =?  endpoint.auth.state  ?=(^ host)
        %-  (trace 2 |.("eauth: storing endpoint at {(trip u.host)}"))
        :+  user.endpoint.auth.state
          `(cat 3 ?:(secure 'https://' 'http://') u.host)
        now
      ::
      =;  out=[moves=(list move) server-state]
        out(moves [give-session-tokens :(weld moz moves.fex moves.out)])
      ::NOTE  that we don't provide a 'set-cookie' header here.
      ::      +handle-response does that for us.
      ?~  redirect
        (handle-response %start 204^~ ~ &)
      =/  actual-redirect  ?:(=(u.redirect '') '/' u.redirect)
      (handle-response %start 303^['location' actual-redirect]~ ~ &)
    ::  +handle-logout: handles an http request for logging out
    ::
    ++  handle-logout
      |=  [[session-id=@uv =identity] =request:http]
      ^-  [(list move) server-state]
      ::  whatever we end up doing, we always respond with a redirect
      ::
      =/  response=$>(%start http-event:http)
        =/  redirect=(unit @t)
          %+  get-header:http  'redirect'
          args:(parse-request-line url.request)
        :*  %start
            response-header=[303 ['location' (fall redirect '/~/login')]~]
            data=~
            complete=%.y
        ==
      ::  read options from the body
      ::  all: log out all sessions with this identity?
      ::  sid: which session do we log out? (defaults to requester's)
      ::  hos: host to log out from, for eauth logins (sid signifies the nonce)
      ::
      =/  arg=header-list:http
        ?~  body.request  ~
        (fall (rush q.u.body.request yquy:de-purl:html) ~)
      =/  all=?
        ?=(^ (get-header:http 'all' arg))
      =/  sid=(unit @uv)
        ?.  ?=(%ours -.identity)  `session-id
        ?~  sid=(get-header:http 'sid' arg)  `session-id
        ::  if you provided the parameter, but it doesn't parse, we just
        ::  no-op. otherwise, a poorly-implemented frontend might result in
        ::  accidental log-outs, which would be very annoying.
        ::
        (slaw %uv u.sid)
      =/  hos=(unit @p)
        ?.  ?=(%ours -.identity)  ~
        (biff (get-header:http 'host' arg) (cury slaw %p))
      ?~  sid
        (handle-response response)
      ::  if this is an eauth remote logout, send the %shut
      ::
      =*  auth  auth.state
      ?:  ?=(^ hos)
        =^  moz  state  (handle-response response)
        :-  [(send-plea:client:eauth u.hos %0 %shut u.sid) moz]
        =/  book  (~(gut by visiting.auth) u.hos *logbook)
        =.  qeu.book  (~(put to qeu.book) u.sid)
        =.  visiting.auth  (~(put by visiting.auth) u.hos book)
        state
      ::  if the requester is logging themselves out, make them drop the cookie
      ::
      =?  headers.response-header.response  =(u.sid session-id)
        :_  headers.response-header.response
        ['set-cookie' (session-cookie-string session-id |)]
      ::  close the session as requested, then send the response
      ::
      =^  moz1  state  (close-session u.sid all)
      =^  moz2  state  (handle-response response)
      [[give-session-tokens (weld moz1 moz2)] state]
    ::  +session-id-from-request: attempt to find a session cookie
    ::
    ++  session-id-from-request
      |=  =request:http
      ^-  (unit @uv)
      ::  are there cookies passed with this request?
      ::
      =/  cookie-header=@t
        %+  roll  header-list.request
        |=  [[key=@t value=@t] c=@t]
        ?.  =(key 'cookie')
          c
        (cat 3 (cat 3 c ?~(c 0 '; ')) value)
      ::  is the cookie line valid?
      ::
      ?~  cookies=(rush cookie-header cock:de-purl:html)
        ~
      ::  is there an urbauth cookie?
      ::
      ?~  urbauth=(get-header:http (crip "urbauth-{(scow %p our)}") u.cookies)
        ~
      ::  if it's formatted like a valid session cookie, produce it
      ::
      `(unit @)`(rush u.urbauth ;~(pfix (jest '0v') viz:ag))
    ::  +request-is-logged-in: checks to see if the request has non-guest id
    ::
    ++  request-is-logged-in
      |=  =request:http
      ^-  ?
      ?~  session-id=(session-id-from-request request)
        |
      ?~  session=(~(get by sessions.auth.state) u.session-id)
        |
      &(!?=(%fake -.identity.u.session) (lte now expiry-time.u.session))
    ::  +request-is-authenticated: checks to see if the request is "us"
    ::
    ::    We are considered authenticated if this request has an urbauth
    ::    Cookie for the local identity that is not expired.
    ::
    ++  request-is-authenticated
      |=  =request:http
      ^-  ?
      ::  does the request pass a session cookie?
      ::
      ?~  session-id=(session-id-from-request request)
        %.n
      ::  is this a session that we know about?
      ::
      ?~  session=(~(get by sessions.auth.state) `@uv`u.session-id)
        %.n
      ::  does this session have our id, and is it still valid?
      ::
      &(?=(%ours -.identity.u.session) (lte now expiry-time.u.session))
    ::  +start-session: create a new session with %local or %guest identity
    ::
    ++  start-session
      |=  kind=?(%local %guest [%eauth who=@p])
      ^-  [[session=@uv =identity moves=(list move)] server-state]
      =;  [key=@uv sid=identity]
        :-  :+  key  sid
            ::  if no session existed previously, we must kick off the
            ::  session expiry timer
            ::
            ?^  sessions.auth.state  ~
            [duct %pass /sessions/expire %b %wait (add now session-timeout)]~
        =-  state(sessions.auth -)
        %+  ~(put by sessions.auth.state)  key
        [sid (add now session-timeout) ~]
      ::  create a new session with a fake identity
      ::
      =/  sik=@uv  new-session-key
      :-  sik
      ?:  ?=(%local kind)      [%ours ~]
      ?:  ?=([%eauth @] kind)  [%real who.kind]
      :-  %fake
      ::  pre-scramble our ship name into its displayed value, and
      ::  truncate it to be at most moon-length, so that we can overlay
      ::  it onto the end of a comet name for visual consistency.
      ::  to prevent escalation, make sure the guest identity isn't ours.
      ::
      |-
      =;  nom=@p
        ?.  =(our nom)  nom
        $(eny (shas %next-name eny))
      %+  end  3^16
      %^  cat  3
        (end 3^8 (fein:ob our))
      (~(raw og (shas %fake-name eny)) 128)
    ::  +session-for-request: get the session details for the request
    ::
    ::    creates a guest session if the request does not have a valid session.
    ::    there is no need to call +give-session-tokens after this, because
    ::    guest session do not make valid "auth session" tokens.
    ::
    ++  session-for-request
      |=  =request:http
      ^-  [[session=@uv =identity moves=(list move)] server-state]
      =*  new  (start-session %guest)
      ?~  sid=(session-id-from-request request)
        new
      ?~  ses=(~(get by sessions.auth.state) u.sid)
        new
      ?:  (gth now expiry-time.u.ses)
        new
      [[u.sid identity.u.ses ~] state]
    ::  +close-session: delete a session and its associated channels
    ::
    ::    if :all is true, deletes all sessions that share the same identity.
    ::    if this closes an %ours session, the caller is responsible for
    ::    also calling +give-session-tokens afterwards.
    ::
    ++  close-session
      |=  [session-id=@uv all=?]
      ^-  [(list move) server-state]
      ?~  ses=(~(get by sessions.auth.state) session-id)
        [~ state]
      ::  delete the session(s) and find the associated ids & channels
      ::
      =^  [siz=(list @uv) channels=(list @t)]  sessions.auth.state
        =*  sessions  sessions.auth.state
        ::  either delete just the specific session and its channels,
        ::
        ?.  all
          :-  [[session-id]~ ~(tap in channels.u.ses)]
          (~(del by sessions) session-id)
        ::  or delete all sessions with the identity from :session-id
        ::
        %+  roll  ~(tap by sessions)
        |=  $:  [sid=@uv s=session]
                [[siz=(list @uv) caz=(list @t)] sez=(map @uv session)]
            ==
        ^+  [[siz caz] sez]
        ?.  =(identity.s identity.u.ses)
          ::  identity doesn't match, so re-store this session
          ::
          [[siz caz] (~(put by sez) sid s)]
        ::  identity matches, so register this session as closed
        ::
        [[[sid siz] (weld caz ~(tap in channels.s))] sez]
      ::  close all affected channels and send their responses
      ::
      =|  moves1=(list move)
      |-  ^-  (quip move server-state)
      ?^  channels
        %-  %+  trace  1
            |.("{(trip i.channels)} discarding channel due to closed session")
        =^  moz  state
          (discard-channel:by-channel i.channels |)
        $(moves1 (weld moves1 moz), channels t.channels)
      ::  lastly, %real sessions require additional cleanup
      ::
      ?.  ?=(%real -.identity.u.ses)  [moves1 state]
      =^  moves2  visitors.auth.state
        %+  roll  ~(tap by visitors.auth.state)
        |=  [[nonce=@uv visa=visitor] [moz=(list move) viz=(map @uv visitor)]]
        ?^  +.visa  [moz (~(put by viz) nonce visa)]
        :_  viz
        %+  weld  moz
        ?~  duct.visa  ~
        [(send-boon:server:eauth(duct u.duct.visa) %0 %shut nonce)]~
      [(weld `(list move)`moves1 `(list move)`moves2) state]
    ::  +code: returns the same as |code
    ::
    ++  code
      ^-  @ta
      =/  res=(unit (unit cage))
        (rof ~ /eyre %j [our %code da+now] /(scot %p our))
      (rsh 3 (scot %p ;;(@ q.q:(need (need res)))))
    ::  +session-cookie-string: compose session cookie
    ::
    ++  session-cookie-string
      |=  [session=@uv extend=?]
      ^-  @t
      %-  crip
      =;  max-age=tape
        "urbauth-{(scow %p our)}={(scow %uv session)}; Path=/; Max-Age={max-age}"
      %-  format-ud-as-integer
      ?.  extend  0
      (div (msec:milly session-timeout) 1.000)
    ::
    ::
    ++  eauth
      =*  auth  auth.state
      |%
      ++  server
        |%
        ::  +start: initiate an eauth login attempt for the :ship identity
        ::
        ++  start
          |=  [=ship base=(unit @t) last=@t]
          ^-  [(list move) server-state]
          %-  (trace 2 |.("eauth: starting eauth into {(scow %p ship)}"))
          =/  nonce=@uv
            |-
            =+  n=(~(raw og (shas %eauth-nonce eny)) 64)
            ?.  (~(has by visitors.auth) n)  n
            $(eny (shas %try-again n))
          =/  visit=visitor  [~ `[duct now] ship base last ~]
          =.  visitors.auth  (~(put by visitors.auth) nonce visit)
          :_  state
          ::  we delay serving an http response until we receive a scry %tune
          ::
          :~  (send-keen %keen ship nonce now)
              (start-timeout /visitors/(scot %uv nonce))
          ==
        ::  +on-tune: receive a client-url remote scry result
        ::
        ++  on-tune
          |=  [ship=@p nonce=@uv url=@t]
          ^-  [(list move) server-state]
          %-  (trace 2 |.("eauth: %tune from {(scow %p ship)}"))
          ::  guarantee the ship still controls the nonce
          ::
          =/  visa=visitor  (~(got by visitors.auth) nonce)
          ?>  &(?=(^ +.visa) =(ship ship.visa))
          ::  redirect the visitor to their own confirmation page
          ::
          =.  visitors.auth  (~(put by visitors.auth) nonce visa(pend ~))
          %-  handle-response(duct http:(need pend.visa))
          =;  url=@t  [%start 303^['location' url]~ ~ &]
          %+  rap  3
          :~  url
              '?server='  (scot %p our)
              '&nonce='   (scot %uv nonce)
          ==
        ::  +on-plea: receive an eauth network message from a client
        ::
        ++  on-plea
          |=  [=ship plea=eauth-plea]
          ^-  [(list move) server-state]
          %-  (trace 2 |.("eauth: {(trip +<.plea)} from {(scow %p ship)}"))
          =;  res=[(list move) server-state]
            =^  moz  state  res
            [[[duct %give %done ~] moz] state]
          ?-  +<.plea
              %open
            ::  this attempt may or may not have been started in +start yet
            ::
            =/  visa=visitor
              %+  ~(gut by visitors.auth)  nonce.plea
              [~ ~ ship ~ '/' ~]
            ?>  ?=(^ +.visa)
            ?>  =(ship ship.visa)
            ::NOTE  that token might still be empty, in which case the http
            ::      client will probably signal an abort when they return
            ::
            =.  duct.visa      `duct
            =.  toke.visa      token.plea
            =.  visitors.auth  (~(put by visitors.auth) nonce.plea visa)
            ::  if the eauth attempt was started on our side, we may know the
            ::  specific base url the user used; make sure they go back there
            ::
            =/  url=@t
              %-  need
              ?~  base.visa  eauth-url
              eauth-url(user.endpoint.auth base.visa)
            [[(send-boon %0 %okay nonce.plea url)]~ state]
          ::
              %shut
            ::  the visitor wants the associated session gone
            ::
            ?~  visa=(~(get by visitors.auth) nonce.plea)  [~ state]
            =.  visitors.auth  (~(del by visitors.auth) nonce.plea)
            =?  sessions.auth  ?=(@ +.u.visa)
              (~(del by sessions.auth) sesh.u.visa)
            [[(send-boon %0 %shut nonce.plea)]~ state]
          ==
        ::  +cancel: the client aborted the eauth attempt, so clean it up
        ::
        ++  cancel
          |=  [nonce=@uv last=@t]
          ^-  [(list move) server-state]
          ::  if the eauth attempt doesn't exist, or it was already completed,
          ::  we cannot cancel it
          ::
          ?~  visa=(~(get by visitors.auth) nonce)  [~ state]
          ?@  +.u.visa  [~ state]
          ::  delete the attempt, and go back to the login page
          ::
          %-  (trace 2 |.("eauth: cancelling login"))
          =.  visitors.auth  (~(del by visitors.auth) nonce)
          =^  moz  state
            =/  url=@t
              %^  cat  3  '/~/login?eauth&redirect='
              (crip (en-urlt:html (trip last)))
            (handle-response %start 303^['location' url]~ ~ &)
          :_  state
          %+  weld  moz
          ?~  duct.u.visa  ~
          [(send-boon(duct u.duct.u.visa) %0 %shut nonce)]~
        ::  +expire: host-side cancel an eauth attempt if it's still pending
        ::
        ++  expire
          |=  nonce=@uv
          ^-  [(list move) server-state]
          ?~  visa=(~(get by visitors.auth) nonce)
            [~ state]
          ?@  +.u.visa  [~ state]
          %-  (trace 2 |.("eauth: expiring"))
          =^  moz  state
            ?~  pend.u.visa  [~ state]
            %-  return-static-data-on-duct(duct http.u.pend.u.visa)
            [503 'text/html' (eauth-error-page %server last.u.visa)]
          =?  moz  ?=(^ pend.u.visa)
            [(send-keen %yawn ship.u.visa nonce keen.u.pend.u.visa) moz]
          =.  visitors.auth  (~(del by visitors.auth) nonce)
          :_  state
          %+  weld  moz
          ?~  duct.u.visa  ~
          [(send-boon(duct u.duct.u.visa) %0 %shut nonce)]~
        ::  +finalize: eauth attempt was approved: mint the client a new session
        ::
        ::    gives the http response on the current duct
        ::
        ++  finalize
          |=  [=plea=^duct nonce=@uv =ship last=@t]
          ^-  [(list move) server-state]
          %-  (trace 2 |.("eauth: finalizing login for {(scow %p ship)}"))
          ::  clean up the session they're changing out from,
          ::  mint the new session,
          ::  associate it with the nonce,
          ::  and the finalization request,
          ::  and send the visitor the cookie + final redirect
          ::
          =^  moz1  state
            (close-session session-id:(~(got by connections.state) duct) |)
          =^  [sid=@uv * moz2=(list move)]  state
            (start-session %eauth ship)
          =.  visitors.auth
            %+  ~(jab by visitors.auth)  nonce
            |=(v=visitor v(+ sid))
          =.  connections.state
            %+  ~(jab by connections.state)  duct
            |=  o=outstanding-connection
            o(session-id sid)
          =^  moz3  state
            =;  hed  (handle-response %start 303^hed ~ &)
            :~  ['location' last]
                ['set-cookie' (session-cookie-string sid &)]
            ==
          [:(weld moz1 moz2 moz3) state]
        ::  +on-fail: we crashed or received an empty %tune, clean up
        ::
        ++  on-fail
          |=  [=ship nonce=@uv]
          ^-  [(list move) server-state]
          ::  if the eauth attempt doesn't exist, or it was already completed,
          ::  we can no-op here
          ::
          ?~  visa=(~(get by visitors.auth) nonce)  [~ state]
          ?@  +.u.visa  [~ state]
          ::  delete the attempt, and go back to the login page
          ::
          %-  (trace 2 |.("eauth: failed login"))
          =.  visitors.auth  (~(del by visitors.auth) nonce)
          =^  moz  state
            ?~  pend.u.visa  [~ state]
            %-  return-static-data-on-duct(duct http.u.pend.u.visa)
            [503 'text/html' (eauth-error-page %server last.u.visa)]
          :_  state
          %+  weld  moz
          ?~  duct.u.visa  ~
          [(send-boon(duct u.duct.u.visa) %0 %shut nonce)]~
        ::
        ::TODO  +on-request?
        ::
        ++  send-keen
          |=  [kind=?(%keen %yawn) =ship nonce=@uv =time]
          ^-  move
          %-  (trace 2 |.("eauth: %{(trip kind)} into {(scow %p ship)}"))
          ::  we round down the time to make it more likely to hit cache,
          ::  at the expense of not working if the endpoint changed within
          ::  the last hour.
          ::
          =/  =wire       /eauth/keen/(scot %p ship)/(scot %uv nonce)
          =.   time       (sub time (mod time ~h1))
          =/  =spar:ames  [ship /e/x/(scot %da time)//eauth/url]
          [duct %pass wire %a ?-(kind %keen keen+spar, %yawn yawn+spar)]
        ::
        ++  send-boon
          |=  boon=eauth-boon
          ^-  move
          %-  (trace 2 |.("eauth: sending {(trip +<.boon)}"))
          [duct %give %boon boon]
        --
      ::
      ++  client
        |%
        ::  +start: as the client, approve or abort an eauth attempt
        ::
        ::    assumes the duct is of an incoming eauth start/approve request
        ::
        ++  start
          |=  [host=ship nonce=@uv grant=?]
          ^-  [(list move) server-state]
          =/  token=@uv  (~(raw og (shas %eauth-token eny)) 128)
          ::  we always send an %open, because we need to redirect the user
          ::  back to the host. and we always set a timeout, because we may
          ::  not get a response quickly enough.
          ::
          :-  :~  (send-plea host %0 %open nonce ?:(grant `token ~))
                  (start-timeout /visiting/(scot %p host)/(scot %uv nonce))
              ==
          ::  make sure we aren't attempting with this nonce already,
          ::  then remember the secret so we can include it in the redirect
          ::
          =/  book  (~(gut by visiting.auth) host *logbook)
          ?<  (~(has by map.book) nonce)
          =.  visiting.auth
            %+  ~(put by visiting.auth)  host
            :-  (~(put to qeu.book) nonce)
            (~(put by map.book) nonce [`duct ?:(grant `token ~)])
          state
        ::  +on-done: receive n/ack for plea we sent
        ::
        ++  on-done
          |=  [host=ship good=?]
          ^-  [(list move) server-state]
          %-  %-  trace
              ?:  good
                [2 |.("eauth: ack from {(scow %p host)}")]
              [1 |.("eauth: nack from {(scow %p host)}")]
          =/  book  (~(gut by visiting.auth) host *logbook)
          ?~  ~(top to qeu.book)
            %.  [~ state]
            (trace 0 |.("eauth: done on empty queue from {(scow %p host)}"))
          =^  nonce=@uv  qeu.book  ~(get to qeu.book)
          ?:  good
            =.  visiting.auth
              ?:  =([~ ~] book)
                (~(del by visiting.auth) host)
              (~(put by visiting.auth) host book)
            [~ state]
          =/  port  (~(get by map.book) nonce)
          ?~  port  [~ state]
          ::  delete the attempt/session, serve response if needed
          ::
          =.  visiting.auth
            =.  map.book
              (~(del by map.book) nonce)
            ?:  =([~ ~] book)
              (~(del by visiting.auth) host)
            (~(put by visiting.auth) host book)
          ::
          ?@  u.port       [~ state]
          ?~  pend.u.port  [~ state]
          %^  return-static-data-on-duct(duct u.pend.u.port)  503  'text/html'
          (eauth-error-page ~)
        ::  +on-boon: receive an eauth network response from a host
        ::
        ::    crashes on unexpected circumstances, in response to which we
        ::    should abort the eauth attempt
        ::
        ++  on-boon
          |=  [host=ship boon=eauth-boon]
          ^-  [(list move) server-state]
          %-  (trace 2 |.("eauth: %{(trip +<.boon)} from {(scow %p host)}"))
          ?-  +<.boon
              %okay
            =/  book  (~(got by visiting.auth) host)
            =/  port  (~(got by map.book) nonce.boon)
            ?>  ?=(^ port)
            ?>  ?=(^ pend.port)
            ::  update the outgoing sessions map, deleting if we aborted
            ::
            =.  visiting.auth
              ?^  toke.port
                %+  ~(put by visiting.auth)  host
                :-  qeu.book
                ::NOTE  optimistic
                (~(put by map.book) nonce.boon now)
              =.  map.book
                (~(del by map.book) nonce.boon)
              ?:  =([~ ~] book)
                (~(del by visiting.auth) host)
              (~(put by visiting.auth) host book)
            ::  always serve a redirect, with either the token, or abort signal
            ::
            =;  url=@t
              %-  handle-response(duct u.pend.port)
              [%start 303^['location' url]~ ~ &]
            %+  rap  3
            :*  url.boon
                '?nonce='  (scot %uv nonce.boon)
                ?~  toke.port  ['&abort']~
                ~['&token=' (scot %uv u.toke.port)]
            ==
          ::
              %shut
            ::  the host has deleted the corresponding session
            ::
            =.  visiting.auth
              =/  book
                (~(gut by visiting.auth) host *logbook)
              =.  map.book
                (~(del by map.book) nonce.boon)
              ?:  =([~ ~] book)
                (~(del by visiting.auth) host)
              (~(put by visiting.auth) host book)
            [~ state]
          ==
        ::
        ++  expire
          |=  [host=ship nonce=@uv]
          ^-  [(list move) server-state]
          =/  book  (~(gut by visiting.auth) host *logbook)
          =/  port  (~(get by map.book) nonce)
          ::  if the attempt was completed, we don't expire it
          ::
          ?~  port    [~ state]
          ?@  u.port  [~ state]
          ::  delete pending attempts, serve response if needed
          ::
          %-  %+  trace  1
              |.("eauth: attempt into {(scow %p host)} expired")
          =.  visiting.auth
            =.  map.book
              (~(del by map.book) nonce)
            ?:  =([~ ~] book)
              (~(del by visiting.auth) host)
            (~(put by visiting.auth) host book)
          ::
          ?~  pend.u.port  [~ state]
          %^  return-static-data-on-duct(duct u.pend.u.port)  503  'text/html'
          (eauth-error-page ~)
        ::
        ++  send-plea
          |=  [=ship plea=eauth-plea]
          ^-  move
          ::NOTE  no nonce in the wire, to avoid proliferating flows
          =/  =wire  /eauth/plea/(scot %p ship)
          %-  (trace 2 |.("eauth: {(trip +<.plea)} into {(scow %p ship)}"))
          [[/eyre/eauth/synthetic]~ %pass wire %a %plea ship %e /eauth/0 plea]
        ::
        ++  confirmation-page
          |=  [server=ship nonce=@uv]
          ^-  octs
          %-  as-octs:mimes:html
          %-  crip
          %-  en-xml:html
          =/  favicon  %+
            weld  "<svg width='10' height='10' viewBox='0 0 10 10' xmlns='http://www.w3.org/2000/svg'>"
                  "<circle r='3.09' cx='5' cy='5' /></svg>"
          ;html
            ;head
              ;meta(charset "utf-8");
              ;meta(name "viewport", content "width=device-width, initial-scale=1, shrink-to-fit=no");
              ;link(rel "icon", type "image/svg+xml", href (weld "data:image/svg+xml;utf8," favicon));
              ;title:"Urbit"
              ;style:"{(trip auth-styling)}"
              ;style:'''
                     form {
                       border: 1px solid var(--black20);
                       border-radius: 4px;
                       padding: 1rem;
                       align-items: stretch;
                       font-size: 14px;
                     }
                     .red {
                       background: var(--black05) !important;
                       color: var(--black60) !important;
                       border: 1px solid var(--black60) !important;
                     }
                     code {
                       font-weight: bold;
                       font-family: "Source Code Pro", monospace;
                     }
                     button {
                       display: inline-block;
                     }
                     '''
            ==
            ;body
              ;form(action "/~/eauth", method "post")
                ; Hello, {(scow %p our)}.
                ; You are trying to log in to:
                ;code:"{(scow %p server)}"
                ;input(type "hidden", name "server", value (scow %p server));
                ;input(type "hidden", name "nonce", value (scow %uv nonce));
                ;button(type "submit", name "grant", value "grant"):"approve"
                ;button(type "submit", name "reject", class "red"):"reject"
              ==
            ==
          ==
        --
      ::  +on-request: http request to the /~/eauth endpoint
      ::
      ++  on-request
        |=  [[session-id=@uv =identity] =request:http]
        ^-  [(list move) server-state]
        ::  we may need the requester to log in before proceeding
        ::
        =*  login
          =;  url=@t  (handle-response %start 303^['location' url]~ ~ &)
          %^  cat  3  '/~/login?redirect='
          (crip (en-urlt:html (trip url.request)))
        ::  or give them a generic, static error page in unexpected cases
        ::
        =*  error  %^  return-static-data-on-duct  400  'text/html'
                   (eauth-error-page ~)
        ::  GET requests either render the confirmation page,
        ::  or finalize an eauth flow
        ::
        ?:  ?=(%'GET' method.request)
          =/  args=(map @t @t)  (malt args:(parse-request-line url.request))
          =/  server=(unit @p)  (biff (~(get by args) 'server') (cury slaw %p))
          =/  nonce=(unit @uv)  (biff (~(get by args) 'nonce') (cury slaw %uv))
          =/  token=(unit @uv)  (biff (~(get by args) 'token') (cury slaw %uv))
          =/  abort=?           (~(has by args) 'abort')
          ::
          ?~  nonce  error
          ::
          ?^  server
            ::  request for confirmation page
            ::
            ?.  ?=(%ours -.identity)  login
            =/  book  (~(gut by visiting.auth) u.server *logbook)
            =/  door  (~(get by map.book) u.nonce)
            ?~  door
              ::  nonce not yet used, render the confirmation page as normal
              ::
              %^  return-static-data-on-duct  200  'text/html'
              (confirmation-page:client u.server u.nonce)
            ::  if we're still awaiting a redirect target, we choose to serve
            ::  this latest request instead
            ::
            ?@  u.door         error
            ?~  pend.u.door    error
            =.  map.book       (~(put by map.book) u.nonce u.door(pend `duct))
            =.  visiting.auth  (~(put by visiting.auth) u.server book)
            %-  return-static-data-on-duct(duct u.pend.u.door)
            [202 'text/plain' (as-octs:mimes:html 'continued elsewhere...')]
          ::  important to provide an error response for unexpected states
          ::
          =/  visa=(unit visitor)  (~(get by visitors.auth) u.nonce)
          ?~  visa         error
          ?@  +.u.visa     error
          =*  error  %^  return-static-data-on-duct  400  'text/html'
                     (eauth-error-page %server last.u.visa)
          ::  request for finalization, must either abort or provide a token
          ::
          ::NOTE  yes, this means that unauthenticated clients can abort
          ::      any eauth attempt they know the nonce for, but that should
          ::      be pretty benign
          ?:  abort  (cancel:^server u.nonce last.u.visa)
          ?~  token  error
          ::  if this request provides a token, but the client didn't, complain
          ::
          ?~  toke.u.visa  error
          ::  verify the request
          ::
          ?.  =(u.token u.toke.u.visa)
            %-  (trace 1 |.("eauth: token mismatch"))
            error
          ?~  duct.u.visa  error
          (finalize:^server u.duct.u.visa u.nonce ship.u.visa last.u.visa)
        ::
        ?.  ?=(%'POST' method.request)
          %^  return-static-data-on-duct  405  'text/html'
          (eauth-error-page ~)
        ?.  =(%ours -.identity)  login
        ::  POST requests are always submissions of the confirmation page
        ::
        =/  args=(map @t @t)
          (malt (fall (rush q:(fall body.request *octs) yquy:de-purl:html) ~))
        =/  server=(unit @p)  (biff (~(get by args) 'server') (cury slaw %p))
        =/  nonce=(unit @uv)  (biff (~(get by args) 'nonce') (cury slaw %uv))
        =/  grant=?           =(`'grant' (~(get by args) 'grant'))
        ::
        =*  error   %^  return-static-data-on-duct  400  'text/html'
                    (eauth-error-page ~)
        ?~  server  error
        ?~  nonce   error
        =/  book    (~(gut by visiting.auth) u.server *logbook)
        ?:  (~(has by map.book) u.nonce)  error
        (start:client u.server u.nonce grant)
      ::
      ++  eauth-url
        ^-  (unit @t)
        =/  end=(unit @t)  (clap user.endpoint.auth auth.endpoint.auth head)
        ?~  end  ~
        `(cat 3 u.end '/~/eauth')
      ::
      ++  start-timeout
        |=  =path
        ^-  move
        [duct %pass [%eauth %expire path] %b %wait (add now ~m5)]
      --
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
      |=  [[session-id=@uv =identity] =address =request:http]
      ^-  [(list move) server-state]
      ::  parse out the path key the subscription is on
      ::
      =+  request-line=(parse-request-line url.request)
      ?.  ?=([@t @t @t ~] site.request-line)
        ::  url is not of the form '/~/channel/'
        ::
        %^  return-static-data-on-duct  400  'text/html'
        (error-page 400 & url.request "malformed channel url")
      ::  channel-id: unique channel id parsed out of url
      ::
      =+  channel-id=i.t.t.site.request-line
      ::
      ?:  =('PUT' method.request)
        ::  PUT methods starts/modifies a channel, and returns a result immediately
        ::
        (on-put-request channel-id identity request)
      ::
      ?:  =('GET' method.request)
        (on-get-request channel-id [session-id identity] request)
      ?:  =('POST' method.request)
        ::  POST methods are used solely for deleting channels
        (on-put-request channel-id identity request)
      ::
      ((trace 0 |.("session not a put")) `state)
    ::  +on-cancel-request: cancels an ongoing subscription
    ::
    ::    One of our long lived sessions just got closed. We put the associated
    ::    session back into the waiting state.
    ::
    ++  on-cancel-request
      ^-  [(list move) server-state]
      ::  lookup the session id by duct
      ::
      %-  (trace 1 |.("{<duct>} moving channel to waiting state"))
      ::
      ?~  maybe-channel-id=(~(get by duct-to-key.channel-state.state) duct)
        ((trace 0 |.("{<duct>} no channel to move")) `state)
      ::
      =/  maybe-session
        (~(get by session.channel-state.state) u.maybe-channel-id)
      ?~  maybe-session
        ((trace 1 |.("{<maybe-session>} session doesn't exist")) `state)
      ::
      =/  heartbeat-cancel=(list move)
        ?~  heartbeat.u.maybe-session  ~
        :~  %^  cancel-heartbeat-move
              u.maybe-channel-id
            date.u.heartbeat.u.maybe-session
          duct.u.heartbeat.u.maybe-session
        ==
      ::
      =/  expiration-time=@da  (add now channel-timeout)
      ::
      :-  %+  weld  heartbeat-cancel
        [(set-timeout-move u.maybe-channel-id expiration-time) moves]
      %_    state
          session.channel-state
        %+  ~(jab by session.channel-state.state)  u.maybe-channel-id
        |=  =channel
        ::  if we are canceling a known channel, it should have a listener
        ::
        ?>  ?=([%| *] state.channel)
        channel(state [%& [expiration-time duct]], heartbeat ~)
      ::
          duct-to-key.channel-state
        (~(del by duct-to-key.channel-state.state) duct)
      ==
    ::  +update-timeout-timer-for: sets a timeout timer on a channel
    ::
    ::    This creates a channel if it doesn't exist, cancels existing timers
    ::    if they're already set (we cannot have duplicate timers), and (if
    ::    necessary) moves channels from the listening state to the expiration
    ::    state.
    ::
    ++  update-timeout-timer-for
      |=  [mode=?(%json %jam) =identity channel-id=@t]
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
          [mode identity [%& expiration-time duct] 0 now ~ ~ ~ ~]
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
    ++  set-heartbeat-move
      |=  [channel-id=@t heartbeat-time=@da]
      ^-  move
      :^  duct  %pass  /channel/heartbeat/[channel-id]
      [%b %wait heartbeat-time]
    ::
    ++  cancel-heartbeat-move
      |=  [channel-id=@t heartbeat-time=@da =^duct]
      ^-  move
      :^  duct  %pass  /channel/heartbeat/[channel-id]
      [%b %rest heartbeat-time]
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
    ::    GET requests connect to a channel for the server to send events to
    ::    the client in text/event-stream format.
    ::
    ++  on-get-request
      |=  [channel-id=@t [session-id=@uv =identity] =request:http]
      ^-  [(list move) server-state]
      ::  if the channel doesn't exist, we cannot serve it.
      ::  this 404 also lets clients know if their channel was reaped since
      ::  they last connected to it.
      ::
      ?.  (~(has by session.channel-state.state) channel-id)
        %^  return-static-data-on-duct  404  'text/html'
        (error-page 404 | url.request ~)
      ::
      =/  mode=?(%json %jam)
        (find-channel-mode %'GET' header-list.request)
      =^  [exit=? =wall moves=(list move)]  state
        ::  the request may include a 'Last-Event-Id' header
        ::
        =/  maybe-last-event-id=(unit @ud)
          ?~  maybe-raw-header=(get-header:http 'last-event-id' header-list.request)
            ~
          (rush u.maybe-raw-header dum:ag)
        =/  channel
          (~(got by session.channel-state.state) channel-id)
        ::  we put some demands on the get request, and may need to do some
        ::  cleanup for prior requests.
        ::
        ::  find the channel creator's identity, make sure it matches
        ::
        ?.  =(identity identity.channel)
          =^  mos  state
            %^  return-static-data-on-duct  403  'text/html'
            (error-page 403 | url.request ~)
          [[& ~ mos] state]
        ::  make sure the request "mode" doesn't conflict with a prior request
        ::
        ::TODO  or could we change that on the spot, given that only a single
        ::      request will ever be listening to this channel?
        ?.  =(mode mode.channel)
          =^  mos  state
            %^  return-static-data-on-duct  406  'text/html'
            =;  msg=tape  (error-page 406 %.y url.request msg)
            "channel already established in {(trip mode.channel)} mode"
          [[& ~ mos] state]
        ::  when opening an event-stream, we must cancel our timeout timer
        ::  if there's no duct already bound. else, kill the old request,
        ::  we will replace its duct at the end of this arm
        ::
        =^  cancel-moves  state
          ?:  ?=([%& *] state.channel)
            :_  state
            (cancel-timeout-move channel-id p.state.channel)^~
          =.  duct-to-key.channel-state.state
            (~(del by duct-to-key.channel-state.state) p.state.channel)
          =/  cancel-heartbeat
            ?~  heartbeat.channel  ~
            :_  ~
            %+  cancel-heartbeat-move  channel-id
            [date duct]:u.heartbeat.channel
          =-  [(weld cancel-heartbeat -<) ->]
          (handle-response(duct p.state.channel) [%cancel ~])
        ::  flush events older than the passed in 'Last-Event-ID'
        ::
        =?  state  ?=(^ maybe-last-event-id)
          (acknowledge-events channel-id u.maybe-last-event-id)
        ::TODO  that did not remove them from the channel queue though!
        ::      we may want to account for maybe-last-event-id, for efficiency.
        ::      (the client _should_ ignore events it heard previously if we do
        ::      end up re-sending them, but _requiring_ that feels kinda risky)
        ::
        ::  combine the remaining queued events to send to the client
        ::
        =;  event-replay=wall
          [[| - cancel-moves] state]
        %-  zing
        %-  flop
        =/  queue  events.channel
        =|  events=(list wall)
        |-
        ^+  events
        ?:  =(~ queue)
          events
        =^  head  queue  ~(get to queue)
        =,  p.head
        ::NOTE  these will only fail if the mark and/or json types changed,
        ::      since conversion failure also gets caught during first receive.
        ::      we can't do anything about this, so consider it unsupported.
        =/  said
          (channel-event-to-tape channel request-id channel-event)
        ?~  said  $
        $(events [(event-tape-to-wall id +.u.said) events])
      ?:  exit  [moves state]
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
            (wall-to-octs wall)
            complete=%.n
        ==
      ::  associate this duct with this session key
      ::
      =.  duct-to-key.channel-state.state
        (~(put by duct-to-key.channel-state.state) duct channel-id)
      ::  associate this channel with the session cookie
      ::
      =.  sessions.auth.state
        %+  ~(jab by sessions.auth.state)
          session-id
        |=  =session
        session(channels (~(put in channels.session) channel-id))
      ::  initialize sse heartbeat
      ::
      =/  heartbeat-time=@da  (add now ~s20)
      =/  heartbeat  (set-heartbeat-move channel-id heartbeat-time)
      ::  record the mode & duct for future output,
      ::  and record heartbeat-time for possible future cancel
      ::
      =.  session.channel-state.state
        %+  ~(jab by session.channel-state.state)  channel-id
        |=  =channel
        %_  channel
          mode       mode
          state      [%| duct]
          heartbeat  (some [heartbeat-time duct])
        ==
      ::
      [[heartbeat :(weld http-moves moves)] state]
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
        =^  acked  events.channel
          (prune-events events.channel last-event-id)
        =.  unacked.channel
          (subtract-acked-events acked unacked.channel)
        channel(last-ack now)
      ==
    ::  +on-put-request: handles a PUT request
    ::
    ::    PUT requests send commands from the client to the server. We receive
    ::    a set of commands in JSON format in the body of the message.
    ::    channels don't exist until a PUT request is sent. it's valid for
    ::    this request to contain an empty list of commands.
    ::
    ++  on-put-request
      |=  [channel-id=@t =identity =request:http]
      ^-  [(list move) server-state]
      ::  if the channel already exists, and is not of this identity, 403
      ::
      ::    the creation case happens in the +update-timeout-timer-for below
      ::
      ?:  ?~  c=(~(get by session.channel-state.state) channel-id)  |
          !=(identity identity.u.c)
        %^  return-static-data-on-duct  403  'text/html'
        (error-page 403 | url.request ~)
      ::  error when there's no body
      ::
      ?~  body.request
        %^  return-static-data-on-duct  400  'text/html'
        (error-page 400 %.y url.request "no put body")
      ::
      =/  mode=?(%json %jam)
        (find-channel-mode %'PUT' header-list.request)
      ::  if we cannot parse requests from the body, give an error
      ::
      =/  maybe-requests=(each (list channel-request) @t)
        (parse-channel-request mode u.body.request)
      ?:  ?=(%| -.maybe-requests)
        %^  return-static-data-on-duct  400  'text/html'
        (error-page 400 & url.request (trip p.maybe-requests))
      ::  check for the existence of the channel-id
      ::
      ::    if we have no session, create a new one set to expire in
      ::    :channel-timeout from now. if we have one which has a timer, update
      ::    that timer.
      ::
      =.  ..on-put-request  (update-timeout-timer-for mode identity channel-id)
      ::  for each request, execute the action passed in
      ::
      =+  requests=p.maybe-requests
      ::  gall-moves: put moves here first so we can flop for ordering
      ::  errors: if we accumulate any, discard the gall-moves and revert
      ::
      =|  gall-moves=(list move)
      =|  errors=(map @ud @t)
      =/  og-state  state
      =/  from=ship
        ?+(-.identity who.identity %ours our)
      |-
      ::
      ?~  requests
        ?:  =(~ errors)
          ::  everything succeeded, mark the request as completed
          ::
          =^  http-moves  state
            %-  handle-response
            :*  %start
                [status-code=204 headers=~]
                data=~
                complete=%.y
            ==
          ::
          [:(weld (flop gall-moves) http-moves moves) state]
        ::  some things went wrong. revert all operations & give 400
        ::
        %-  (trace 1 |.("{<channel-id>} reverting due to errors"))
        =.  state  og-state
        =^  http-moves  state
          %^  return-static-data-on-duct  400  'text/html'
          %-  as-octs:mimes:html
          %+  rap  3
          %+  turn  (sort ~(tap by errors) dor)
          |=  [id=@ud er=@t]
          (rap 3 (crip (a-co:co id)) ': ' er '<br/>' ~)
        [(weld http-moves moves) state]
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
          ?(%poke %poke-json)
        =,  i.requests
        ::
        ?.  |(=(from our) =(ship our))
          =+  [request-id 'non-local operation']
          $(errors (~(put by errors) -), requests t.requests)
        ::
        =.  gall-moves
          =/  =wire  /channel/poke/[channel-id]/(scot %ud request-id.i.requests)
          :_  gall-moves
          ^-  move
          %+  deal-as
            /channel/poke/[channel-id]/(scot %ud request-id)
          :^  from  ship  app
          ^-  task:agent:gall
          :+  %poke-as  mark
          ?-  -.i.requests
            %poke       [%noun !>(noun)]
            %poke-json  [%json !>(json)]
          ==
        ::
        $(requests t.requests)
      ::
          %subscribe
        =,  i.requests
        ::
        ?.  |(=(from our) =(ship our))
          =+  [request-id 'non-local operation']
          $(errors (~(put by errors) -), requests t.requests)
        ::
        ::TODO  could error if the subscription is a duplicate
        =.  gall-moves
          :_  gall-moves
          ^-  move
          %-  (trace 1 |.("subscribing to {<app>} on {<path>}"))
          %+  deal-as
            (subscription-wire channel-id request-id from ship app)
          [from ship app %watch path]
        ::
        =.  session.channel-state.state
          %+  ~(jab by session.channel-state.state)  channel-id
          |=  =channel
          =-  channel(subscriptions -)
          %+  ~(put by subscriptions.channel)
            request-id
          [ship app path duct]
        ::
        $(requests t.requests)
      ::
          %unsubscribe
        =,  i.requests
        ::
        ?.  |(=(from our) =(ship our))
          =+  [request-id 'non-local operation']
          $(errors (~(put by errors) -), requests t.requests)
        ::
        =/  usession  (~(get by session.channel-state.state) channel-id)
        ?~  usession
          $(requests t.requests)
        =/  subscriptions  subscriptions:u.usession
        ::
        ?~  maybe-subscription=(~(get by subscriptions) subscription-id)
          ::  the client sent us a weird request referring to a subscription
          ::  which isn't active.
          ::
          %.  $(requests t.requests)
          =*  msg=tape  "{(trip channel-id)} {<subscription-id>}"
          (trace 0 |.("missing subscription in unsubscribe {msg}"))
        ::
        =.  gall-moves
          :_  gall-moves
          ^-  move
          =,  u.maybe-subscription
          %-  (trace 1 |.("leaving subscription to {<app>}"))
          %+  deal-as
            (subscription-wire channel-id subscription-id from ship app)
          [from ship app %leave ~]
        ::
        =.  session.channel-state.state
          %+  ~(jab by session.channel-state.state)  channel-id
          |=  =channel
          %_  channel
            subscriptions  (~(del by subscriptions.channel) subscription-id)
            unacked        (~(del by unacked.channel) subscription-id)
          ==
        ::
        $(requests t.requests)
      ::
          %delete
        %-  (trace 1 |.("{<channel-id>} discarding due to %delete PUT"))
        =^  moves  state
          (discard-channel channel-id |)
        =.  gall-moves
          (weld gall-moves moves)
        $(requests t.requests)
      ::
      ==
    ::  +on-gall-response: sanity-check a gall response, send as event
    ::
    ++  on-gall-response
      |=  [channel-id=@t request-id=@ud extra=wire =sign:agent:gall]
      ^-  [(list move) server-state]
      ::  if the channel doesn't exist, we should clean up subscriptions
      ::
      ::    this is a band-aid solution. you really want eyre to have cleaned
      ::    these up on-channel-delete in the first place.
      ::    until the source of that bug is discovered though, we keep this
      ::    in place to ensure a slightly tidier home.
      ::
      ?.  ?&  !(~(has by session.channel-state.state) channel-id)
              ?=(?(%fact %watch-ack) -.sign)
              ?=([@ @ *] extra)
          ==
        (emit-event channel-id request-id sign)
      =/  =ship     (slav %p i.extra)
      =*  app=term  i.t.extra
      =*  msg=tape  "{(trip channel-id)} {(trip app)}"
      %-  (trace 0 |.("removing watch for non-existent channel {msg}"))
      :_  state
      :_  ~
      ^-  move
      =/  [as=@p old=?]
        ?+  t.t.extra  ~|([%strange-wire extra] !!)
          ~      [our &]
          [@ ~]  [(slav %p i.t.t.extra) |]
        ==
      =/  =wire  (subscription-wire channel-id request-id as ship app)
      %+  deal-as
        ::NOTE  we previously used a wire format that had the local identity
        ::      implicit, instead of explicit at the end of the wire. if we
        ::      detect we used the old wire here, we must re-use that format
        ::      (without id in the wire) for sending the %leave.
        ?:(old (snip wire) wire)
      [as ship app %leave ~]
    ::  +emit-event: records an event occurred, possibly sending to client
    ::
    ::    When an event occurs, we need to record it, even if we immediately
    ::    send it to a connected browser so in case of disconnection, we can
    ::    resend it.
    ::
    ::    This function is responsible for taking the event sign and converting
    ::    it into a text/event-stream. The :sign then may get sent, and is
    ::    stored for later resending until acknowledged by the client.
    ::
    ++  emit-event
      |=  [channel-id=@t request-id=@ud =sign:agent:gall]
      ^-  [(list move) server-state]
      ::
      =/  channel=(unit channel)
        (~(get by session.channel-state.state) channel-id)
      ?~  channel
        :_  state  :_  ~
        [duct %pass /flog %d %flog %crud %eyre-no-channel >id=channel-id< ~]
      ::  it's possible that this is a sign emitted directly alongside a fact
      ::  that triggered a clog & closed the subscription. in that case, just
      ::  drop the sign.
      ::  poke-acks are not paired with subscriptions, so we can process them
      ::  regardless.
      ::
      ?:  ?&  !?=(%poke-ack -.sign)
              !(~(has by subscriptions.u.channel) request-id)
          ==
        [~ state]
      ::  attempt to convert the sign to json.
      ::  if conversion succeeds, we *can* send it. if the client is actually
      ::  connected, we *will* send it immediately.
      ::
      =/  maybe-channel-event=(unit channel-event)
        (sign-to-channel-event sign u.channel request-id)
      ?~  maybe-channel-event  [~ state]
      =/  =channel-event  u.maybe-channel-event
      =/  said=(unit (quip move tape))
        (channel-event-to-tape u.channel request-id channel-event)
      =?  moves  ?=(^ said)
        (weld moves -.u.said)
      =*  sending  &(?=([%| *] state.u.channel) ?=(^ said))
      ::
      =/  next-id  next-id.u.channel
      ::  if we can send it, store the event as unacked
      ::
      =?  events.u.channel  ?=(^ said)
        %-  ~(put to events.u.channel)
        [next-id request-id channel-event]
      ::  if it makes sense to do so, send the event to the client
      ::
      =?  moves  sending
        ^-  (list move)
        :_  moves
        ::NOTE  assertions in this block because =* is flimsy
        ?>  ?=([%| *] state.u.channel)
        :+  p.state.u.channel  %give
        ^-  gift
        :*  %response  %continue
        ::
            ^=  data
            %-  wall-to-octs
            (event-tape-to-wall next-id +:(need said))
        ::
            complete=%.n
        ==
      =?  next-id  ?=(^ said)  +(next-id)
      ::  update channel's unacked counts, find out if clogged
      ::
      =^  clogged  unacked.u.channel
        ::  only apply clog logic to facts.
        ::  and of course don't count events we can't send as unacked.
        ::
        ?:  ?|  !?=(%fact -.sign)
                ?=(~ said)
            ==
          [| unacked.u.channel]
        =/  num=@ud
          (~(gut by unacked.u.channel) request-id 0)
        :_  (~(put by unacked.u.channel) request-id +(num))
        ?&  (gte num clog-threshold)
            (lth (add last-ack.u.channel clog-timeout) now)
        ==
      ::  if we're clogged, or we ran into an event we can't serialize,
      ::  kill this gall subscription.
      ::
      =*  msg=tape  "on {(trip channel-id)} for {(scow %ud request-id)}"
      =/  kicking=?
        ?:  clogged
          ((trace 0 |.("clogged {msg}")) &)
        ?.  ?=(~ said)  |
        ((trace 0 |.("can't serialize event, kicking {msg}")) &)
      =?  moves      kicking
        :_  moves
        ::NOTE  this shouldn't crash because we
        ::      - never fail to serialize subscriptionless signs (%poke-ack),
        ::      - only clog on %facts, which have a subscription associated,
        ::      - and already checked whether we still have that subscription.
        =+  (~(got by subscriptions.u.channel) request-id)
        %-  (trace 1 |.("leaving subscription to {<app>}"))
        %+  deal-as
          (subscription-wire channel-id request-id identity.u.channel ship app)
        [identity.u.channel ship app %leave ~]
      ::  update channel state to reflect the %kick
      ::
      =?  u.channel  kicking
        %_  u.channel
          subscriptions  (~(del by subscriptions.u.channel) request-id)
          unacked        (~(del by unacked.u.channel) request-id)
          events         %-  ~(put to events.u.channel)
                         :+  next-id
                           request-id
                         (need (sign-to-channel-event [%kick ~] u.channel request-id))
        ==
      ::  if a client is connected, send the kick event to them
      ::
      =?  moves  &(kicking ?=([%| *] state.u.channel))
        :_  moves
        :+  p.state.u.channel  %give
        ^-  gift
        :*  %response  %continue
        ::
            ^=  data
            %-  wall-to-octs
            %+  event-tape-to-wall  next-id
            +:(need (channel-event-to-tape u.channel request-id %kick ~))
        ::
            complete=%.n
        ==
      =?  next-id   kicking  +(next-id)
      ::
      :-  (flop moves)
      %_    state
          session.channel-state
        %+  ~(put by session.channel-state.state)  channel-id
        u.channel(next-id next-id)
      ==
    ::  +sign-to-channel-event: strip the vase from a sign:agent:gall
    ::
    ++  sign-to-channel-event
      |=  [=sign:agent:gall =channel request-id=@ud]
      ^-  (unit channel-event)
      ?.  ?=(%fact -.sign)  `sign
      ?~  desk=(app-to-desk channel request-id)  ~
      :-  ~
      [%fact u.desk [p q.q]:cage.sign]
    ::  +app-to-desk
    ::
    ++  app-to-desk
      |=  [=channel request-id=@ud]
      ^-  (unit desk)
      =/  sub  (~(get by subscriptions.channel) request-id)
      ?~  sub
        ((trace 0 |.("no subscription for request-id {(scow %ud request-id)}")) ~)
      =/  des=(unit (unit cage))
        (rof ~ /eyre %gd [our app.u.sub da+now] /$)
      ?.  ?=([~ ~ *] des)
        ((trace 0 |.("no desk for app {<app.u.sub>}")) ~)
      `!<(=desk q.u.u.des)
    ::  +channel-event-to-tape: render channel-event from request-id in specified mode
    ::
    ++  channel-event-to-tape
      |=  [=channel request-id=@ud =channel-event]
      ^-  (unit (quip move tape))
      ?-  mode.channel
        %json  %+  bind  (channel-event-to-json channel request-id channel-event)
               |=((quip move json) [+<- (trip (en:json:html +<+))])
        %jam   =-  `[~ (scow %uw (jam -))]
               [request-id channel-event]
      ==
    ::  +channel-event-to-json: render channel event as json channel event
    ::
    ++  channel-event-to-json
      ~%  %eyre-channel-event-to-json  ..part  ~
      |=  [=channel request-id=@ud event=channel-event]
      ^-  (unit (quip move json))
      ::  for facts, we try to convert the result to json
      ::
      =/  [from=(unit [=desk =mark]) jsyn=(unit sign:agent:gall)]
        ?.  ?=(%fact -.event)       [~ `event]
        ?:  ?=(%json mark.event)
          ?~  jsin=((soft json) noun.event)
            %.  [~ ~]
            (slog leaf+"eyre: dropping fake json for {(scow %ud request-id)}" ~)
          [~ `[%fact %json !>(u.jsin)]]
        ::  find and use tube from fact mark to json
        ::
        ::
        =*  have=mark  mark.event
        =/  convert=(unit vase)
          =/  cag=(unit (unit cage))
            (rof ~ /eyre %cf [our desk.event da+now] /[have]/json)
          ?.  ?=([~ ~ *] cag)  ~
          `q.u.u.cag
        ?~  convert
          ((trace 0 |.("no convert from {(trip have)} to json")) [~ ~])
        ~|  "conversion failed from {(trip have)} to json"
        [`[desk.event have] `[%fact %json (slym u.convert noun.event)]]
      ?~  jsyn  ~
      %-  some
      :-  ?~  from  ~
          :_  ~
          :^  duct  %pass  /conversion-cache/[mark.u.from]
          [%c %warp our desk.u.from `[%sing %f da+now /[mark.u.from]/json]]
      =*  sign  u.jsyn
      =,  enjs:format
      %-  pairs
      ^-  (list [@t json])
      :-  ['id' (numb request-id)]
      ?-    -.sign
          %poke-ack
        :~  ['response' [%s 'poke']]
          ::
            ?~  p.sign
              ['ok' [%s 'ok']]
            ['err' (wall (render-tang-to-wall 100 u.p.sign))]
        ==
      ::
          %fact
        :+  ['response' [%s 'diff']]
          :-  'json'
          ~|  [%unexpected-fact-mark p.cage.sign]
          ?>  =(%json p.cage.sign)
          !<(json q.cage.sign)
        ::
        ?~  from  ~
        ['mark' [%s mark.u.from]]~
      ::
          %kick
        ['response' [%s 'quit']]~
      ::
          %watch-ack
        :~  ['response' [%s 'subscribe']]
          ::
            ?~  p.sign
              ['ok' [%s 'ok']]
            ['err' (wall (render-tang-to-wall 100 u.p.sign))]
        ==
      ==
    ::
    ++  event-tape-to-wall
      ~%  %eyre-tape-to-wall  ..part  ~
      |=  [event-id=@ud =tape]
      ^-  wall
      :~  (weld "id: " (format-ud-as-integer event-id))
          (weld "data: " tape)
          ""
      ==
    ::
    ++  on-channel-heartbeat
      |=  channel-id=@t
      ^-  [(list move) server-state]
      ::
      =/  res
        %-  handle-response
        :*  %continue
            data=(some (as-octs:mimes:html ':\0a'))
            complete=%.n
        ==
      =/  http-moves  -.res
      =/  new-state  +.res
      =/  heartbeat-time=@da  (add now ~s20)
      :_  %_    new-state
              session.channel-state
            %+  ~(jab by session.channel-state.state)  channel-id
            |=  =channel
            channel(heartbeat (some [heartbeat-time duct]))
          ==
      (snoc http-moves (set-heartbeat-move channel-id heartbeat-time))
    ::  +discard-channel: remove a channel from state
    ::
    ::    cleans up state, timers, and gall subscriptions of the channel
    ::
    ++  discard-channel
      |=  [channel-id=@t expired=?]
      ^-  [(list move) server-state]
      ::
      =/  usession=(unit channel)
        (~(get by session.channel-state.state) channel-id)
      ?~  usession
        [~ state]
      =/  session=channel  u.usession
      ::
      :_  %_    state
              session.channel-state
            (~(del by session.channel-state.state) channel-id)
          ::
              duct-to-key.channel-state
            ?.  ?=(%| -.state.session)  duct-to-key.channel-state.state
            (~(del by duct-to-key.channel-state.state) p.state.session)
          ==
      =/  heartbeat-cancel=(list move)
        ?~  heartbeat.session  ~
        :~  %^  cancel-heartbeat-move
              channel-id
            date.u.heartbeat.session
          duct.u.heartbeat.session
        ==
      =/  expire-cancel=(list move)
        ?:  expired  ~
        ?.  ?=(%& -.state.session)  ~
        =,  p.state.session
        [(cancel-timeout-move channel-id date duct)]~
      %+  weld  heartbeat-cancel
      %+  weld  expire-cancel
      ::  produce a list of moves which cancels every gall subscription
      ::
      %+  turn  ~(tap by subscriptions.session)
      |=  [request-id=@ud ship=@p app=term =path duc=^duct]
      ^-  move
      %-  (trace 1 |.("{<channel-id>} leaving subscription to {<app>}"))
      %+  deal-as
        (subscription-wire channel-id request-id identity.session ship app)
      [identity.session ship app %leave ~]
    --
  ::  +handle-gall-error: a call to +poke-http-response resulted in a %coup
  ::
  ++  handle-gall-error
    |=  =tang
    ^-  [(list move) server-state]
    ::
    ?~  connection-state=(~(get by connections.state) duct)
      %.  `state
      (trace 0 |.("{<duct>} error on invalid outstanding connection"))
    =*  connection  u.connection-state
    =/  moves-1=(list move)
      ?.  ?=(%app -.action.connection)
        ~
      :_  ~
      =,  connection
      %-  (trace 1 |.("leaving subscription to {<app.action>}"))
      (deal-as /watch-response/[eyre-id] identity our app.action %leave ~)
    ::
    =^  moves-2  state
      %^  return-static-data-on-duct  500  'text/html'
      ::
      %-  internal-server-error  :*
          authenticated.inbound-request.connection
          url.request.inbound-request.connection
          tang
      ==
    [(weld moves-1 moves-2) state]
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
      ((trace 0 |.("{<duct>} invalid outstanding connection")) `state)
    ::
    |^  ^-  [(list move) server-state]
        ::
        ?-    -.http-event
        ::
            %start
          ?^  response-header.u.connection-state
            ((trace 0 |.("{<duct>} error multiple start")) error-connection)
          ::  extend the request's session's + cookie's life
          ::
          =^  response-header  sessions.auth.state
            =,  authentication
            =*  session-id  session-id.u.connection-state
            =*  sessions    sessions.auth.state
            =*  inbound     inbound-request.u.connection-state
            =*  headers     headers.response-header.http-event
            ::
            ?.  (~(has by sessions) session-id)
              ::  if the session has expired since the request was opened,
              ::  tough luck, we don't create/revive sessions here
              ::
              [response-header.http-event sessions]
            :_  %+  ~(jab by sessions)  session-id
                |=  =session
                session(expiry-time (add now session-timeout))
            =-  response-header.http-event(headers -)
            =/  cookie=(pair @t @t)
              ['set-cookie' (session-cookie-string session-id &)]
            |-
            ?~  headers
              [cookie ~]
            ?:  &(=(key.i.headers p.cookie) =(value.i.headers q.cookie))
              headers
            [i.headers $(headers t.headers)]
          ::
          =*  connection  u.connection-state
          ::
          ::  if the request was a simple cors request from an approved origin
          ::  append the necessary cors headers to the response
          ::
          =/  origin=(unit origin)
            %+  get-header:http  'origin'
            header-list.request.inbound-request.connection
          =?  headers.response-header
              ?&  ?=(^ origin)
                  (~(has in approved.cors-registry.state) u.origin)
              ==
            %^  set-header:http  'Access-Control-Allow-Origin'       u.origin
            %^  set-header:http  'Access-Control-Allow-Credentials'  'true'
            headers.response-header
          ::
          =.  response-header.http-event  response-header
          =.  connections.state
            ?:  complete.http-event
              ::  XX  optimize by not requiring +put:by in +request
              ::
              (~(del by connections.state) duct)
            ::
            %-  (trace 2 |.("{<duct>} start"))
            %+  ~(put by connections.state)  duct
            %=  connection
              response-header  `response-header
              bytes-sent  ?~(data.http-event 0 p.u.data.http-event)
            ==
          ::
          pass-response
        ::
            %continue
          ?~  response-header.u.connection-state
            %.  error-connection
            (trace 0 |.("{<duct>} error continue without start"))
          ::
          =.  connections.state
            ?:  complete.http-event
              %-  (trace 2 |.("{<duct>} completed"))
              (~(del by connections.state) duct)
            ::
            %-  (trace 2 |.("{<duct>} continuing"))
            ?~  data.http-event
              connections.state
            ::
            %+  ~(put by connections.state)  duct
            =*  size  p.u.data.http-event
            =*  conn  u.connection-state
            conn(bytes-sent (add size bytes-sent.conn))
          ::
          pass-response
        ::
            %cancel
          ::  todo: log this differently from an ise.
          ::
          ((trace 1 |.("cancel http event")) error-connection)
        ==
    ::
    ++  pass-response
      ^-  [(list move) server-state]
      [[duct %give %response http-event]~ state]
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
      :_  state
      :-  [duct %give %response %cancel ~]
      ?.  ?=(%app -.action.u.connection-state)
        ~
      :_  ~
      =,  u.connection-state
      %-  %+  trace  1
          |.("leaving subscription to {<app.action>}")
      (deal-as /watch-response/[eyre-id] identity our app.action %leave ~)
    --
  ::  +set-response: remember (or update) a cache mapping
  ::
  ++  set-response
    |=  [url=@t entry=(unit cache-entry)]
    ^-  [(list move) server-state]
    =/  aeon  ?^(prev=(~(get by cache.state) url) +(aeon.u.prev) 1)
    =.  cache.state  (~(put by cache.state) url [aeon entry])
    :_  state
    [outgoing-duct.state %give %grow /cache/(scot %ud aeon)/(scot %t url)]~
  ::  +add-binding: conditionally add a pairing between binding and action
  ::
  ::    Adds =binding =action if there is no conflicting bindings.
  ::
  ++  add-binding
    |=  [=binding =action]
    ^-  [(list move) server-state]
    =^  success  bindings.state
      ::  prevent binding in reserved namespaces
      ::
      ?:  ?|  ?=([%'~' *] path.binding)    ::  eyre
              ?=([%'~_~' *] path.binding)  ::  runtime
          ==
        [| bindings.state]
      [& (insert-binding [binding duct action] bindings.state)]
    :_  state
    [duct %give %bound & binding]~
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
    ^-  [=action suburl=@t]
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
    =?  parsed-url  ?=([%'~' %channel-jam *] parsed-url)
      parsed-url(i.t %channel)
    ::
    =/  bindings  bindings.state
    |-
    ::
    ?~  bindings
      [[%four-oh-four ~] url]
    ::
    ?.  (host-matches site.binding.i.bindings raw-host)
      $(bindings t.bindings)
    ?~  suffix=(find-suffix path.binding.i.bindings parsed-url)
      $(bindings t.bindings)
    ::
    :-  action.i.bindings
    %^  cat  3
      %+  roll
        ^-  (list @t)
        (join '/' (flop ['' u.suffix]))
      (cury cat 3)
    ?~  ext.request-line  ''
    (cat 3 '.' u.ext.request-line)
  ::  +give-session-tokens: send valid local session tokens to unix
  ::
  ++  give-session-tokens
    ^-  move
    :-  outgoing-duct.state
    :+  %give  %sessions
    %-  sy
    %+  murn  ~(tap by sessions.auth.state)
    |=  [sid=@uv session]
    ?.  ?=(%ours -.identity)  ~
    (some (scot %uv sid))
  ::  +new-session-key
  ::
  ++  new-session-key
    |-  ^-  @uv
    =/  candidate=@uv  (~(raw og (shas %session-key eny)) 128)
    ?.  (~(has by sessions.auth.state) candidate)
      candidate
    $(eny (shas %try-again candidate))
  ::
  ++  deal-as
    |=  [=wire identity=$@(@p identity) =ship =dude:gall =task:agent:gall]
    ^-  move
    =/  from=@p
      ?@  identity  identity
      ?+(-.identity who.identity %ours our)
    [duct %pass wire %g %deal [from ship /eyre] dude task]
  ::
  ++  trace
    |=  [pri=@ print=(trap tape)]
    ?:  (lth verb.state pri)  same
    (slog leaf+"eyre: {(print)}" ~)
  --
::
++  forwarded-params
  |=  =header-list:http
  ^-  (unit (list (map @t @t)))
  %+  biff
    (get-header:http 'forwarded' header-list)
  unpack-header:http
::
++  forwarded-for
  |=  forwards=(list (map @t @t))
  ^-  (unit address)
  ?.  ?=(^ forwards)  ~
  =*  forward  i.forwards
  ?~  for=(~(get by forward) 'for')  ~
  ::NOTE  per rfc7239, non-ip values are also valid. they're not useful
  ::      for the general case, so we ignore them here. if needed,
  ::      request handlers are free to inspect the headers themselves.
  ::
  %+  rush  u.for
  ;~  sfix
    ;~(pose (stag %ipv4 ip4) (stag %ipv6 (ifix [sel ser] ip6)))
    ;~(pose ;~(pfix col dim:ag) (easy ~))
  ==
::
++  forwarded-secure
  |=  forwards=(list (map @t @t))
  ^-  (unit ?)
  ?.  ?=(^ forwards)  ~
  =*  forward  i.forwards
  ?~  proto=(~(get by forward) 'proto')  ~
  ?+  u.proto  ~
    %http   `|
    %https  `&
  ==
::
++  parse-request-line
  |=  url=@t
  ^-  [[ext=(unit @ta) site=(list @t)] args=(list [key=@t value=@t])]
  (fall (rush url ;~(plug apat:de-purl:html yque:de-purl:html)) [[~ ~] ~])
::  +insert-binding: add a new binding, replacing any existing at its path
::
++  insert-binding
  |=  $:  new=[=binding =duct =action]
          bindings=(list [=binding =duct =action])
      ==
  ^+  bindings
  ?~  bindings  [new]~
  =*  bid  binding.i.bindings
  ::  replace already bound paths
  ::
  ?:  =([site path]:bid [site path]:binding.new)
    ~>  %slog.[0 leaf+"eyre: replacing existing binding at {<`path`path.bid>}"]
    [new t.bindings]
  ::  if new comes before bid, prepend it.
  ::  otherwise, continue our search.
  ::
  =;  new-before-bid=?
    ?:  new-before-bid  [new bindings]
    [i.bindings $(bindings t.bindings)]
  ?:  =(site.binding.new site.bid)
    (aor path.bid path.binding.new)
  (aor (fall site.bid '') (fall site.binding.new ''))
::
++  channel-wire
  |=  [channel-id=@t request-id=@ud]
  ^-  wire
  /channel/subscription/[channel-id]/(scot %ud request-id)
::
++  subscription-wire
  |=  [channel-id=@t request-id=@ud as=$@(@p identity) =ship app=term]
  ^-  wire
  =/  from=@p
    ?@  as  as
    ?+(-.as who.as %ours our)
  %+  weld  (channel-wire channel-id request-id)
  ::NOTE  including the originating identity is important for the band-aid
  ::      solution currently present in +on-gall-response, where we may
  ::      need to issue a %leave after we've forgotten the identity with
  ::      which the subscription was opened.
  /(scot %p ship)/[app]/(scot %p from)
--
::  end the =~
::
.  ==
::  begin with a default +axle as a blank slate
::
=|  ax=axle
::  a vane is activated with current date, entropy, and a namespace function
::
|=  [now=@da eny=@uvJ rof=roof]
::  allow jets to be registered within this core
::
~%  %http-server  ..part  ~
|%
++  call
  ~/  %eyre-call
  |=  [=duct dud=(unit goof) wrapped-task=(hobo task)]
  ^-  [(list move) _http-server-gate]
  ::
  =/  task=task  ((harden task) wrapped-task)
  ::
  ::  XX handle more error notifications
  ::
  ?^  dud
    :_  http-server-gate
    ::  always print the error trace
    ::
    :-  [duct %slip %d %flog %crud [-.task tang.u.dud]]
    ^-  (list move)
    ::  if a request caused the crash, respond with a 500
    ::
    ?.  ?=(?(%request %request-local) -.task)  ~
    ^~
    =/  data  (as-octs:mimes:html 'crud!')
    =/  head
      :~  ['content-type' 'text/html']
          ['content-length' (crip (a-co:co p.data))]
      ==
    [duct %give %response %start 500^head `data &]~
  ::  %init: tells us what our ship name is
  ::
  ?:  ?=(%init -.task)
    ::  initial value for the login handler
    ::
    =.  bindings.server-state.ax
      =-  (roll - insert-binding)
      ^-  (list [binding ^duct action])
      :~  [[~ /~/login] duct [%authentication ~]]
          [[~ /~/eauth] duct [%eauth ~]]
          [[~ /~/logout] duct [%logout ~]]
          [[~ /~/channel] duct [%channel ~]]
          [[~ /~/scry] duct [%scry ~]]
          [[~ /~/name] duct [%name ~]]
          [[~ /~/host] duct [%host ~]]
      ==
    [~ http-server-gate]
  ::  %trim: in response to memory pressure
  ::
  ::    Cancel all inactive channels
  ::    XX cancel active too if =(0 trim-priority) ?
  ::
  ?:  ?=(%trim -.task)
    =*  event-args  [[eny duct now rof] server-state.ax]
    =*  by-channel  by-channel:(per-server-event event-args)
    =*  channel-state  channel-state.server-state.ax
    ::
    =/  inactive=(list @t)
      =/  full=(set @t)  ~(key by session.channel-state)
      =/  live=(set @t)
        (~(gas in *(set @t)) ~(val by duct-to-key.channel-state))
      ~(tap in (~(dif in full) live))
    ::
    ?:  =(~ inactive)
      [~ http-server-gate]
    ::
    =/  len=tape  (scow %ud (lent inactive))
    ~>  %slog.[0 leaf+"eyre: trim: closing {len} inactive channels"]
    ::
    =|  moves=(list (list move))
    |-  ^-  [(list move) _http-server-gate]
    =*  channel-id  i.inactive
    ?~  inactive
      [(zing (flop moves)) http-server-gate]
    ::  discard channel state, and cancel any active gall subscriptions
    ::
    =^  mov  server-state.ax  (discard-channel:by-channel channel-id |)
    $(moves [mov moves], inactive t.inactive)
  ::
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
        [[eny duct.i.connections now rof] server-state.ax]
      =/  cancel-request  cancel-request:(per-server-event event-args)
      =^  moves  server-state.ax  cancel-request
      ::
      $(closed-connections (weld moves closed-connections), connections t.connections)
    ::  save duct for future %give to unix
    ::
    =.  outgoing-duct.server-state.ax  duct
    ::  send all cache mappings to runtime
    ::
    =/  cache-moves=(list move)
      %+  turn  ~(tap by cache.server-state.ax)
      |=  [url=@t cache-val=[aeon=@ud val=(unit cache-entry)]]
      [duct %give %grow /cache/(scot %u aeon.cache-val)/(scot %t url)]
    ::
    :_  http-server-gate
    :*  ::  hand back default configuration for now
        ::
        [duct %give %set-config http-config.server-state.ax]
        ::  provide a list of valid auth tokens
        ::
        =<  give-session-tokens
        (per-server-event [eny duct now rof] server-state.ax)
      ::
        (zing ~[closed-connections cache-moves])
    ==
  ::
  ?:  ?=(%code-changed -.task)
    ~>  %slog.[0 leaf+"eyre: code-changed: throwing away local sessions"]
    =*  event-args  [[eny duct now rof] server-state.ax]
    ::  find all the %ours sessions, we must close them
    ::
    =/  siz=(list @uv)
      %+  murn  ~(tap by sessions.auth.server-state.ax)
      |=  [sid=@uv session]
      ?:(?=(%ours -.identity) (some sid) ~)
    =|  moves=(list (list move))
    |-  ^-  [(list move) _http-server-gate]
    ?~  siz
      [(zing (flop moves)) http-server-gate]
    ::  discard the session, clean up its channels
    ::
    =^  mov  server-state.ax
      (close-session:authentication:(per-server-event event-args) i.siz |)
    $(moves [mov moves], siz t.siz)
  ::
  ?:  ?=(%eauth-host -.task)
    =.  user.endpoint.auth.server-state.ax  host.task
    =.  time.endpoint.auth.server-state.ax  now
    [~ http-server-gate]
  ::
  ::  all other commands operate on a per-server-event
  ::
  =/  event-args  [[eny duct now rof] server-state.ax]
  =/  server  (per-server-event event-args)
  ::
  ?-    -.task
      ::  %live: notifies us of the ports of our live http servers
      ::
      %live
    =.  ports.server-state.ax  +.task
    ::  enable http redirects if https port live and cert set
    ::
    =.  redirect.http-config.server-state.ax
      &(?=(^ secure.task) ?=(^ secure.http-config.server-state.ax))
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
      =.  redirect.config
        ?&  ?=(^ secure.ports.server-state.ax)
            ?=(^ cert.http-rule.task)
        ==
      :_  http-server-gate
      =*  out-duct  outgoing-duct.server-state.ax
      ?~  out-duct  ~
      [out-duct %give %set-config config]~
        ::  %turf: add or remove domain name
        ::
        %turf
      =*  domains  domains.server-state.ax
      =/  mod=(set turf)
        ?:  ?=(%put action.http-rule.task)
          (~(put in domains) turf.http-rule.task)
        (~(del in domains) turf.http-rule.task)
      ?:  =(domains mod)
        [~ http-server-gate]
      =.  domains  mod
      :_  http-server-gate
      =/  cmd
        [%acme %poke `cage`[%acme-order !>(mod)]]
      [duct %pass /acme/order %g %deal [our our /eyre] cmd]~
    ==
  ::
      %plea
    ~|  path.plea.task
    ?>  ?=([%eauth %'0' ~] path.plea.task)
    =+  plea=;;(eauth-plea payload.plea.task)
    =^  moves  server-state.ax
      (on-plea:server:eauth:authentication:server ship.task plea)
    [moves http-server-gate]
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
      %approve-origin
    =.  cors-registry.server-state.ax
      =,  cors-registry.server-state.ax
      :+  (~(del in requests) origin.task)
        (~(put in approved) origin.task)
      (~(del in rejected) origin.task)
    [~ http-server-gate]
  ::
      %reject-origin
    =.  cors-registry.server-state.ax
      =,  cors-registry.server-state.ax
      :+  (~(del in requests) origin.task)
        (~(del in approved) origin.task)
      (~(put in rejected) origin.task)
    [~ http-server-gate]
  ::
      %spew
    =.  verb.server-state.ax  veb.task
    `http-server-gate
  ::
      %set-response
    =^  moves  server-state.ax  (set-response:server +.task)
    [moves http-server-gate]
  ==
::
++  take
  ~/  %eyre-take
  |=  [=wire =duct dud=(unit goof) =sign]
  ^-  [(list move) _http-server-gate]
  =>  %=    .
          sign
        ?:  ?=(%gall -.sign)
          ?>  ?=(%unto +<.sign)
          sign
        sign
      ==
  ::  :wire must at least contain two parts, the type and the build
  ::
  ?>  ?=([@ *] wire)
  ::
  |^  ^-  [(list move) _http-server-gate]
      ::
      ?:  ?=(%eauth i.wire)
        eauth
      ?^  dud
        ~|(%eyre-take-dud (mean tang.u.dud))
      ?+    i.wire
          ~|([%bad-take-wire wire] !!)
      ::
        %run-app-request   run-app-request
        %watch-response    watch-response
        %sessions          sessions
        %channel           channel
        %acme              acme-ack
        %conversion-cache  `http-server-gate
      ==
  ::
  ++  run-app-request
    ::
    ?>  ?=([%gall %unto *] sign)
    ::
    ::
    ?>  ?=([%poke-ack *] p.sign)
    ?>  ?=([@ *] t.wire)
    ?~  p.p.sign
      ::  received a positive acknowledgment: take no action
      ::
      [~ http-server-gate]
    ::  we have an error; propagate it to the client
    ::
    =/  event-args  [[eny duct now rof] server-state.ax]
    =/  handle-gall-error
      handle-gall-error:(per-server-event event-args)
    =^  moves  server-state.ax
      (handle-gall-error u.p.p.sign)
    [moves http-server-gate]
  ::
  ++  watch-response
    ::
    =/  event-args  [[eny duct now rof] server-state.ax]
    ::
    ?>  ?=([@ *] t.wire)
    ?:  ?=([%gall %unto %watch-ack *] sign)
      ?~  p.p.sign
        ::  received a positive acknowledgment: take no action
        ::
        [~ http-server-gate]
      ::  we have an error; propagate it to the client
      ::
      =/  handle-gall-error
        handle-gall-error:(per-server-event event-args)
      =^  moves  server-state.ax  (handle-gall-error u.p.p.sign)
      [moves http-server-gate]
    ::
    ?:  ?=([%gall %unto %kick ~] sign)
      =/  handle-response  handle-response:(per-server-event event-args)
      =^  moves  server-state.ax
        (handle-response %continue ~ &)
      [moves http-server-gate]
    ::
    ?>  ?=([%gall %unto %fact *] sign)
    =/  =mark  p.cage.p.sign
    =/  =vase  q.cage.p.sign
    ?.  ?=  ?(%http-response-header %http-response-data %http-response-cancel)
        mark
      =/  handle-gall-error
        handle-gall-error:(per-server-event event-args)
      =^  moves  server-state.ax
        (handle-gall-error leaf+"eyre bad mark {(trip mark)}" ~)
      [moves http-server-gate]
    ::
    =/  =http-event:http
      ?-  mark
        %http-response-header  [%start !<(response-header:http vase) ~ |]
        %http-response-data    [%continue !<((unit octs) vase) |]
        %http-response-cancel  [%cancel ~]
      ==
    =/  handle-response  handle-response:(per-server-event event-args)
    =^  moves  server-state.ax
      (handle-response http-event)
    [moves http-server-gate]
  ::
  ++  channel
    ::
    =/  event-args  [[eny duct now rof] server-state.ax]
    ::  channel callback wires are triples.
    ::
    ?>  ?=([@ @ @t *] wire)
    ::
    ?+    i.t.wire
        ~|([%bad-channel-wire wire] !!)
    ::
        %timeout
      ?>  ?=([%behn %wake *] sign)
      ?^  error.sign
        [[duct %slip %d %flog %crud %wake u.error.sign]~ http-server-gate]
      =*  id  i.t.t.wire
      %-  %+  trace:(per-server-event event-args)  1
          |.("{(trip id)} cancelling channel due to timeout")
      =^  moves  server-state.ax
        (discard-channel:by-channel:(per-server-event event-args) id &)
      [moves http-server-gate]
    ::
        %heartbeat
      =/  on-channel-heartbeat
        on-channel-heartbeat:by-channel:(per-server-event event-args)
      =^  moves  server-state.ax
        (on-channel-heartbeat i.t.t.wire)
      [moves http-server-gate]
    ::
        ?(%poke %subscription)
      ?>  ?=([%gall %unto *] sign)
      ~|  eyre-sub=wire
      ?>  ?=([@ @ @t @ *] wire)
      ?<  ?=(%raw-fact -.p.sign)
      =*  channel-id  i.t.t.wire
      =*  request-id  i.t.t.t.wire
      =*  extra-wire  t.t.t.t.wire
      =/  on-gall-response
        on-gall-response:by-channel:(per-server-event event-args)
      ::  ~&  [%gall-response sign]
      =^  moves  server-state.ax
        %-  on-gall-response
        [channel-id (slav %ud request-id) extra-wire p.sign]
      [moves http-server-gate]
    ==
  ::
  ++  sessions
    ::
    ?>  ?=([%behn %wake *] sign)
    ::
    ?^  error.sign
      [[duct %slip %d %flog %crud %wake u.error.sign]~ http-server-gate]
    ::NOTE  we are not concerned with expiring channels that are still in
    ::      use. we require acks for messages, which bump their session's
    ::      timer. channels have their own expiry timer, too.
    ::  remove cookies that have expired
    ::
    =*  sessions  sessions.auth.server-state.ax
    =.  sessions.auth.server-state.ax
      %-  ~(gas by *(map @uv session))
      %+  skip  ~(tap in sessions)
      |=  [cookie=@uv session]
      (lth expiry-time now)
    ::  if there's any cookies left, set a timer for the next expected expiry
    ::
    ^-  [(list move) _http-server-gate]
    :_  http-server-gate
    :-  =<  give-session-tokens
        (per-server-event [eny duct now rof] server-state.ax)
    ?:  =(~ sessions)  ~
    =;  next-expiry=@da
      [duct %pass /sessions/expire %b %wait next-expiry]~
    %+  roll  ~(tap by sessions)
    |=  [[@uv session] next=@da]
    ?:  =(*@da next)  expiry-time
    (min next expiry-time)
  ::
  ++  eauth
    =*  auth  auth.server-state.ax
    =*  args  [[eny duct now rof] server-state.ax]
    ^-  [(list move) _http-server-gate]
    ~|  [wire +<.sign]
    ?+  t.wire  !!
        [%plea @ ~]
      =/  =ship  (slav %p i.t.t.wire)
      ::
      ?:  |(?=(^ dud) ?=([%ames %lost *] sign))
        %-  %+  trace:(per-server-event args)  0
            ?~  dud  |.("eauth: lost boon from {(scow %p ship)}")
            |.("eauth: crashed on %{(trip +<.sign)} from {(scow %p ship)}")
        ::NOTE  when failing on pending attempts, we just wait for the timer
        ::      to clean up. when failing on live sessions, well, we should
        ::      just be careful not to crash when receiving %shut boons.
        ::      (we do not want to have the nonce in the wire, so this is the
        ::      best handling we can do. the alternative is tracking)
        [~ http-server-gate]
      ::
      ?:  ?=([%ames %done *] sign)
        =^  moz  server-state.ax
          %.  [ship ?=(~ error.sign)]
          on-done:client:eauth:authentication:(per-server-event args)
        [moz http-server-gate]
      ::
      ?>  ?=([%ames %boon *] sign)
      =/  boon  ;;(eauth-boon payload.sign)
      =^  moz  server-state.ax
        %.  [ship boon]
        on-boon:client:eauth:authentication:(per-server-event args)
      [moz http-server-gate]
    ::
        [%keen @ @ ~]
      =/  client=@p  (slav %p i.t.t.wire)
      =/  nonce=@uv  (slav %uv i.t.t.t.wire)
      ::
      ?^  dud
        =^  moz  server-state.ax
          %.  [client nonce]
          on-fail:server:eauth:authentication:(per-server-event args)
        [moz http-server-gate]
      ::
      ?>  ?=([%ames %tune *] sign)
      ?>  =(client ship.sign)
      =/  url=(unit @t)
        ?~  roar.sign  ~
        ?~  q.dat.u.roar.sign  ~
        ;;((unit @t) q.u.q.dat.u.roar.sign)
      =^  moz  server-state.ax
        ?~  url
          %.  [client nonce]
          on-fail:server:eauth:authentication:(per-server-event args)
        %.  [client nonce u.url]
        on-tune:server:eauth:authentication:(per-server-event args)
      [moz http-server-gate]
    ::
        [%expire %visiting @ @ ~]
      ?>  ?=([%behn %wake *] sign)
      =/  server=@p  (slav %p i.t.t.t.wire)
      =/  nonce=@uv  (slav %uv i.t.t.t.t.wire)
      =^  moz  server-state.ax
        %.  [server nonce]
        expire:client:eauth:authentication:(per-server-event args)
      [~ http-server-gate]
    ::
        [%expire %visitors @ ~]
      =/  nonce=@uv  (slav %uv i.t.t.t.wire)
      =^  moz  server-state.ax
        (expire:server:eauth:authentication:(per-server-event args) nonce)
      [moz http-server-gate]
    ==
  ::
  ++  acme-ack
    ?>  ?=([%gall %unto *] sign)
    ::
    ?>  ?=([%poke-ack *] p.sign)
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
  =>  |%
      +$  axle-any
        $%  [date=%~2020.10.18 server-state=server-state-0]
            [date=%~2022.7.26 server-state=server-state-0]
            [date=%~2023.2.17 server-state=server-state-1]
            [date=%~2023.3.16 server-state=server-state-2]
            [date=%~2023.4.11 server-state-3]
            [date=%~2023.5.15 server-state]
        ==
      ::
      +$  server-state-0
        $:  bindings=(list [=binding =duct =action])
            =cors-registry
            connections=(map duct outstanding-connection-3)
            auth=authentication-state-3
            channel-state=channel-state-2
            domains=(set turf)
            =http-config
            ports=[insecure=@ud secure=(unit @ud)]
            outgoing-duct=duct
        ==
      ::
      +$  server-state-1
        $:  bindings=(list [=binding =duct =action])
            =cors-registry
            connections=(map duct outstanding-connection-3)
            auth=authentication-state-3
            channel-state=channel-state-2
            domains=(set turf)
            =http-config
            ports=[insecure=@ud secure=(unit @ud)]
            outgoing-duct=duct
            verb=@                                                ::  <-  new
        ==
      ::
      +$  server-state-2
        $:  bindings=(list [=binding =duct =action])
            cache=(map url=@t [aeon=@ud val=(unit cache-entry)])  ::  <- new
            =cors-registry
            connections=(map duct outstanding-connection-3)
            auth=authentication-state-3
            channel-state=channel-state-2
            domains=(set turf)
            =http-config
            ports=[insecure=@ud secure=(unit @ud)]
            outgoing-duct=duct
            verb=@
        ==
      +$  channel-state-2
        $:  session=(map @t channel-2)
            duct-to-key=(map duct @t)
        ==
      +$  channel-2
        $:  state=(each timer duct)
            next-id=@ud
            last-ack=@da
            events=(qeu [id=@ud request-id=@ud channel-event=channel-event-2])
            unacked=(map @ud @ud)
            subscriptions=(map @ud [ship=@p app=term =path duc=duct])
            heartbeat=(unit timer)
        ==
      +$  channel-event-2
        $%  $>(%poke-ack sign:agent:gall)
            $>(%watch-ack sign:agent:gall)
            $>(%kick sign:agent:gall)
            [%fact =mark =noun]
        ==
      ::
      +$  server-state-3
        $:  bindings=(list [=binding =duct =action])
            cache=(map url=@t [aeon=@ud val=(unit cache-entry)])
            =cors-registry
            connections=(map duct outstanding-connection-3)
            auth=authentication-state-3
            channel-state=channel-state-3
            domains=(set turf)
            =http-config
            ports=[insecure=@ud secure=(unit @ud)]
            outgoing-duct=duct
            verb=@
        ==
      +$  outstanding-connection-3
        $:  =action
            =inbound-request
            response-header=(unit response-header:http)
            bytes-sent=@ud
        ==
      +$  authentication-state-3  sessions=(map @uv session-3)
      +$  session-3
        $:  expiry-time=@da
            channels=(set @t)
        ==
      +$  channel-state-3
        $:  session=(map @t channel-3)
            duct-to-key=(map duct @t)
        ==
      +$  channel-3
        $:  mode=?(%json %jam)
            state=(each timer duct)
            next-id=@ud
            last-ack=@da
            events=(qeu [id=@ud request-id=@ud =channel-event])
            unacked=(map @ud @ud)
            subscriptions=(map @ud [ship=@p app=term =path duc=duct])
            heartbeat=(unit timer)
        ==
      --
  |=  old=axle-any
  ^+  http-server-gate
  ?-    -.old
  ::
  ::  adds /~/name
  ::
      %~2020.10.18
    %=  $
        date.old  %~2022.7.26
    ::
        bindings.server-state.old
      %+  insert-binding
        [[~ /~/name] outgoing-duct.server-state.old [%name ~]]
      bindings.server-state.old
    ==
  ::
  ::  enables https redirects if certificate configured
  ::  inits .verb
  ::
      %~2022.7.26
    =.  redirect.http-config.server-state.old
      ?&  ?=(^ secure.ports.server-state.old)
          ?=(^ secure.http-config.server-state.old)
      ==
    $(old [%~2023.2.17 server-state.old(|8 [|8 verb=0]:server-state.old)])
  ::
  ::  inits .cache
  ::
      %~2023.2.17
    $(old [%~2023.3.16 [bindings ~ +]:server-state.old])
  ::
  ::  inits channel mode and desks in unacked events
  ::
      %~2023.3.16
  ::
  ::  Prior to this desks were not part of events.channel.
  ::  When serializing we used to rely on the desk stored in
  ::  subscriptions.channel, but this state is deleted when we clog.
  ::  This migration adds the desk to events.channel, but we can not
  ::  scry in +load to populate the desks in the old events,
  ::  so we just kick all subscriptions on all channels.
    %=    $
        date.old  %~2023.4.11
    ::
        server-state.old
      %=  server-state.old
          session.channel-state
        %-  ~(run by session.channel-state.server-state.old)
        |=  c=channel-2
        =;  new-events
          :-  %json
          c(events new-events, unacked ~, subscriptions ~)
        =|  events=(qeu [id=@ud request-id=@ud =channel-event])
        =/  l  ~(tap in ~(key by subscriptions.c))
        |-
        ?~  l  events
        %=  $
          l          t.l
          next-id.c  +(next-id.c)
          events     (~(put to events) [next-id.c i.l %kick ~])
        ==
      ==
    ==
  ::
  ::  guarantees & stores a session for each request, and a @p identity for
  ::  each session and channel
  ::
      %~2023.4.11
    %=  $
      date.old  %~2023.5.15
    ::
        connections.old
      %-  ~(run by connections.old)
      |=  outstanding-connection-3
      ^-  outstanding-connection
      [action inbound-request [*@uv [%ours ~]] response-header bytes-sent]
    ::
        auth.old
      :_  [~ ~ [~ ~ now]]
      %-  ~(run by sessions.auth.old)
      |=  s=session-3
      ^-  session
      [[%ours ~] s]
    ::
        session.channel-state.old
      %-  ~(run by session.channel-state.old)
      |=  c=channel-3
      ^-  channel
      [-.c [%ours ~] +.c]
    ::
        bindings.old
      %+  insert-binding  [[~ /~/host] outgoing-duct.old [%host ~]]
      %+  insert-binding  [[~ /~/eauth] outgoing-duct.old [%eauth ~]]
      bindings.old
    ==
  ::
      %~2023.5.15
    http-server-gate(ax old)
  ==
::  +stay: produce current state
::
++  stay  `axle`ax
::  +scry: request a path in the urbit namespace
::
++  scry
  ~/  %eyre-scry
  ^-  roon
  |=  [lyc=gang pov=path car=term bem=beam]
  ^-  (unit (unit cage))
  =*  ren  car
  =*  why=shop  &/p.bem
  =*  syd  q.bem
  =/  lot=coin  $/r.bem
  =*  tyl  s.bem
  ::
  ?.  ?=(%& -.why)
    ~
  =*  who  p.why
  ::
  ?.  ?=(%$ -.lot)
    [~ ~]
  ?.  =(our who)
    ?.  =([%da now] p.lot)
      [~ ~]
    ~&  [%r %scry-foreign-host who]
    ~
  ?:  &(?=(%x ren) ?=(%$ syd))
    =,  server-state.ax
    ?+  tyl  [~ ~]
      [%$ %whey ~]         =-  ``mass+!>(`(list mass)`-)
                           :~  bindings+&+bindings.server-state.ax
                               auth+&+auth.server-state.ax
                               connections+&+connections.server-state.ax
                               channels+&+channel-state.server-state.ax
                               axle+&+ax
                           ==
    ::
      [%cors ~]            ``noun+!>(cors-registry)
      [%cors %requests ~]  ``noun+!>(requests.cors-registry)
      [%cors %approved ~]  ``noun+!>(approved.cors-registry)
      [%cors %rejected ~]  ``noun+!>(rejected.cors-registry)
    ::
        [%cors ?(%approved %rejected) @ ~]
      =*  kind  i.t.tyl
      =*  orig  i.t.t.tyl
      ?~  origin=(slaw %t orig)  [~ ~]
      ?-  kind
        %approved  ``noun+!>((~(has in approved.cors-registry) u.origin))
        %rejected  ``noun+!>((~(has in rejected.cors-registry) u.origin))
      ==
    ::
        [%eauth %url ~]
      =*  endpoint  endpoint.auth.server-state.ax
      ?.  ?=(%da -.p.lot)  [~ ~]
      ::  we cannot answer for something prior to the last set time,
      ::  or something beyond the present moment.
      ::
      ?:  ?|  (lth q.p.lot time.endpoint)
              (gth q.p.lot now)
          ==
        ~
      :^  ~  ~  %noun
      !>  ^-  (unit @t)
      =<  eauth-url:eauth:authentication
      (per-server-event [eny *duct now rof] server-state.ax)
    ::
        [%authenticated %cookie @ ~]
      ?~  cookies=(slaw %t i.t.t.tyl)  [~ ~]
      :^  ~  ~  %noun
      !>  ^-  ?
      %-  =<  request-is-authenticated:authentication
          (per-server-event [eny *duct now rof] server-state.ax)
      %*(. *request:http header-list ['cookie' u.cookies]~)
    ::
        [%cache @ @ ~]
      ?~  aeon=(slaw %ud i.t.tyl)        [~ ~]
      ?~  url=(slaw %t i.t.t.tyl)        [~ ~]
      ?~  entry=(~(get by cache) u.url)  [~ ~]
      ?.  =(u.aeon aeon.u.entry)         [~ ~]
      ?~  val=val.u.entry                [~ ~]
      ``noun+!>(u.val)
    ==
  ?.  ?=(%$ ren)
    [~ ~]
  ?+  syd  [~ ~]
    %bindings              ``noun+!>(bindings.server-state.ax)
    %connections           ``noun+!>(connections.server-state.ax)
    %authentication-state  ``noun+!>(auth.server-state.ax)
    %channel-state         ``noun+!>(channel-state.server-state.ax)
  ::
      %host
    %-  (lift (lift |=(a=hart:eyre [%hart !>(a)])))
    ^-  (unit (unit hart:eyre))
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
      =/  port=(unit @ud)
        ?.  ?=(^ secure.ports)
          ?:(=(80 insecure.ports) ~ `insecure.ports)
        ?:(=(443 u.secure.ports) ~ secure.ports)
      ``[?=(^ secure.ports) port host]
    ==
  ==
--
