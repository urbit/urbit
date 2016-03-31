::  This is a driver for the Github API v3.
::
::  You can interact with this in a few different ways:
::
::    - .^(%gx /=gh=/read{/endpoint}) or subscribe to
::      /scry/x/read{/endpoint} for authenticated reads.
::
::    - subscribe to /scry/x/listen/{owner}/{repo}/{events...}
::      for webhook-powered event notifications.  For event list,
::      see https://developer.github.com/webhooks/.
::
::  See the %github app for example usage.
::
/?  314
/-  gh, plan-acct
::  /ape/gh/split.hoon defines ++split, which splits a request
::  at the end of the longest possible endpoint.
::
//  /%/split
::
!:
=>  |%
    ++  move  (pair bone card)
    ++  sub-result
      $%  {$arch arch}
          {$gh-issues issues:gh}
          {$gh-issue-comment issue-comment:gh}
          {$json json}
          {$null $~}
      ==
    ++  card  
      $%  {$diff sub-result}
          {$them wire (unit hiss)}
          {$hiss wire {$~ $~} $httr {$hiss hiss}}
          {$poke wire {ship $hood} $write-plan-account {knot plan-acct}}
      ==
    ++  hook-response
      $%  {$gh-issues issues:gh}
          {$gh-issue-comment issue-comment:gh}
      ==
    --
::
|_  {hid/bowl cnt/@ hook/(map @t {id/@t listeners/(set bone)})}
++  prep
  |=  a/(unit _+<+)  ^-  (quip move +>)
  ?^  a  [~ +>(+<+ u.a)]
  (peer-scry %x %read /user)
::
::  This core manages everything related to a particular request.
::
::  Each request has a particular 'style', which is currently
::  one of 'read', or 'listen'.  ++scry handles all three types
::  of requests.
::
++  help
  |=  {ren/care style/@tas pax/path}
  =^  arg  pax  [+ -]:(split pax)
  =|  mow/(list move)
  |%
  ::  Resolve core.
  ::
  ++  abet
    ^-  {(list move) _+>.$}
    [(flop mow) +>.$]
  ::
  ::  Append path to api.github.com and parse to a purl.
  ::
  ++  endpoint-to-purl
    |=  endpoint/path
    (scan "https://api.github.com{<`path`endpoint>}" auri:epur)
  ::
  ::  Send a hiss
  ::
  ++  send-hiss 
    |=  hiz/hiss
    ^+  +>
    =+  wir=`wire`[(scot %ud cnt) ren (pack style arg) pax]
    =+  new-move=[ost.hid %hiss wir `~ %httr [%hiss hiz]]
    ::  ~&  [%sending-hiss new-move]
    +>.$(mow [new-move mow])
  ::
  ::  Decide how to handle a request based on its style.
  ::
  ++  scry
    ^+  .
    ?+  style  ~|(invalid-style+[style want=p:!>(*?($read $listen))] !!)
      $read     read
      $listen   listen
    ==
  ::
  ++  read  (send-hiss (endpoint-to-purl pax) %get ~ ~)
  ::
  ::  Create or update a webhook to listen for a set of events.
  ::
  ++  listen
    ^+  .
    =+  paf=`path`(weld pax arg)
    ?>  ?=({@ @ *} paf)
    =+  events=t.t.paf
    |-  ^+  +>+.$
    ?~  events
      +>+.$
    ?:  (~(has by hook) i.events)
      =.  +>+.$  (update-hook i.events)
      $(events t.events)
    =.  +>+.$  (create-hook i.events)
    $(events t.events)
  ::
  ::  Set up a webhook.
  ::
  ++  create-hook
    |=  event/@t
    ^+  +>
    =+  paf=`path`(weld pax arg)
    ?>  ?=({@ @ *} paf)
    =+  clean-event=`tape`(turn (trip event) |=(a/@tD ?:(=('_' a) '-' a)))
    =.  hook
      %+  ~(put by hook)  (crip clean-event)
      =+  %+  fall
            (~(get by hook) (crip clean-event))
          *{id/@t listeners/(set bone)}
      [id (~(put in listeners) ost.hid)]
    %-  send-hiss
    :*  %+  scan
          =+  [(trip i.paf) (trip i.t.paf)]
          "https://api.github.com/repos/{-<}/{->}/hooks"
        auri:epur
        %post  ~  ~
        %-  taco  %-  crip  %-  pojo  %-  jobe  :~
          name+s+%web
          active+b+&
          events+a+~[s+event] ::(turn `(list ,@t)`t.t.pax |=(a=@t s/a))
          :-  %config
          %-  jobe  :~
            =+  =+  clean-event
                "http://107.170.195.5:8443/~/to/gh/gh-{-}.json?anon&wire=/"
            [%url s+(crip -)]
            [%'content_type' s+%json]
          ==
        ==
    ==
  ::
  ::  Add current bone to the list of subscribers for this event.
  ::
  ++  update-hook
    |=  event/@t
    ^+  +>
    =+  hok=(~(got by hook) event)
    %_    +>.$
        hook
      %+  ~(put by hook)  event
      hok(listeners (~(put in listeners.hok) ost.hid))
    ==
  --
::
::  Pokes that aren't caught in more specific arms are handled
::  here.  These should be only from webhooks firing, so if we
::  get any mark that we shouldn't get from a webhook, we reject
::  it.  Otherwise, we spam out the event to everyone who's
::  listening for that event.
::
++  poke
  |=  response/hook-response
  ^-  {(list move) _+>.$}
  =+  hook-data=(~(get by hook) (rsh 3 3 -.response))
  ?~  hook-data
    ~&  [%strange-hook hook response]
    [~ +>.$]
  ::  ~&  response=response
  :_  +>.$
  %+  turn  (~(tap in listeners.u.hook-data))
  |=  ost/bone
  [ost %diff response]
::
::  Here we handle PUT, POST, and DELETE requests.  We probably
::  should return the result somehow, but that doesn't fit well
::  into poke semantics.
::
++  poke-gh-poke
  |=  {method/meth endpoint/(list @t) jon/json}
  ^-  {(list move) _+>.$}
  :_  +>.$  :_  ~
  :*  ost.hid  %hiss  /poke/[method]  `~  %httr  %hiss 
      ~|  stuff="https://api.github.com{<(path endpoint)>}"
      (scan "https://api.github.com{<(path endpoint)>}" auri:epur)
      method  ~  `(taco (crip (pojo jon)))
  ==
::
::  When a peek on a path blocks, ford turns it into a peer on
::  /scry/{care}/{path}.  You can also just peer to this
::  directly.
::
::  After some sanity checking we hand control to ++scry in
::  ++help.
::
++  peer-scry
  |=  pax/path
  ^-  {(list move) _+>.$}
  ?>  ?=({care ^} pax)
  ::  =-  ~&  [%peered -]  -
  [abet(cnt +(cnt))]:scry:(help i.pax i.t.pax t.t.pax)
::
::  Deconstruct an http request into a json result or error information  
::
++  parse-json  
  |=  res/httr  ^-  (each json {err/term inf/(list {term json})})
  ?~  r.res
    [%| %empty-response code+(jone p.res) ~]
  =+  jon=(rush q.u.r.res apex:poja)
  ?~  jon
    [%| %bad-json code+(jone p.res) body+s+q.u.r.res ~]
  ?.  =(2 (div p.res 100))
    [%| %request-rejected code+(jone p.res) msg+u.jon ~]
  [%& u.jon]
::
::  Anywhere recieved data should go besides the initiating request
::
++  side-effects
  |=  {pax/path rep/(each json ^)}  ^-  (list move)
  ?.  ?=($& -.rep)  ~                 ::  XX logging maybe?
  ?~  pax  ~
  ?+    -.pax  ~
      $user
    =+  =;  jop  usr=(need (jop p.rep))
        (ot login+so url+(cu some (su aurf:urlp)) ~):jo
    [ost.hid %poke user+~ [our.hid %hood] %write-plan-account ~.github usr]~
  ==
::
::  HTTP response.  We make sure the response is good, then
::  produce the result (as JSON) to whoever sent the request.
::
++  sigh-httr
  |=  {way/wire res/httr}
  ^-  {(list move) _+>.$}
  ?.  ?=({@ care @ *} way)
    ~&  bad-wire+[way res=res]
    [~ +>.$]
  =+  ^-  {ren/care {syl/term arg/path} pax/path}
      [i.t.way (need (puck i.t.t.way)) t.t.t.way]
  :_  +>.$
  =+  rep=(parse-json res)
  :_  (side-effects pax rep)
  :+  ost.hid  %diff
  ?+    ren  null+~
      $x
    ?.  ?=($& -.rep)
      json+(jobe err+s+err.p.rep inf.p.rep)
    ::
    ::  Once we know we have good data, we drill into the JSON
    ::  to find the specific piece of data referred to by 'arg'
    ::
    |-  ^-  sub-result
    ?~  arg
      json+p.rep
    =+  dir=((om:jo some) p.rep)
    ?~  dir
      json+(jobe err+s+%json-not-object code+(jone p.res) body+p.rep ~)
    =+  new-jon=(~(get by u.dir) i.arg)
    $(arg t.arg, p.rep ?~(new-jon ~ u.new-jon))
  ::
      $y
    ?.  ?=($& -.rep)
      ~&  [%scry-gh-y err.p.rep inf.p.rep]
      arch+*arch
    ::
    ::  Once we know we have good data, we drill into the JSON
    ::  to find the specific piece of data referred to by 'arg'
    ::
    |-  ^-  sub-result
    =+  dir=((om:jo some) p.rep)
    ?~  dir
      [%arch `(shax (jam p.rep)) ~]
    ?~  arg
      [%arch `(shax (jam p.rep)) (~(run by u.dir) _~)]
    =+  new-jon=(~(get by u.dir) i.arg)
    $(arg t.arg, p.rep ?~(new-jon ~ u.new-jon))
  ==
::
++  sigh-tang
  |=  {way/wire tan/tang}
  ^-  {(list move) _+>.$}
  ((slog >%gh-sigh-tang way< tan) `+>.$)
::
::  We can't actually give the response to pretty much anything
::  without blocking, so we just block unconditionally.
::
++  peek
  |=  {ren/@tas tyl/path}
  ^-  (unit (unit (pair mark *)))
  ~ ::``noun/[ren tyl]
--
