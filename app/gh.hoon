::  This is a driver for the Github API v3.
::
::  You can interact with this in a few different ways:
::
::    - .^(%gx /=gh=/read{/endpoint}) or subscribe to
::      /scry/x/read{/endpoint} for authenticated reads.
::
::    - subscribe to /scry/x/listen/{owner}/{repo}/{events...}
::      for webhook-powered event notifications.  For event list,
::      see kttps://developer.github.com/webhooks/.
::
::  See the %github app for example usage.
::
/?  314
/-  gh, plan-acct
/+  gh-parse
::
!:
=>  |%
    ++  place
      $:  guard/mold
          read-x/$-(path move)
          read-y/$-(path move)
          sigh-x/$-(jon/json (unit sub-result))
          sigh-y/$-(jon/json (unit arch))
      ==
    ++  move  (pair bone card)
    ++  sub-result
      $%  {$arch arch}
          {$gh-list-issues (list issue:gh)}
          {$gh-issues issues:gh}
          {$gh-issue-comment issue-comment:gh}
          {$json json}
          {$null $~}
      ==
    ++  card  
      $%  {$diff sub-result}
          {$them wire (unit hiss)}
          {$hiss wire {$~ $~} $httr {$hiss hiss}}
      ==
    ++  hook-response
      $%  {$gh-issues issues:gh}
          {$gh-issue-comment issue-comment:gh}
      ==
    --
::
|_  {hid/bowl cnt/@ hook/(map @t {id/@t listeners/(set bone)})}
++  prep  _`.
::
::  List of endpoints
::
++  places
  |=  wir/wire
  =<  
    ^-  (list place)
    :~  ^-  place
        :*  guard=$~
            read-x=read-null
            read-y=(read-static %issues ~)
            sigh-x=sigh-strange
            sigh-y=sigh-strange
        ==
        ^-  place
        :*  guard={$issues $~}
            read-x=read-null
            read-y=(read-static %mine %by-repo ~)
            sigh-x=sigh-strange
            sigh-y=sigh-strange
        ==
        ^-  place
        :*  guard={$issues $mine $~}
            read-x=(read-get /issues)
            read-y=(read-get /issues)
            sigh-x=sigh-list-issues-x
            sigh-y=sigh-list-issues-y
        ==
        ^-  place
        :*  guard={$issues $by-repo $~}
            read-x=read-null
            ^=  read-y
            |=  pax/path
            =+  /(scot %p our.hid)/home/(scot %da now.hid)/web/plan
            =+  .^({* acc/(map knot plan-acct)} %cx -)
          ::
            ((read-static usr:(~(got by acc) %github) ~) pax)
            sigh-x=sigh-strange
            sigh-y=sigh-strange
        ==
        ^-  place
        :*  guard={$issues $by-repo @t $~}
            read-x=read-null
            read-y=|=(pax/path (get /users/[-.+>.pax]/repos))
            sigh-x=sigh-strange
            ^=  sigh-y
            |=  jon/json
            %+  bind  ((ar:jo repository:gh-parse) jon)
            |=  repos/(list repository:gh)
            :-  `(shax (jam repos))
            (malt (turn repos |=(repository:gh [name ~])))
        ==
        ^-  place
        :*  guard={$issues $by-repo @t @t $~}
            read-x=|=(pax/path (get /repos/[-.+>.pax]/[-.+>+.pax]/issues))
            read-y=|=(pax/path (get /repos/[-.+>.pax]/[-.+>+.pax]/issues))
            sigh-x=sigh-list-issues-x
            sigh-y=sigh-list-issues-y
        ==
    ==
  =>
    |%                              ::  generic helpers (should be library)
    ++  read-null  |=(pax/path [ost.hid %diff %null ~])
    ++  read-static
      |=  children/(list @t)
      |=  pax/path
      [ost.hid %diff %arch ~ (malt (turn children |=(@t [+< ~])))]
    ::
    ++  read-get
      |=  endpoint/path
      |=  pax/path
      (get endpoint)
    ::
    ++  sigh-strange  |=(jon/json ~)
    ::
    ++  get
      |=  endpoint/path
      ^-  move
      :*  ost.hid  %hiss  wir  `~  %httr  %hiss
          (endpoint-to-purl endpoint)  %get  ~  ~
      ==
    ::
    ++  endpoint-to-purl
      |=  endpoint/path
      (scan "https://api.github.com{<`path`endpoint>}" auri:epur)
    --
  |%                                ::  gh-specific helpers
  ++  sigh-list-issues-x
    |=  jon/json
    %+  bind  ((ar:jo issue:gh-parse) jon)
    |=  issues/(list issue:gh)
    gh-list-issues+issues
  ::
  ++  sigh-list-issues-y
    |=  jon/json
    %+  bind  ((ar:jo issue:gh-parse) jon)
    |=  issues/(list issue:gh)
    :-  `(shax (jam issues))
    (malt (turn issues |=(issue:gh [(rsh 3 2 (scot %ui number)) ~])))
  --
::
::  This core manages everything related to a particular request.
::
::  Each request has a particular 'style', which is currently
::  one of 'read', or 'listen'.  ++scry handles all three types
::  of requests.
::
++  help
  |=  {ren/care style/@tas pax/path}
  ::  =^  arg  pax  [+ -]:(split pax)
  =|  arg/path
  =|  mow/(list move)
  |%
  ::  Resolve core.
  ::
  ++  abet
    ^-  {(list move) _+>.$}
    [(flop mow) +>.$]
  ::
  ::  Send a move.
  ::
  ++  send
    |=  mov/move
    +>.$(mow [mov mow])
  ::
  ::  Send a hiss
  ::
  ++  send-hiss 
    |=  hiz/hiss
    ^+  +>
    =+  wir=`wire`[ren style pax]
    (send ost.hid %hiss wir `~ %httr [%hiss hiz])
  ::
  ::  Decide how to handle a request based on its style.
  ::
  ++  scry
    ^+  .
    ?+  style  ~|([%invalid-style style] !!)
      $read     read
      $listen   listen
    ==
  ::
  ::  Match to the endpoint in ++places and execute read-x or read-y
  ::
  ++  read
    ~&  [%read pax]
    =+  places=(places ren style pax)
    |-  ^+  +>.$
    ?~  places
      ~&  [%strange-path pax]
      (send ost.hid %diff ?+(ren !! $x null+~, $y arch+*arch))
    =+  match=((soft guard.i.places) pax)
    ?~  match
      $(places t.places)
    (send (?+(ren !! $x read-x.i.places, $y read-y.i.places) pax))
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
::  HTTP response.  We make sure the response is good, then
::  produce the result (as JSON) to whoever sent the request.
::
++  sigh-httr
  |=  {way/wire res/httr}
  ^-  {(list move) _+>.$}
  ?.  ?=({care ?($read $listen) @ *} way)
    ~&  res=res
    [~ +>.$]
  =*  ren  i.way
  =*  style  i.t.way
  =*  pax  t.t.way
  :_  +>.$  :_  ~
  :+  ost.hid  %diff
  ?+    ren  null+~
      $x
    ?~  r.res
      json+(jobe err+s+%empty-response code+(jone p.res) ~)
    =+  jon=(rush q.u.r.res apex:poja)
    ?~  jon
      json+(jobe err+s+%bad-json code+(jone p.res) body+s+q.u.r.res ~)
    ?.  =(2 (div p.res 100))
      json+(jobe err+s+%request-rejected code+(jone p.res) msg+u.jon ~)
    =+  places=(places ren style pax)
    |-  ^-  sub-result
    ?~  places
      ~&([%sigh-strange-path pax] null+~)
    =+  match=((soft guard.i.places) pax)
    ?~  match
      $(places t.places)
    =+  (sigh-x.i.places u.jon)
    ?~  -
      ~&  [err+s+%response-not-valid pax+pax code+(jone p.res) msg+u.jon]
      null+~
    u.-
  ::
      $y
    ?~  r.res
      ~&  [err+s+%empty-response code+(jone p.res)]
      arch+*arch
    =+  jon=(rush q.u.r.res apex:poja)
    ?~  jon
      ~&  [err+s+%bad-json code+(jone p.res) body+s+q.u.r.res]
      arch+*arch
    ?.  =(2 (div p.res 100))
      ~&  [err+s+%request-rejected code+(jone p.res) msg+u.jon]
      arch+*arch
    =+  places=(places ren style pax)
    |-  ^-  sub-result
    ?~  places
      ~&([%sigh-strange-path pax] arch+*arch)
    =+  match=((soft guard.i.places) pax)
    ?~  match
      $(places t.places)
    =+  (sigh-y.i.places u.jon)
    ?~  -
      ~&  [err+s+%response-not-valid pax+pax code+(jone p.res) msg+u.jon]
      arch+*arch
    arch+u.-
  ==
::
++  sigh-tang
  |=  {way/wire tan/tang}
  ^-  {(list move) _+>.$}
  ((slog >%gh-sigh-tang< tan) `+>.$)
::
::  We can't actually give the response to pretty much anything
::  without blocking, so we just block unconditionally.
::
++  peek
  |=  {ren/@tas tyl/path}
  ^-  (unit (unit (pair mark *)))
  ~ ::``noun/[ren tyl]
--
