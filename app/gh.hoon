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
/+  gh-parse
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
  ::  Produce null
  ::
  ++  send-null
    (send ost.hid %diff %null ~)
  ::
  ::  Produce empty arch
  ::
  ++  send-null-arch
    (send ost.hid %diff %arch ~ ~)
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
    =+  wir=`wire`[ren (scot %ud cnt) (scot %uv (jam arg)) style pax]
    (send ost.hid %hiss wir `~ %httr [%hiss hiz])
  ::
  ::  Send a %get hiss
  ::
  ++  get
    |=  endpoint/path
    (send-hiss (endpoint-to-purl endpoint) %get ~ ~)
  ::
  ::  Decide how to handle a request based on its style.
  ::
  ++  scry
    ^+  .
    ?+  style  ~|([%invalid-style style] !!)
      $read     ?+(ren ~&([%invalid-care ren] !!) $x read-x, $y read-y)
      $listen   listen
    ==
  ::
  ++  read-x
    ~&  [%read-x pax]
    ?+    pax  ~&([%strange-path pax] send-null)
        $~  send-null
        {$issues *}
      ?+    +.pax  ~&([%strange-path pax] send-null)
          $~         send-null
          {$mine *}  (get /issues)
          {$by-repo *}
        ?+  +>.pax  ~&([%strange-path pax] send-null)
          $~          send-null
          {@t $~}     send-null
          {@t @t $~}  (get /repos/[-.+>.pax]/[-.+>+.pax]/issues)
        ==
      ==
    ==
  ::
  ++  read-y
    ~&  [%read-y pax]
    ?+    pax  ~&([%strange-path pax] send-null-arch)
        $~  (send-children %issues ~)
        {$issues *}
      ?+    +.pax  ~&([%strange-path pax] send-null-arch)
          $~            (send-children %mine %by-repo ~)
          {$mine *}     (get /issues)
          {$by-repo *}
        ?+    +>.pax  ~&([%strange-path pax] send-null-arch)
            $~
          =+  /(scot %p our.hid)/home/(scot %da now.hid)/web/plan
          =+  .^({* acc/(map knot plan-acct)} %cx -)
          (send-children usr:(~(got by acc) %github) ~)
        ::
            {@t $~}     (get /users/[-.+>.pax]/repos)
            {@t @t $~}  (get /repos/[-.+>.pax]/[-.+>+.pax]/issues)
        ==
      ==
    ==
  ::
  ::  (send-hiss (endpoint-to-purl /issues) %get ~ ~)
  ::
  ::  Produce an arch with the given list of children
  ::
  ++  send-children
    |=  children/(list @t)
    %-  send  :*
      ost.hid  %diff  %arch  ~
      (malt (turn children |=(@t [+< ~])))
    ==
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
  ?.  ?=({care @ @ @ *} way)
    ~&  res=res
    [~ +>.$]
  =+  arg=(path (cue (slav %uv i.t.t.way)))
  =*  pax  t.t.t.way
  :_  +>.$  :_  ~
  :+  ost.hid  %diff
  ?+    i.way  null+~
      $x
    ?~  r.res
      json+(jobe err+s+%empty-response code+(jone p.res) ~)
    =+  jon=(rush q.u.r.res apex:poja)
    ?~  jon
      json+(jobe err+s+%bad-json code+(jone p.res) body+s+q.u.r.res ~)
    ?.  =(2 (div p.res 100))
      json+(jobe err+s+%request-rejected code+(jone p.res) msg+u.jon ~)
    ?+    +.pax  ~&([%sigh-strange-path pax] null+~)
        {$issues $mine $~}
      =+  issues=((ar:jo issue:gh-parse) u.jon)
      ?~  issues
        ~&  [err+s+%response-not-issues pax+pax code+(jone p.res) msg+u.jon]
        null+~
      gh-list-issues+u.issues
    ::
        {$issues $by-repo @t @t $~}
      =+  issues=((ar:jo issue:gh-parse) u.jon)
      ?~  issues
        ~&  [err+s+%response-not-issues pax+pax code+(jone p.res) msg+u.jon]
        null+~
      gh-list-issues+u.issues
    ==
    ::  ::
    ::  ::  Once we know we have good data, we drill into the JSON
    ::  ::  to find the specific piece of data referred to by 'arg'
    ::  ::
    ::  |-  ^-  sub-result
    ::  ?~  arg
    ::    json+u.jon
    ::  =+  dir=((om:jo some) u.jon)
    ::  ?~  dir
    ::    json+(jobe err+s+%json-not-object code+(jone p.res) body+u.jon ~)
    ::  =+  new-jon=(~(get by u.dir) i.arg)
    ::  $(arg t.arg, u.jon ?~(new-jon ~ u.new-jon))
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
    ?+    +.pax  ~&([%sigh-strange-path pax] arch+*arch)
        {$issues $mine $~}
      =+  issues=((ar:jo issue:gh-parse) u.jon)
      ?~  issues
        ~&  [err+s+%response-not-issues pax+pax code+(jone p.res) msg+u.jon]
        arch+*arch
      :+  %arch  `(shax (jam u.issues))
      (malt (turn u.issues |=(issue:gh [id ~])))
    ::
        {$issues $by-repo @t $~}
      =+  repos=((ar:jo repository:gh-parse) u.jon)
      ?~  repos
        ~&  [err+s+%response-not-repos pax+pax code+(jone p.res) msg+u.jon]
        arch+*arch
      :+  %arch  `(shax (jam u.repos))
      (malt (turn u.repos |=(repository:gh [name ~])))
    ::
        {$issues $by-repo @t @t $~}
      =+  issues=((ar:jo issue:gh-parse) u.jon)
      ?~  issues
        ~&  [err+s+%response-not-issues pax+pax code+(jone p.res) msg+u.jon]
        arch+*arch
      :+  %arch  `(shax (jam u.issues))
      (malt (turn u.issues |=(issue:gh [(rsh 3 2 (scot %ui number)) ~])))
    ==
    ::  ::
    ::  ::  Once we know we have good data, we drill into the JSON
    ::  ::  to find the specific piece of data referred to by 'arg'
    ::  ::
    ::  |-  ^-  sub-result
    ::  =+  dir=((om:jo some) u.jon)
    ::  ?~  dir
    ::    [%arch `(shax (jam u.jon)) ~]
    ::  ?~  arg
    ::    [%arch `(shax (jam u.jon)) (~(run by u.dir) _~)]
    ::  =+  new-jon=(~(get by u.dir) i.arg)
    ::  $(arg t.arg, u.jon ?~(new-jon ~ u.new-jon))
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
