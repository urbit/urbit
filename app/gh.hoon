::  This is a connector for the Github API v3.
::
::  You can interact with this in a few different ways:
::
::    - .^({type} %gx /=gh={/endpoint}) to read data or
::      .^(arch %gy /=gh={/endpoint}) to explore the possible
::      endpoints.
::
::    - subscribe to /listen/{owner}/{repo}/{events...} for
::      webhook-powered event notifications.  For event list, see
::      https://developer.github.com/webhooks/.
::
::  This is written with the standard structure for api
::  connectors, as described in lib/connector.hoon.
::
/?  314
/-  gh, plan-acct
/+  gh-parse, connector
::
::
=,  html
=>  |%
    ++  move  (pair bone card)
    ++  card  
      $%  {$diff sub-result}
          {$them wire (unit hiss)}
          {$hiss wire {$~ $~} $httr {$hiss hiss}}
      ==
    ::
    ::  Types of results we produce to subscribers.
    ::
    ++  sub-result
      $%  {$arch arch}
          {$gh-issue issue:gh}
          {$gh-list-issues (list issue:gh)}
          {$gh-issues issues:gh}
          {$gh-issue-comment issue-comment:gh}
          {$json json}
          {$null $~}
      ==
    ::
    ::  Types of webhooks we expect.
    ::
    ++  hook-response
      $%  {$gh-issues issues:gh}
          {$gh-issue-comment issue-comment:gh}
      ==
    --
=+  connector=(connector move sub-result)  ::  Set up connector library
::
=,  ^gall
|_  $:  hid/bowl
        hook/(map @t {id/@t listeners/(set bone)})  ::  map events to listeners
    ==
::  ++  prep  _`.  ::  Clear state when code changes
::
::  List of endpoints
::
++  places
  |=  wir/wire
  ^-  (list place:connector)
  =+  (helpers:connector ost.hid wir "https://api.github.com")
  =>  |%                              ::  gh-specific helpers
      ++  read-sentinel
        |=(pax/path [ost %diff %arch `0vsen.tinel ~])
      ::
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
        %-  malt  ^-  (list {@ta $~})
        :-  [%gh-list-issues ~]
        (turn issues |=(issue:gh [(rsh 3 2 (scot %ui number)) ~]))
      --
  :~  ^-  place                       ::  /
      :*  guard=$~
          read-x=read-null
          read-y=(read-static %issues ~)
          sigh-x=sigh-strange
          sigh-y=sigh-strange
      ==
      ^-  place                       ::  /issues
      :*  guard={$issues $~}
          read-x=read-null
          read-y=(read-static %mine %by-repo ~)
          sigh-x=sigh-strange
          sigh-y=sigh-strange
      ==
      ^-  place                       ::  /issues/mine
      :*  guard={$issues $mine $~}
          read-x=(read-get /issues)
          read-y=(read-static %gh-list-issues ~)
          sigh-x=sigh-list-issues-x
          sigh-y=sigh-list-issues-y
      ==
      ^-  place                       ::  /issues/mine/<mark>
      :*  guard={$issues $mine @t $~}
          read-x=read-null
          read-y=read-sentinel
          sigh-x=sigh-list-issues-x
          sigh-y=sigh-list-issues-y
      ==
      ^-  place                       ::  /issues/by-repo
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
      ^-  place                       ::  /issues/by-repo/<user>
      :*  guard={$issues $by-repo @t $~}
          read-x=read-null
          read-y=|=(pax/path (get /users/[-.+>.pax]/repos))
          sigh-x=sigh-strange
          ^=  sigh-y
          |=  jon/json
          %+  bind  ((ar:jo repository:gh-parse) jon)
          |=  repos/(list repository:gh)
          [~ (malt (turn repos |=(repository:gh [name ~])))]
      ==
      ^-  place                       ::  /issues/by-repo/<user>/<repo>
      :*  guard={$issues $by-repo @t @t $~}
          read-x=|=(pax/path (get /repos/[-.+>.pax]/[-.+>+.pax]/issues))
          read-y=|=(pax/path (get /repos/[-.+>.pax]/[-.+>+.pax]/issues))
          sigh-x=sigh-list-issues-x
          sigh-y=sigh-list-issues-y
      ==
      ^-  place                       ::  /issues/by-repo/<user>/<repo>/<number>
      :*  guard={$issues $by-repo @t @t @t $~}
          ^=  read-x
          |=(pax/path (get /repos/[-.+>.pax]/[-.+>+.pax]/issues/[-.+>+>.pax]))
        ::
          ^=  read-y
          |=  pax/path
          %.  pax
          ?:  ((sane %tas) -.+>+>.pax)
            read-sentinel
          (read-static %gh-issue ~)
        ::
          ^=  sigh-x
          |=  jon/json
          %+  bind  (issue:gh-parse jon)
          |=  issue/issue:gh
          gh-issue+issue
        ::
          sigh-y=sigh-strange
      ==
      ^-  place                       ::  /issues/by-repo/<u>/<r>/<n>/<mark>
      :*  guard={$issues $by-repo @t @t @t @t $~}
          read-x=read-null
          read-y=read-sentinel
          sigh-x=sigh-strange
          sigh-y=sigh-strange
      ==
  ==
::
::  When a peek on a path blocks, ford turns it into a peer on
::  /scry/{care}/{path}.  You can also just peer to this
::  directly.
::
::  We hand control to ++scry.
::
++  peer-scry
  |=  pax/path
  ^-  {(list move) _+>.$}
  ?>  ?=({care *} pax)
  :_  +>.$  :_  ~
  (read:connector ost.hid (places %read pax) i.pax t.pax)
::
::  HTTP response.  We make sure the response is good, then
::  produce the result (as JSON) to whoever sent the request.
::
++  sigh-httr
  |=  {way/wire res/httr}
  ^-  {(list move) _+>.$}
  ?.  ?=({$read care @ *} way)
    ~&  res=res
    [~ +>.$]
  =*  style  i.way
  =*  ren  i.t.way
  =*  pax  t.t.way
  :_  +>.$  :_  ~
  :+  ost.hid  %diff
  (sigh:connector (places ren style pax) ren pax res)
::
::  HTTP error.  We just print it out, though maybe we should
::  also produce a result so that the request doesn't hang?
::
++  sigh-tang
  |=  {way/wire tan/tang}
  ^-  {(list move) _+>.$}
  %-  (slog >%gh-sigh-tang< tan)
  [[ost.hid %diff null+~]~ +>.$]
::
::  We can't actually give the response to pretty much anything
::  without blocking, so we just block unconditionally.
::
++  peek
  |=  {ren/@tas tyl/path}
  ^-  (unit (unit (pair mark *)))
  ~ ::``noun/[ren tyl]
::
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::         Webhook-powered event streams (/listen)            ::
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::
::  To listen to a webhook-powered stream of events, subscribe
::  to /listen/<user>/<repo>/<events...>
::
::  We hand control to ++listen.
::
++  peer-listen
  |=  pax/path
  ^-  {(list move) _+>.$}
  ?.  ?=({@ @ *} pax)
    ~&  [%bad-listen-path pax]
    [~ +>.$]
  (listen pax)
::
::  This core handles event subscription requests by starting or
::  updating the webhook flow for each event.
::
++  listen
  |=  pax/path
  =|  mow/(list move)
  =<  abet:listen
  |%
  ++  abet                                              ::  Resolve core.
    ^-  {(list move) _+>.$}
    [(flop mow) +>.$]
  ::
  ++  send-hiss                                         ::  Send a hiss
    |=  hiz/hiss
    ^+  +>
    =+  wir=`wire`[%x %listen pax]
    +>.$(mow [[ost.hid %hiss wir `~ %httr [%hiss hiz]] mow])
  ::
  ::  Create or update a webhook to listen for a set of events.
  ::
  ++  listen
    ^+  .
    =+  pax=pax  ::  TMI-proofing
    ?>  ?=({@ @ *} pax)
    =+  events=t.t.pax
    |-  ^+  +>+.$
    ?~  events
      +>+.$
    ?:  (~(has by hook) i.events)
      $(+>+ (update-hook i.events), events t.events)
    $(+>+ (create-hook i.events), events t.events)
  ::
  ::  Set up a webhook.
  ::
  ++  create-hook
    |=  event/@t
    ^+  +>
    ?>  ?=({@ @ *} pax)
    =+  clean-event=`tape`(turn (trip event) |=(a/@tD ?:(=('_' a) '-' a)))
    =.  hook
      %+  ~(put by hook)  (crip clean-event)
      =+  %+  fall
            (~(get by hook) (crip clean-event))
          *{id/@t listeners/(set bone)}
      [id (~(put in listeners) ost.hid)]
    %-  send-hiss
    :*  %+  scan
          =+  [(trip i.pax) (trip i.t.pax)]
          "https://api.github.com/repos/{-<}/{->}/hooks"
        auri:urlp
        %post  ~  ~
        %-  as-octt  %-  en-json  %-  pairs:enjs  :~
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
--
