::  This is a driver for the Github API v3.
::
::  You can interact with this in a few different ways:
::
::    - .^(%gx /=gh=/read-unauth{/endpoint}) or subscribe to
::      /scry/x/read-unauth{/endpoint} for unauthenticated reads.
::
::    - .^(%gx /=gh=/read-auth{/endpoint}) or subscribe to
::      /scry/x/read-auth{/endpoint} for authenticated reads.
::
::    - subscribe to /scry/x/listen/{owner}/{repo}/{events...}
::      for webhook-powered event notifications.  For event list,
::      see https://developer.github.com/webhooks/.
::
::  See the %github app for example usage.
/?    314
/-  gh
=>  |%
    ++  move  (pair bone card)
    ++  sub-result
      $%  [%json json]
          [%gh-issues issues:gh]
          [%gh-issue-comment issue-comment:gh]
      ==
    ++  card  
      $%  [%diff sub-result]
          [%them wire (unit hiss)]
          [%hiss wire [~ ~] %httr [%hiss hiss]]
      ==
    ++  hook-response
      $%  [%gh-issues issues:gh]
          [%gh-issue-comment issue-comment:gh]
      ==
    --
|_  [hid=bowl cnt=@ hook=(map ,@t ,[id=@t listeners=(set bone)])]
::++  prep  ,_`.
::
::  This core manages everything related to a particular request.
::
::  Each request has a particular 'style', which is currently
::  one of 'read-auth', 'read-unauth', or 'listen'.  ++scry
::  handles all three types of requests.
++  help
  |=  [style=@tas pax=path]
  =|  mow=(list move)
  |%
  ::  Resolve core.
  ++  abet
    ^-  [(list move) _+>.$]
    [(flop mow) +>.$]
  ::
  ::  Append path to api.github.com and parse to a purl.
  ++  real-pax  (scan "https://api.github.com{<`path`pax>}" auri:epur)
  ::
  ::  Send a hiss with the correct authentication
  ++  send-hiss 
    |=  [aut=? hiz=hiss]
    ^+  +>
    =+  wir=`wire`[%x (scot %ud cnt) pax]
    =+  ^=  new-move
        ?.  aut  [ost.hid %them wir ~ hiz]
        [ost.hid %hiss wir `~ %httr [%hiss hiz]]
    +>.$(mow [new-move mow])
  ::
  ::  Decide how to handle a request based on its style.
  ++  scry
    ^+  .
    =-  ~&  [%requesting -]  -
    ?+  style  ~|(%invalid-style !!)
      %read-unauth  read-unauth
      %read-auth    read-auth
      %listen       listen
    ==
  ::
  ++  read-unauth  (send-hiss | real-pax %get ~ ~)
  ++  read-auth    (send-hiss & real-pax %get ~ ~)
  ::
  ::  Create or update a webhook to listen for a set of events.
  ++  listen
    ^+  .
    ?>  ?=([@ @ *] pax)
    =+  events=t.t.pax
    |-  ^+  +>.$
    ?~  events
      +>.$
    ?:  (~(has by hook) i.events)
      =.  +>.$  =>((update-hook i.events) ?>(?=([@ @ *] pax) .))
      $(events t.events)
    =.  +>.$  =>((create-hook i.events) ?>(?=([@ @ *] pax) .))
    $(events t.events)
  ::
  ::  Set up a webhook.
  ++  create-hook
    |=  event=@t
    ^+  +>
    ?>  ?=([@ @ *] pax)
    =+  clean-event=`tape`(turn (trip event) |=(a=@tD ?:(=('_' a) '-' a)))
    =.  hook
      %+  ~(put by hook)  (crip clean-event)
      =+  %+  fall
            (~(get by hook) (crip clean-event))
          *[id=@t listeners=(set bone)]
      [id (~(put in listeners) ost.hid)]
    %-  send-hiss
    :*  &
        %+  scan
          =+  [(trip i.pax) (trip i.t.pax)]
          "https://api.github.com/repos/{-<}/{->}/hooks"
        auri:epur
        %post  ~  ~
        %-  taco  %-  crip  %-  pojo  %-  jobe  :~
          name/s/%web
          active/b/&
          events/a/~[s/event] ::(turn `(list ,@t)`t.t.pax |=(a=@t s/a))
          :-  %config
          %-  jobe  :~
            =+  =+  clean-event
                "http://107.170.195.5:8443/~/to/gh/gh-{-}.json?anon&wire=/"
            [%url s/(crip -)]
            [%'content_type' s/%json]
          ==
        ==
    ==
  ::
  ::  Add current bone to the list of subscribers for this event.
  ++  update-hook
    |=  event=@t
    ^+  +>
    ?>  ?=([@ @ @ *] pax)
    =+  hok=(~(got by hook) event)
    %_    +>.$
        hook
      %+  ~(put by hook)  event
      hok(listeners (~(put in listeners.hok) ost.hid))
    ==
  --
::
::  Pokes that haven't already been caught are handled here.
::  These should be only from webhooks firing, so if we get any
::  mark that we shouldn't get from a webhook, we reject it.
::  Otherwise, we spam out the event to everyone who's listening
::  for that event.
++  poke
  |=  response=hook-response
  ^-  [(list move) _+>.$]
  =+  hook-data=(~(get by hook) (rsh 3 3 -.response))
  ?~  hook-data
    ~&  [%strange-hook hook response]
    `+>.$
  ~&  response=response
  :_  +>.$
  %+  turn  (~(tap in listeners.u.hook-data))
  |=  ost=bone
  [ost %diff response]
::
::  When a peek on a path blocks, ford turns it into a peer on
::  /scry/{care}/{path}.  You can also just peer to this
::  directly.
::
::  After some sanity checking we hand control to ++scry in
::  ++help.
++  peer-scry-x
  |=  pax=path
  ^-  [(list move) _+>.$]
  ?>  ?=(^ pax)
  =-  ~&  [%peered -]  -
  [abet(cnt now.hid)]:scry:(help i.pax t.pax)
::
::  HTTP response.  We make sure the response is good, then
::  produce the result (as JSON) to whoever sent the request.
++  sigh-httr-x  thou-x
++  thou-x
  |=  [way=wire res=httr]
  ^-  [(list move) _+>.$]
  ?>  ?=([@ *] way)
  :_  +>.$  :_  ~
  :^  ost.hid  %diff  %json
  ?~  r.res
    (jobe err/s/%empty-response code/(jone p.res) ~)
  =+  (rush q.u.r.res apex:poja)
  ?~  -
    (jobe err/s/%bad-json code/(jone p.res) body/s/q.u.r.res ~)
  ?.  =(2 (div p.res 100))
    (jobe err/s/%request-rejected code/(jone p.res) msg/u.- ~)
  u.-
::
::  We can't actually give the response to pretty much anything
::  without blocking, so we just block unconditionally.
++  peek
  |=  [ren=@tas tyl=path]
  ^-  (unit (unit (pair mark ,*)))
  ~ ::``noun/[ren tyl]
--




::
::++  poke-json
::  |=  jon=json
::  ^-  [(list move) _+>.$]
::  =+  ^-  [repo=json sender=json hok=(list ,@t) hook-id=@t zen=json]
::      %-  need
::      %.  jon
::      =>  jo
::      (ot repository/some sender/some hook/(ot events/(ar so) ~) 'hook_id'^no zen/some ~)
::  ?.  ?=([@ ~] hok)
::    ~&  [%weird-hook hook-id hok]
::    [~ +>.$]
::  ~&  [%id hook-id hok]
::  =+  old-bones=`(set bone)`(biff (~(get by hook) i.hok) tail)
::  [~ +>.$(hook (~(put by hook) i.hok [hook-id (~(put in old-bones) ost.hid)]))]
.
