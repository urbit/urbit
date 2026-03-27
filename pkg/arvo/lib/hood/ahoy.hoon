::  ahoy: monitor peers' kelvin versions via remote scry
::
::  flow:
::
:: %comb poke
::   -> gather all peers in peers.ames-state
::   -> pass pending peers and last known cases to -comb
::
::  -comb thread:
::
:: on-arvo %keen response (handle-sage):
::   -> parse kelvin from response
::   -> if hash != last-hash: peek next case for same peer
::   -> if hash == last-hash: will send dry %mate (test-mate)
::   -> try next peer in pending queue
::
:: on-arvo timer fire:
::   -> peer timed out: yawn, mark no-response
::   -> try next peer in pending queue
::
::  when all peers are done, give back result to agent
::
::  for all peers which hash == last-hash, ahoy (dry mode supported)
::
:: on-arvo %mate done (take-mate):
::   -> if error: mark broken
::   -> if success: send %ahoy $plea (send-ahoy)
::   -> /test in wire = dry ahoy (test both sides without real migration)
::
:: on-arvo %ahoy ack (take-ahoy):
::   -> if error: mark broken
::   -> if success: do migration in %ames
::   -> if dry=%.y this is redundant with the first %mate
::
:: on-arvo %migrate done (take-migrate):
::   -> if error: mark broken (bad state - different protocols)
::   -> if success: mark migrated (%mesa)
::
/+  strandio
=*  card  card:agent:gall
|%
+$  state  state-0
+$  any-state
 $~  *state
 $%  state-0
 ==
+$  state-0
  $:  %0
      ::  peers not responding, as of last attempt
      ::
      no-response=(map ship attempt=@da)
      ::  last known kids hash and case per peer, timestamped
      ::
      ::
      hashes=(map ship [num=@ud has=@uvi when=@da])
      ::  timeout duration to cancel +peek
      ::
      timeout=_~s12
      ::  migrated peers
      ::
      migrants=(map ship ?(%mesa %ames))
      ::  peers we can't migrate
      ::
      broken=(map ship attempt=@da)  :: XX add offending flows?
      ::  hash to trigger migration
      ::
      last-hash=@uvi
      ::  ships with an outstanding %ahoy plea in flight
      ::
      pending-ahoy=(set ship)
  ==
::
--
=>  |%  ++  dispatch-thread
          |=  [=term test=?]
          ^-  wire
          :+  %ahoy  %thread
          ?+  term  ~|(term !!)
            %comb  ?:(test /test /)
            %prob  ?:(test /force /)
          ==
        ::
        ++  dispatch-flow
          |=  [=term =ship test=?]
          ^-  wire
          :+  %ahoy  term
          ?:(test /test/(scot %p ship) /(scot %p ship))
        ::
        ::
        ++  migrate
          |=  [=term =ship test=?]
          ^-  card
          :+  %pass
            (dispatch-flow term ship test)
          =/  dry=?
            ?:  =(%mate term)  %.y
            ?>  =(%migr term)
            test
          [%arvo %a %mate `ship dry]
        ::
        ++  send-ahoy
          |=  [=ship test=?]
          ^-  card
          =/  =path  ?:(test /test/mesa-2 /mesa-2)
          [%pass (dispatch-flow %send ship test) %arvo %a %plea ship %$ path %ahoy ~]
    ::
    --
::
|=  [=bowl:gall sat=state]
=|  moz=(list card)
|%
++  this  .
+$  state      ^state      ::  proxy
+$  any-state  ^any-state  ::  proxy
++  abet  [(flop moz) sat]
++  flog  |=(=flog:dill (emit %pass /di %arvo %d %flog flog))
++  emit  |=(card this(moz [+< moz]))
++  emil
  |=  caz=(list card)
  ^+  this
  ?~(caz this $(caz t.caz, this (emit i.caz)))
::
++  on-init  =<  abet
  =.  this  (emit %pass /ahoy/chums %arvo %b %wait now.bowl)
  %_    this
      last-hash.sat
    0vq.kuk4m.cqifa.8eq29.9dp1q.utcd7.bakuk.h9es3.cpbcf.nkf4r.pjehb
  ==
::
++  on-peek  abet
::
++  on-load
  |=  [hood-version=@ud old=any-state]  =<  abet
  ?>  ?=(%0 -.old)
  this(sat old)
::  handle ahoy actions:
::
::  [%comb dry=? veb=? nuke=?]     ::  start checking one peer at at time
::                                 ::  as soon as we get a response, or
::                                 ::  we timeout, we try the next peer
::                                 ::
::  [%prob who=@p force=? dry=?]   ::  start %ahoy flow only for .who
::  [%cancel ~]                    ::  cancel all pending checks
::  [%set-timeout dur=@dr]         ::  change timeout duration
::  [%set-hash has=@uvi]           ::  change kids hash to trigger migration
::  [%refresh dry=?]               ::  only no-response peers
::  [%update dry=?]                ::  only peers not on latest hash
::  [%wipe ship=(unit @p)]         ::  remove .ship from broken
::
::
++  poke
  |=  [=mark =vase]  =<  abet
  ?>  =(our src):bowl
  |^  ?+  mark  ~|([%poke-ahoy-bad-mark mark] !!)
    %ahoy-comb         =;(f (f !<(_+<.f vase)) comb)
    %ahoy-prob         =;(f (f !<(_+<.f vase)) prob)
    %ahoy-cancel       this
    %ahoy-set-timeout  =;(f (f !<(_+<.f vase)) time)
    %ahoy-set-hash     =;(f (f !<(_+<.f vase)) hash)
    %ahoy-refresh      this
    %ahoy-update       this
    %ahoy-wipe         =;(f (f !<(_+<.f vase)) wipe)
  ==
  ::
  ++  comb
    |=  [dry=? veb=? nuke=?]
    =?  broken.sat       nuke  ~
    =?  no-response.sat  nuke  ~
    =?  hashes.sat       nuke  ~
    =?  migrants.sat     nuke  ~
    =?  this  nuke
      (emit %pass /ahoy/chums %arvo %b %wait now.bowl)
    ::  get all peers from ames
    ::
    =/  peers=(map ship ?(%alien %known))
      .^  (map ship ?(%alien %known))  %ax
        /(scot %p our.bowl)//(scot %da now.bowl)/peers
      ==
    ::  filter to just known peers (not aliens, those are offline)
    ::
    =/  timeout-time  (add now.bowl timeout.sat)
    =/  pend=(list ship)
      %-  flop  ^-  (list ship=@p)
      %-   ~(rep by peers)
      |=  [[who=ship sta=?(%alien %known)] p=(list ship)]
      ?:  =(our.bowl who)  p
      ?.  ?=(%known sta)   p
      ::  ignore comets
      ::
      ?:  ?=(%pawn (clan:title who))
        p
      who^p
    ::
    =/  data=^vase  !>([~ timeout.sat hashes.sat pend last-hash.sat veb])
    %+  emit  %pass
    [(dispatch-thread %comb dry) %arvo %k %fard q.byk.bowl %comb %noun data]
  ::
  ++  time       |=(tim=@dr this(timeout.sat tim))
  ++  hash       |=(has=@uvi this(last-hash.sat has))
  ++  wipe       |=  who=(unit @p)  ^+  this
                 ?^(who (wipe-ship u.who^** this) (~(rep by broken.sat) wipe-ship))
  ++  wipe-ship  |=([[who=@p *] =_this] this(broken.sat (~(del by broken.sat) who)))
  ::
  ++  prob
    |=  [=ship force=? dry=?]
    ::  XX check that the peer is not in .chums.ames-state
    ::
    ::  if we know that %ahoy is not supported skip %ahoy, if last attempt
    ::  was less than a day ago
    ::
    ?:  (~(has in pending-ahoy.sat) ship)
      this
    ?:  ?&  ?~  bro=(~(get by broken.sat) ship)
              %.n
            (lth (sub now.bowl attempt=u.bro) ~d1)
        ==
      this
    =/  data=^vase  !>([~ timeout.sat hashes.sat [ship]~ last-hash.sat veb=|])
    %^  emit  %pass
      (dispatch-thread ?:(dry comb/dry=& prob/force))
    [%arvo %k %fard q.byk.bowl %comb %noun data]
  ::
  --
  ::
::
++  take-arvo
  |=  [=wire =sign-arvo]  =<  abet
  =>  .(wire `(pole knot)`wire)
  |^  ?+    wire  ~|([%ahoy-bad-take-wire wire +<.sign-arvo] !!)
      ::  deferred on-init timer to fill out migrant %chums
      ::
          [%chums *]
        (take-timer ?>(?=(%wake +<.sign-arvo) +>.sign-arvo))
      ::
          [%thread rest=*]
        :: - /thread       : migrate with no test migration
        :: - /thread/force : force a local test migration first
        ::                   (a local test migration would crash if there
        ::                    is no flow information)
        :: - /thread/test  : test migration; ship will remain in .peers)
        ::                   (we force a local migration first)
        ::
        =/  [force-test=? dry=?]
          ?+  rest.wire  |^|
            [%force *]   &^|
            [%test *]    &^&
          ==
        (take-thread force-test dry)
      ::
          [?(%mate %send %migr) rest=*]
      ::  %ahoy flow:  XX move to a thread?
      ::
      ::   1. get %done for test migration locally; if no error send ahoy
      ::
      ::   2. get %ack for ahoy, if no error, migrate
      ::
      ::   3. get %done for local migration; it should not have errored
      ::
        =/  [test=? who=@p]
          ?+  rest.wire  ~|(wire !!)
            [who=@ ~]        [%.n (slav %p who.rest.wire)]
            [%test who=@ ~]  [%.y (slav %p who.rest.wire)]
          ==
        %.  [who ?>(?=(%done +<.sign-arvo) +>.sign-arvo) test]
        ?+  wire  !!
          [%mate *]  take-mate
          [%send *]  take-ahoy
          [%migr *]  take-migrate
        ==
      ==
  ::
  ++  take-timer
    |=  error=(unit tang)
    ::  scry for chums and fill out migrated peers
    ::
    =+  .^  chums=(map ship ?(%known %alien))  %ax
          /(scot %p our.bowl)//(scot %da now.bowl)/chums
        ==
    %_    this
        migrants.sat
      %-  ~(rep by chums)
      |=  [[=ship s=?(%known %alien)] migs=_migrants.sat]
      (~(put by migs) ship %mesa)
    ==
  ::
  ++  take-thread
    |=  [force-test=? dry=?]
    ?>  ?=([%khan %arow *] sign-arvo)
    ?:  ?=(%.n -.p.sign-arvo)
      (flog %crud [mote tang]:p.p.sign-arvo)
    =+  !<([=_hashes.sat =_no-response.sat] q.p.p.sign-arvo)
    =:       hashes.sat  (~(uni by hashes.sat) hashes)
        no-response.sat  (~(uni by no-response.sat) no-response)
      ==
    =+  .^  chums=(map ship ?(%known %alien))  %ax
          /(scot %p our.bowl)//(scot %da now.bowl)/chums
        ==
    %-  emil
    ::  ahoy peers on last-hash not yet migrated (skip if %ahoy is pending)
    ::
    %-  ~(rep by hashes)
    |=  [[who=@p [num=@ud has=@uvi when=@da]] moz=_moz]
    ?.  =(last-hash.sat has)  moz
    ::  XX do last +peek check to see if online?
    ::
    ::  filter by last hash and start %ahoying with a test
    ::  migration first
    ::
    ?:  (~(has by chums) who)
      ::  if .who has been migrated by a previous %ahoy; skip
      ::
      moz
    ?:  (~(has in pending-ahoy.sat) who)
      moz
    :_  moz
    ?:  force-test
      (migrate %mate who dry)
    =.  pending-ahoy.sat  (~(put in pending-ahoy.sat) who)
    (send-ahoy who dry)
  ::
  ++  take-mate
    |=  [who=@p error=(unit tang) dry=?]
    ^+  this
    ?^  error
      ~&  >>  "ahoy: dry mate failed for {<who>}"
      this(broken.sat (~(put by broken.sat) who now.bowl))
    ::  mate succeded; ahoy
    ::
    ?:  (~(has in pending-ahoy.sat) who)
      this
    (emit (send-ahoy who dry))
  ::
  ++  take-ahoy
    |=  [who=@p error=(unit tang) dry=?]
    ^+  this
    =.  pending-ahoy.sat  (~(del in pending-ahoy.sat) who)
    ?^  error
      ~&  >>  "ahoy: broken %ahoy for {<who>}"
      this(broken.sat (~(put by broken.sat) who now.bowl))  ::  migrate failed
    (emit (migrate %migr who dry))
  ::
  ++  take-migrate
    |=  [who=@p error=(unit tang) dry=?]
    ^+  this
    ?^  error
      ::  XX if not a dry, this is bad;
      ::     they won't be able to communicate since they are on different
      ::     sides of the protocol and we will have to manually do:
      ::     `|pass [%a %rege `ship dry=%.n]` on the other ship to restore coms
      ::
      ~&  >>>  "ahoy: broken migration for {<who>}"
      this(broken.sat (~(put by broken.sat) who now.bowl))  ::  migrate failed
    =.  this
      %+  flog  %text
      "ahoy: %mesa {?:(dry "dry " "")}migration completed for {<who>}"
    ::  migration succeded
    ::
    %_  this
      ::  if we previously encountered a broken flow
      ::  XX  while doing a dry migration?
      ::
      broken.sat    (~(del by broken.sat) who)
      migrants.sat  ?:(dry migrants.sat (~(put by migrants.sat) who %mesa))
    ==
  ::
  --
::
--
