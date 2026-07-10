::  ahoy: migrate peers to %mesa
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
+$  state  state-1
+$  any-state
 $~  *state
 $%  state-0
     state-1
 ==
+$  state-0  [%0 _+:*state-1]
+$  state-1
  $:  %1
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
      ::  peers we can't migrate
      ::
      broken=(map ship attempt=@da)  :: XX add offending flows?
      ::  hash to trigger migration
      ::
      last-hash=_0v11.11111.11111.11111.11111.11111.
                      11111.11111.11111.11111.11111  :: disabled for now
      ::  ships with an %ahoy flow in flight
      ::
      pending-ahoy=(set ship)
      ::  verbosity
      ::
      veb=_|
  ==
::
--
=>  |%  ++  dispatch-thread
          |=  [=term test=? who=(unit ship)]
          ^-  wire
          =/  base=wire
            ::  test means different things for %prob and %comb:
            ::
            ::  (XX currently only %prob supported)
            ::
            ::    %prob is never dry, but a test migration can be skipped
            ::    %comb can be dry, so we will test the migrations on both
            ::      ends without moving the peer to .chums
            ::
            :+  %ahoy  %thread
            ?+  term  ~|(term !!)
              %comb  ?:(test /test /)
              %prob  ?:(test /prob/force /prob)
            ==
          ?~  who  base
          (snoc base (scot %p u.who))
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
  %_    this
      last-hash.sat
    0v1l.j5i77.ga13o.okjjk.tv6m9.c9pg4.7i10l.9mgpp.shtut.q530n.41il8  :: 409k-2
  ==
::
++  on-peek  abet
::
++  on-load
  |=  [hood-version=@ud old=any-state]  =<  abet
  |-  ^-  this
  ?:  ?=(%0 -.old)
    ::  disable %mesa as the default core for new peers
    ::
    $(old old(- %1), this (emit %pass /ames %arvo %a %load %ames))
  ?>  ?=(%1 -.old)
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
    %ahoy-verb         =+(!<(_~ vase) verb)
  ==
  ::
  ++  comb
    |=  [dry=? veb=? nuke=?]
    ?:  &  this  :: XX disabled
    =?  broken.sat       nuke  ~
    =?  no-response.sat  nuke  ~
    =?  hashes.sat       nuke  ~
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
    [(dispatch-thread %comb dry ~) %arvo %k %fard q.byk.bowl %comb %noun data]
  ::
  ++  time       |=(tim=@dr this(timeout.sat tim))
  ++  hash       |=(has=@uvi this(last-hash.sat has))
  ++  verb       this(veb.sat !veb.sat)
  ++  wipe       |=  who=(unit @p)  ^+  this
                 ?^(who (wipe-ship u.who^** this) (~(rep by broken.sat) wipe-ship))
  ++  wipe-ship  |=([[who=@p *] =_this] this(broken.sat (~(del by broken.sat) who)))
  ::
  ++  prob
    |=  [=ship force=?]
    ::  XX check that the peer is not in .chums.ames-state
    ::
    ::  if we know that %ahoy is not supported skip %ahoy, if last attempt
    ::  was less than a day ago
    ::
    ?:  (~(has in pending-ahoy.sat) ship)
      ~?  >>  veb.sat  "already pending"^ship
      this
    ?:  ?&  ?~  bro=(~(get by broken.sat) ship)
              %.n
            (lth (sub now.bowl attempt=u.bro) ~d1)
        ==
      this
    =/  data=^vase  !>
      ::  we use hashes.sat to start peeking from the last known case
      ::
      =/  [num=@ud has=@uvi wen=@da]
        ?~  case=(~(get by hashes.sat) ship)
          [1 0v0 now.bowl]
        u.case
      ::
      [~ timeout.sat num^has^wen ship last-hash.sat veb.sat]
    =.  pending-ahoy.sat  (~(put in pending-ahoy.sat) ship)
    %^  emit  %pass
      (dispatch-thread %prob force `ship)
    [%arvo %k %fard q.byk.bowl %prob %noun data]
  ::
  --
::
++  take-arvo
  |=  [=wire =sign-arvo]  =<  abet
  =>  .(wire `(pole knot)`wire)
  |^  ?+    wire  ~|([%ahoy-bad-take-wire wire +<.sign-arvo] !!)
      ::
          [%thread %comb rest=*]
        =/  dry=?
          ?+  rest.wire  !!
            ~            %.n
            [%test ~]    %.y
          ==
        :: XX TODO %comb
        ::
        this
      ::
          [%thread %prob rest=*]
        :: - /thread/prob       : migrate with no test migration
        :: - /thread/prob/force : force a local test migration first
        ::                         (a local test migration would crash
        ::                          if there is no flow information)
        ::  XX only for %comb; unused
        :: - /thread/test  : test migration; ship will remain in .peers)
        ::                   (we force a local migration first)
        ::
        =/  [force-test=? who=ship]
          ?+  rest.wire  !!
            [%force who=@ ~]  [& (slav %p who.rest.wire)]
            [who=@ ~]         [| (slav %p who.rest.wire)]
          ==
        (take-thread force-test who)
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
  ++  take-thread
    |=  [force-test=? who=ship]
    ?>  ?=([%khan %arow *] sign-arvo)
    ?:  ?=(%.n -.p.sign-arvo)
      ::  if the thread crashed, remove from pending so we can %prob again
      ::
      =.  pending-ahoy.sat  (~(del in pending-ahoy.sat) who)
      (flog %crud [mote tang]:p.p.sign-arvo)
    =+  !<  [[num=@ud has=@uvi wen=@da] no-response=?]
            q.p.p.sign-arvo
    ::
    =?  hashes.sat  !no-response  :: only update hashes if we did get a response
      ?.  (~(has by hashes.sat) who)
        (~(put by hashes.sat) who num^has^wen)
      ::  the ship exists; update only if this
      ::  case is higher than previous
      ::
      %+  ~(jab by hashes.sat)  who
      |=  old=[num=@ud has=@uvi wen=@da]
      ?.  (gte num num.old)  old
      [num has wen]
    ::
    =.  no-response.sat
      ::  if none of these thread's attempts give a response
      ::  mark as such.
      ::
      ::  if the ship could have been not responding before,
      ::  but we got a response, preemptively delete from no-response
      ::
      ?:  no-response
        (~(put by no-response.sat) who wen)
      (~(del by no-response.sat) who)
    ::
    =+  .^  chums=(map ship ?(%known %alien))  %ax
          /(scot %p our.bowl)//(scot %da now.bowl)/chums
        ==
    ::  ahoy the peer if on last-hash, not yet migrated, and not pending
    ::
    ?:  ?|  !=(last-hash.sat has)
            (~(has by chums) who)
        ==
      ::  delete the peer from pending and try again on next %prob
      ::
      ::  (if .who is in chums, it should have been migrated by a previous
      ::   %ahoy -- possibly from the other ship; skip)
      ::
      this(pending-ahoy.sat (~(del in pending-ahoy.sat) who))
    ?.  (~(has in pending-ahoy.sat) who)
      ::  this shouldn't happen since we only should have one active thread
      ::  and we have not deleted .who from pending yet
      ::
      this
    %-  emit
    ::  if .force-test == %.y, do test migration first
    ::
    ?:(force-test (migrate %mate who dry=%.n) (send-ahoy who dry=%.n))
  ::
  ++  take-mate
    |=  [who=@p error=(unit tang) dry=?]
    ^+  this
    ?~  error
      ::  peer still in pending; wait for the real %ahoy
      ::
      (emit (send-ahoy who dry))
    ::  flow ends here and we got a response; remove pending
    ::
    =.  pending-ahoy.sat
      (~(del in pending-ahoy.sat) who)
    ~&  >>  "ahoy: dry mate failed for {<who>}"
    this(broken.sat (~(put by broken.sat) who now.bowl))
  ::
  ++  take-ahoy
    |=  [who=@p error=(unit tang) dry=?]
    ^+  this
    ::  as soon as the ahoy comes back, delete from pending
    ::
    =.  pending-ahoy.sat  (~(del in pending-ahoy.sat) who)
    ?~  error
      (emit (migrate %migr who dry))
    ::  migration failed
    ::
    ~&  >>  "ahoy: broken %ahoy for {<who>}"
    this(broken.sat (~(put by broken.sat) who now.bowl))
  ::
  ++  take-migrate
    |=  [who=@p error=(unit tang) dry=?]
    ^+  this
    ?^  error
      ::  XX if not dry, this is bad;
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
    ==
  ::
  --
::
--
