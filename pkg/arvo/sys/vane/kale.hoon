!:                                                      ::  /vane/kale
::                                                      ::  %reference/0
!?  150
::
::
::  %kale: secrets and promises.
::
::  todo:
::
::    - communication with other vanes:
::      - actually use %behn for expiring secrets
::      - report %ames propagation errors to user
::
::    - nice features:
::      - scry namespace
::      - task for converting invites to tickets
::
|=  pit/vase
=,  pki:kale
=,  able:kale
=,  crypto
=,  kale
=,  ethereum
=,  rpc
=,  azimuth
=,  point=point:able:kale
::                                                      ::::
::::                    # models                        ::  data structures
  ::                                                    ::::
::  the %kale state comes in two parts: absolute
::  and relative.
::
::  ++state-relative is subjective, denormalized and
::  derived.  it consists of all the state we need to
::  manage subscriptions efficiently.
::
=>  |%
+$  state                                               ::  all vane state
  $:  ver=$0                                            ::  vane version
      pki=state-pki                                     ::  
      etn=state-eth-node                                ::  eth connection state
      sap=state-snapshots                               ::  state snapshots
  ==                                                    ::
+$  state-pki                                           ::  urbit metadata
  $:  $=  own                                           ::  vault (vein)
        $:  yen=(set duct)                              ::  trackers
            sig=(unit oath)                             ::  for a moon
            tuf=(list turf)                             ::  domains
            boq=@ud                                     ::  boot block
            nod=(unit purl:eyre)                        ::  eth gateway
            fak=_|                                      ::  fake keys
            lyf=life                                    ::  version
            jaw=(map life ring)                         ::  private keys
        ==                                              ::
      $=  zim                                           ::  ethereum (vent)
        $:  yen=(jug duct ship)                         ::  trackers
            ney=(jug ship duct)                         ::  reverse trackers
            dns=dnses                                   ::  on-chain dns state
            pos=(map ship point)                        ::  on-chain ship state
        ==                                              ::
  ==                                                    ::
+$  state-snapshots                                     ::  rewind points
  $:  interval=_100                                     ::  block interval
      max-count=_10                                     ::  max snaps
      count=@ud                                         ::  length of snaps
      last-block=@ud                                    ::  number of last snap
      snaps=(qeu [block-number=@ud snap=snapshot])      ::  old states
  ==                                                    ::
+$  message                                             ::  message to her kale
  $%  [%nuke whos=(set ship)]                           ::  cancel trackers
      [%public-keys whos=(set ship)]                    ::  view ethereum events
      [%public-keys-result who=ship =vent-result]       ::  tmp workaround
  ==                                                    ::
+$  card                                                ::  i/o action
  (wind note gift)                                      ::
::                                                      ::
+$  move                                                ::  output
  [p=duct q=card]                                       ::
::                                                      ::
+$  note                                                ::  out request $->
  $~  [%a %memo *ship *message:ames]                    ::
  $%  $:  %a                                            ::    to %ames
          $>(%memo task:able:ames)                      ::  send message
      ==                                                ::
      $:  %k                                            ::    to self
          $>(%look task)                                ::  set ethereum source
      ==                                                ::
      $:  @tas                                          ::
  $%  $>(%init vane-task)                               ::  report install
  ==  ==  ==                                            ::
::                                                      ::
+$  sign                                                ::  in result $<-
  $~  [%a %done ~]                                      ::
  $%  [%a $>(%memo gift:able:ames)]                     ::  message result
      [%a $>(%done gift:able:ames)]                     ::  message ack
  ==                                                    ::
--  ::
::                                                      ::::
::::                    # light                         ::  light cores
  ::                                                    ::::
=>  |%
::                                                      ::  ++ez
::::                    ## ethereum^light               ::  wallet algebra
  ::                                                    ::::
++  ez
  ::  simple ethereum-related utility arms.
  ::
  |%
  ::
  ::  +order-events: sort changes by block and log numbers
  ::
  ++  order-events
    |=  loz=(list (pair event-id diff-azimuth))
    ^+  loz
    %+  sort  loz
    ::  sort by block number, then by event log number,
    ::TODO  then by diff priority.
    |=  [[[b1=@ud l1=@ud] *] [[b2=@ud l2=@ud] *]]
    ?.  =(b1 b2)  (lth b1 b2)
    ?.  =(l1 l2)  (lth l1 l2)
    &
  --
--
::                                                      ::::
::::                    #  heavy                        ::  heavy engines
  ::                                                    ::::
=>  |%
::                                                      ::  ++of
::::                    ## main^heavy                   ::  main engine
  ::                                                    ::::
++  of
  ::  this core handles all top-level %kale semantics,
  ::  changing state and recording moves.
  ::
  ::  logically we could nest the ++su core within it, but
  ::  we keep them separated for clarity.  the ++curd and
  ::  ++cure arms complete relative and absolute effects,
  ::  respectively, at the top level.
  ::
  ::  XX doc
  ::
  ::  a general pattern here is that we use the ++et core
  ::  to generate absolute effects (++change), then invoke
  ::  ++su to calculate the derived effect of these changes.
  ::
  ::  for ethereum-related events, this is preceded by
  ::  invocation of ++et, which produces ethereum-level
  ::  changes (++chain). these get turned into absolute
  ::  effects by ++cute.
  ::
  ::  arvo issues: should be merged with the top-level
  ::  vane interface when that gets cleaned up a bit.
  ::
  =|  moz/(list move)
  =|  $:  $:  ::  our: identity
              ::  now: current time
              ::  eny: unique entropy
              ::
              our=ship
              now=@da
              eny=@uvJ
          ==
          ::  all vane state
          ::
          state
      ==
  ::  lex: all durable state
  ::  moz: pending actions
  ::
  =*  lex  ->
  |%
  ::                                                    ::  ++abet:of
  ++  abet                                              ::  resolve
    [(flop moz) lex]
  ::                                                    ::  ++sein:of
  ++  sein                                              ::  sponsor
    |=  who=ship
    ^-  ship
    ::  XX save %dawn sponsor in .own.sub, check there
    ::
    =/  pot  (~(get by pos.zim.pki) who)
    ?:  ?&  ?=(^ pot)
            ?=(^ sponsor.u.pot)
        ==
      u.sponsor.u.pot
    (^sein:title who)
  ::                                                    ::  ++saxo:of
  ++  saxo                                              ::  sponsorship chain
    |=  who/ship
    ^-  (list ship)
    =/  dad  (sein who)
    [who ?:(=(who dad) ~ $(who dad))]
  ::                                                    ::  ++call:of
  ++  call                                              ::  invoke
    |=  $:  ::  hen: event cause
            ::  tac: event data
            ::
            hen/duct
            tac/task
        ==
    ^+  +>
    ?-    -.tac
    ::
    ::  boot from keys
    ::    $:  $dawn
    ::        =seed
    ::        spon=ship
    ::        czar=(map ship [=life =pass])
    ::        turf=(list turf)}
    ::        bloq=@ud
    ::        node=purl
    ::    ==
    ::
        %dawn
      ::  single-homed
      ::
      ?>  =(our who.seed.tac)
      ::  save our boot block
      ::
      =.  boq.own.pki  bloq.tac
      ::  save our ethereum gateway (required for galaxies)
      ::
      =.  nod.own.pki  node.tac
      ::  save our parent signature (only for moons)
      ::
      =.  sig.own.pki  sig.seed.tac
      ::  if we're given a snapshot, restore it
      ::
      =.  +>.$
        ?~  snap.tac  +>.$
        (restore-snap hen u.snap.tac |)
      ::  load our initial public key, overriding snapshot
      ::
      =.  pos.zim.pki
        =/  cub  (nol:nu:crub:crypto key.seed.tac)
        %+  ~(put by pos.zim.pki)
          our
        [1 lyf.seed.tac (my [lyf.seed.tac [1 pub:ex:cub]] ~) `spon.tac]
      ::  our initial private key
      ::
      =.  lyf.own.pki  lyf.seed.tac
      =.  jaw.own.pki  (my [lyf.seed.tac key.seed.tac] ~)
      ::  XX save sponsor in .own.pki
      ::  XX reconcile with .dns.eth
      ::  set initial domains
      ::
      =.  tuf.own.pki  turf.tac
      ::  our initial galaxy table as a +map from +life to +public
      ::
      =/  point-diffs=(list [who=ship =point-diff])
        %~  tap  by
        %-  ~(run by czar.tac)
        |=([=life =pass] `point-diff`[%changed-keys life 1 pass])
      =.  +>.$
        |-  ^+  +>.^$
        ?~  point-diffs
          +>.^$
        =.  +>.^$
          %-  curd  =<  abet
          %-  public-keys:~(feel su hen our pki etn sap)
          [%diff who point-diff]:i.point-diffs
        $(point-diffs t.point-diffs)
      ::
      =.  moz
        %+  weld  moz
        ::  order is crucial!
        ::
        ::    %dill must init after %gall
        ::    the %give init (for unix) must be after %dill init
        ::    %kale init must be deferred (makes http requests)
        ::
        ^-  (list move)
        :~  [hen %give %init our]
            [hen %slip %e %init our]
            [hen %slip %d %init our]
            [hen %slip %g %init our]
            [hen %slip %c %init our]
            [hen %slip %a %init our]
        ==
      +>.$
    ::
    ::  boot fake
    ::    [%fake =ship]
    ::
        %fake
      ::  single-homed
      ::
      ?>  =(our ship.tac)
      ::  fake keys are deterministically derived from the ship
      ::
      =/  cub  (pit:nu:crub:crypto 512 our)
      ::  save our parent signature (only for moons)
      ::
      ::    XX move logic to zuse
      ::
      =.  sig.own.pki
        ?.  ?=(%earl (clan:title our))
          ~
        =/  yig  (pit:nu:crub:crypto 512 (^sein:title our))
        [~ (sign:as:yig (shaf %earl (sham our 1 pub:ex:cub)))]
      ::  our initial public key
      ::
      =.  pos.zim.pki
        %+  ~(put by pos.zim.pki)
          our
        [rift=1 life=1 (my [`@ud`1 [`life`1 pub:ex:cub]] ~) `(^sein:title our)]
      ::  our private key
      ::
      ::    Private key updates are disallowed for fake ships,
      ::    so we do this first.
      ::
      =.  lyf.own.pki  1
      =.  jaw.own.pki  (my [1 sec:ex:cub] ~)
      ::  set the fake bit
      ::
      =.  fak.own.pki  &
      ::  initialize other vanes per the usual procedure
      ::
      ::    Except for ourselves!
      ::
      =.  moz
        %+  weld  moz
        ^-  (list move)
        :~  [hen %give %init our]
            [hen %slip %e %init our]
            [hen %slip %d %init our]
            [hen %slip %g %init our]
            [hen %slip %c %init our]
            [hen %slip %a %init our]
        ==
      +>.$
    ::
    ::  set ethereum source
    ::    [%look p=(each ship purl)]
    ::
        %look
      %-  curd  =<  abet
      (sources:~(feel su hen our pki etn sap) [whos source]:tac)
    ::
    ::  cancel all trackers from duct
    ::    {$nuke whos=(set ship)}
    ::
        $nuke
      =/  ships=(list ship)
        %~  tap  in
        %-  ~(int in whos.tac)
        (~(get ju yen.zim.pki) hen)
      =.  ney.zim.pki
        |-  ^-  (jug ship duct)
        ?~  ships
          ney.zim.pki
        (~(del ju $(ships t.ships)) i.ships hen)
      =.  yen.zim.pki
        |-  ^-  (jug duct ship)
        ?~  ships
          yen.zim.pki
        (~(del ju $(ships t.ships)) hen i.ships)
      ?^  whos.tac
        +>.$
      %_  +>.$
        yen.own.pki  (~(del in yen.own.pki) hen)
        yen.etn      (~(del in yen.etn) hen)
      ==
    ::
    ::  watch public keys
    ::    [%public-keys ships=(set ship)]
    ::
        %public-keys
      %-  curd  =<  abet
      (~(public-keys ~(feed su hen our pki etn sap) hen) ships.tac)
    ::
    ::  seen after breach
    ::    [%meet our=ship who=ship]
    ::
        %meet
      ::  XX what do
      ~&  %meet-kale
      +>.$
    ::
    ::  restore snapshot
    ::    [%snap snap=snapshot kick=?]
        %snap
      (restore-snap hen snap.tac kick.tac)
    ::
    ::  sources subscription
    ::    [%sources ~]
    ::
        %sources
      (curd abet:~(sources ~(feed su hen our pki etn sap) hen))
    ::
    ::  XX should be a subscription
    ::  XX reconcile with .dns.eth
    ::  request domains
    ::    [%turf ~]
    ::
        %turf
      ::  ships with real keys must have domains,
      ::  those with fake keys must not
      ::
      ?<  =(fak.own.pki ?=(^ tuf.own.pki))
      +>.$(moz [[hen %give %turf tuf.own.pki] moz])
    ::
    ::  Update from app
    ::    [%vent-update =vent-result]
    ::
        %vent-update
      %-  curd  =<  abet
      (public-keys:~(feel su hen our pki etn sap) vent-result.tac)
    ::
    ::  learn of kernel upgrade
    ::    [%vega ~]
    ::
        %vega
      +>.$
    ::
    ::  watch private keys
    ::    {$private-keys $~}
    ::
        %private-keys
      (curd abet:~(private-keys ~(feed su hen our pki etn sap) hen))
    ::
        %wegh
      %_    +>
          moz
        :_  moz
        ^-  move
        :^  hen  %give  %mass
        ^-  mass
        :+  %kale  %|
        :~  pki+&+pki
            etn+&+etn
            sap+&+sap
            dot+&+lex
        ==
      ==
    ::
    ::  authenticated remote request
    ::    {$west p/ship q/path r/*}
    ::
        $west
      =*  her  p.tac
      =/  mes  (message r.tac)
      ?-    -.mes
      ::
      ::  cancel trackers
      ::    [%nuke whos=(set ship)]
      ::
          %nuke
        =.  moz  [[hen %give %mack ~] moz]
        $(tac mes)
      ::
      ::  view ethereum events
      ::    [%public-keys whos=(set ship)]
      ::
          %public-keys
        =.  moz  [[hen %give %mack ~] moz]
        $(tac mes)
      ::
      ::  receive keys result
      ::    [%public-keys-result =vent-result]
      ::
          %public-keys-result
        =.  moz  [[hen %give %mack ~] moz]
        $(tac [%vent-update vent-result.mes])
      ==
    ::
    ::  rewind to snapshot
    ::    {$wind p/@ud}
    ::
        %wind
      (wind hen p.tac)
    ==
  ::
  ++  take
    |=  [tea=wire hen=duct hin=sign]
    ^+  +>
    ?>  ?=([@ *] tea)
    =*  wir  t.tea
    ?-  hin
        [%a %woot *]
      ?~  q.hin  +>.$
      ?~  u.q.hin  ~&(%ares-fine +>.$)
      ~&  [%woot-bad p.u.u.q.hin]
      ~_  q.u.u.q.hin
      ::TODO  fail:et
      +>.$
    ==
  ::                                                    ::  ++curd:of
  ++  curd                                              ::  relative moves
    |=  $:  moz/(list move)
            pki/state-pki
            etn/state-eth-node
            sap/state-snapshots
        ==
    +>(pki pki, etn etn, sap sap, moz (weld (flop moz) ^moz))
  ::                                                    ::  ++wind:of
  ++  wind                                              ::  rewind to snap
    |=  [hen=duct block=@ud]
    ^+  +>
    ::  XX  what do
    !!
  ::                                                    ::  ++restore-block:of
  ++  restore-block                                     ::  rewind before block
    |=  [hen=duct block=@ud]
    !!
    ::  %+  cute  hen  =<  abet
    ::  XX
    ::  (~(restore-block et hen our now sub.lex etn.lex sap.lex) block)
  ::                                                    ::  ++restore-snap:of
  ++  restore-snap                                      ::  restore snapshot
    |=  [hen=duct snap=snapshot look=?]
    !!
    ::  %+  cute  hen  =<  abet
    ::  XX
    ::  %-  ~(restore-snap et hen our now sub.lex etn.lex sap.lex)
    ::  [snap look]
  --
::                                                      ::  ++su
::::                    ## relative^heavy               ::  subjective engine
  ::                                                    ::::
++  su
      ::  the ++su core handles all derived state,
      ::  subscriptions, and actions.
      ::
      ::  ++feed:su registers subscriptions.
      ::
      ::  ++feel:su checks if a ++change should notify
      ::  any subscribers.
      ::
  =|  moz=(list move)
  =|  $:  hen/duct
          our/ship
          state-pki
          state-eth-node
          state-snapshots
      ==
  ::  moz: moves in reverse order
  ::  pki: relative urbit state
  ::
  =*  pki  ->+<
  =*  etn  ->+>-
  =*  sap  ->+>+
  |%
  ++  this-su  .
  ::                                                    ::  ++abet:su
  ++  abet                                              ::  resolve
    [(flop moz) pki etn sap]
  ::                                                    ::  ++exec:su
  ++  emit
    |=  =move
    +>.$(moz [move moz])
  ::
  ++  exec                                              ::  mass gift
    |=  {yen/(set duct) cad/card}
    =/  noy  ~(tap in yen)
    |-  ^+  this-su
    ?~  noy  this-su
    $(noy t.noy, moz [[i.noy cad] moz])
  ::
  ++  vent-give
    |=  [yen=(set duct) =vent-result]
    =+  yez=~(tap in yen)
    |-  ^+  this-su
    ?~  yez  this-su
    =*  d  i.yez
    ?.  ?=([[%a @ @ *] *] d)
      %-  emit
      [d %give %public-keys vent-result]
    =/  our  (slav %p i.t.i.d)
    =/  who  (slav %p i.t.t.i.d)
    =/  =message  [%public-keys-result who vent-result]
    =.  this-su
      %-  emit
      :^    d
          %pass
        /public-keys-result
      ^-  note
      [%a %want who /k/public-keys-result message]
    $(yez t.yez)
  ::
  ++  get-source
    |=  who=@p
    ^-  source
    =/  ship-source  (~(get by ship-sources.etn) who)
    ?^  ship-source
      (~(got by sources) u.ship-source)
    ?:  =((clan:title who) %earl)
      [%& (^sein:title who)]
    (~(got by sources) default-source.etn)
  ::
  ++  get-source-id
    |=  =source
    ^-  [source-id _this-su]
    =/  source-reverse  (~(get by sources-reverse) source)
    ?^  source-reverse
      [u.source-reverse this-su]
    :-  top-source-id.etn
    %_  this-su
      top-source-id.etn    +(top-source-id.etn)
      sources.etn          (~(put by sources) top-source-id.etn source)
      sources-reverse.etn  (~(put by sources-reverse) source top-source-id.etn)
    ==
  ::
  ++  extract-snap                                    ::  extract rewind point
    ^-  snapshot
    ~
  ::                                                    ::  ++feed:su
  ++  feed                                              ::  subscribe to view
    |_  ::  hen: subscription source
        ::
        hen/duct
    ::
    ++  public-keys
      |=  whos=(set ship)
      ?:  fak.own.pki
        (public-keys:fake whos)
      =.  ney.zim
        =/  whol=(list ship)  ~(tap in whos)
        |-  ^-  (jug ship duct)
        ?~  whol
          ney.zim
        (~(put ju $(whol t.whol)) i.whol hen)
      =/  =vent-result
        :-  %full
        ?:  =(~ whos)
          pos.zim
        %-  my  ^-  (list (pair ship point))
        %+  murn
          ~(tap in whos)
        |=  who=ship
        ^-  (unit (pair ship point))
        =/  pub  (~(get by pos.zim) who)
        ?~  pub  ~
        ?:  =(0 life.u.pub)  ~
        `[who u.pub]
      =.  yen.zim
        %-  ~(gas ju yen.zim)
        %+  turn  ~(tap in whos)
        |=  who=ship
        [hen who]
      =.  ..feed  (vent-give (sy hen ~) vent-result)
      ..feed
    ::
    ++  private-keys                                            ::  private keys
      %_  ..feed
        moz      [[hen %give %private-keys [lyf jaw]:own] moz]
        yen.own  (~(put in yen.own) hen)
      ==
    ::
    ++  sources
      %_  ..feed
          yen.etn  (~(put in yen.etn) hen)
          moz
        %-  welp  :_  moz
        %+  turn
          ~(tap by ship-sources-reverse.etn)
        |=  [=source-id whos=(set ship)]
        [hen %give %source whos (~(got by sources.etn) source-id)]
      ==
    ::                                                  ::  ++fake:feed:su
    ++  fake                                            ::  fake subs and state
      ?>  fak.own.pki
      |%
      ++  public-keys
        |=  whos=(set ship)
        =/  whol=(list ship)  ~(tap in whos)
        =/  passes
          |-  ^-  (list [who=ship =pass])
          ?~  whol
            ~
          =/  cub  (pit:nu:crub:crypto 512 i.whol)
          :-  [i.whol pub:ex:cub]
          $(whol t.whol)
        =/  points=(list (pair ship point))
          %+  turn  passes
          |=  [who=ship =pass]
          ^-  [who=ship =point]
          [who [rift=1 life=1 (my [1 1 pass] ~) `(^sein:title who)]]
        =.  moz  [[hen %give %public-keys %full (my points)] moz]
        |-  ^+  ..feel
        ?~  passes
          ..feel
        =.  ..feel
          %-  public-keys:feel
          [%diff who.i.passes %changed-keys 1 1 pass.i.passes]
        $(passes t.passes)
      --
    --
  ::                                                    ::  ++feel:su
  ++  feel                                              ::  update tracker
    |%
    ::                                                  ::  ++pubs:feel:su
    ++  public-keys
      |=  =vent-result
      ^+  ..feel
      ?:  ?=(%full -.vent-result)
        =.  pos.zim  (~(uni by pos.zim) points.vent-result)
        =/  pointl=(list [who=ship =point])
          ~(tap by points.vent-result)
        |-  ^+  ..feel
        ?~  pointl
          ..feel
        %+  vent-give
          (~(get ju ney.zim) who.i.pointl)
        [%full (my i.pointl ~)]
      =*  who  who.vent-result
      =*  point-diff  point-diff.vent-result
      =/  maybe-point  (~(get by pos.zim) who)
      =/  =point  (fall maybe-point *point)
      =.  point
        ?-  -.point-diff
            %new-sponsor
          point(sponsor sponsor.point-diff)
        ::
            %changed-continuity
          point(rift (max rift.point-diff rift.point))
        ::
            %changed-keys
          %_  point
              life  (max life.point-diff life.point)
              keys
            %+  ~(put by keys.point)
              life.point-diff
            [crypto-suite pass]:point-diff
          ==
        ==
      =.  pos.zim  (~(put by pos.zim) who point)
      %+  vent-give
        (~(get ju ney.zim) who)
      ?~  maybe-point
        [%full (my [who point]~)]
      [%diff who point-diff]
    ::                                                  ::  ++vein:feel:su
    ++  private-keys                                    ::  kick private keys
      |=  [=life =ring]
      ^+  ..feel
      ?:  &(=(lyf.own life) =((~(get by jaw.own) life) `ring))
        ..feel
      =.  lyf.own  life
      =.  jaw.own  (~(put by jaw.own) life ring)
      (exec yen.own [%give %private-keys lyf.own jaw.own])
    ::
    ++  sources
      |=  [whos=(set ship) =source]
      ^+  ..feel
      ?:  ?=(%& -.source)
        =/  send-message
          |=  =message
          [hen %pass /public-keys %a %want p.source /k/public-keys message]
        =.  ..feel
          (emit (send-message %nuke whos))
        (emit (send-message %public-keys whos))
      =^  =source-id  this-su  (get-source-id source)
      =.  ..feed
        ?~  whos
          ..feed(default-source.etn source-id)
        =/  whol=(list ship)  ~(tap in `(set ship)`whos)
        =.  ship-sources.etn
          |-  ^-  (map ship ^source-id)
          ?~  whol
            ship-sources.etn
          (~(put by $(whol t.whol)) i.whol source-id)
        =.  ship-sources-reverse
          %-  ~(gas ju ship-sources-reverse)
          (turn whol |=(=ship [source-id ship]))
        ..feed
      (exec yen.etn [%give %source whos source])
    --
  ::                                                    ::  ++meet:su
  ++  meet                                              ::  seen after breach
    |=  [who=ship =life =pass]
    ::  XX rethink meet
    ^+  +>
    !!
  --
--
::                                                      ::::
::::                    #  vane                         ::  interface
  ::                                                    ::::
::
::  lex: all durable %kale state
::
=|  lex/state
|=  $:  ::
        ::  our: identity
        ::  now: current time
        ::  eny: unique entropy
        ::  ski: namespace resolver
        ::
        our=ship
        now=@da
        eny=@uvJ
        ski=sley
    ==
^?
|%
::                                                      ::  ++call
++  call                                                ::  request
  |=  $:  ::  hen: cause of this event
          ::  hic: event data
          ::
          hen/duct
          hic/(hypo (hobo task:able))
      ==
  ^-  [(list move) _..^$]
  =/  =task:able
    ?.  ?=($soft -.q.hic)
      q.hic
    (task:able p.q.hic)
  =^  did  lex
    abet:(~(call of [our now eny] lex) hen task)
  [did ..^$]
::                                                      ::  ++load
++  load                                                ::  upgrade
  |=  $:  ::  old: previous state
          ::
          old/*
          ::  old/state
      ==
  ^+  ..^$
  ..^$
  ::  ..^$(lex old)
::                                                      ::  ++scry
++  scry                                                ::  inspect
  |=  $:  ::  fur: event security
          ::  ren: access mode
          ::  why: owner
          ::  syd: desk (branch)
          ::  lot: case (version)
          ::  tyl: rest of path
          ::
          fur/(unit (set monk))
          ren/@tas
          why/shop
          syd/desk
          lot/coin
          tyl/spur
      ==
  ^-  (unit (unit cage))
  ::  XX review for security, stability, cases other than now
  ::
  ?.  =(lot [%$ %da now])  ~
  ?.  =(%$ ren)  [~ ~]
  ?+    syd
      ~
  ::
      %code
    ?.  ?=([@ ~] tyl)  [~ ~]
    ?.  =([%& our] why)
      [~ ~]
    =/  who  (slaw %p i.tyl)
    ?~  who  [~ ~]
    =/  sec  (~(got by jaw.own.pki.lex) lyf.own.pki.lex)
    =/  cub  (nol:nu:crub:crypto sec)
    ::  XX use pac:ex:cub?
    ::
    ``[%noun !>((end 6 1 (shaf %pass (shax sec:ex:cub))))]
  ::
      %life
    ?.  ?=([@ ~] tyl)  [~ ~]
    ?.  =([%& our] why)
      [~ ~]
    =/  who  (slaw %p i.tyl)
    ?~  who  [~ ~]
    ::  fake ships always have life=1
    ::
    ?:  fak.own.pki.lex
      ``[%atom !>(1)]
    ?:  =(u.who p.why)
      ``[%atom !>(lyf.own.pki.lex)]
    =/  pub  (~(get by pos.zim.pki.lex) u.who)
    ?~  pub  ~
    ``[%atom !>(life.u.pub)]
  ::
      %rift
    ?.  ?=([@ ~] tyl)  [~ ~]
    ?.  =([%& our] why)
      [~ ~]
    =/  who  (slaw %p i.tyl)
    ?~  who  [~ ~]
    ::  fake ships always have rift=1
    ::
    ?:  fak.own.pki.lex
      ``[%atom !>(1)]
    =/  pos  (~(get by pos.zim.pki.lex) u.who)
    ?~  pos  ~
    ``[%atom !>(rift.u.pos)]
  ::
      %deed
    ?.  ?=([@ @ ~] tyl)  [~ ~]
    ?.  &(?=(%& -.why) =(p.why our))
      [~ ~]
    =/  who  (slaw %p i.tyl)
    =/  lyf  (slaw %ud i.t.tyl)
    ?~  who  [~ ~]
    ?~  lyf  [~ ~]
    =/  rac  (clan:title u.who)
    ::
    ?:  ?=(%pawn rac)
      ?.  =(u.who p.why)
        [~ ~]
      ?.  =(1 u.lyf)
        [~ ~]
      =/  sec  (~(got by jaw.own.pki.lex) u.lyf)
      =/  cub  (nol:nu:crub:crypto sec)
      =/  sig  (sign:as:cub (shaf %self (sham [u.who 1 pub:ex:cub])))
      :^  ~  ~  %noun
      !>  ^-  deed:ames
      [1 pub:ex:cub `sig]
    ::
    ?:  ?=(%earl rac)
      ?.  =(u.who p.why)
        [~ ~]
      ?:  (gth u.lyf lyf.own.pki.lex)
        ~
      ?:  (lth u.lyf lyf.own.pki.lex)
        [~ ~]
      =/  sec  (~(got by jaw.own.pki.lex) u.lyf)
      =/  cub  (nol:nu:crub:crypto sec)
      :^  ~  ~  %noun
      !>  ^-  deed:ames
      [u.lyf pub:ex:cub sig.own.pki.lex]
    ::
    =/  pub  (~(get by pos.zim.pki.lex) u.who)
    ?~  pub
      ~
    ?:  (gth u.lyf life.u.pub)
      ~
    =/  pas  (~(get by keys.u.pub) u.lyf)
    ?~  pas
      ~
    :^  ~  ~  %noun
    !>  `deed:ames`[u.lyf pass.u.pas ~]
  ::
      %earl
    ?.  ?=([@ @ @ ~] tyl)  [~ ~]
    ?.  =([%& our] why)
      [~ ~]
    =/  who  (slaw %p i.tyl)
    =/  lyf  (slaw %ud i.t.tyl)
    =/  pub  (slaw %ux i.t.t.tyl)
    ?~  who  [~ ~]
    ?~  lyf  [~ ~]
    ?~  pub  [~ ~]
    ?:  (gth u.lyf lyf.own.pki.lex)
      ~
    ?:  (lth u.lyf lyf.own.pki.lex)
      [~ ~]
    :: XX check that who/lyf hasn't been booted
    ::
    =/  sec  (~(got by jaw.own.pki.lex) u.lyf)
    =/  cub  (nol:nu:crub:crypto sec)
    =/  sig  (sign:as:cub (shaf %earl (sham u.who u.lyf u.pub)))
    ``[%atom !>(sig)]
  ::
      %sein
    ?.  ?=([@ ~] tyl)  [~ ~]
    ?.  =([%& our] why)
      [~ ~]
    =/  who  (slaw %p i.tyl)
    ?~  who  [~ ~]
    :^  ~  ~  %atom
    !>  ^-  ship
    (~(sein of [our now eny] lex) u.who)
  ::
      %saxo
    ?.  ?=([@ ~] tyl)  [~ ~]
    ?.  =([%& our] why)
      [~ ~]
    =/  who  (slaw %p i.tyl)
    ?~  who  [~ ~]
    :^  ~  ~  %noun
    !>  ^-  (list ship)
    (~(saxo of [our now eny] lex) u.who)
  ::
      %sources
    ?.  ?=(~ tyl)  [~ ~]
    :^  ~  ~  %noun  !>
    etn.lex
  ::
      %snap
    ?.  ?=(~ tyl)  [~ ~]
    ?:  =(~ snaps.sap.lex)
      `~
    :^  ~  ~  %noun  !>
    |-  ^-  snapshot
    =^  snap=[@ud snap=snapshot]  snaps.sap.lex
      ~(get to snaps.sap.lex)
    ?:  =(~ snaps.sap.lex)
      snap.snap
    $
  ::
      %turf
    ?.  ?=(~ tyl)  [~ ~]
    [~ ~ %noun !>(tuf.own.pki.lex)]
  ==
::                                                      ::  ++stay
++  stay                                                ::  preserve
  lex
::                                                      ::  ++take
++  take                                                ::  accept
  |=  $:  ::  tea: order
          ::  hen: cause
          ::  hin: result
          ::
          tea/wire
          hen/duct
          hin/(hypo sign)
      ==
  ^-  [(list move) _..^$]
  =^  did  lex  abet:(~(take of [our now eny] lex) tea hen q.hin)
  [did ..^$]
--
