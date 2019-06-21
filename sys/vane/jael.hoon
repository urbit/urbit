!:                                                      ::  /van/jael
::                                                      ::  %reference/0
!?  150
::
::
::  %jael: secrets and promises.
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
=,  pki:jael
=,  able:jael
=,  crypto
=,  jael
=,  ethereum
=,  rpc
=,  azimuth
::                                                      ::::
::::                    # models                        ::  data structures
  ::                                                    ::::
::  the %jael state comes in two parts: absolute
::  and relative.
::
::  ++state-relative is subjective, denormalized and
::  derived.  it consists of all the state we need to
::  manage subscriptions efficiently.
::
=>  |%
++  state                                               ::  all vane state
  $:  ver=$0                                            ::  vane version
      yen=(set duct)                                    ::  raw observers
      sub=state-relative                                ::  all relative state
      etn=state-eth-node                                ::  eth connection state
      sap=state-snapshots                               ::  state snapshots
  ==                                                    ::
++  state-relative                                      ::  urbit metadata
  $:  $=  own                                           ::  vault (vein)
        $:  yen=(set duct)                              ::  trackers
            sig=(unit oath)                             ::  for a moon
            :: XX reconcile with .dns.eth               ::
            tuf=(list turf)                             ::  domains
            :: XX use for eth replay                    ::
            boq=@ud                                     ::  boot block
            nod=(unit purl:eyre)                        ::  eth gateway
            fak=_|                                      ::  fake keys
            lyf=life                                    ::  version
            jaw=(map life ring)                         ::  private keys
        ==                                              ::
      $=  puk                                           ::  public keys (pubs)
        $:  yen=(jug ship duct)                         ::  trackers
            kyz=(map ship public)                       ::  public key state
        ==                                              ::
      $=  eth                                           ::  ethereum (vent)
        ::TODO  the subscribers here never hear dns or pos...
        $:  yen=(set duct)                              ::  trackers
            dns=dnses                                   ::  on-chain dns state
            pos=(map ship point)                        ::  on-chain ship state
            ::TODO  do we want (map ship diff-point) too?
        ==                                              ::
  ==                                                    ::
++  state-snapshots                                     ::  rewind points
  $:  interval=_100                                     ::  block interval
      max-count=_10                                     ::  max snaps
      count=@ud                                         ::  length of snaps
      last-block=@ud                                    ::  number of last snap
      snaps=(qeu [block-number=@ud snap=snapshot])      ::  old states
  ==                                                    ::
++  message                                             ::  message to her jael
  $%  [%nuke ~]                                         ::  cancel trackers
      [%vent ~]                                         ::  view ethereum events
      [%vent-result p=vent-result]                      ::  tmp workaround
  ==                                                    ::
++  card                                                ::  i/o action
  (wind note gift)                                      ::
::                                                      ::
+$  move                                                ::  output
  [p=duct q=card]                                       ::
::                                                      ::
+$  note                                                ::  out request $->
  $~  [%b %wait *@da]                                   ::
  $%  $:  %b                                            ::    to %behn
          $>  $?  %rest                                 ::  cancel timer
                  %wait                                 ::  set timer
              ==                                        ::
          task:able:behn                                ::
      ==                                                ::
      $:  %a                                            ::    to %ames
          $>(%want task:able:ames)                      ::  send message
      ==                                                ::
      $:  %j                                            ::    to self
          $>(%look task)                                ::  set ethereum source
      ==                                                ::
      $:  %l                                            ::    to %lient
          $>(%request task:able:http-client)            ::  http request
      ==                                                ::
      $:  @tas                                          ::
  $%  $>(%init vane-task)                               ::  report install
      $>(%sunk vane-task)                               ::  report death
  ==  ==  ==                                            ::
::                                                      ::
+$  sign                                                ::  in result $<-
  $~  [%b %wake ~]                                      ::
  $%  [%b $>(%wake gift:able:behn)]                     ::  wakeup
      [%j $>(%vent gift)]                               ::  ethereum changes
      [%a $>(%woot gift:able:ames)]                     ::  message result
      [%l $>(%http-response gift:able:http-client)]     ::  http response
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
  ::  this core handles all top-level %jael semantics,
  ::  changing state and recording moves.
  ::
  ::  logically we could nest the ++su core within it, but
  ::  we keep them separated for clarity.  the ++curd and
  ::  ++cure arms complete relative and absolute effects,
  ::  respectively, at the top level.
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
  ::                                                    ::  ++scry:of
  ++  scry                                              ::  read
    |=  {syd/@tas pax/path}
    ~|  %jael-scry-of-stub
    =^  mar  pax  =/(a (flop pax) [-.a (flop t.+.a)])
    !!
  ::                                                    ::  ++sein:of
  ++  sein                                              ::  sponsor
    |=  who=ship
    ^-  ship
    ::  XX save %dawn sponsor in .own.sub, check there
    ::
    =/  pot  (~(get by pos.eth.sub) who)
    ?:  ?&  ?=(^ pot)
            ?=(^ net.u.pot)
        ==
      who.sponsor.u.net.u.pot
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
      =.  boq.own.sub  bloq.tac
      ::  save our ethereum gateway (required for galaxies)
      ::
      =.  nod.own.sub  node.tac
      ::  save our parent signature (only for moons)
      ::
      =.  sig.own.sub  sig.seed.tac
      ::  if we're given a snapshot, restore it
      ::
      =.  +>.$
        ?~  snap.tac  +>.$
        (restore-snap hen u.snap.tac |)
      ::  load our initial public key, overriding snapshot
      ::
      =.  kyz.puk.sub
        =/  cub  (nol:nu:crub:crypto key.seed.tac)
        %+  ~(put by kyz.puk.sub)
          our
        [lyf.seed.tac (my [lyf.seed.tac pub:ex:cub] ~)]
      ::  our initial private key
      ::
      =.  lyf.own.sub  lyf.seed.tac
      =.  jaw.own.sub  (my [lyf.seed.tac key.seed.tac] ~)
      ::  our initial galaxy table as a +map from +life to +public
      ::
      =/  kyz
        %-  ~(run by czar.tac)
        |=([=life =pass] `public`[life (my [life pass] ~)])
      =.  +>.$
        %-  curd  =<  abet
        (pubs:~(feel su hen our sub etn sap) kyz)
      ::  XX save sponsor in .own.sub
      ::  XX reconcile with .dns.eth
      ::  set initial domains
      ::
      =.  tuf.own.sub  turf.tac
      ::
      =.  moz
        %+  weld  moz
        ::  order is crucial!
        ::
        ::    %dill must init after %gall
        ::    the %give init (for unix) must be after %dill init
        ::    %jael init must be deferred (makes http requests)
        ::
        ^-  (list move)
        :~  [hen %pass /(scot %p our)/init %b %wait +(now)]
            [hen %give %init our]
            [hen %slip %r %init our]
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
      =.  sig.own.sub
        ?.  ?=(%earl (clan:title our))
          ~
        =/  yig  (pit:nu:crub:crypto 512 (^sein:title our))
        [~ (sign:as:yig (shaf %earl (sham our 1 pub:ex:cub)))]
      ::  our initial public key
      ::
      =.  kyz.puk.sub
        (~(put by kyz.puk.sub) our [1 (my [1 pub:ex:cub] ~)])
      ::  our private key
      ::
      ::    Private key updates are disallowed for fake ships,
      ::    so we do this first.
      ::
      =.  lyf.own.sub  1
      =.  jaw.own.sub  (my [1 sec:ex:cub] ~)
      ::  set the fake bit
      ::
      =.  fak.own.sub  &
      ::  initialize other vanes per the usual procedure
      ::
      ::    Except for ourselves!
      ::
      =.  moz
        %+  weld  moz
        ^-  (list move)
        :~  [hen %give %init our]
            [hen %slip %r %init our]
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
      %+  cute  hen  =<  abet
      (~(look et hen our now sub.lex etn.lex sap.lex) src.tac)
    ::
    ::  cancel all trackers from duct
    ::    {$nuke $~}
    ::
        $nuke
      %_  +>
        yen          (~(del in yen) hen)
        yen.own.sub  (~(del in yen.own.sub) hen)
        yen.eth.sub  (~(del in yen.eth.sub) hen)
      ==
    ::
    ::  watch public keys
    ::    [%pubs =ship]
    ::
        %pubs
      %-  curd  =<  abet
      (~(pubs ~(feed su hen our sub etn sap) hen) ship.tac)
    ::
    ::  seen after breach
    ::    [%meet our=ship who=ship]
    ::
        %meet
      %+  cure  hen
      [%meet ship.tac life.tac pass.tac]~
    ::
    ::  restore snapshot
    ::    [%snap snap=snapshot kick=?]
        %snap
      (restore-snap hen snap.tac kick.tac)
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
      ?<  =(fak.own.sub ?=(^ tuf.own.sub))
      +>.$(moz [[hen %give %turf tuf.own.sub] moz])
    ::
    ::  learn of kernel upgrade
    ::    [%vega ~]
    ::
        %vega
      +>.$
    ::
    ::  watch private keys
    ::    {$vein $~}
    ::
        $vein
      (curd abet:~(vein ~(feed su hen our sub etn sap) hen))
    ::
    ::  watch ethereum events
    ::    [%vent ~]
    ::
        %vent
      =.  moz  [[hen %give %mack ~] moz]
      (curd abet:~(vent ~(feed su hen our sub etn sap) hen))
    ::
    ::  monitor all
    ::    {$vine $~}
    ::
        $vine
      +>(yen (~(put in yen) hen))
    ::
        %wegh
      %_    +>
          moz
        :_  moz
        ^-  move
        :^  hen  %give  %mass
        ^-  mass
        :+  %jael  %|
        :~  yen+&+yen
            sub+&+sub
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
      ::    [%nuke ~]
      ::
          %nuke
        $(tac mes)
      ::
      ::  view ethereum events
      ::    [%vent ~]
      ::
          %vent
        $(tac [%vent ~])
      ::
      ::
          %vent-result
        ::  ignore if not from currently configured source.
        ?.  &(-.source.etn =(her p.source.etn))
          +>.$
        =.  moz  [[hen %give %mack ~] moz]
        %+  cute  hen  =<  abet
        (~(hear-vent et hen our now sub.lex etn.lex sap.lex) p.mes)
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
    ::
        [%l %http-response *]
      ?.  ?=(%finished -.client-response.hin)
        +>.$
      ~!  hin
      %+  cute  hen  =<  abet
      %^  ~(finished et hen our now sub.lex etn.lex sap.lex)  wir
        response-header.client-response.hin
      full-file.client-response.hin
    ::
        [%b %wake ~]
      %+  cute  hen
      ::  XX cleanup
      ::
      ?.  ?=([%init ~] wir)
        abet:~(wake et hen our now sub.lex etn.lex sap.lex)
      abet:(~(init et hen our now [sub etn sap]:lex) our (sein our))
    ::
        [%b %wake ^]
      ::  TODO: handle behn error
      ::
      ~&  %jael-wake-bad^u.error.hin
      +>.$
    ::
        [%j %vent *]
      %+  cute  hen  =<  abet
      (~(hear-vent et hen our now sub.lex etn.lex sap.lex) p.hin)
    ==
  ::                                                    ::  ++curd:of
  ++  curd                                              ::  relative moves
    |=  $:  moz/(list move)
            sub/state-relative
            etn/state-eth-node
            sap/state-snapshots
        ==
    +>(sub sub, etn etn, sap sap, moz (weld (flop moz) ^moz))
  ::                                                    ::  ++cure:of
  ++  cure                                              ::  absolute edits
    |=  [hen=duct hab=(list change)]
    ^+  +>
    (curd abet:(~(apex su hen our sub etn sap) hab))
  ::                                                    ::  ++cute:of
  ++  cute                                              ::  ethereum changes
    |=  $:  hen=duct
            mos=(list move)
            ven=chain
            sub=state-relative
            etn=state-eth-node
            sap=state-snapshots
        ==
    ^+  +>
    =:  ^sub  sub
        ^etn  etn
        ^sap  sap
    ==
    %-  cure(moz (weld (flop mos) moz))
    [hen [%ethe ven]~]
  ::                                                    ::  ++wind:of
  ++  wind                                              ::  rewind to snap
    |=  [hen=duct block=@ud]
    ^+  +>
    =.  +>.$  (restore-block hen block)
    %=    +>.$
        moz
      =-  [[hen %pass /wind/look %j %look -] moz]
      ?-  -.source.etn
        %&  &+p.source.etn
        %|  |+node.p.source.etn
      ==
    ==
  ::                                                    ::  ++restore-block:of
  ++  restore-block                                     ::  rewind before block
    |=  [hen=duct block=@ud]
    %+  cute  hen  =<  abet
    (~(restore-block et hen our now sub.lex etn.lex sap.lex) block)
  ::                                                    ::  ++restore-snap:of
  ++  restore-snap                                      ::  restore snapshot
    |=  [hen=duct snap=snapshot look=?]
    %+  cute  hen  =<  abet
    %-  ~(restore-snap et hen our now sub.lex etn.lex sap.lex)
    [snap look]
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
      ::  ++fire:su generates outgoing network messages.
      ::
      ::  ++form:su generates the actual report data.
      ::
  =|  moz=(list move)
  =|  evs=logs
  =|  $:  hen/duct
          our/ship
          state-relative
          state-eth-node
          state-snapshots
      ==
  ::  moz: moves in reverse order
  ::  sub: relative urbit state
  ::
  =*  sub  ->+<
  =*  etn  ->+>-
  =*  sap  ->+>+
  |%
  ::                                                    ::  ++abet:su
  ++  abet                                              ::  resolve
    ::TODO  we really want to just send the %give, but ames is being a pain.
    :: =>  (exec yen.eth [%give %vent |+evs])
    =>  ?~  evs  .
        (vent-pass yen.eth chain+|+evs)
    [(flop moz) sub etn sap]
  ::                                                    ::  ++apex:su
  ++  apex                                              ::  apply changes
    |=  hab/(list change)
    ^+  +>
    ?~  hab  +>
    %=    $
        hab  t.hab
        +>
      ?-  -.i.hab
        %ethe  (file can.i.hab)
        %meet  (meet +.i.hab)
        %priv  !!  ::  update private key
      ==
    ==
  ::                                                    ::  ++exec:su
  ++  exec                                              ::  mass gift
    |=  {yen/(set duct) cad/card}
    =/  noy  ~(tap in yen)
    |-  ^+  ..exec
    ?~  noy  ..exec
    $(noy t.noy, moz [[i.noy cad] moz])
  ::
  ++  vent-pass
    |=  [yen=(set duct) res=vent-result]
    =+  yez=~(tap in yen)
    |-  ^+  ..vent-pass
    ?~  yez  ..vent-pass
    =*  d  i.yez
    ?>  ?=([[%a @ @ *] *] d)
    =+  our=(slav %p i.t.i.d)
    =+  who=(slav %p i.t.t.i.d)
    %+  exec  [d ~ ~]
    :+  %pass
      /(scot %p our)/vent-result
    ^-  note
    [%a %want who /j/(scot %p our)/vent-result %vent-result res]
  ::
  ++  extract-snap                                    ::  extract rewind point
    ^-  snapshot
    :*  kyz.puk.sub
        [dns pos]:eth.sub
        heard.etn
        latest-block.etn
    ==
  ::                                                    ::  ++feed:su
  ++  feed                                              ::  subscribe to view
    |_  ::  hen: subscription source
        ::
        hen/duct
    ::
    ++  pubs
      |=  who=ship
      ?:  fak.own.sub
        (pubs:fake who)
      %_  ..feed
        moz      =/  pub  (~(get by kyz.puk) who)
                 ?~  pub  moz
                 ?:  =(0 life.u.pub)  moz
                 [[hen %give %pubs u.pub] moz]
        yen.puk  (~(put ju yen.puk) who hen)
      ==
    ::                                                  ::  ++vein:feed:su
    ++  vein                                            ::  private keys
      %_  ..feed
        moz      [[hen %give %vein [lyf jaw]:own] moz]
        yen.own  (~(put in yen.own) hen)
      ==
    ::
    ++  vent
      =/  last-snap  extract-snap
      %.  [[hen ~ ~] snap+last-snap]
      %_  vent-pass
      :: %_  ..feed  ::TODO  see ++abet
        :: moz      [[hen %give %vent] moz]
        yen.eth  (~(put in yen.eth) hen)
      ==
    ::                                                  ::  ++fake:feed:su
    ++  fake                                            ::  fake subs and state
      ?>  fak.own.sub
      |%
      ++  pubs
        |=  who=ship
        =/  cub  (pit:nu:crub:crypto 512 who)
        =/  pub  [life=1 (my [1 pub:ex:cub] ~)]
        =.  moz  [[hen %give %pubs pub] moz]
        (pubs:feel (my [who pub] ~))
      --
    --
  ::                                                    ::  ++feel:su
  ++  feel                                              ::  update tracker
    |%
    ::                                                  ::  ++pubs:feel:su
    ++  pubs                                            ::  kick public keys
      ::  puz: new public key states
      ::
      |=  puz=(map ship public)
      =/  pus  ~(tap by puz)
      ::
      ::  process change for each ship separately
      ::  XX check for diffs before sending?
      ::
      |-  ^+  ..feel
      ?~  pus  ..feel
      =;  fel  $(pus t.pus, ..feel fel)
      =*  who  p.i.pus
      =*  pub  q.i.pus
      ::
      ::  update public key store and notify subscribers
      ::  of the new state
      ::
      :: ~&  [%sending-pubs-about who life.pub]
      %+  exec(kyz.puk (~(put by kyz.puk) who pub))
        (~(get ju yen.puk) who)
      [%give %pubs pub]
    ::                                                  ::  ++vein:feel:su
    ++  vein                                            ::  kick private keys
      ^+  ..feel
      =/  yam  vein:form
      ?:  &(=(lyf.own p.yam) =(jaw.own q.yam))
        ..feel
      =.  lyf.own  p.yam
      =.  jaw.own  q.yam
      (exec yen.own [%give %vein lyf.own jaw.own])
    ::
    ++  vent
      |=  ver=vent-result
      ^+  ..feel
      ::TODO  see ++abet
      :: (exec yen.eth [%give %vent can])
      (vent-pass yen.eth ver)
    --
  ::                                                    ::  ++form:su
  ++  form                                              ::  generate reports
    |%
    ::                                                  ::  ++vein:form:su
    ++  vein                                            ::  private key report
      ^-  (pair life (map life ring))
      !!
    --
  ::                                                    ::  ++meet:su
  ++  meet                                              ::  seen after breach
    |=  [who=ship =life =pass]
    ^+  +>
    =;  new=public
      (pubs:feel (my [who new] ~))
    ::
    =/  old=(unit public)
      (~(get by kyz.puk) who)
    ?:  ?|  ?=(?(%earl %pawn) (clan:title who))
            ::  XX save %dawn sponsor in .own.sub, check there
            ::  XX or move sein:of to sein:su?
            ::  XX full saxo chain?
            ::
            =(who (^sein:title our))
        ==
      ?~  old
        [life (my [life pass] ~)]
      =/  fyl  life.u.old
      =/  sap  (~(got by pubs.u.old) fyl)
      ~|  [%met-mismatch who life=[old=fyl new=life] pass=[old=sap new=pass]]
      ?>  ?:  =(fyl life)
            =(sap pass)
          =(+(fyl) life)
      [life (~(put by pubs.u.old) life pass)]
    ?.  ?=(^ old)
      ~|  [%met-unknown-ship who]  !!
    =/  fyl  life.u.old
    =/  sap  (~(got by pubs.u.old) fyl)
    ~|  [%met-mismatch who life=[old=fyl new=life] pass=[old=sap new=pass]]
    ?>  &(=(fyl life) =(sap pass))
    [life pubs.u.old]
  ::                                                    ::  ++file:su
  ++  file                                              ::  process event logs
    ::TODO  whenever we add subscriptions for data,
    ::      outsource the updating of relevant state
    ::      to a ++feel arm.
    |=  [new=? evs=logs]
    ^+  +>
    =?  +>  new
      ::TODO  should we be mutating state here,
      ::      or better to move this into ++vent:feel?
      +>(dns.eth *dnses, pos.eth ~, kyz.puk ~)
    =?  +>  |(new !=(0 ~(wyt by evs)))
      %-  vent:feel
      :-  %chain
      ?:(new &+evs |+evs)
    ::
    =+  vez=(order-events:ez ~(tap by evs))
    =|  kyz=(map ship public)
    |^  ?~  vez  (pubs:feel kyz)
        =^  kyn  ..file  (file-event i.vez)
        $(vez t.vez, kyz kyn)
    ::
    ++  get-public
      |=  who=ship
      ^-  public
      %+  fall  (~(get by kyz) who)
      ::NOTE  we can only do this because ++pubs:feel
      ::      sends out entire new state, rather than
      ::      just the processed changes.
      %+  fall  (~(get by kyz.puk) who)
      *public
    ::
    ++  file-keys
      |=  [who=ship =life =pass]
      ^+  kyz
      =/  pub  (get-public who)
      =/  puk  (~(get by pubs.pub) life)
      ?^  puk
        ::  key known, nothing changes
        ~|  [%key-mismatch who life `@ux`u.puk `@ux`pass (get-public ~zod)]
        ?>(=(u.puk pass) kyz)
      %+  ~(put by kyz)  who
      :-  (max life life.pub)
      (~(put by pubs.pub) life pass)
    ::
    ++  file-discontinuity
      |=  who=ship
      ^+  kyz
      =+  (get-public who)
      (~(put by kyz) who -)
    ::
    ++  file-event
      |=  [wer=event-id dif=diff-azimuth]
      ^+  [kyz ..file]
      ?:  (~(has in heard) wer)
        ~&  %ignoring-already-heard-event
        [kyz ..file]
      ::
      ::  sanity check, should never fail if we operate correctly
      ::
      ?>  (gte block.wer latest-block)
      =:  evs           (~(put by evs) wer dif)
          heard         (~(put in heard) wer)
          latest-block  (max latest-block block.wer)
      ==
      =^  kyz  ..file
        ?-  -.dif
          %point  ~|(wer=wer (file-point +.dif))
          %dns    [kyz (file-dns +.dif)]
        ==
      [kyz (file-snap wer)]
    ::
    ++  file-point
      |=  [who=ship dif=diff-point]
      ^+  [kyz ..file]
      =-  ::TODO  =; with just the type
        =.  pos.eth  (~(put by pos.eth) who pon)
        ::  new keys
        ::
        ?:  ?=(%& -.new)
          [(file-keys who p.new) ..file]
        ::  kept continuity (no-op)
        ::
        ?:  p.new
          [kyz ..file]
        ::  discontinuity
        ::
        :-  (file-discontinuity who)
        %=  ..file
          moz  =/  rit=rift
                 ~|  sunk-unknown+who
                 =<  continuity-number
                 %+  fall
                   net:(fall (~(get by pos.eth) who) *point)
                 *[life pass continuity-number=@ud [? @p] (unit @p)]
               %+  weld
                 ::  %-  flop
                 ^-  (list move)
                 :~  [hen %slip %a %sunk who rit]
                     [hen %slip %c %sunk who rit]
                     [hen %slip %d %sunk who rit]
                     [hen %slip %f %sunk who rit]
                     [hen %slip %g %sunk who rit]
                 ==
               moz
        ==
      ::  pon: updated point
      ::  new: new keypair or "kept continuity?" (yes is no-op)
      ^-  [pon=point new=(each (pair life pass) ?)]
      =+  pot=(fall (~(get by pos.eth) who) *point)
      ::
      ::  sanity checks, should never fail if we operate correctly
      ::
      ~|  [%diff-order-insanity -.dif who (~(get by pos.eth) who)]
      ?>  ?+  -.dif  &
            %spawned      ?>  ?=(^ kid.pot)
                          !(~(has in spawned.u.kid.pot) who.dif)
            %keys         ?>  ?=(^ net.pot)
                          =(life.dif +(life.u.net.pot))
            %continuity   ?>  ?=(^ net.pot)
                          =(new.dif +(continuity-number.u.net.pot))
          ==
      ::
      ::  apply point changes, catch continuity and key changes
      ::
      :-  (apply-point-diff pot dif)
      =*  nop  |+&  ::  no-op
      ?+  -.dif  nop
        %continuity   |+|
        %keys         &+[life pass]:dif
        %full         ?~  net.new.dif  nop
                      ::TODO  do we want/need to do a diff-check
                      &+[life pass]:u.net.new.dif
      ==
    ::
    ++  file-dns
      |=  dns=dnses
      ..file(dns.eth dns)
    ::
    ++  file-snap                                       ::  save snapshot
      |=  wer=event-id
      ^+  ..file
      =?    sap
          %+  lth  2
          %+  sub.add
            (div block.wer interval.sap)
          (div last-block.sap interval.sap)
        :: ~&  :*  %snap
        ::         count=count.sap
        ::         max-count=max-count.sap
        ::         last-block=last-block.sap
        ::         interval=interval.sap
        ::         lent=(lent ~(tap to snaps.sap))
        ::     ==
        %=  sap
          snaps       (~(put to snaps.sap) block.wer extract-snap)
          count       +(count.sap)
          last-block  block.wer
        ==
      =?  sap  (gth count.sap max-count.sap)
        ~&  :*  %dump
                count=count.sap
                max-count=max-count.sap
                lent=(lent ~(tap to snaps.sap))
            ==
        %=  sap
          snaps  +:~(get to snaps.sap)
          count  (dec count)
        ==
      ..file
    --
  --
::                                                      ::  ++et
::::                    ## ethereum^heavy               ::  ethereum engine
  ::                                                    ::::
++  et
  ::
  ::  the ++et core handles all logic necessary to maintain the
  ::  absolute record of on-chain state changes, "events".
  ::
  ::  we either subscribe to a parent ship's existing record, or
  ::  communicate directly with an ethereum node.
  ::
  ::  moves: effects; either behn timers, subscriptions,
  ::         or ethereum node rpc requests.
  ::  reset: whether the found changes assume a fresh state.
  ::  changes: on-chain changes heard from our source.
  ::
  =|  moves=(list move)
  =+  reset=|
  =|  changes=logs
  =|  rewind-block=(unit @ud)
  =|  $:  hen=duct
          our=ship
          now=@da
          state-relative
          state-eth-node
          state-snapshots
      ==
  =*  sub  ->+>-
  =*  etn  ->+>+<
  =*  sap  ->+>+>
  ::
  ::  +|  outward
  |%
  ::
  ::  +abet: produce results
  ::
  ++  abet
    ^-  $:  (list move)  chain  state-relative
            state-eth-node  state-snapshots
        ==
    =.  .
      ?~  rewind-block
        .
      ::  if we're rewinding to a block, then we throw away any moves
      ::  and changes we were going to make.
      ::
      =:  moves    *(list move)
          changes  *logs
        ==
      (restore-block u.rewind-block)
    [(flop moves) ?:(reset &+changes |+changes) sub etn sap]
  ::
  ::  +put-move: store side-effect
  ::
  ++  put-move
    |=  mov=move
    %_(+> moves [mov moves])
  ::
  ::  +put-request: store rpc request to ethereum node
  ::
  ++  put-request
    |=  [wir=wire id=(unit @t) req=request]
    (put-move (rpc-hiss wir (request-to-json id req)))
  ::
  ::  +put-change: store change made by event
  ::
  ++  put-change
    |=  [cause=event-id dif=diff-azimuth]
    ?:  (~(has by changes) cause)  ::  one diff per event
      ~&  [%duplicate-cause cause]
      !!
    +>(changes (~(put by changes) cause dif))
  ::
  ::  +|  move-generation
  ::
  ::  +wrap-note: %pass a note using a made-up duct
  ::
  ++  wrap-note
    |=  [wir=wire not=note]
    ^-  move
    :-  [/jael/eth-logic ~ ~]
    [%pass (weld /(scot %p our) wir) not]
  ::
  ::
  ::  +rpc-hiss: make an http request to our ethereum rpc source
  ::
  ++  rpc-hiss
    |=  [wir=wire jon=json]
    ^-  move
    %+  wrap-note  wir
    :^  %l  %request
      ?>  ?=(%| -.source)
      (light-json-request node.p.source jon)
    *outbound-config:http-client
  ::
  ::  +|  source-operations
  ::
  ::  +listen-to-ship: depend on who for ethereum events
  ::
  ++  listen-to-ship
    |=  [our=ship who=ship]
    %-  put-move(source &+who)
    %+  wrap-note  /vent/(scot %p who)
    [%a %want who /j/(scot %p our)/vent `*`[%vent ~]]
  ::
  ::  +unsubscribe-from-source: stop listening to current source ship
  ::
  ++  unsubscribe-from-source
    |=  our=ship
    %-  put-move
    ?>  ?=(%& -.source)
    %+  wrap-note  /vent/(scot %p p.source)
    ::TODO  should we maybe have a %nuke-vent,
    ::      or do we have a unique duct here?
    [%a %want p.source /j/(scot %p our)/vent `*`[%nuke ~]]
  ::
  ::  +listen-to-node: start syncing from a node
  ::
  ::    Get latest block from eth node and compare to our own latest block.
  ::    Get intervening blocks in chunks until we're caught up, then set
  ::    up a filter going forward.
  ::
  ++  listen-to-node
    |=  url=purl:eyre
    get-latest-block(source |+%*(. *node-src node url))
  ::
  ::  +|  catch-up-operations
  ::
  ::  +get-latest-block
  ::
  ::    Get latest known block number from eth node.
  ::
  ++  get-latest-block
    (put-request /catch-up/block-number `'block number' %eth-block-number ~)
  ::
  ::  +catch-up: get next chunk
  ::
  ++  catch-up
    |=  from-block=@ud
    ?:  (gte from-block foreign-block)
      new-filter
    =/  next-block  (min foreign-block (add from-block 5.760)) ::  ~d1
    ~&  [%catching-up from=from-block to=foreign-block]
    %-  put-request
    :+  /catch-up/step/(scot %ud from-block)/(scot %ud next-block)
      `'catch up'
    :*  %eth-get-logs
        `number+from-block
        `number+next-block
        ~[azimuth:contracts]
        ~
    ==
  ::
  ::  +|  filter-operations
  ::
  ::  +new-filter: request a new polling filter
  ::
  ::    Listens only to the Azimuth state contract, and only from
  ::    the last-heard block onward.
  ::
  ++  new-filter
    %-  put-request
    :+  /filter/new  `'new filter'
    :*  %eth-new-filter
        `number+latest-block
        ::  XX We want to load from a snapshot at least 40 blocks behind, then
        ::  replay to the present
        ::  `[%number ?:((lte latest-block 40) 0 (sub.add latest-block 40))]
        ::TODO  or Azimuth origin block when 0
        ~  ::TODO  we should probably chunck these, maybe?
        ::  https://stackoverflow.com/q/49339489
        ~[azimuth:contracts]
        ~
    ==
  ::
  ::  +read-filter: get all events the filter captures
  ::
  ++  read-filter
    ?>  ?=(%| -.source)
    %-  put-request
    :+  /filter/logs  `'filter logs'
    [%eth-get-filter-logs filter-id.p.source]
  ::
  ::  +poll-filter: get all new events since the last poll (or filter creation)
  ::
  ++  poll-filter
    ?>  ?=(%| -.source)
    ?:  =(0 filter-id.p.source)
      ~&  %no-filter-bad-poll
      .
    %-  put-request
    :+  /filter/changes  `'poll filter'
    [%eth-get-filter-changes filter-id.p.source]
  ::
  ::  +wait-poll: remind us to poll in four minutes
  ::
  ::    Four minutes because Ethereum RPC filters time out after five.
  ::    We don't check for an existing timer or clear an old one here,
  ::    sane flows shouldn't see this being called superfluously.
  ::
  ++  wait-poll
    ?>  ?=(%| -.source)
    =+  wen=(add now ~m4)
    %-  put-move(poll-timer.p.source wen)
    (wrap-note /poll %b %wait wen)
  ::
  ::  +cancel-wait-poll: remove poll reminder
  ::
  ++  cancel-wait-poll
    ?>  ?=(%| -.source)
    %-  put-move(poll-timer.p.source *@da)
    %+  wrap-note  /poll/cancel
    [%b %rest poll-timer.p.source]
  ::
  ::  +|  configuration
  ::
  ::  +init: initialize with default ethereum connection
  ::
  ::    for galaxies, we default to a localhost geth node.
  ::    for stars and under, we default to the parent ship.
  ::
  ++  init
    |=  [our=ship bos=ship]
    ^+  +>
    ::  TODO: ship or node as sample?
    ::
    =.  latest-block  (max latest-block launch:contracts)
    ?:  |(=(our bos) ?=(^ nod.own))
      ~|  %jael-init-node
      (listen-to-node (need nod.own))
    (listen-to-ship our bos)
  ::
  ::  +look: configure the source of ethereum events
  ::
  ++  look
    |=  src=(each ship purl:eyre)
    ^+  +>
    =.  +>
      ?:  ?=(%| -.source)
        cancel-wait-poll
      (unsubscribe-from-source our)
    ?:  ?=(%| -.src)
      (listen-to-node p.src)
    (listen-to-ship our p.src)
  ::
  ::  +|  subscription-results
  ::
  ::  +hear-vent: process incoming events
  ::
  ++  hear-vent
    |=  =vent-result
    ^+  +>
    ?-  -.vent-result
        ::  `look` can be | because we know we're listening to a ship
        ::  rather than a node, so the subscription was never broken
        ::
        %snap  (restore-snap snap.vent-result |)
        %chain
      ?-  -.can.vent-result
        %&   (assume p.can.vent-result)
        ::
          %|
        =+  evs=~(tap by p.can.vent-result)
        |-
        ?~  evs  +>.^$
        =.  +>.^$  (accept i.evs)
        $(evs t.evs)
      ==
    ==
  ::
  ::  +assume: clear state and process events
  ::
  ++  assume
    |=  evs=logs
    ^+  +>
    %.  chain+|+evs
    %_  hear-vent
      heard         ~
      latest-block  0
      reset         &
    ==
  ::
  ::  +accept: process single event
  ::
  ++  accept
    |=  [cause=event-id dif=diff-azimuth]
    ^+  +>
    ?:  (~(has in heard) cause)
      ~&  %accept-ignoring-duplicate-event
      +>.$
    (put-change cause dif)
  ::
  ::  +|  filter-results
  ::
  ::  +wake: kick polling, unless we changed source
  ::
  ++  wake
    ?.  ?=(%| -.source)  .
    poll-filter
  ::
  ::  +sigh: parse rpc response and process it
  ::
  ++  sigh
    |=  [cuz=wire mar=mark res=vase]
    ^+  +>
    ?:  ?=(%& -.source)  +>
    ?:  ?=(%tang mar)
      ::TODO  proper error handling
      ~_  q.res
      ~&  [%yikes cuz]
      +>.$
    ?>  ?=(%httr mar)
    =+  raw-rep=~|(res ;;(httr:eyre q.res))
    =+  rep=(httr-to-rpc-response raw-rep)
    (complete-with-rpc-response cuz rep)
  ::
  ++  finished
    |=  [cuz=wire =response-header:http full-file=(unit mime-data:http-client)]
    ^+  +>
    ?:  ?=(%& -.source)  +>
    ::
    =+  rep=(httr-to-rpc-response (to-httr:http-client response-header full-file))
    (complete-with-rpc-response cuz rep)
  ::
  ++  complete-with-rpc-response
    |=  [cuz=wire rep=response:rpc:jstd]
    ^+  +>
    ::
    ?:  ?=(%fail -.rep)
      ?:  =(405 p.hit.rep)
        ~&  'HTTP 405 error (expected if using infura)'
        +>.$
      ?.  =(5 (div p.hit.rep 100))
        ~&  [%http-error hit.rep]
        +>.$
      ?+  cuz
        :: ~&  [%retrying-node ((soft tang) q.res)]
        wait-poll
          [%catch-up %step @ta @ta ~]
        ~&  %retrying-catch-up
        (catch-up (slav %ud `@ta`i.t.t.cuz))
      ==
    ?+  cuz  ~|([%weird-sigh-wire cuz] !!)
        [%filter %new *]
      (take-new-filter rep)
    ::
        [%filter *]
      (take-filter-results rep)
    ::
        [%catch-up %block-number ~]
      (take-block-number rep)
    ::
        [%catch-up %step @ta @ta ~]
      =/  from-block  (slav %ud `@ta`i.t.t.cuz)
      =/  next-block  (slav %ud `@ta`i.t.t.t.cuz)
      (take-catch-up-step rep from-block next-block)
    ==
  ::
  ::  httr-to-rpc-response
  ::
  ++  httr-to-rpc-response
    |=  hit/httr:eyre
    ^-  response:rpc:jstd
    ~|  hit
    ?.  ?=($2 (div p.hit 100))
      fail+hit
    =/  a=json  (need (de-json:html q:(need r.hit)))
    =,  dejs-soft:format
    ^-  response:rpc:jstd
    =;  dere
      =+  res=((ar dere) a)
      ?~  res  (need (dere a))
      [%batch u.res]
    |=  a=json
    ^-  (unit response:rpc:jstd)
    =/  res=(unit [@t json])
      ::TODO  breaks when no id present
      ((ot id+so result+some ~) a)
    ?^  res  `[%result u.res]
    ~|  a
    :+  ~  %error  %-  need
    ((ot id+so error+(ot code+no message+so ~) ~) a)
  ::
  ::  +take-new-filter: store filter-id and read it
  ::
  ++  take-new-filter
    |=  rep=response:rpc:jstd
    ^+  +>
    ~|  rep
    ?<  ?=(%batch -.rep)
    ?<  ?=(%fail -.rep)
    ?:  ?=(%error -.rep)
      ~&  [%filter-error--retrying message.rep]
      new-filter
    ?>  ?=(%| -.source)
    =-  read-filter(filter-id.p.source -)
    (parse-eth-new-filter-res res.rep)
  ::
  ::  +take-filter-results: parse results into event-logs and process them
  ::
  ++  take-filter-results
    |=  rep=response:rpc:jstd
    ^+  +>
    ?<  ?=(%batch -.rep)
    ?<  ?=(%fail -.rep)
    ?:  ?=(%error -.rep)
      ?.  ?|  =('filter not found' message.rep)  ::  geth
              =('Filter not found' message.rep)  ::  parity
          ==
        ~&  [%unhandled-filter-error +.rep]
        +>
      ::~&  [%filter-timed-out--recreating block=latest-block +.rep]
      ::  arguably should rewind 40 blocks on the off chance the chain reorganized
      ::  when we blinked.  this will also restart the filter.
      ::
      ::  (restore-block ?:((lth latest-block 40) 0 (sub.add latest-block 40)))
      ::
      ::  counter-argument: it's a royal pain to restore from a snapshot
      ::  every time you can't ping the node for 5 minutes.  this is likely
      ::  to destabilize the network.  better to manually restore if we
      ::  notice an anomaly.
      ::
      ::  third way: don't trust anything that doesn't have 40 confirmations
      ::
      new-filter
    ::  kick polling timer, only if it hasn't already been.
    =?  +>  ?&  ?=(%| -.source)
                (gth now poll-timer.p.source)
            ==
      wait-poll
    (take-events rep)
  ::
  ::  +take-block-number: take block number and start catching up
  ::
  ++  take-block-number
    |=  rep=response:rpc:jstd
    ^+  +>
    ?<  ?=(%batch -.rep)
    ?<  ?=(%fail -.rep)
    ?:  ?=(%error -.rep)
      ~&  [%take-block-number-error--retrying message.rep]
      get-latest-block
    =.  foreign-block  (parse-eth-block-number res.rep)
    ~&  [%setting-foreign-block foreign-block]
    (catch-up latest-block)
  ::
  ::  +take-catch-up-step: process chunk
  ::
  ++  take-catch-up-step
    |=  [rep=response:rpc:jstd from-block=@ud next-block=@ud]
    ^+  +>
    ?<  ?=(%batch -.rep)
    ?<  ?=(%fail -.rep)
    ?:  ?=(%error -.rep)
      ~&  [%catch-up-step-error--retrying message.rep]
      (catch-up from-block)
    =.  +>.$  (take-events rep)
    (catch-up next-block)
  ::
  ::  +take-events: process events
  ::
  ++  take-events
    |=  rep=response:rpc:jstd
    ^+  +>
    ?<  ?=(%batch -.rep)
    ?<  ?=(%fail -.rep)
    ?<  ?=(%error -.rep)
    ?.  ?=(%a -.res.rep)
      ~&  [%events-not-array rep]
      !!
    =*  changes  p.res.rep
    ~?  (gth (lent changes) 0)
      :*  %processing-changes
          changes=(lent changes)
          block=latest-block
          id=?.(?=(%| -.source) ~ `@ux`filter-id.p.source)
      ==
    |-  ^+  +>.^$
    ?~  changes  +>.^$
    =.  +>.^$
      (take-event-log (parse-event-log i.changes))
    $(changes t.changes)
  ::
  ::  +take-event-log: obtain changes from event-log
  ::
  ++  take-event-log
    |=  log=event-log
    ^+  +>
    ?~  mined.log
      ~&  %ignoring-unmined-event
      +>
    =*  place  u.mined.log
    ?:  (~(has in heard) block-number.place log-index.place)
      ?.  removed.u.mined.log
        ::  ~&  [%ignoring-duplicate-event tx=transaction-hash.u.mined.log]
        +>
      ::  block was reorganized away, so rewind to this block and
      ::  start syncing again.
      ::
      ~&  :*  'removed event!  Perhaps chain has reorganized?'
              tx-hash=transaction-hash.u.mined.log
              block-number=block-number.u.mined.log
              block-hash=block-hash.u.mined.log
          ==
      %=    +>
          rewind-block
        :-  ~
        ?~  rewind-block
          block-number.place
        (min block-number.place u.rewind-block)
      ==
    =+  cuz=[block-number.place log-index.place]
    ::
    ?:  =(i.topics.log changed-dns:azimuth-events)
      =+  ^-  [pri=tape sec=tape ter=tape]
        %+  decode-results  data.log
        ~[%string %string %string]
      %+  put-change  cuz
      [%dns (crip pri) (crip sec) (crip ter)]
    ::
    =+  dif=(event-log-to-point-diff log)
    ?~  dif  +>.$
    (put-change cuz %point u.dif)
  ::                                                    ::  ++restore-block:et
  ++  restore-block                                     ::  rewind before block
    |=  block=@ud
    ^+  +>
    =/  old-qeu  snaps.sap
    =:  snaps.sap       ~
        count.sap       0
        last-block.sap  0
      ==
    =^  snap=snapshot  +>.$
      ?:  |(=(~ old-qeu) (lth block block-number:(need ~(top to old-qeu))))
        [%*(. *snapshot latest-block launch:contracts) +>.$]
      |-  ^-  [snapshot _+>.^$]
      =^  snap=[block-number=@ud snap=snapshot]  old-qeu
        ~(get to old-qeu)
      =:  count.sap       +(count.sap)
          last-block.sap  block-number.snap
          snaps.sap       (~(put to snaps.sap) snap)
        ==
      ?:  |(=(~ old-qeu) (lth block block-number:(need ~(top to old-qeu))))
        [snap.snap +>.^$]
      $
    ~&  [%restoring-block block latest-block.snap ~(wyt by pos.eth.snap)]
    (restore-snap snap &)
  ::                                                    ::  ++restore-snap:et
  ++  restore-snap                                      ::  restore snapshot
    |=  [snap=snapshot look=?]
    ^+  +>
    ::  update pub subscribers
    ::
    =.  +>.$
      =/  subs=(list (pair ship (set duct)))
        ~(tap by yen.puk.sub)
      |-  ^+  +>.^$
      ?~  subs  +>.^$
      =/  pub  (fall (~(get by kyz.snap) p.i.subs) *public)
      =.  +>.^$  (exec q.i.subs [%give %pubs pub])
      $(subs t.subs)
    ::  update vent subscribers
    ::
    =.  +>.$  (vent-pass yen.eth snap+snap)
    ::  keep the following in sync with ++extract-snap:file:su
    ::
    %=    +>.$
        heard.etn         heard.snap
        latest-block.etn  latest-block.snap
        kyz.puk.sub       kyz.snap
        dns.eth.sub       dns.eth.snap
        pos.eth.sub       pos.eth.snap
        sap               sap(last-block 0)
        moves
      ?.  look  moves
      =-  [[hen %pass /wind/look %j %look -] moves]
      ?-  -.source.etn
        %&  &+p.source.etn
        %|  |+node.p.source.etn
      ==
    ==
  ::                                                    ::  ++exec:et
  ++  exec                                              ::  mass gift
    |=  {yen/(set duct) cad/card}
    =/  noy  ~(tap in yen)
    |-  ^+  ..exec
    ?~  noy  ..exec
    $(noy t.noy, moves [[i.noy cad] moves])
  ::                                                    ::  ++vent-pass:et
  ++  vent-pass                                         ::  "give" vent
    |=  [yen=(set duct) res=vent-result]
    =+  yez=~(tap in yen)
    |-  ^+  ..vent-pass
    ?~  yez  ..vent-pass
    =*  d  i.yez
    ?>  ?=([[%a @ @ *] *] d)
    =+  our=(slav %p i.t.i.d)
    =+  who=(slav %p i.t.t.i.d)
    %+  exec  [d ~ ~]
    :+  %pass
      /(scot %p our)/vent-result
    ^-  note
    [%a %want who /j/(scot %p our)/vent-result %vent-result res]
  ::                                                    ::  ++feed:su
  --
--
::                                                      ::::
::::                    #  vane                         ::  interface
  ::                                                    ::::
::
::  lex: all durable %jael state
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
          old/state
      ==
  ^+  ..^$
  ..^$(lex old)
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
    =/  sec  (~(got by jaw.own.sub.lex) lyf.own.sub.lex)
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
    ?:  fak.own.sub.lex
      ``[%atom !>(1)]
    ?:  =(u.who p.why)
      ``[%atom !>(lyf.own.sub.lex)]
    =/  pub  (~(get by kyz.puk.sub.lex) u.who)
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
    ?:  fak.own.sub.lex
      ``[%atom !>(1)]
    =/  pos  (~(get by pos.eth.sub.lex) u.who)
    ?~  pos  ~
    ?~  net.u.pos  ~
    ``[%atom !>(continuity-number.u.net.u.pos)]
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
      =/  sec  (~(got by jaw.own.sub.lex) u.lyf)
      =/  cub  (nol:nu:crub:crypto sec)
      =/  sig  (sign:as:cub (shaf %self (sham [u.who 1 pub:ex:cub])))
      :^  ~  ~  %noun
      !>  ^-  deed:ames
      [1 pub:ex:cub `sig]
    ::
    ?:  ?=(%earl rac)
      ?.  =(u.who p.why)
        [~ ~]
      ?:  (gth u.lyf lyf.own.sub.lex)
        ~
      ?:  (lth u.lyf lyf.own.sub.lex)
        [~ ~]
      =/  sec  (~(got by jaw.own.sub.lex) u.lyf)
      =/  cub  (nol:nu:crub:crypto sec)
      :^  ~  ~  %noun
      !>  ^-  deed:ames
      [u.lyf pub:ex:cub sig.own.sub.lex]
    ::
    =/  pub  (~(get by kyz.puk.sub.lex) u.who)
    ?~  pub
      ~
    ?:  (gth u.lyf life.u.pub)
      ~
    =/  pas  (~(get by pubs.u.pub) u.lyf)
    ?~  pas
      ~
    :^  ~  ~  %noun
    !>  `deed:ames`[u.lyf u.pas ~]
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
    ?:  (gth u.lyf lyf.own.sub.lex)
      ~
    ?:  (lth u.lyf lyf.own.sub.lex)
      [~ ~]
    :: XX check that who/lyf hasn't been booted
    ::
    =/  sec  (~(got by jaw.own.sub.lex) u.lyf)
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
      %eth-status
    ?.  ?=(~ tyl)  [~ ~]
    :^  ~  ~  %noun  !>
    ^-  [latest-block=@ud source=(each ship node-src)]
    [latest-block.etn.lex source.etn.lex]
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
    [~ ~ %noun !>(tuf.own.sub.lex)]
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
