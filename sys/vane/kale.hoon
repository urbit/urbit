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
::  ++  state-pki                                           ::  urbit metadata
::    $:  $=  own                                           ::  vault (vein)
::          $:  yen=(set duct)                              ::  trackers
::              sig=(unit oath)                             ::  for a moon
::              tuf=(list turf)                             ::  domains
::              boq=@ud                                     ::  boot block
::              nod=(unit purl:eyre)                        ::  eth gateway
::              fak=_|                                      ::  fake keys
::              lyf=life                                    ::  version
::              jaw=(map life ring)                         ::  private keys
::          ==                                              ::
::        $=  zim                                           ::  ethereum (vent)
::          $:  yen=(map filter duct)                       ::  trackers
::              dns=dnses                                   ::  on-chain dns state
::              pos=(map ship point)                        ::  on-chain ship state
::          ==                                              ::
::    ==                                                    ::
++  state-snapshots                                     ::  rewind points
  $:  interval=_100                                     ::  block interval
      max-count=_10                                     ::  max snaps
      count=@ud                                         ::  length of snaps
      last-block=@ud                                    ::  number of last snap
      snaps=(qeu [block-number=@ud snap=snapshot])      ::  old states
  ==                                                    ::
++  message                                             ::  message to her kale
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
  $~  [%a %woot ~zod ~]                                      ::
  $%  [%j $>(%vent gift)]                               ::  ethereum changes
      [%a $>(%woot gift:able:ames)]                     ::  message result
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
  ::                                                    ::  ++scry:of
  ++  scry                                              ::  read
    |=  {syd/@tas pax/path}
    ~|  %kale-scry-of-stub
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
        ::    %kale init must be deferred (makes http requests)
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
      !!
      ::  %+  cute  hen  =<  abet
      ::  XX
      ::  (~(look et hen our now sub.lex etn.lex sap.lex) src.tac)
      ::
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
        :+  %kale  %|
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
        !!
        ::  %+  cute  hen  =<  abet
        ::  XX
        ::  (~(hear-vent et hen our now sub.lex etn.lex sap.lex) p.mes)
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
        [%j %vent *]
      !!
      ::  %+  cute  hen  =<  abet
      ::  XX
      ::  (~(hear-vent et hen our now sub.lex etn.lex sap.lex) p.hin)
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
          old/state
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
