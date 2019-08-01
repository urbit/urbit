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
        $:  yen=(set tracker)                           ::  trackers
            sig=(unit oath)                             ::  for a moon
            tuf=(list turf)                             ::  domains
            boq=@ud                                     ::  boot block
            nod=purl:eyre                               ::  eth gateway
            fak=_|                                      ::  fake keys
            lyf=life                                    ::  version
            jaw=(map life ring)                         ::  private keys
        ==                                              ::
      $=  zim                                           ::  public
        $:  yen=(jug tracker ship)                      ::  trackers
            ney=(jug ship tracker)                      ::  reverse trackers
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
  ==                                                    ::
+$  message-response                                    ::  response from her kale
  $%  [%public-keys-result =public-keys-result]         ::  %public-keys response
  ==
::  $tracker: client; either domestic (here=%.y) or foreign
::
+$  tracker  [here=? =duct]
+$  card                                                ::  i/o action
  (wind note gift)                                      ::
::                                                      ::
+$  move                                                ::  output
  [p=duct q=card]                                       ::
::                                                      ::
+$  note                                                ::  out request $->
  $~  [%a %plea *ship *plea:ames]                       ::
  $%  $:  %a                                            ::    to %ames
          $>(%plea task:able:ames)                      ::  send request message
      ==                                                ::
      $:  %g                                            ::    to self
          $>(%deal task:able:gall)                      ::  set ethereum source
      ==                                                ::
      $:  %k                                            ::    to self
          $>(%listen task)                              ::  set ethereum source
      ==                                                ::
      $:  @tas                                          ::
  $%  $>(%init vane-task)                               ::  report install
  ==  ==  ==                                            ::
::                                                      ::
+$  peer-sign  [=ship =udiff:point]                     ::
::                                                      ::
+$  sign                                                ::  in result $<-
  $~  [%a %done ~]                                      ::
  $%  $:  %a
          $%  $>(%boon gift:able:ames)                  ::  message response
              $>(%done gift:able:ames)                  ::  message (n)ack
      ==  ==
      $:  %g                                            ::
          $>  $?  %onto                                 ::
                  %unto                                 ::
              ==                                        ::
          gift:able:gall                                ::
  ==  ==
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
  ++  emit
    |=  =move
    +>.$(moz [move moz])
  ::
  ++  poke-watch
    |=  [hen=duct app=term =purl:eyre]
    %-  emit
    :*  hen
        %pass
        /[app]/poke
        %g
        %deal
        [our our]
        app
        %poke
        %azimuth-tracker-poke
        !>([%watch (crip (en-purl:html purl))])
    ==
  ::
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
    |=  $:  ::  hyr: domestic? or foreign
            ::  hen: event cause
            ::  tac: event data
            ::
            hyr=?
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
    ::        czar=(map ship [=rift =life =pass])
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
      =.  nod.own.pki
        %+  fall  node.tac
        (need (de-purl:html 'http://eth-mainnet.urbit.org:8545'))
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
        [1 lyf.seed.tac (my [lyf.seed.tac [1 pub:ex:cub]] ~) `ship.spon.tac]
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
      =/  spon-point=point
        ~|  [%sponsor-point point]
        ?>  ?=(^ net.spon.tac)
        :*  continuity-number.u.net.spon.tac
            life.u.net.spon.tac
            (malt [life.u.net.spon.tac 1 pass.u.net.spon.tac] ~)
            ?.  has.sponsor.u.net.spon.tac
              ~
            `who.sponsor.u.net.spon.tac
        ==
      =/  points=(map =ship =point)
        %-  ~(run by czar.tac)
        |=  [=a=rift =a=life =a=pass]
        ^-  point
        [a-rift a-life (malt [a-life 1 a-pass] ~) ~]
      =.  points
        (~(put by points) ship.spon.tac spon-point)
      =.  +>.$
        %-  curd  =<  abet
        (public-keys:~(feel su hyr hen our pki etn sap) %full points)
      ::
      ::  start subscriptions
      ::
      =.  +>.$  (poke-watch hen %azimuth-tracker nod.own.pki)
      =.  +>.$
        ?-    (clan:title our)
            %czar
          %-  curd  =<  abet
          (sources:~(feel su hyr hen our pki etn sap) ~ [%| %azimuth-tracker])
        ::
            *
          =.  +>.$
            %-  curd  =<  abet
            %+  sources:~(feel su hyr hen our pki etn sap)
              (silt ship.spon.tac ~)
            [%| %azimuth-tracker]
          %-  curd  =<  abet
          (sources:~(feel su hyr hen our pki etn sap) ~ [%& ship.spon.tac])
        ==
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
    ::    [%listen whos=(set ship) =source]
    ::
        %listen
      ~&  [%kale-listen whos source]:tac
      %-  curd  =<  abet
      (sources:~(feel su hyr hen our pki etn sap) [whos source]:tac)
    ::
    ::  cancel all trackers from duct
    ::    {$nuke whos=(set ship)}
    ::
        $nuke  (on-nuke [hyr hen] whos.tac)
    ::
    ::  watch public keys
    ::    [%public-keys ships=(set ship)]
    ::
        %public-keys  (on-public-keys [hyr hen] ships.tac)
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
    ::  XX should be a subscription
    ::  XX reconcile with .dns.eth
    ::  request domains
    ::    [%turf ~]
    ::
        %turf
      ::  ships with real keys must have domains,
      ::  those with fake keys must not
      ::
      ~|  [fak.own.pki tuf.own.pki]
      ?<  =(fak.own.pki ?=(^ tuf.own.pki))
      +>.$(moz [[hen %give %turf tuf.own.pki] moz])
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
      (curd abet:~(private-keys ~(feed su hyr hen our pki etn sap) hyr hen))
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
    ::    [%plea =ship =plea:ames]
    ::
        %plea
      ::  send ack immediately
      ::
      =.  moz  [[hen %give %done ~] moz]
      ::  coerce and handle message
      ::
      =/  mes  ;;(message payload.plea.tac)
      ?-  -.mes
        %nuke         (on-nuke [hyr hen] whos.mes)
        %public-keys  (on-public-keys [hyr hen] whos.mes)
      ==
    ::
    ::  rewind to snapshot
    ::    {$wind p/@ud}
    ::
        %wind
      (wind hen p.tac)
    ==
  ::  +on-nuke: cancel trackers
  ::
  ++  on-nuke
    |=  [=tracker whos=(set ship)]
    ^+  +>
    ::
    =/  ships=(list ship)
      %~  tap  in
      %-  ~(int in whos)
      (~(get ju yen.zim.pki) tracker)
    =.  ney.zim.pki
      |-  ^-  (jug ship ^tracker)
      ?~  ships
        ney.zim.pki
      (~(del ju $(ships t.ships)) i.ships tracker)
    =.  yen.zim.pki
      |-  ^-  (jug ^tracker ship)
      ?~  ships
        yen.zim.pki
      (~(del ju $(ships t.ships)) tracker i.ships)
    ?^  whos
      +>.$
    %_  +>.$
      yen.own.pki  (~(del in yen.own.pki) tracker)
    ==
  ::
  ++  on-public-keys
    |=  [=tracker whos=(set ship)]
    ^+  +>
    ::
    =/  feeder  ~(feed su here.tracker duct.tracker our pki etn sap)
    (curd abet:(~(public-keys feeder tracker) whos))
  ::
  ++  take
    |=  [tea=wire hyr=? hen=duct hin=sign]
    ^+  +>
    ::  at the moment, we only get a +sign from ames
    ::
    ?>  hyr
    ?>  ?=([@ *] tea)
    =*  wir  t.tea
    ?-    hin
        [%a %done *]
      ?~  error.hin  +>.$
      ~&  [%kale-ames-nack u.error.hin]
      ~_  tang.u.error.hin
      ::TODO  fail:et
      +>.$
    ::
        [%a %boon *]
      =.  moz  [[hen %give %done ~] moz]
      =+  ;;  res=message-response  payload.hin
      ?>  ?=(%public-keys-result -.res)
      ::
      %-  curd  =<  abet
      (public-keys:~(feel su hyr hen our pki etn sap) public-keys-result.res)
    ::
        [%g %onto *]
      ~&  [%kale-onto tea hin]
      +>.$
    ::
        [%g %unto *]
      ?-  +>-.hin
          $quit           ~|([%kale-unexpected-quit tea hin] !!)
          $http-response  ~|([%kale-unexpected-http-response tea hin] !!)
          $coup
        ?~  p.p.+>.hin
          +>.$
        %-  (slog leaf+"kale-bad-coup" u.p.p.+>.hin)
        +>.$
      ::
          $reap
        ?~  p.p.+>.hin
          +>.$
        %-  (slog u.p.p.+>.hin)
        ~|([%kale-unexpected-reap tea hin] +>.$)
      ::
          $diff
        ?>  ?=([@ *] tea)
        =*  app  i.tea
        =/  =peer-sign  ;;(peer-sign q.q.p.p.+>.hin)
        %-  curd  =<  abet
        (~(new-event su hyr hen our pki etn sap) peer-sign)
      ==
    ==
  ::                                                    ::  ++curd:of
  ++  curd                                              ::  relative moves
    |=  $:  moz/(list move)
            pki/state-pki
            etn/state-eth-node
            sap/state-snapshots
        ==
    +>(pki pki, etn etn, sap sap, moz (weld (flop moz) ^moz))
  ::
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
  =|  $:  hyr=?
          hen=duct
          our=ship
          state-pki
          state-eth-node
          state-snapshots
      ==
  ::  moz: moves in reverse order
  ::  pki: relative urbit state
  ::
  =*  pki  &4.-
  =*  etn  &5.-
  =*  sap  |5.-
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
  ++  emit-peer
    |=  [app=term =path]
    %-  emit
    :*  hen
        %pass
        [app path]
        %g
        %deal
        [our our]
        app
        %peer
        path
    ==
  ::
  ++  peer
    |=  [app=term whos=(set ship)]
    ?:  =(~ whos)
      (emit-peer app /)
    =/  whol=(list ship)  ~(tap in whos)
    |-  ^+  this-su
    ?~  whol  this-su
    =.  this-su  (emit-peer app /(scot %p i.whol))
    $(whol t.whol)
  ::
  ++  public-keys-give
    |=  [yen=(set tracker) =public-keys-result]
    =+  yez=~(tap in yen)
    |-  ^+  this-su
    ?~  yez  this-su
    =*  t  i.yez
    =.  this-su
      ?:  here.t
        (emit duct.t %give %public-keys public-keys-result)
      (emit duct.t %give %boon %public-keys-result public-keys-result)
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
  ++  new-event
    |=  [=a=ship =a=udiff:point]
    ^+  this-su
    =/  a-point=point  (~(gut by pos.zim.pki) a-ship *point)
    =/  a-diff=(unit diff:point)  (udiff-to-diff:point a-udiff a-point)
    ?~  a-diff
      this-su
    (public-keys:feel %diff a-ship u.a-diff)
  ::
  ++  extract-snap                                      ::  extract rewind point
    ^-  snapshot
    ~
  ::
  ++  feed
    |_  ::  hyr: domestic? or foreign
        ::  hen: subscription source
        ::
        $:  hyr=?
            hen=duct
        ==
    ::
    ::  Handle subscription to public-keys
    ::
    ++  public-keys
      |=  whos=(set ship)
      ^+  ..feed
      ?:  fak.own.pki
        (public-keys:fake whos)
      ::  Subscribe to parent of moons
      ::
      =.  ..feed
        =/  moons=(jug ship ship)
          %-  ~(gas ju *(jug spon=ship who=ship))
          %+  murn  ~(tap in whos)
          |=  who=ship
          ^-  (unit [spon=ship child=ship])
          ?.  =(%earl (clan:title who))
            ~
          `[(^sein:title who) who]
        =/  moonl=(list [spon=ship ships=(set ship)])
          ~(tap by moons)
        |-  ^+  ..feed
        ?~  moonl
          ..feed
        ?.  =(our spon.i.moonl)
          =.  ..feed  (sources:feel ships.i.moonl [%& spon.i.moonl])
          $(moonl t.moonl)
        =/  sec  (~(got by jaw.own.pki) lyf.own.pki)
        =/  points=(map ship point)
          =/  our-moonl  ~(tap in ships.i.moonl)
          %-  malt
          |-  ^-  (list [ship point])
          ?~  our-moonl
            ~
          =/  moon-sec  (shaf %earl (sham our lyf.own.pki i.our-moonl))
          =/  cub  (nol:nu:crub:crypto moon-sec)
          =/  =pass  pub:ex:cub
          :-  [i.our-moonl 1 1 (malt [1 1 pass] ~) `our]
          $(our-moonl t.our-moonl)
        (public-keys-give (sy [hyr hen] ~) [%full points])
      ::  Add to subscriber list
      ::
      =.  ney.zim
        =/  whol=(list ship)  ~(tap in whos)
        |-  ^-  (jug ship tracker)
        ?~  whol
          ney.zim
        (~(put ju $(whol t.whol)) i.whol [hyr hen])
      =.  yen.zim
        %-  ~(gas ju yen.zim)
        %+  turn  ~(tap in whos)
        |=  who=ship
        [[hyr hen] who]
      ::  Give initial result
      ::
      =/  =public-keys-result
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
      (public-keys-give (sy [hyr hen] ~) public-keys-result)
    ::
    ::  Handle subscription to private-keys
    ::
    ++  private-keys
      %_  ..feed
        moz      [[hen %give %private-keys [lyf jaw]:own] moz]
        yen.own  (~(put in yen.own) hyr hen)
      ==
    ::
    ++  fake
      ?>  fak.own.pki
      |%
      ++  public-keys
        |=  whos=(set ship)
        ~&  [%fake-1 whos]
        =/  whol=(list ship)  ~(tap in whos)
        =/  passes
          |-  ^-  (list [who=ship =pass])
          ?~  whol
            ~
          =/  cub  (pit:nu:crub:crypto 512 i.whol)
          :-  [i.whol pub:ex:cub]
          $(whol t.whol)
        ~&  [%fake-2 passes]
        =/  points=(list (pair ship point))
          %+  turn  passes
          |=  [who=ship =pass]
          ^-  [who=ship =point]
          [who [rift=1 life=1 (my [1 1 pass] ~) `(^sein:title who)]]
        ~&  [%fake-3 points]
        =.  moz  [[hen %give %public-keys %full (my points)] moz]
        ~&  [%fake-4 moz]
        ..feel
      --
    --
  ::
  ++  feel
    |%
    ::
    ::  Update public-keys
    ::
    ++  public-keys
      |=  =public-keys-result
      ^+  this-su
      ?:  ?=(%full -.public-keys-result)
        =.  pos.zim  (~(uni by pos.zim) points.public-keys-result)
        =/  pointl=(list [who=ship =point])
          ~(tap by points.public-keys-result)
        |-  ^+  this-su
        ?~  pointl
          this-su
        %+  public-keys-give
          (~(get ju ney.zim) who.i.pointl)
        [%full (my i.pointl ~)]
      =*  who  who.public-keys-result
      =/  a-diff=diff:point  diff.public-keys-result
      =/  maybe-point  (~(get by pos.zim) who)
      =/  =point  (fall maybe-point *point)
      =.  point
        ?-  -.a-diff
            %spon  point(sponsor to.a-diff)
            %rift  point(rift to.a-diff)
            %keys
          %_  point
              life  life.to.a-diff
              keys
            %+  ~(put by keys.point)
              life.to.a-diff
            [crypto-suite pass]:to.a-diff
          ==
        ==
      =.  pos.zim  (~(put by pos.zim) who point)
      %+  public-keys-give
        (~(get ju ney.zim) who)
      ?~  maybe-point
        [%full (my [who point]~)]
      [%diff who a-diff]
    ::
    ::  Update private-keys
    ::
    ++  private-keys
      |=  [=life =ring]
      ^+  this-su
      ?:  &(=(lyf.own life) =((~(get by jaw.own) life) `ring))
        this-su
      =.  lyf.own  life
      =.  jaw.own  (~(put by jaw.own) life ring)
      =/  yen  (~(run in yen.own) tail)
      (exec yen [%give %private-keys lyf.own jaw.own])
    ::
    ++  sources
      |=  [whos=(set ship) =source]
      ^+  this-su
      ?:  ?=(%& -.source)
        =/  send-message
          |=  =message
          [hen %pass /public-keys %a %plea p.source %k /public-keys message]
        (emit (send-message %public-keys whos))
      =^  =source-id  this-su  (get-source-id source)
      =.  this-su
        ?~  whos
          ::
          =.  default-source.etn  source-id
          this-su
        =/  whol=(list ship)  ~(tap in `(set ship)`whos)
        =.  ship-sources.etn
          |-  ^-  (map ship ^source-id)
          ?~  whol
            ship-sources.etn
          (~(put by $(whol t.whol)) i.whol source-id)
        =.  ship-sources-reverse.etn
          %-  ~(gas ju ship-sources-reverse.etn)
          (turn whol |=(=ship [source-id ship]))
        this-su
      (peer p.source whos)
    --
  ::
  ::  No-op
  ::
  ++  meet
    |=  [who=ship =life =pass]
    ^+  +>
    +>.$
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
  =/  here=?  !=(%plea -.task)
  =^  did  lex
    abet:(~(call of [our now eny] lex) here hen task)
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
      !>  [1 pub:ex:cub `sig]
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
      !>  [u.lyf pub:ex:cub sig.own.pki.lex]
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
    !>  [u.lyf pass.u.pas ~]
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
  =/  =sign  q.hin
  ?>  ?=(%a -.sign)
  ?>  ?=(?(%boon %done) +<.sign)
  =/  here=?  %.y
  =^  did  lex  abet:(~(take of [our now eny] lex) tea here hen sign)
  [did ..^$]
--
