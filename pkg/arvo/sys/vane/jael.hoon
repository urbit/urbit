!:                                                      ::  /vane/jael
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
=,  point=point:able:jael
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
+$  state                                               ::  all vane state
  $:  ver=$0                                            ::  vane version
      pki=state-pki                                     ::
      etn=state-eth-node                                ::  eth connection state
  ==                                                    ::
+$  state-pki                                           ::  urbit metadata
  $:  $=  own                                           ::  vault (vein)
        $:  yen=(set duct)                              ::  trackers
            sig=(unit oath)                             ::  for a moon
            tuf=(list turf)                             ::  domains
            boq=@ud                                     ::  boot block
            nod=purl:eyre                               ::  eth gateway
            fak=_|                                      ::  fake keys
            lyf=life                                    ::  version
            jaw=(map life ring)                         ::  private keys
        ==                                              ::
      $=  zim                                           ::  public
        $:  yen=(jug duct ship)                         ::  trackers
            ney=(jug ship duct)                         ::  reverse trackers
            nel=(set duct)                              ::  trackers of all
            dns=dnses                                   ::  on-chain dns state
            pos=(map ship point)                        ::  on-chain ship state
        ==                                              ::
  ==                                                    ::
+$  message                                             ::  message to her jael
  $%  [%nuke whos=(set ship)]                           ::  cancel trackers
      [%public-keys whos=(set ship)]                    ::  view ethereum events
  ==                                                    ::
+$  message-result
  $%  [%public-keys-result =public-keys-result]         ::  public keys boon
  ==
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
      $:  %b                                            ::    to %behn
          $>(%wait task:able:behn)                      ::  set timer
      ==                                                ::
      $:  %g                                            ::    to %gall
          $>(%deal task:able:gall)                      ::  talk to app
      ==                                                ::
      $:  %j                                            ::    to self
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
  $%  $:  %a                                            ::
          $%  $>(%boon gift:able:ames)                  ::  message response
              $>(%done gift:able:ames)                  ::  message (n)ack
              $>(%lost gift:able:ames)                  ::  lost boon
      ==  ==                                            ::
      $:  %b                                            ::
          $>(%wake gift:able:behn)                      ::
      ==                                                ::
      $:  %g                                            ::
          $>  $?  %onto                                 ::
                  %unto                                 ::
              ==                                        ::
          gift:able:gall                                ::
      ==
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
=>
~%  %jael  ..is  ~
|%
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
    ::        czar=(map ship [=rift =life =pass])
    ::        turf=(list turf)}
    ::        bloq=@ud
    ::        node=purl
    ::    ==
    ::
        %dawn
      ::  single-homed
      ::
      ~|  [our who.seed.tac]
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
      ::  load our initial public key
      ::
      =/  spon-ship=(unit ship)
        =/  flopped-spon  (flop spon.tac)
        ?~(flopped-spon ~ `ship.i.flopped-spon)
      =.  pos.zim.pki
        =/  cub  (nol:nu:crub:crypto key.seed.tac)
        %+  ~(put by pos.zim.pki)
          our
        [0 lyf.seed.tac (my [lyf.seed.tac [1 pub:ex:cub]] ~) spon-ship]
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
      =/  spon-points=(list [ship point])
        %+  turn  spon.tac
        |=  [=ship az-point=point:azimuth]
        ~|  [%sponsor-point az-point]
        ?>  ?=(^ net.az-point)
        :*  ship
            continuity-number.u.net.az-point
            life.u.net.az-point
            (malt [life.u.net.az-point 1 pass.u.net.az-point] ~)
            ?.  has.sponsor.u.net.az-point
              ~
            `who.sponsor.u.net.az-point
        ==
      =/  points=(map =ship =point)
        %-  ~(run by czar.tac)
        |=  [=a=rift =a=life =a=pass]
        ^-  point
        [a-rift a-life (malt [a-life 1 a-pass] ~) ~]
      =.  points
        (~(gas by points) spon-points)
      =.  +>.$
        %-  curd  =<  abet
        (public-keys:~(feel su hen our now pki etn) %full points)
      ::
      ::  start subscriptions
      ::
      =.  +>.$  (poke-watch hen %azimuth-tracker nod.own.pki)
      =.  +>.$
        ::  get everything from azimuth-tracker because jael subscriptions
        ::  seem to be flaky for now
        ::
        ?:  &
          %-  curd  =<  abet
          (sources:~(feel su hen our now pki etn) ~ [%| %azimuth-tracker])
        ::
        ?-    (clan:title our)
            %czar
          %-  curd  =<  abet
          (sources:~(feel su hen our now pki etn) ~ [%| %azimuth-tracker])
        ::
            *
          =.  +>.$
            %-  curd  =<  abet
            %+  sources:~(feel su hen our now pki etn)
              (silt (turn spon-points head))
            [%| %azimuth-tracker]
          %-  curd  =<  abet
          (sources:~(feel su hen our now pki etn) ~ [%& (need spon-ship)])
        ==
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
      ~&  [%jael-listen whos source]:tac
      %-  curd  =<  abet
      (sources:~(feel su hen our now pki etn) [whos source]:tac)
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
      =?  nel.zim.pki  ?=(~ whos.tac)
        (~(del in nel.zim.pki) hen)
      ?^  whos.tac
        +>.$
      %_  +>.$
        yen.own.pki  (~(del in yen.own.pki) hen)
      ==
    ::
    ::  update private keys
    ::
        %rekey
      %-  curd  =<  abet
      (private-keys:~(feel su hen our now pki etn) life.tac ring.tac)
    ::
    ::  update private keys
    ::
        %moon
      ?.  =(%earl (clan:title ship.tac))
        ~&  [%not-moon ship.tac]
        +>.$
      ?.  =(our (^sein:title ship.tac))
        ~&  [%not-our-moon ship.tac]
        +>.$
      %-  curd  =<  abet
      (~(new-event su hen our now pki etn) [ship udiff]:tac)
    ::
    ::  watch public keys
    ::    [%public-keys ships=(set ship)]
    ::
        %public-keys
      %-  curd  =<  abet
      (~(public-keys ~(feed su hen our now pki etn) hen) ships.tac)
    ::
    ::  seen after breach
    ::    [%meet our=ship who=ship]
    ::
        %meet
      +>.$
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
      +>.$::
    ::  in response to memory pressure
    ::    [%trim p=@ud]
    ::
        %trim
      +>.$
    ::
    ::  watch private keys
    ::    {$private-keys $~}
    ::
        %private-keys
      (curd abet:~(private-keys ~(feed su hen our now pki etn) hen))
    ::
        %wegh
      %_    +>
          moz
        :_  moz
        ^-  move
        :^  hen  %give  %mass
        ^-  mass
        :+  %jael  %|
        :~  pki+&+pki
            etn+&+etn
            dot+&+lex
        ==
      ==
    ::
    ::  authenticated remote request
    ::    {$west p/ship q/path r/*}
    ::
        %plea
      =*  her  ship.tac
      =/  mes  ;;(message payload.plea.tac)
      ?-    -.mes
      ::
      ::  cancel trackers
      ::    [%nuke whos=(set ship)]
      ::
          %nuke
        =.  moz  [[hen %give %done ~] moz]
        $(tac mes)
      ::
      ::  view ethereum events
      ::    [%public-keys whos=(set ship)]
      ::
          %public-keys
        =.  moz  [[hen %give %done ~] moz]
        $(tac mes)
      ==
    ==
  ::
  ++  take
    |=  [tea=wire hen=duct hin=sign]
    ^+  +>
    ?-  hin
        [%a %done *]
      ?~  error.hin  +>.$
      ~&  [%done-bad tag.u.error.hin]
      %-  (slog tang.u.error.hin)
      ::TODO  fail:et
      +>.$
    ::
        [%a %boon *]
      =+  ;;  [%public-keys-result =public-keys-result]  payload.hin
      %-  curd  =<  abet
      (public-keys:~(feel su hen our now pki etn) public-keys-result)
    ::
        [%a %lost *]
      ::  TODO: better error handling
      ::
      ~|  %jael-ames-lost
      !!
    ::
        [%b %wake *]
      ?^  error.hin
        %-  %+  slog
              leaf+"jael unable to resubscribe, run :azimuth-tracker|listen"
            u.error.hin
          +>.$
      ?>  ?=([%breach @ ~] tea)
      =/  =source-id  (slav %ud i.t.tea)
      =/  =source  (~(got by sources.etn) source-id)
      =/  ships  (~(get ju ship-sources-reverse.etn) source-id)
      %-  curd  =<  abet
      (sources:~(feel su hen our now pki etn) ships source)
    ::
        [%g %onto *]
      ~&  [%jael-onto tea hin]
      +>.$
    ::
        [%g %unto *]
      ?-  +>-.hin
          $kick      ~|([%jael-unexpected-quit tea hin] !!)
          $poke-ack
        ?~  p.p.+>.hin
          +>.$
        %-  (slog leaf+"jael-bad-coup" u.p.p.+>.hin)
        +>.$
      ::
          $watch-ack
        ?~  p.p.+>.hin
          +>.$
        %-  (slog u.p.p.+>.hin)
        ~|([%jael-unexpected-reap tea hin] +>.$)
      ::
          $fact
        ?>  ?=([@ *] tea)
        =*  app  i.tea
        =/  =peer-sign  ;;(peer-sign q.q.cage.p.+>.hin)
        %-  curd  =<  abet
        (~(new-event su hen our now pki etn) peer-sign)
      ==
    ==
  ::                                                    ::  ++curd:of
  ++  curd                                              ::  relative moves
    |=  $:  moz/(list move)
            pki/state-pki
            etn/state-eth-node
        ==
    +>(pki pki, etn etn, moz (weld (flop moz) ^moz))
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
  =|  $:  hen=duct
          our=ship
          now=@da
          state-pki
          state-eth-node
      ==
  ::  moz: moves in reverse order
  ::  pki: relative urbit state
  ::
  =*  pki  ->+>-
  =*  etn  ->+>+
  |%
  ++  this-su  .
  ::                                                    ::  ++abet:su
  ++  abet                                              ::  resolve
    [(flop moz) pki etn]
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
        %watch
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
    |=  [yen=(set duct) =public-keys-result]
    =+  yez=~(tap in yen)
    |-  ^+  this-su
    ?~  yez  this-su
    =*  d  i.yez
    =.  this-su
      ?.  &(?=([[%a @ @ *] *] d) !=(%pubs i.t.i.d))
        %-  emit
        [d %give %public-keys public-keys-result]
      %-  emit
      [d %give %boon %public-keys-result public-keys-result]
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
  ++  subscribers-on-ship
    |=  =ship
    ^-  (set duct)
    =/  specific-subs  (~(get ju ney.zim) ship)
    =/  general-subs=(set duct)
      ?:  ?=(?(%czar %king %duke) (clan:title ship))
        nel.zim
      ~
    (~(uni in specific-subs) general-subs)
  ::
  ++  feed
    |_  ::  hen: subscription source
        ::
        hen/duct
    ::
    ::  Handle subscription to public-keys
    ::
    ++  public-keys
      |=  whos=(set ship)
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
          ?:  (~(has by ship-sources) who)
            ~
          `[(^sein:title who) who]
        =/  moonl=(list [spon=ship ships=(set ship)])
          ~(tap by moons)
        |-  ^+  ..feed
        ?~  moonl
          ..feed
        ?:  =(our spon.i.moonl)
          $(moonl t.moonl)
        =.  ..feed  (sources:feel ships.i.moonl [%& spon.i.moonl])
        $(moonl t.moonl)
      ::  Add to subscriber list
      ::
      =.  ney.zim
        =/  whol=(list ship)  ~(tap in whos)
        |-  ^-  (jug ship duct)
        ?~  whol
          ney.zim
        (~(put ju $(whol t.whol)) i.whol hen)
      =.  yen.zim
        %-  ~(gas ju yen.zim)
        %+  turn  ~(tap in whos)
        |=  who=ship
        [hen who]
      =?  nel.zim  ?=(~ whos)
        (~(put in nel.zim) hen)
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
      =.  ..feed  (public-keys-give (sy hen ~) public-keys-result)
      ..feed
    ::
    ::  Handle subscription to private-keys
    ::
    ++  private-keys
      %_  ..feed
        moz      [[hen %give %private-keys [lyf jaw]:own] moz]
        yen.own  (~(put in yen.own) hen)
      ==
    ::
    ++  fake
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
      ^+  ..feel
      ?:  ?=(%full -.public-keys-result)
        =/  pointl=(list [who=ship =point])
          ~(tap by points.public-keys-result)
        |-  ^+  ..feel
        ?~  pointl
          ..feel(pos.zim (~(uni by pos.zim) points.public-keys-result))
        ::  if changing rift upward, then signal a breach
        ::
        =?    ..feel
            =/  point
              (~(get by pos.zim) who.i.pointl)
            ?&  ?=(^ point)
                (gth rift.point.i.pointl rift.u.point)
            ==
          =.  ..feel
            %+  public-keys-give
              (subscribers-on-ship who.i.pointl)
            [%breach who.i.pointl]
          =/  sor  (~(get by sources-reverse) %& who.i.pointl)
          ?~  sor
            ..feel
          ::  delay resubscribing because Ames is going to clear any
          ::  messages we send now.
          ::
          (emit hen %pass /breach/(scot %ud u.sor) %b %wait now)
        ::
        =.  ..feel
          %+  public-keys-give
            (subscribers-on-ship who.i.pointl)
          [%full (my i.pointl ~)]
        $(pointl t.pointl)
      ::
      ?:  ?=(%breach -.public-keys-result)
        ::  we calculate our own breaches based on our local state
        ::
        ..feel
      =*  who  who.public-keys-result
      =/  a-diff=diff:point  diff.public-keys-result
      =/  maybe-point  (~(get by pos.zim) who)
      =/  =point  (fall maybe-point *point)
      ::  if changing rift upward, then signal a breach
      ::
      =?    ..feel
          ?&  ?=(%rift -.a-diff)
              (gth to.a-diff rift.point)
          ==
        %+  public-keys-give
          (subscribers-on-ship who)
        [%breach who]
      ::
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
      ::
      =.  pos.zim  (~(put by pos.zim) who point)
      %+  public-keys-give
        (subscribers-on-ship who)
      ?~  maybe-point
        [%full (my [who point]~)]
      [%diff who a-diff]
    ::
    ::  Update private-keys
    ::
    ++  private-keys
      |=  [=life =ring]
      ^+  ..feel
      ?:  &(=(lyf.own life) =((~(get by jaw.own) life) `ring))
        ..feel
      =.  lyf.own  life
      =.  jaw.own  (~(put by jaw.own) life ring)
      (exec yen.own [%give %private-keys lyf.own jaw.own])
    ::
    ::  Change sources for ships
    ::
    ++  sources
      |=  [whos=(set ship) =source]
      ^+  ..feel
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
        =.  ship-sources-reverse.etn
          %-  ~(gas ju ship-sources-reverse.etn)
          (turn whol |=(=ship [source-id ship]))
        ..feed
      ::
      ?:  ?=(%& -.source)
        =/  send-message
          |=  =message
          [hen %pass /public-keys %a %plea p.source %j /public-keys message]
        (emit (send-message %public-keys whos))
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
          dud=(unit goof)
          hic/(hypo (hobo task:able))
      ==
  ^-  [(list move) _..^$]
  ?^  dud
    ~|(%jael-call-dud (mean tang.u.dud))
  ::
  =/  =task:able  ((harden task:able) q.hic)
  =^  did  lex
    abet:(~(call of [our now eny] lex) hen task)
  [did ..^$]
::                                                      ::  ++load
++  load                                                ::  upgrade
  |=  $:  ::  old: previous state
          ::
          ::  old/*
          old/state
      ==
  ^+  ..^$
  ::  ..^$
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
      %lyfe                                             ::  unitized %life
    ?.  ?=([@ ~] tyl)  [~ ~]
    ?.  =([%& our] why)
      [~ ~]
    =/  who  (slaw %p i.tyl)
    ?~  who  [~ ~]
    ::  fake ships always have life=1
    ::
    ?:  fak.own.pki.lex
      ``[%noun !>((some 1))]
    ?:  =(u.who p.why)
      ``[%noun !>((some lyf.own.pki.lex))]
    =/  pub  (~(get by pos.zim.pki.lex) u.who)
    ?~  pub  ``[%noun !>(~)]
    ``[%noun !>((some life.u.pub))]
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
      %ryft                                             ::  unitized %rift
    ?.  ?=([@ ~] tyl)  [~ ~]
    ?.  =([%& our] why)
      [~ ~]
    =/  who  (slaw %p i.tyl)
    ?~  who  [~ ~]
    ::  fake ships always have rift=1
    ::
    ?:  fak.own.pki.lex
      ``[%noun !>((some 1))]
    =/  pos  (~(get by pos.zim.pki.lex) u.who)
    ?~  pos  ``[%noun !>(~)]
    ``[%noun !>((some rift.u.pos))]
  ::
      %vein
    ?.  ?=([@ ~] tyl)  [~ ~]
    ?.  &(?=(%& -.why) =(p.why our))
      [~ ~]
    =/  lyf  (slaw %ud i.tyl)
    ?~  lyf  [~ ~]
    ::
    ?~  r=(~(get by jaw.own.pki.lex) u.lyf)
      [~ ~]
    ::
    [~ ~ %noun !>(u.r)]
  ::
      %deed
    ?.  ?=([@ @ ~] tyl)  [~ ~]
    ?.  &(?=(%& -.why) =(p.why our))
      [~ ~]
    =/  who  (slaw %p i.tyl)
    =/  lyf  (slaw %ud i.t.tyl)
    ?~  who  [~ ~]
    ?~  lyf  [~ ~]
    ::
    ?:  fak.own.pki.lex
      =/  cub  (pit:nu:crub:crypto 512 u.who)
      :^  ~  ~  %noun
      !>  [1 pub:ex:cub ~]
    ::
    =/  rac  (clan:title u.who)
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
    ?.  ?=([@ @ ~] tyl)  [~ ~]
    ?.  =([%& our] why)
      [~ ~]
    =/  who  (slaw %p i.tyl)
    =/  lyf  (slaw %ud i.t.tyl)
    ?~  who  [~ ~]
    ?~  lyf  [~ ~]
    ?:  (gth u.lyf lyf.own.pki.lex)
      ~
    ?:  (lth u.lyf lyf.own.pki.lex)
      [~ ~]
    :: XX check that who/lyf hasn't been booted
    ::
    =/  sec  (~(got by jaw.own.pki.lex) u.lyf)
    =/  moon-sec  (shaf %earl (sham our u.lyf sec u.who))
    =/  cub  (pit:nu:crub:crypto 128 moon-sec)
    =/  =seed  [u.who 1 sec:ex:cub ~]
    ``[%seed !>(seed)]
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
      %subscriptions
    ?.  ?=([@ ~] tyl)  [~ ~]
    ?.  =([%& our] why)
      [~ ~]
    :^  ~  ~  %noun
    !>([yen ney nel]:zim.pki.lex)
  ::
      %sources
    ?.  ?=(~ tyl)  [~ ~]
    :^  ~  ~  %noun  !>
    etn.lex
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
          dud=(unit goof)
          hin/(hypo sign)
      ==
  ^-  [(list move) _..^$]
  ?^  dud
    ~|(%jael-take-dud (mean tang.u.dud))
  ::
  =^  did  lex  abet:(~(take of [our now eny] lex) tea hen q.hin)
  [did ..^$]
--
