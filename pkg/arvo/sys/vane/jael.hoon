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
|=  our=ship
=,  pki:jael
=,  jael
=,  crypto
=,  jael
=,  ethereum-types
=,  azimuth-types
=,  point=point:jael
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
+$  state-4
  $:  %4
      pki=state-pki-4                                   ::
      etn=state-eth-node                                ::  eth connection state
      tim=[%plea (unit resend-timer)]                   ::  nacked plea timer
  ==                                                    ::
+$  state-pki-4                                         ::  urbit metadata
  $:  $=  own                                           ::  vault (vein)
        $:  yen=(set duct)                              ::  trackers
            sig=(unit oath)                             ::  for a moon
            tuf=(list turf)                             ::  domains
            fak=_|                                      ::  fake keys
            lyf=life                                    ::  version
            step=@ud                                    ::  login code step
            jaw=(map life ring)                         ::  private keys
        ==                                              ::
      $=  zim                                           ::  public
        $:  tel=(set duct)                              ::  trackers of all turfs
            fen=(jug duct ship)                         ::  trackers
            nef=(jug ship duct)                         ::  reverse trackers
            fel=(set duct)                              ::  trackers of all fiefs
            fes=(map ship fief)                         ::  routes
            yen=(jug duct ship)                         ::  trackers
            ney=(jug ship duct)                         ::  reverse trackers
            nel=(set duct)                              ::  trackers of all
            dns=dnses                                   ::  on-chain dns state
            pos=(map ship point)                        ::  on-chain ship state
            mon=(jug [=duct source=ship] moon=ship)     ::  moon pending state
        ==                                              ::
  ==                                                    ::
+$  message-all
  $%  [%0 message]
  ==
+$  message                                             ::  message to her jael
  $%  [%nuke whos=(set ship)]                           ::  cancel trackers
      [%public-keys whos=(set ship)]                    ::  view ethereum events
      [%fief whos=(set ship)]
  ==                                                    ::
+$  point-1
  $:  =rift
      =life
      keys=(map life [crypto-suite=@ud =pass])
      sponsor=(unit @p)
  ==
::
++  message-result
  =<  message-result
  |%
  ++  message-result
    |=  a=*
    ^-  message-result-1
    ?:  ?=([~ %2] -.a)  (message-result-1 a)
    =/  b  (message-result-0 a)
    ?.  ?=(%full +<.b)  [~+%2 b]
    :^  [~ %2]  %public-keys-result  %full
    %-  ~(run by points.public-keys-result.b)
    |=  point-1
    ^-  point
    [rift life keys sponsor ~]
  ::
  +$  message-result-1
    $:  [~ %2]
      $%  [%public-keys-result =public-keys-result]     ::  public keys boon
      ==
    ==
  ::
  +$  message-result-0
    $%  [%public-keys-result public-keys-result=public-keys-result-0]
    ==
  ::
  +$  public-keys-result-0
    $%  [%full points=(map ship point-1)]
        [%diff who=ship diff=$<(%fief diff:point)]
        [%breach who=ship]
    ==
  ::
  --
::
+$  resend-timer
  [=duct =wire date=@da]
::
+$  card                                                ::  i/o action
  (wind note gift)                                      ::
::                                                      ::
+$  move                                                ::  output
  [p=duct q=card]                                       ::
::                                                      ::
+$  note                                                ::  out request $->
  $~  [%a %plea *ship *plea:ames]                       ::
  $%  $:  %a                                            ::    to %ames
          $>(%plea task:ames)                           ::  send request message
      ==                                                ::
      $:  %b                                            ::    to %behn
          $>(%wait task:behn)                           ::  set timer
      ==                                                ::
      $:  %e                                            ::    to %eyre
          [%code-changed ~]                             ::  notify code changed
      ==                                                ::
      $:  %g                                            ::    to %gall
          $>(%deal task:gall)                           ::  talk to app
      ==                                                ::
      $:  %j                                            ::    to self
          $>(%listen task)                              ::  set ethereum source
      ==                                                ::
      $:  @tas                                          ::
  $%  $>(%init vane-task)                               ::  report install
  ==  ==  ==                                            ::
::                                                      ::
+$  sign                                                ::  in result $<-
  $~  [%behn %wake ~]                                   ::
  $%  $:  %ames                                         ::
          $%  $>(%boon gift:ames)                       ::  message response
              $>(%done gift:ames)                       ::  message (n)ack
              $>(%lost gift:ames)                       ::  lost boon
      ==  ==                                            ::
      $:  %behn                                         ::
          $>(%wake gift:behn)                           ::
      ==                                                ::
      $:  %gall                                         ::
          $>(%unto gift:gall)                           ::
      ==                                                ::
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
~%  %jael  ..part  ~
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
  =|  moz=(list move)
  =|  $:  $:  ::  now: current time
              ::  eny: unique entropy
              ::
              now=@da
              eny=@uvJ
          ==
          ::  all vane state
          ::
          state-4
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
    |=  [hen=duct app=term pok=*]
    %-  emit
    [hen %pass /[app]/poke %g %deal [our our /jael] app %poke %noun !>(pok)]
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
    |=  who=ship
    ^-  (list ship)
    =/  dad  (sein who)
    [who ?:(=(who dad) ~ $(who dad))]
  ::                                                    ::  ++call:of
  ++  call                                              ::  invoke
    |=  $:  ::  hen: event cause
            ::  tac: event data
            ::
            hen=duct
            tac=task
        ==
    ^+  +>
    ?-    -.tac
    ::
    ::  boot from keys
    ::    $:  %dawn
    ::        =feed
    ::        spon=ship
    ::        czar=(map ship [=rift =life =pass])
    ::        turf=(list turf)
    ::        bloq=@ud
    ::        node=purl
    ::        srcs=(list [p=term q=*])
    ::    ==
    ::
        %dawn
      ::  single-homed
      ::
      =>  .(+.tac (to-latest:dawn-event +.tac))
      ?>  ?=([%2 ~] -.feed.tac)
      ~|  [our who.feed.tac]
      ?>  =(our who.feed.tac)
      ::  load our initial public key
      ::
      =/  spon-ship=(unit ship)
        =/  flopped-spon  (flop spon.tac)
        ?~(flopped-spon ~ `ship.i.flopped-spon)
      =/  kyz=(list [lyf=life key=ring])  kyz.feed.tac
      ?~  kyz  !!
      =/  ky=[lyf=life key=ring]  i.kyz
      =.  pos.zim.pki
        =/  cic  (nol:nu:cric:crypto key.ky)
        %+  ~(put by pos.zim.pki)
          our
        [ryf.feed.tac lyf.ky (my [lyf.ky [num:ex:cic pub:ex:cic]] ~) spon-ship ~]
      ::  our initial private key
      ::
      =.  lyf.own.pki  lyf.ky
      =.  jaw.own.pki  (malt kyz)
      ::  XX save sponsor in .own.pki
      ::  XX reconcile with .dns.eth
      ::  set initial domains
      ::
      =.  tuf.own.pki  turf.tac
      ::  our initial galaxy table as a +map from +life to +public
      ::
      =/  points  (~(gas by lams.tac) spon.tac)
      =.  +>.$
        %-  curd  =<  abet
        (public-keys:~(feel su hen now pki etn) [pos fes]:zim.pki %full points)
      ::
      ::  start subscriptions
      ::
      =.  +>.$
        ?:  =(~ srcs.tac)
          %^  poke-watch  hen  %azimuth
          !>([%watch 'http://eth-mainnet.urbit.org:8545/' %default])
        |-  ^+  +>.^$
        ?~  srcs.tac  +>.^$
        =.  +>.^$  (poke-watch hen i.srcs.tac)
        $(srcs.tac t.srcs.tac)
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
        :~  [hen %slip %e %init ~]
            [hen %slip %d %init ~]
            [hen %slip %g %init ~]
            [hen %slip %c %init ~]
            [hen %slip %a %init ~]
        ==
      +>.$
    ::
    ::  boot fake
    ::    [%fake =ship]
    ::
        %fake
      ::  single-homed
      ::
      =>  ?@  +.tac  [cic=(pit:nu:cric:crypto 512 our %b ~) .]
          =/  cic
            %-  nol:nu:cric:crypto
            ?@  -.feed.tac
              ?>  =(1 lyf.feed.tac)
              key.feed.tac
            ?-  -.feed.tac
              [%1 ~]  (~(got by (malt kyz.feed.tac)) 1)
              [%2 ~]  (~(got by (malt kyz.feed.tac)) 1)
            ==
          .(tac [%fake ship=`@p`fig:ex:cic])
      ?>  =(our ship.tac)
      ::  fake keys are deterministically derived from the ship
      ::
      ::  our initial public key
      ::
      =.  pos.zim.pki
        %+  ~(put by pos.zim.pki)
          our
        [rift=0 life=1 (my [`@ud`1 [num:ex:cic pub:ex:cic]] ~) `(^sein:title our) ~]
      ::  our private key
      ::
      ::    Private key updates are disallowed for fake ships,
      ::    so we do this first.
      ::
      =.  lyf.own.pki  1
      =.  jaw.own.pki  (my [1 sec:ex:cic] ~)
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
        :~  [hen %slip %e %init ~]
            [hen %slip %d %init ~]
            [hen %slip %g %init ~]
            [hen %slip %c %init ~]
            [hen %slip %a %init ~]
        ==
      +>.$
    ::
    ::  set ethereum source
    ::    [%listen whos=(set ship) =source]
    ::
        %listen
      ::  %-  (slog leaf+"jael: listen {<whos.tac>} {<source.tac>}" ~)
      %-  curd  =<  abet
      (sources:~(feel su hen now pki etn) [whos source]:tac)
    ::
    ::  cancel all trackers from duct
    ::    [%nuke whos=(set ship)]
    ::
        %nuke
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
      =.  nef.zim.pki
        |-  ^-  (jug ship duct)
        ?~  ships
          nef.zim.pki
        (~(del ju $(ships t.ships)) i.ships hen)
      =.  fen.zim.pki
        |-  ^-  (jug duct ship)
        ?~  ships
          fen.zim.pki
        (~(del ju $(ships t.ships)) hen i.ships)
      =?  fel.zim.pki  ?=(~ whos.tac)
        (~(del in fel.zim.pki) hen)
      =?  tel.zim.pki  ?=(~ whos.tac)
        (~(del in tel.zim.pki) hen)
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
      (private-keys:~(feel su hen now pki etn) life.tac ring.tac)
    ::
    ::  resend private key to subscribers
    ::
        %resend
      %-  curd  =<  abet
      %-  ~(exec su hen now pki etn)
      [yen.own.pki [%give %private-keys [lyf jaw]:own.pki]]
    ::
    ::  register moon keys
    ::
        %moon
      ?.  =(%earl (clan:title ship.tac))
        ~&  [%not-moon ship.tac]
        +>.$
      ?.  =(our (^sein:title ship.tac))
        ~&  [%not-our-moon ship.tac]
        +>.$
      %-  curd  =<  abet
      (~(new-event su hen now pki etn) [ship udiff]~:tac)
    ::
    ::  rotate web login code
    ::
        %step
      %=  +>.$
        step.own.pki  +(step.own.pki)
        moz           [[hen %pass / %e %code-changed ~] moz]
      ==
    ::
    ::  watch public keys
    ::    [%public-keys ships=(set ship)]
    ::
        %public-keys
      %-  curd  =<  abet
      (~(public-keys ~(feed su hen now pki etn) hen) ships.tac)
    ::  watch fiefs
    ::    [%fief ships=(set ship)]
    ::
        %fief
      %-  curd  =<  abet
      (~(fiefs ~(feed su hen now pki etn) hen) ships.tac)

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
      =.  tel.zim.pki  (~(put in tel.zim.pki) hen)
      +>.$(moz [[hen %give %turf tuf.own.pki] moz])
    ::
    ::  learn of kernel upgrade
    ::    [%vega ~]
    ::
        %vega
      +>.$
    ::
    ::  in response to memory pressure
    ::    [%trim p=@ud]
    ::
        %trim
      +>.$
    ::
    ::  watch private keys
    ::    [%private-keys ~]
    ::
        %private-keys
      (curd abet:~(private-keys ~(feed su hen now pki etn) hen))
    ::
    ::  authenticated remote request
    ::    [%west p=ship q=path r=*]
    ::
        %plea
      =*  her  ship.tac
      =+  ;;(=message-all payload.plea.tac)
      ?>  ?=(%0 -.message-all)
      =/  =message  +.message-all
      ?-    -.message
      ::
      ::  view fief events
      ::    [%fief whos=(set ship)]
      ::
          %fief
        =.  moz  [[hen %give %done ~] moz]
        $(tac message)
      ::
      ::  cancel trackers
      ::    [%nuke whos=(set ship)]
      ::
          %nuke
        =.  moz  [[hen %give %done ~] moz]
        $(tac message)
      ::
      ::  view ethereum events
      ::    [%public-keys whos=(set ship)]
      ::
          %public-keys
        =.  moz  [[hen %give %done ~] moz]
        $(tac message)
      ==
    ::
    ::  pretend ships breached
    ::    [%ruin ships=(set ship)]
    ::
        %ruin
      ::NOTE  we blast this out to _all_ known ducts, because the common
      ::      use case for this is comets, about who nobody cares.
      =/  dus  (~(uni in nel.zim.pki) ~(key by yen.zim.pki))
      =/  sus  ~(. su hen now pki etn)
      =;  core=_sus
        (curd abet:core)
      %-  ~(rep in ships.tac)
      |=  [=ship s=_sus]
      (exec:s dus %give %public-keys %breach ship)
    ==
  ::
  ++  take
    |=  [tea=wire hen=duct hin=sign]
    ^+  +>
    ?-  hin
        [%ames %done *]
      ?~  error.hin  +>.$
      ~&  [%done-bad tag.u.error.hin]
      %-  (slog tang.u.error.hin)
      ::  if first time hearing an error, start timer to retry sending the $plea
      ::
      ::    XX  allow to change the rate via a %jael task in 409
      ::
      =?  moz  ?=([%plea ~] tim)
        [hen %pass /public-keys %b %wait `@da`(add now ~m10)]^moz
      =?  tim  ?=([%plea ~] tim)
        plea/`[hen /public-keys `@da`(add now ~m10)]
      +>.$
    ::
        [%ames %boon *]
      =+  ;;  [%public-keys-result =public-keys-result]  payload.hin
      %-  curd  =<  abet
      (public-keys:~(feel su hen now pki etn) [pos fes]:zim.pki public-keys-result)
    ::
        [%ames %lost *]
      ::  TODO: better error handling
      ::
      ~|  %jael-ames-lost
      !!
    ::
        [%behn %wake *]
      ?^  error.hin
        =+  ?.  ?=([%breach @ ~] tea)  ~  :: XX log %plea timer crashed
            %-  %+  slog
                  leaf+"jael unable to resubscribe, run :azimuth|listen"
                u.error.hin
            ~
        +>.$
      ?:  ?=([%public-keys ~] tea)
        ::  resend any pleas that are outstanding
        ::
        =/  sus  ~(. su hen now pki etn)
        =;  core=_sus
          =.  tim  plea/~
          (curd abet:core)
        =/  plea=[@tas path %0 term whos=(set @p)]
          [%j /public-keys %0 %public-keys whos=~]
        %-  ~(rep by mon.zim.pki)
        |=  [[[hen=duct source=ship] whos=(set ship)] s=_sus]
        (emit:s hen %pass /public-keys %a %plea source plea(whos whos))
      ?>  ?=([%breach @ ~] tea)
      =/  =source-id  (slav %ud i.t.tea)
      =/  =source  (~(got by sources.etn) source-id)
      =/  ships  (~(get ju ship-sources-reverse.etn) source-id)
      %-  curd  =<  abet
      (sources:~(feel su hen now pki etn) ships source)
    ::
        [%gall %unto *]
      ?-    +>-.hin
          %raw-fact  !!
      ::
          %kick
        ?>  ?=([@ *] tea)
        =*  app  i.tea
        ::NOTE  we expect azimuth-tracker to be kill
        ?:  =(%azimuth-tracker app)  +>.$
        ~|([%jael-unexpected-quit tea hin] !!)
      ::
          %poke-ack
        ?~  p.p.+>.hin
          +>.$
        %-  (slog leaf+"jael-bad-coup" u.p.p.+>.hin)
        +>.$
      ::
          %watch-ack
        ?~  p.p.+>.hin
          +>.$
        %-  (slog u.p.p.+>.hin)
        ~|([%jael-unexpected-reap tea hin] +>.$)
      ::
          %fact
        ?>  ?=([@ *] tea)
        =*  app  i.tea
        =+  ;;(=udiffs:point q.q.cage.p.+>.hin)
        %-  curd  =<  abet
        (~(new-event su hen now pki etn) udiffs)
      ==
    ==
  ::                                                    ::  ++curd:of
  ++  curd                                              ::  relative moves
    |=  $:  moz=(list move)
            pki=state-pki-4
            etn=state-eth-node
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
          now=@da
          state-pki-4
          state-eth-node
      ==
  ::  moz: moves in reverse order
  ::  pki: relative urbit state
  ::
  =*  pki  ->+<
  =*  etn  ->+>
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
    |=  [yen=(set duct) cad=card]
    ^+  this-su
    %-  ~(rep in yen)
    |=  [=duct su=_this-su]
    su(moz [[duct cad] moz])
  ::
  ++  emit-peer
    |=  [app=term =path]
    %-  emit
    :*  hen
        %pass
        [app path]
        %g
        %deal
        [our our /jael]
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
    ^+  this-su
    =/  yaz  %+  skid  ~(tap in yen)
      |=  d=duct
      &(?=([[%ames @ @ *] *] d) !=(%public-keys i.t.i.d))
    =/  yez  (weld p.yaz (sort q.yaz duct-sorter))
    |-  ^+  this-su
    ?~  yez  this-su
    =*  d  i.yez
    =.  this-su
      ?.  &(?=([[%ames @ @ *] *] d) !=(%public-keys i.t.i.d))
        %-  emit
        [d %give %public-keys public-keys-result]
      %-  emit
      [d %give %boon %public-keys-result public-keys-result]
    $(yez t.yez)
    ::
  ++  fiefs-give
    |=  [yen=(set duct) =fiefs-result]
    ^+  this-su
    =/  yaz  %+  skid  ~(tap in yen)
      |=  d=duct
      &(?=([[%ames @ @ *] *] d) !=(%fief i.t.i.d))
    =/  yez  (weld p.yaz (sort q.yaz duct-sorter))
    |-  ^+  this-su
    ?~  yez  this-su
    =*  d  i.yez
    =.  this-su
      ?.  &(?=([[%ames @ @ *] *] d) !=(%fief i.t.i.d))
        %-  emit
        [d %give %fief fiefs-result]
      %-  emit
      [d %give %boon %fief-result fiefs-result]
    $(yez t.yez)
  ::  We want to notify Ames, then Clay, then Gall.  This happens to
  ::  be alphabetical, but this is mostly a coincidence. We also have
  ::  to notify Gall the vane before we notify any Gall agents, so we
  ::  can kiss the coincidence goodbye.
  ::
  ++  duct-sorter
    |=  [a=duct b=duct]
    ?.  ?=([[@ *] *] a)
      |
    ?.  ?=([[@ *] *] b)
      &
    ?:  &(?=([[%gall *] *] a) ?=([[%gall *] *] b))
      ?=([%gall %sys *] i.a)
    (lth (end 3 i.i.a) (end 3 i.i.b))
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
    |=  =udiffs:point
    ^+  this-su
    =+  orig=[pos=pos.zim.pki fes=fes.zim.pki]
    |-  ^+  this-su
    ?~  udiffs
      this-su
    =/  a-point=point  (~(gut by pos.zim.pki) ship.i.udiffs *point)
    =/  a-diff=(unit diff:point)  (udiff-to-diff:point udiff.i.udiffs a-point)
    =?  this-su  ?=(^ a-diff)
      =?    this-su
          ?&  =(our ship.i.udiffs)
              ?=(%keys -.u.a-diff)
              (~(has by jaw.own) life.to.u.a-diff)
          ==
        ::  if this about our keys, and we already know these, start using them
        ::
        =.  lyf.own  life.to.u.a-diff
        ::  notify subscribers (ames) to start using our new private keys
        ::
        (exec yen.own [%give %private-keys [lyf jaw]:own])
      ::
      (public-keys:feel orig %diff ship.i.udiffs u.a-diff)
    $(udiffs t.udiffs)
  ::
  ++  subscribers-on-ship
    |=  =ship
    ^-  (set duct)
    ::  union of general and ship-specific subs
    ::
    %-  ~(uni in nel.zim)
    (~(get ju ney.zim) ship)
  ::
  ++  subscribers-on-fief
    |=  =ship
    ^-  (set duct)
    ::  union of general and ship-specific subs
    ::
    %-  ~(uni in fel.zim)
    (~(get ju nef.zim) ship)
  ::
  ++  feed
    |_  ::  hen: subscription source
        ::
        hen=duct
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
    ::  Handle subscription to fiefs
    ::
    ++  fiefs
      |=  whos=(set ship)
      ^+  ..feed
      ?:  fak.own.pki
        ..feed  ::(fiefs:fake whos)
      ::  Add to subscriber list
      ::
      =.  nef.zim
        =/  whol=(list ship)  ~(tap in whos)
        |-  ^-  (jug ship duct)
        ?~  whol
          nef.zim
        (~(put ju $(whol t.whol)) i.whol hen)
      =.  fen.zim
        %-  ~(gas ju fen.zim)
        %+  turn  ~(tap in whos)
        |=  who=ship
        [hen who]
      =?  fel.zim  ?=(~ whos)
        (~(put in fel.zim) hen)
      ::  Give initial result
      ::
      =/  =fiefs-result
        ?:  =(~ whos)
          (~(run by fes.zim) some)
        %-  ~(gas by *fiefs-result)
        %+  murn
          ~(tap in whos)
        |=  who=ship
        ^-  (unit (pair ship (unit fief)))
        =/  fef  (~(get by fes.zim) who)
        ?~  fef  ~
        `[who fef]
      =.  ..feed  (fiefs-give (sy hen ~) fiefs-result)
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
          ?:  =(%pawn (clan:title i.whol))  $(whol t.whol)
          =/  cic  (pit:nu:cric:crypto 512 i.whol %b ~)
          :-  [i.whol pub:ex:cic]
          $(whol t.whol)
        =/  points=(list (pair ship point))
          %+  turn  passes
          |=  [who=ship =pass]
          ^-  [who=ship =point]
          [who [rift=0 life=1 (my [1 1 pass] ~) `(^sein:title who) ~]]
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
      |=  [orig=[pos=(map ship point) fes=(map ship fief)] =public-keys-result]
      ^+  ..feel
      ?:  ?=(%full -.public-keys-result)
        =/  pointl=(list [who=ship =point])
          ~(tap by points.public-keys-result)
        |-  ^+  ..feel
        ?~  pointl
          ..feel(pos.zim (~(uni by pos.zim) points.public-keys-result))
        =?  ..feel
            ?&  ?!  .=  (~(get by fes.orig) who.i.pointl)
                fief.point.i.pointl
            ==
          =.  fes.zim
            ?~  fief.point.i.pointl
              (~(del by fes.zim) who.i.pointl)
            (~(put by fes.zim) [who u.fief.point]:i.pointl)
          %+  fiefs-give
            (subscribers-on-fief who.i.pointl)
          [[who fief.point]:i.pointl ~ ~]
        ::  if changing rift upward and we already had keys for them,
        ::  then signal a breach
        ::
        =?    ..feel
            =/  point
              (~(get by pos.zim) who.i.pointl)
            ?&  (~(has by pos.orig) who.i.pointl)
                ?=(^ point)
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
        ::  moon keys not pending; delete
        ::
        =?  mon.zim  ?=(%earl (clan:title who.i.pointl))
          =/  sponsor=ship
            ?^  sponsor.point.i.pointl
              u.sponsor.point.i.pointl
            (^sein:title who.i.pointl)
          (~(del ju mon.zim) hen^sponsor who.i.pointl)
        $(pointl t.pointl)
      =?  ..feel
          ?&  ?=([%diff * %fief *] public-keys-result)
              ?!  .=  (~(get by fes.orig) who.public-keys-result)
              to.diff.public-keys-result
          ==
        =.  fes.zim
          ?~  to.diff.public-keys-result
            (~(del by fes.zim) who.public-keys-result)
          (~(put by fes.zim) [who u.to.diff]:public-keys-result)
        %+  fiefs-give
          (subscribers-on-fief who.public-keys-result)
        [[who to.diff]:public-keys-result ~ ~]
      ::
      ?:  ?=(%breach -.public-keys-result)
        ::  we calculate our own breaches based on our local state
        ::
        ..feel
      =*  who  who.public-keys-result
      =/  a-diff=diff:point  diff.public-keys-result
      =/  maybe-point  (~(get by pos.zim) who)
      =/  =point  (fall maybe-point *point)
      ::  if changing rift upward and we already had keys for them, then
      ::  signal a breach
      ::
      =?    ..feel
          ?&  (~(has by pos.orig) who)
              ?=(^ maybe-point)
              ?=(%rift -.a-diff)
              (gth to.a-diff rift.point)
          ==
        =.  ..feel
          %+  public-keys-give
            (subscribers-on-ship who)
          [%breach who]
        =/  sor  (~(get by sources-reverse) %& who)
        ?~  sor
          ..feel
        ::  delay resubscribing because Ames is going to clear any
        ::  messages we send now.
        ::
        (emit hen %pass /breach/(scot %ud u.sor) %b %wait now)
      ::
      =.  point
        ?-  -.a-diff
            %fief  point(fief to.a-diff)
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
      ::  only eagerly update lyf if we were behind the chain life
      ::
      =?  lyf.own
          ?|  ?=(%earl (clan:title our))
              ?&  (gth life lyf.own)
                ::
                  =+  pon=(~(get by pos.zim) our)
                  ?~  pon  |
                  (lth lyf.own life.u.pon)
          ==  ==
        life
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
        =.  mon.zim
          %-  ~(rep in whos)
          |=  [=ship m=_mon.zim]
          (~(put ju m) [hen p.source] ship)
        %-  emit
        =/  =plea:ames  [%j /public-keys %0 %public-keys whos]
        [hen %pass /public-keys %a %plea p.source plea]
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
=|  lex=state-4
|=  $:  ::  now: current time
        ::  eny: unique entropy
        ::  rof: namespace resolver
        ::
        now=@da
        eny=@uvJ
        rof=roof
    ==
^?
|%
::                                                      ::  ++call
++  call                                                ::  request
  |=  $:  ::  hen: cause of this event
          ::  hic: event data
          ::
          hen=duct
          dud=(unit goof)
          hic=(hobo task)
      ==
  ^-  [(list move) _..^$]
  ?^  dud
    ~|(%jael-call-dud (mean tang.u.dud))
  ::
  =/  =task  ((harden task) hic)
  =^  did  lex
    abet:(~(call of [now eny] lex) hen task)
  [did ..^$]
::                                                      ::  ++load
++  load                                                ::  upgrade
  =>  |%
      ::
      +$  any-state  $%(state-1 state-2 state-3 state-4)
      +$  state-3
        $:  %3
            pki=state-pki-3
            etn=state-eth-node
            tim=[%plea (unit resend-timer)]
        ==
      +$  state-pki-3
        $:  $=  own
              $:  yen=(set duct)  sig=(unit oath)  tuf=(list turf)  fak=_|
                  lyf=life        step=@ud         jaw=(map life ring)
              ==
            $=  zim
              $:  yen=(jug duct ship)  ney=(jug ship duct)  nel=(set duct)
                  dns=dnses            pos=(map ship point-1)
                  mon=(jug [=duct source=ship] moon=ship)
        ==    ==
      +$  state-2
        $:  %2
            pki=state-pki-2
            etn=state-eth-node
        ==
      +$  state-pki-2
        $:  $=  own
              $:  yen=(set duct)  sig=(unit oath)  tuf=(list turf)  fak=_|
                  lyf=life        step=@ud         jaw=(map life ring)
              ==
            $=  zim
              $:  yen=(jug duct ship)  ney=(jug ship duct)  nel=(set duct)
                  dns=dnses            pos=(map ship point-1)
        ==    ==
      +$  state-1
        $:  %1
            pki=state-pki-1
            etn=state-eth-node
        ==
      +$  state-pki-1
        $:  $=  own
              $:  yen=(set duct)  sig=(unit oath)  tuf=(list turf)
                  boq=@ud         nod=purl:eyre    fak=_|
                  lyf=life        step=@ud         jaw=(map life ring)
              ==
            $=  zim
              $:  yen=(jug duct ship)  ney=(jug ship duct)  nel=(set duct)
                  dns=dnses            pos=(map ship point-1)
        ==    ==
      ::
      --
  |=  old=any-state
  ^+  ..^$
  =?  old  ?=(%1 -.old)
    ^-  state-2
    %=  old
      -        %2
      own.pki  own.pki.old(+>+ +>.+>+.own.pki.old)
    ==
  =?  old  ?=(%2 -.old)
    ^-  state-3
    %=  old
      -        %3
      pos.zim.pki  [pos.zim.pki.old mon=~]
              etn  [etn.old plea/~]
    ==
  =?  old  ?=(%3 -.old)
    ^-  state-4
    %=    old
        -  %4
        zim.pki
      :*  tel=~  fen=~  nef=~  fel=~  fes=~
          %=    zim.pki.old
              pos
            %-  ~(run by pos.zim.pki.old)
            |=  point-1
            ^-  point
            [rift life keys sponsor ~]
      ==  ==
    ==
  ?>  ?=(%4 -.old)
  ..^$(lex old)
::                                                      ::  ++scry
++  scry                                                ::  inspect
  ^-  roon
  |=  [lyc=gang pov=path car=term bem=beam]
  ^-  (unit (unit cage))
  =*  ren  car
  =*  why=shop  &/p.bem
  =*  syd  q.bem
  =*  lot=coin  $/r.bem
  =*  tyl  s.bem
  ::
  ::  XX review for security, stability, cases other than now
  ::
  ?.  &(=(lot [%$ %da now]) =([~ ~] lyc))  ~
  ::
  ?:  &(?=(%x ren) =(tyl //whey))
    =/  maz=(list mass)
      :~  pki+&+pki.lex
          etn+&+etn.lex
      ==
    ``mass+!>(maz)
  ::
  ?.  =(%$ ren)  [~ ~]
  ?+    syd
      ~
  ::
      %step
    ?.  ?=([@ ~] tyl)  [~ ~]
    ?.  =([%& our] why)
      [~ ~]
    =/  who  (slaw %p i.tyl)
    ?~  who  [~ ~]
    ``[%noun !>(step.own.pki.lex)]
  ::
      %code
    ?.  ?=([@ ~] tyl)  [~ ~]
    ?.  =([%& our] why)
      [~ ~]
    =/  who  (slaw %p i.tyl)
    ?~  who  [~ ~]
    =/  sec  (~(got by jaw.own.pki.lex) lyf.own.pki.lex)
    =/  sal  (add %pass step.own.pki.lex)
    ``[%noun !>((end 6 (shaf sal (shax sec))))]
  ::
      %fake
    ?.  ?=(~ tyl)  [~ ~]
    ?.  =([%& our] why)
      [~ ~]
    ``[%noun !>(fak.own.pki.lex)]
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
      ?:  ?=(%pawn (clan:title u.who))  ``[%noun !>(~)]
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
    ::  fake ships always have rift=0
    ::
    ?:  fak.own.pki.lex
      ``[%atom !>(0)]
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
    ::  fake ships always have rift=0
    ::
    ?:  fak.own.pki.lex
      ``[%noun !>((some 0))]
    =/  pos  (~(get by pos.zim.pki.lex) u.who)
    ?~  pos  ``[%noun !>(~)]
    ``[%noun !>((some rift.u.pos))]
  ::
      %pont
    ?.  ?=([@ ~] tyl)  [~ ~]
    ?.  =([%& our] why)
      [~ ~]
    =/  who  (slaw %p i.tyl)
    ?~  who  [~ ~]
    ?:  fak.own.pki.lex  [~ ~]
    =/  pos  (~(get by pos.zim.pki.lex) u.who)
    ?~  pos  ~
    ``[%noun !>(u.pos)]
  ::
      %pynt                                             ::  unitized %pont
    ?.  ?=([@ ~] tyl)  [~ ~]
    ?.  =([%& our] why)
      [~ ~]
    =/  who  (slaw %p i.tyl)
    ?~  who  [~ ~]
    ?:  fak.own.pki.lex  [~ ~]
    =/  pos  (~(get by pos.zim.pki.lex) u.who)
    ``[%noun !>(pos)]
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
      %vile
    =*  life  lyf.own.pki.lex
    =*  rift
      ?:  fak.own.pki.lex  0  ::  fake ships always have rift=0
      rift:(~(got by pos.zim.pki.lex) our)
    =/  =feed  [[%2 ~] our rift [life (~(got by jaw.own.pki.lex) life)]~]
    [~ ~ %atom !>((jam feed))]
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
      =/  cic  (pit:nu:cric:crypto 512 u.who %b ~)
      :^  ~  ~  %noun
      !>  [1 pub:ex:cic ~]
    ::
    =/  rac  (clan:title u.who)
    ?:  ?=(%pawn rac)
      ?.  =(u.who p.why)
        [~ ~]
      ?.  =(1 u.lyf)
        [~ ~]
      =/  sec  (~(got by jaw.own.pki.lex) u.lyf)
      =/  cic  (nol:nu:cric:crypto sec)
      =/  sig  %+  sign-raw:ed:crypto  (shaf %self (sham [u.who 1 pub:ex:cic]))
               [sgn.pub sgn.sek]:saf:ex:cic
      :^  ~  ~  %noun
      !>  [1 pub:ex:cic `sig]
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
    =/  cic  (pit:nu:cric:crypto 128 moon-sec %b ~)
    =/  =feed  [[%2 ~] u.who 0 [1 sec:ex:cic]~]
    ``[%feed !>(feed)]
  ::
      %sein
    ?.  ?=([@ ~] tyl)  [~ ~]
    ?.  =([%& our] why)
      [~ ~]
    =/  who  (slaw %p i.tyl)
    ?~  who  [~ ~]
    :^  ~  ~  %atom
    !>  ^-  ship
    (~(sein of [now eny] lex) u.who)
  ::
      %saxo
    ?.  ?=([@ ~] tyl)  [~ ~]
    ?.  =([%& our] why)
      [~ ~]
    =/  who  (slaw %p i.tyl)
    ?~  who  [~ ~]
    :^  ~  ~  %noun
    !>  ^-  (list ship)
    (~(saxo of [now eny] lex) u.who)
  ::
      %sponsors
    ?.  ?=([@ ~] tyl)  [~ ~]
    ?.  =([%& our] why)
      [~ ~]
    =/  who  (slaw %p i.tyl)
    ?~  who  [~ ~]
    :^  ~  ~  %noun
    !>  ^-  (list [=ship =point])
    %+  turn  (~(saxo of [now eny] lex) u.who)
    |=  =ship
    [ship (~(got by pos.zim.pki.lex) ship)]
  ::
      %lamp
    ?.  ?=(~ tyl)  [~ ~]
    ?.  =([%& our] why)
      [~ ~]
    :^  ~  ~  %noun
    !>  ^-  (map ship point)
    %-  ~(gas by *(map ship point))
    %+  murn
        ^-  (list @p)
        %+  weld  `(list @p)`(gulf 0 255)
        `(list @p)`~(tap in ~(key by fes.zim.pki.lex))
    |=  =ship
    ^-  (unit [@p point])
    ?~  p=(~(get by pos.zim.pki.lex) ship)  ~
    `[ship u.p]
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
  ::
      %puby                                            ::  unitized public-key
    ?.  ?=([@ @ ~] tyl)  [~ ~]
    ?.  =([%& our] why)
      [~ ~]
    =/  who  (slaw %p i.tyl)
    =/  lif  (slaw %ud i.t.tyl)
    ?~  who  [~ ~]
    ?~  lif  [~ ~]
    =/  pos  (~(get by pos.zim.pki.lex) u.who)
    ?~  pos  ``[%noun !>(~)]
    ``[%noun !>((~(get by keys.u.pos) u.lif))]
  ::
      %fief
    ?.  ?=(~ tyl)  [~ ~]
    :^  ~  ~  %noun  !>
    fes.zim.pki.lex
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
          tea=wire
          hen=duct
          dud=(unit goof)
          hin=sign
      ==
  ^-  [(list move) _..^$]
  ?^  dud
    ~|(%jael-take-dud (mean tang.u.dud))
  ::
  =^  did  lex  abet:(~(take of [now eny] lex) tea hen hin)
  [did ..^$]
--
