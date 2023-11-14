::  An aquarium of virtual ships.  Put in some fish and watch them!
::
::  usage:
::  |start %aqua
::  /-  aquarium
::  :aqua &pill .^(pill:aquarium %cx %/urbit/pill)
::    OR
::  :aqua &pill +solid
::
::  XX: update these examples
::  Then try stuff:
::  :aqua [%init ~[~bud ~dev]]
::  :aqua [%dojo ~[~bud ~dev] "[our eny (add 3 5)]"]
::  :aqua [%dojo ~[~bud] "|hi ~dev"]
::  :aqua [%wish ~[~bud ~dev] '(add 2 3)']
::  :aqua [%peek ~[~bud] /cx/~bud/base/(scot %da now)/app/curl/hoon]
::  :aqua [%dojo ~[~bud ~dev] '|mount %']
::  :aqua [%file ~[~bud ~dev] %/sys/vane]
::  :aqua [%pause-events ~[~bud ~dev]]
::
::  We get ++unix-event and ++pill from /-aquarium
::
/-  aquarium
/+  pill, azimuth, naive, default-agent, aqua-azimuth, dbug, verb
=,  pill-lib=pill
=,  aquarium
=>  $~  |%
    +$  versioned-state
      $%  state-0
      ==
    +$  state-0
      $:  %0
          pil=$>(%pill pill)
          assembled=*
          tym=@da
          fresh-piers=(map [=ship fake=?] [=pier boths=(list unix-both)])
          fleet-snaps=(map term fleet)
          piers=fleet
      ==
    ::  XX temporarily shadowed, fix and remove
    ::
    +$  pill  pill:pill-lib
    ::
    +$  fleet  [ships=(map ship pier) azi=az-state]
    +$  pier
      $:  snap=*
          event-log=(list unix-timed-event)
          next-events=(qeu unix-event)
          processing-events=?
          namespace=(map path (list yowl:ames))
      ==
    --
::
=|  state-0
=*  state  -
=<
  %-  agent:dbug
  %+  verb  |
  ^-  agent:gall
  |_  =bowl:gall
  +*  this       .
      aqua-core  +>
      ac         ~(. aqua-core bowl)
      def        ~(. (default-agent this %|) bowl)
  ++  on-init           `this
  ++  on-save  !>(state)
  ++  on-load
    |=  old-vase=vase
    ^-  step:agent:gall
    ~&  prep=%aqua
    =+  !<(old=versioned-state old-vase)
    =|  cards=(list card:agent:gall)
    |-
    ?-  -.old
    ::  wipe fleets and piers rather than give them falsely nulled azimuth state
    ::
        %0
      [cards this(state old)]
    ==
  ::
  ++  on-poke
    |=  [=mark =vase]
    ?>  (team:title [our src]:bowl)
    ^-  step:agent:gall
    =^  cards  state
      ?+  mark  ~|([%aqua-bad-mark mark] !!)
          %aqua-events     (poke-aqua-events:ac !<((list aqua-event) vase))
          %pill       (poke-pill:ac !<(pill vase))
          %noun            (poke-noun:ac !<(* vase))
          %azimuth-action  (poke-azimuth-action:ac !<(azimuth-action vase))
      ==
    [cards this]
  ::
  ++  on-watch
    |=  =path
    ^-  step:agent:gall
    ?:  ?=([?(%effects %effect) ~] path)
      `this
    ?:  ?=([%effect @ ~] path)
      `this
    ?.  ?=([?(%effects %effect %evens %boths) @ ~] path)
      ~|  [%aqua-bad-subscribe-path path]
      !!
    ?~  (slaw %p i.t.path)
      ~|  [%aqua-bad-subscribe-path-ship path]
      !!
    `this
  ::
  ++  on-leave  on-leave:def
  ++  on-peek   peek:ac
  ::
  ++  on-agent  on-agent:def
  ::
  ++  on-arvo
    |=  [=wire sign=sign-arvo]
    ^-  step:agent:gall
    ?+  wire  (on-arvo:def wire sign)
        [%wait @ ~]
      ?>  ?=(%wake +<.sign)
      =/  wen=@da  (slav %da i.t.wire)
      =^  cards  state
        (handle-wake:ac wen)
      [cards this]
    ==
  ++  on-fail   on-fail:def
  --
::
::  unix-{effects,events,boths}: collect jar of effects and events to
::    brodcast all at once to avoid gall backpressure
::  moves: Hoist moves into state for cleaner state management
::
=|  unix-effects=(jar ship unix-effect)
=|  unix-events=(jar ship unix-timed-event)
=|  unix-boths=(jar ship unix-both)
=|  cards=(list card:agent:gall)
|_  hid=bowl:gall
::
::  Represents a single ship's state.
::
++  pe
  ::NOTE  if we start needing the fake flag outside of +ahoy and +yaho,
  ::      probably add it as an argument here.
  |=  who=ship
  =+  (~(gut by ships.piers) who *pier)
  =*  pier-data  -
  |%
  ::
  ::  Done; install data
  ::
  ++  abet-pe
    ^+  this
    =.  ships.piers  (~(put by ships.piers) who pier-data)
    this
  ::
  ::  Initialize new ship
  ::
  ++  apex
    =.  pier-data  *pier
    =.  snap  assembled
    ~&  pill-size=(met 3 (jam snap))
    ..abet-pe
  ::
  ::  store post-pill ship for later re-use
  ::
  ++  ahoy
    |=  fake=?
    =?  fresh-piers  !(~(has by fresh-piers) [who fake])
      %+  ~(put by fresh-piers)  [who fake]
      [pier-data (~(get ja unix-boths) who)]
    ..ahoy
  ::
  ::  restore post-pill ship for re-use
  ::
  ++  yaho
    |=  fake=?
    =/  fresh  (~(got by fresh-piers) [who fake])
    =.  pier-data  pier.fresh
    =.  boths.fresh  (flop boths.fresh)
    |-
    ?~  boths.fresh  ..yaho
    =.  ..yaho
      ?-  -.i.boths.fresh
        %effect  (publish-effect +.i.boths.fresh)
        %event   (publish-event +.i.boths.fresh)
      ==
    $(boths.fresh t.boths.fresh)
  ::
  ::  Enqueue events to child arvo
  ::
  ++  push-events
    |=  ues=(list unix-event)
    ^+  ..abet-pe
    =.  next-events  (~(gas to next-events) ues)
    ..abet-pe
  ::
  ::  Send cards to host arvo
  ::
  ++  emit-cards
    |=  ms=(list card:agent:gall)
    =.  this  (^emit-cards ms)
    ..abet-pe
  ::
  ::  Process the events in our queue.
  ::
  ++  plow
    |-  ^+  ..abet-pe
    ?:  =(~ next-events)
      ..abet-pe
    ?.  processing-events
      ..abet-pe
    =^  ue  next-events  ~(get to next-events)
    =/  poke-arm  (mox +23.snap)
    ?>  ?=(%0 -.poke-arm)
    =/  poke  p.poke-arm
    =.  tym  (max +(tym) now.hid)
    =/  poke-result  (mule |.((slum poke tym ue)))
    ?:  ?=(%| -.poke-result)
      %-  (slog >%aqua-crash< >guest=who< p.poke-result)
      $
    =.  snap  +.p.poke-result
    =.  ..abet-pe  (publish-event tym ue)
    =.  ..abet-pe
      ~|  ova=-.p.poke-result
      (handle-effects ;;((list ovum) -.p.poke-result))
    $
  ::
  ::  Peek
  ::
  ++  peek-once
    |=  [=view =desk =spur]
    =/  res  (mox +22.snap)
    ?>  ?=(%0 -.res)
    =/  peek  p.res
    =/  pek  (slum peek [[~ ~] %| %once view desk spur])
    =+  ;;(res=(unit (cask [path (cask)])) pek)
    ::NOTE  it's an %omen, so we unpack a little bit deeper
    (bind res (cork tail (cork tail tail)))
  ::
  ++  peek
    |=  p=*
    =/  res  (mox +22.snap)
    ?>  ?=(%0 -.res)
    =/  peek  p.res
    =/  pax  (path p)
    ?>  ?=([@ @ @ @ *] pax)
    =.  i.t.t.t.pax  (scot %da tym)
    =/  pek  (slum peek [[~ ~] & pax])
    =+  ;;(res=(unit (cask)) pek)
    (bind res tail)
      ::
  ::  Wish
  ::
  ++  wish
    |=  txt=@t
    =/  res  (mox +10.snap)
    ?>  ?=(%0 -.res)
    =/  wish  p.res
    ~&  [who=who %wished (slum wish txt)]
    ..abet-pe
  ::
  ++  mox  |=(* (mock [snap +<] scry))
  ::
  ::  Start/stop processing events.  When stopped, events are added to
  ::  our queue but not processed.
  ::
  ++  start-processing-events  .(processing-events &)
  ++  stop-processing-events  .(processing-events |)
  ::
  ::  Handle all the effects produced by a single event.
  ::
  ++  handle-effects
    |=  effects=(list ovum)
    ^+  ..abet-pe
    ?~  effects
      ..abet-pe
    =.  ..abet-pe
      =/  sof  ((soft unix-effect) i.effects)
      ?~  sof
        ~?  aqua-debug=&  [who=who %unknown-effect i.effects]
        ..abet-pe
      (publish-effect u.sof)
    $(effects t.effects)
  ::
  ::  Give effect to our subscribers
  ::
  ++  publish-effect
    |=  uf=unix-effect
    ^+  ..abet-pe
    =.  unix-effects  (~(add ja unix-effects) who uf)
    =.  unix-boths  (~(add ja unix-boths) who [%effect uf])
    ..abet-pe
  ::
  ::  Give event to our subscribers
  ::
  ++  publish-event
    |=  ute=unix-timed-event
    ^+  ..abet-pe
    =.  event-log  [ute event-log]
    =.  unix-events  (~(add ja unix-events) who ute)
    =.  unix-boths  (~(add ja unix-boths) who [%event ute])
    ..abet-pe
  --
::
++  this  .
::
::  ++apex-aqua and ++abet-aqua must bookend calls from gall
::
++  apex-aqua
  ^+  this
  =:  cards         ~
      unix-effects  ~
      unix-events   ~
      unix-boths    ~
    ==
  this
::
++  abet-aqua
  ^-  (quip card:agent:gall _state)
  ::
  ::  interecept %request effects to handle azimuth subscription
  ::
  =.  this
    %-  emit-cards
    %-  zing
    %+  turn  ~(tap by unix-effects)
    |=  [=ship ufs=(list unix-effect)]
    %+  murn  ufs
    |=  uf=unix-effect
    (router:aqua-azimuth our.hid ship uf azi.piers)
  ::
  =.  this
    =/  =path  /effect
    %-  emit-cards
    %-  zing
    %+  turn  ~(tap by unix-effects)
    |=  [=ship ufs=(list unix-effect)]
    %-  zing
    %+  turn  ufs
    |=  uf=unix-effect
    :~  [%give %fact ~[/effect] %aqua-effect !>(`aqua-effect`[ship uf])]
        [%give %fact ~[/effect/[-.q.uf]] %aqua-effect !>(`aqua-effect`[ship uf])]
    ==
  ::
  =.  this
    =/  =path  /effects
    %-  emit-cards
    %+  turn  ~(tap by unix-effects)
    |=  [=ship ufs=(list unix-effect)]
    [%give %fact ~[path] %aqua-effects !>(`aqua-effects`[ship (flop ufs)])]
  ::
  =.  this
    %-  emit-cards
    %-  zing
    %+  turn  ~(tap by unix-effects)
    |=  [=ship ufs=(list unix-effect)]
    =/  =path  /effect/(scot %p ship)
    %+  turn  ufs
    |=  uf=unix-effect
    [%give %fact ~[path] %aqua-effect !>(`aqua-effect`[ship uf])]
  ::
  =.  this
    %-  emit-cards
    %+  turn  ~(tap by unix-effects)
    |=  [=ship ufs=(list unix-effect)]
    =/  =path  /effects/(scot %p ship)
    [%give %fact ~[path] %aqua-effects !>(`aqua-effects`[ship (flop ufs)])]
  ::
  =.  this
    %-  emit-cards
    %+  turn  ~(tap by unix-events)
    |=  [=ship ve=(list unix-timed-event)]
    =/  =path  /events/(scot %p ship)
    [%give %fact ~[path] %aqua-events !>(`aqua-events`[ship (flop ve)])]
  ::
  =.  this
    %-  emit-cards
    %+  turn  ~(tap by unix-boths)
    |=  [=ship bo=(list unix-both)]
    =/  =path  /boths/(scot %p ship)
    [%give %fact ~[path] %aqua-boths !>(`aqua-boths`[ship (flop bo)])]
  ::
  [(flop cards) state]
::
++  emit-cards
  |=  ms=(list card:agent:gall)
  =.  cards  (weld ms cards)
  this
::
::  Run all events on all ships until all queues are empty
::
++  plow-all
  |-  ^+  this
  =/  who
    =/  pers  ~(tap by ships.piers)
    |-  ^-  (unit ship)
    ?~  pers
      ~
    ?:  &(?=(^ next-events.q.i.pers) processing-events.q.i.pers)
      `p.i.pers
    $(pers t.pers)
  ~?  aqua-debug=|  plowing=who
  ?~  who
    this
  =.  this  abet-pe:plow:(pe u.who)
  $
::
::  Load a pill and assemble arvo.  Doesn't send any of the initial
::  events.
::
++  poke-pill
  |=  p=pill
  ^-  (quip card:agent:gall _state)
  ?<  ?=(%ivory -.p)
  =.  userspace-ova.p
    ::  if there is an azimuth-snapshot in the pill, we stub it out,
    ::  since it would interfere with aqua's azimuth simulation.
    ::
    ^+  userspace-ova.p
    %+  turn  userspace-ova.p
    |=  e=unix-event:pill-lib
    ^+  e
    ?.  ?=(%park -.q.e)   e
    ?.  ?=(%& -.yok.q.e)  e
    =-  e(q.p.yok.q -)
    ^-  (map path (each page lobe:clay))
    %-  ~(urn by q.p.yok.q.e)
    |=  [=path fil=(each page lobe:clay)]
    ^+  fil
    ?.  =(/app/azimuth/version-0/azimuth-snapshot path)  fil
    ?:  ?=(%| -.fil)  fil
    &+azimuth-snapshot+[%0 [0x0 0] *^state:naive ~ ~]
  =.  this  apex-aqua  =<  abet-aqua
  =.  pil  p
  ~&  lent=(met 3 (jam boot-ova.pil))
  =/  res=toon :: (each * (list tank))
    (mock [boot-ova.pil [2 [0 3] [0 2]]] scry)
  =.  fleet-snaps  ~
  ?-  -.res
      %0
    ~&  %suc
    =.  assembled  +7.p.res
    =.  fresh-piers  ~
    this
  ::
      %1
    ~&  [%vere-blocked p.res]
    this
  ::
      %2
    ~&  %vere-fail
    %-  (slog p.res)
    this
  ==
::
::  Handle commands from CLI
::
::    Should put some thought into arg structure, maybe make a mark.
::
::    Should convert some of these to just rewrite into ++poke-events.
::
++  poke-noun
  |=  val=*
  ^-  (quip card:agent:gall _state)
  =.  this  apex-aqua  =<  abet-aqua
  ^+  this
  ::  Could potentially factor out the three lines of turn-ships
  ::  boilerplate
  ::
  ?+  val  ~|(%bad-noun-arg !!)
      [%swap-vanes vs=*]
    ?>  ?=(^ boot-ova.pil)
    ?>  ?=([%7 * %1 installed=*] i.boot-ova.pil)
    =.  installed.i.boot-ova.pil
      %+  roll  (,(list term) vs.val)
      |=  [v=term =_installed.i.boot-ova.pil]
      %^  slum  installed  now.hid
      =/  vane
        ?+  v  ~|([%unknown-vane v] !!)
          %a  %ames
          %b  %behn
          %c  %clay
          %d  %dill
          %e  %eyre
          %g  %gall
          %j  %jael
          %g  %gall
        ==
      =/  pax
        /(scot %p our.hid)/work/(scot %da now.hid)/sys/vane/[vane]
      =/  txt  .^(@ %cx (weld pax /hoon))
      [/vane/[vane] [%veer v pax txt]]
    =>  .(this ^+(this this))
    =^  ms  state  (poke-pill pil)
    (emit-cards ms)
  ::
      [%swap-files @tas]
    =/  =desk  +.val
    =.  userspace-ova.pil
      ::  take all files from a userspace desk
      :_  ~
      %-  unix-event:pill-lib
      %-  file-ovum:pill-lib
      [desk /(scot %p our.hid)/[desk]/(scot %da now.hid) ~]
    =^  ms  state  (poke-pill pil)
    (emit-cards ms)
  ::
      [%wish hers=* p=@t]
    %+  turn-ships  ((list ship) hers.val)
    |=  [who=ship thus=_this]
    =.  this  thus
    (wish:(pe who) p.val)
  ::
      [%unpause-events hers=*]
    =.  this  start-azimuth-timer
    %+  turn-ships  ((list ship) hers.val)
    |=  [who=ship thus=_this]
    =.  this  thus
    start-processing-events:(pe who)
  ::
      [%pause-events hers=*]
    =.  this  stop-azimuth-timer
    %+  turn-ships  ((list ship) hers.val)
    |=  [who=ship thus=_this]
    =.  this  thus
    stop-processing-events:(pe who)
  ::
      [%clear-snap lab=@tas]
    =.  fleet-snaps  ~  ::  (~(del by fleet-snaps) lab.val)
    this
  ==
::
::  Make changes to azimuth state for the current fleet
::
++  poke-azimuth-action
  |=  act=azimuth-action
  ^-  (quip card:agent:gall _state)
  =.  this  apex-aqua  =<  abet-aqua
  ^+  this
  ?-  -.act
  ::
      %init-azimuth
    =.  azi.piers  *az-state
    start-azimuth-timer
  ::
      %spawn
    =.  state  (spawn who.act)
    this
  ::
      %breach
    ::  should we remove the pier from state here?
    =.  state  (breach who.act)
    this
  ::
  ==
::
::  Apply a list of events tagged by ship
::
++  poke-aqua-events
  |=  events=(list aqua-event)
  ^-  (quip card:agent:gall _state)
  =.  this  apex-aqua  =<  abet-aqua
  %+  turn-events  events
  |=  [ae=aqua-event thus=_this]
  =.  this  thus
  ?-  -.ae
  ::
      %init-ship
    ?:  &(fake.ae (~(has by fresh-piers) [who fake]:ae))
      ~&  [%aqua %cached-init +.ae]
      =.  this  abet-pe:(yaho fake):[ae (pe who.ae)]
      ?:  fake.ae  (pe who.ae)
      ::  for real ships, make sure they have their latest keys
      ::
      %.  who.ae
      =<  pe:abet-pe:plow
      %-  push-events:(pe who.ae)
      =/  =life  lyfe:(~(got by lives.azi.piers) who.ae)
      =/  =ring  sec:ex:(get-keys:aqua-azimuth who.ae life)
      [/j/aqua/rekey %rekey life ring]~
    =.  this  abet-pe:(publish-effect:(pe who.ae) [/ %sleep ~])
    =/  initted
      =<  plow
      %-  push-events:apex:(pe who.ae)
      ^-  (list unix-event)
      %-  zing
      :~
        :~  [/ %wack 0]  ::  eny
            :: [/ %verb `|]  :: possible verb
            :^  /  %wyrd  [~.nonce /aqua] :: dummy runtime version + nonce
            ^-  (list (pair term @))
            :~  zuse+zuse
                lull+lull
                arvo+arvo
                hoon+hoon-version
                nock+4
            ==
            [/ %whom who.ae]  ::  who
        ==
        ::
        kernel-ova.pil  :: load compiler
        ::
        :_  ~
        :^  /d/term/1  %boot  &
        ?:  fake.ae
          [%fake who.ae]
        [%dawn (dawn who.ae)]
        ::
        userspace-ova.pil  :: load os
        ::
        :*  [/b/behn/0v1n.2m9vh %born ~]
            [/i/http-client/0v1n.2m9vh %born ~]
            [/e/http-server/0v1n.2m9vh %born ~]
            [/e/http-server/0v1n.2m9vh %live 8.080 `8.445]
            [/a/newt/0v1n.2m9vh %born ~]
            [/d/term/1 %hail ~]
          ::
            ?:  fake.ae  ~
            =+  [%raw-poke %noun %refresh-rate ~s30]
            [/g/aqua/reduce-refresh-rate %deal [. . /]:who.ae %azimuth -]~
        ==
      ==
    =.  this
      abet-pe:(ahoy fake):[ae initted]
    (pe who.ae)
  ::
      %pause-events
    stop-processing-events:(pe who.ae)
  ::
      %snap-ships
    =.  this
      %+  turn-ships  (turn ~(tap by ships.piers) head)
      |=  [who=ship thus=_this]
      =.  this  thus
      (publish-effect:(pe who) [/ %kill ~])
    =.  fleet-snaps
      %+  ~(put by fleet-snaps)  lab.ae
      :_  azi.piers
      %-  malt
      %+  murn  hers.ae
      |=  her=ship
      ^-  (unit (pair ship pier))
      =+  per=(~(get by ships.piers) her)
      ?~  per
        ~
      `[her u.per]
    =.  this   stop-azimuth-timer
    =.  piers  *fleet
    (pe -.hers.ae)
  ::
      %restore-snap
    =.  this
      %+  turn-ships  (turn ~(tap by ships.piers) head)
      |=  [who=ship thus=_this]
      =.  this  thus
      (publish-effect:(pe who) [/ %kill ~])
    =.  piers  (~(got by fleet-snaps) lab.ae)
    =.  this   start-azimuth-timer
    =.  this
      %+  turn-ships  (turn ~(tap by ships.piers) head)
      |=  [who=ship thus=_this]
      =.  this  thus
      (publish-effect:(pe who) [/ %restore ~])
    (pe ~bud)  ::  XX why ~bud?  need an example
  ::
      %read
    ?~  pier=(~(get by ships.piers) from.ae)
      (pe from.ae)
    =/  cash  (~(get by namespace.u.pier) path.ae)
    |-
    ?^  cash
      ?:  (gth num.ae (lent u.cash))
        (pe from.ae)
      ::TODO  depends on /ted/aqua/ames behavior in a weird indirect way
      =/  for=@p  `@`(tail for.ae)  ::NOTE  moons & comets not supported
      =;  task=task-arvo
        ^$(ae [%event for /a/aqua/fine-response task], thus this)
      :+  %hear  `lane:ames`[%| `@`from.ae]
      ^-  blob:ames
      =/  =shot:ames
        ::NOTE  dec is important! so dumb!!
        (sift-shot:ames `@`(snag (dec num.ae) u.cash))
      ::TODO  runtime needs to update rcvr field also
      ::NOTE  rcvr life is allowed to be wrong
      (etch-shot:ames shot(sndr from.ae, rcvr for))
    =/  pacs=(unit (list yowl:ames))
      %+  biff
        (peek-once:(pe from.ae) %ax %$ [%fine %message path.ae])
      (soft (list yowl:ames))
    ?~  pacs  (pe from.ae)
    =.  namespace.u.pier
      (~(put by namespace.u.pier) path.ae u.pacs)
    =.  ships.piers
      (~(put by ships.piers) from.ae u.pier)
    $(cash pacs, thus this)
  ::
      %event
    ~?  &(aqua-debug=| !?=(?(%belt %hear) -.q.ue.ae))
      raw-event=[who.ae -.q.ue.ae]
    ~?  &(debug=| ?=(%receive -.q.ue.ae))
      raw-event=[who.ae ue.ae]
    (push-events:(pe who.ae) [ue.ae]~)
  ==
::
::  Run a callback function against a list of ships, aggregating state
::  and plowing all ships at the end.
::
::    I think we should use patterns like this more often.  Because we
::    don't, here's some points to be aware.
::
::    `fun` must take `this` as a parameter, since it needs to be
::    downstream of previous state changes.  You could use `state` as
::    the state variable, but it muddles the code and it's not clear
::    whether it's better.  You could use the `_(pe)` core if you're
::    sure you'll never need to refer to anything outside of your pier,
::    but I don't think we can guarantee that.
::
::    The callback function must start with `=.  this  thus`, or else
::    you don't get the new state.  Would be great if you could hot-swap
::    that context in here, but we don't know where to put it unless we
::    restrict the callbacks to always have `this` at a particular axis,
::    and that doesn't feel right
::
++  turn-plow
  |*  arg=mold
  |=  [hers=(list arg) fun=$-([arg _this] _(pe))]
  |-  ^+  this
  ?~  hers
    plow-all
  =.  this
    abet-pe:plow:(fun i.hers this)
  $(hers t.hers, this this)
::
++  turn-ships   (turn-plow ship)
++  turn-events  (turn-plow aqua-event)
::
::  Check whether we have a snapshot
::
++  peek
  |=  =path
  ^-  (unit (unit cage))
  ?+  path  ~
      [%x %fleet-snap @ ~]  ``noun+!>((~(has by fleet-snaps) i.t.t.path))
      [%x %fleets ~]        ``noun+!>((turn ~(tap by fleet-snaps) head))
      [%x %ships ~]         ``noun+!>((turn ~(tap by ships.piers) head))
      [%x %pill ~]          ``pill+!>(pil)
      [%x %i @ @ @ @ @ *]
    =/  who  (slav %p i.t.t.path)
    =/  pier  (~(get by ships.piers) who)
    ?~  pier
      ~
    :^  ~  ~  %noun  !>
    (peek:(pe who) t.t.t.path)
      [%x %log-info ~]
    ``noun+!>([lives.azi.piers (lent logs.azi.piers) tym.azi.piers])
  ==
::
::  Trivial scry for mock
::
++  scry  |=([* *] ~)
::
++  handle-wake
  |=  wen=@da
  ^-  (quip card:agent:gall _state)
  =.  this  apex-aqua  =<  abet-aqua
  ?.  =(wen tym.azi.piers)
    this
  =.  state  (spam-logs 10)
  start-azimuth-timer
::
++  start-azimuth-timer
  ^+  this
  =?  this  !=(tym.azi.piers *@da)
    stop-azimuth-timer
  =/  until=@da  (add now.hid ~s40)
  =.  tym.azi.piers  until
  %-  emit-cards
  [%pass /wait/(scot %da until) %arvo %b %wait until]~
::
++  stop-azimuth-timer
  ^+  this
  =*  tym  tym.azi.piers
  ?:  =(tym *@da)
    this
  %-  emit-cards
  [%pass /wait/(scot %da tym) %arvo %b %rest tym]~
::
++  spam-logs
  |=  n=@
  ^-  _state
  =*  loop  $
  ?:  =(n 0)
    state
  =/  new-state=_state
    ?.  (~(has by lives.azi.piers) ~fes)
      (spawn ~fes)
    (cycle-keys ~fes)
  =.  state  new-state
  loop(n (dec n))
::
++  spawn
  |=  who=@p
  ^-  _state
  ?<  (~(has by lives.azi.piers) who)
  =.  lives.azi.piers  (~(put by lives.azi.piers) who [1 0])
  =.  logs.azi.piers
    %+  weld  logs.azi.piers
    :_  ~
    %-  changed-keys:lo:aqua-azimuth
    :*  who
        (get-public:aqua-azimuth who 1 %crypt)
        (get-public:aqua-azimuth who 1 %auth)
        1
        1
    ==
  (spam-logs 10)
::
++  cycle-keys
  |=  who=@p
  ^-  _state
  =/  prev
    ~|  no-such-ship+who
    (~(got by lives.azi.piers) who)
  =/  lyfe  +(lyfe.prev)
  =.  lives.azi.piers  (~(put by lives.azi.piers) who [lyfe rut.prev])
  =.  logs.azi.piers
    %+  weld  logs.azi.piers
    :_  ~
    %-  changed-keys:lo:aqua-azimuth
    :*  who
        (get-public:aqua-azimuth who lyfe %crypt)
        (get-public:aqua-azimuth who lyfe %auth)
        1
        lyfe
    ==
  state
::
++  breach
  |=  who=@p
  ^-  _state
  =.  state  (cycle-keys who)
  =/  prev   (~(got by lives.azi.piers) who)
  =/  rut  +(rut.prev)
  =.  lives.azi.piers  (~(put by lives.azi.piers) who [lyfe.prev rut])
  =.  logs.azi.piers
    %+  weld  logs.azi.piers
    [(broke-continuity:lo:aqua-azimuth who rut) ~]
  (spam-logs 10)
::
++  dawn
  |=  who=ship
  ^-  dawn-event:jael
  =/  clan  (clan:title who)
  ?>  ?=(?(%czar %king %duke %earl) clan)
  =/  spon=(list [ship point:azimuth])
    %-  flop
    |-  ^-  (list [ship point:azimuth])
    =/  =ship  (^sein:title who)
    =/  a-point=[^ship point:azimuth]
      =/  spon-spon  [& (^sein:title ship)]
      =/  life-rift  ~|([ship lives.azi.piers] (~(got by lives.azi.piers) ship))
      =/  =life  lyfe.life-rift
      =/  =rift  rut.life-rift
      =/  =pass
        %^    pass-from-eth:azimuth
            (as-octs:mimes:html (get-public:aqua-azimuth ship life %crypt))
          (as-octs:mimes:html (get-public:aqua-azimuth ship life %auth))
        1
      :^    ship
          *[address address address address]:azimuth
        `[life=life pass rift spon-spon ~]
      ~
    ?:  ?=(%czar (clan:title ship))
      [a-point]~
    [a-point $(who ship)]
  =/  =seed:jael
    =/  life-rift=[lyfe=life rut=rift]
      ?:  =(%earl clan)  [1 0]
      (~(got by lives.azi.piers) who)
    =/  =life  lyfe.life-rift
    [who life sec:ex:(get-keys:aqua-azimuth who life) ~]
  :*  seed
      spon
      get-czars
      ~[~['arvo' 'netw' 'ork']]
      0
      `(need (de-purl:html 'http://fake.aqua.domain/'))
  ==
::
::  Should only do galaxies
::
++  get-czars
  ^-  (map ship [rift life pass])
  %-  malt
  %+  murn
    ~(tap by lives.azi.piers)
  |=  [who=ship lyfe=life rut=rift]
  ?.  =(%czar (clan:title who))
    ~
  %-  some
  :^  who  rut  lyfe
  %^    pass-from-eth:azimuth
      (as-octs:mimes:html (get-public:aqua-azimuth who lyfe %crypt))
    (as-octs:mimes:html (get-public:aqua-azimuth who lyfe %auth))
  1
--
