::                                                      ::  ::
::::  /hoon/talk/app                                    ::  ::
  ::                                                    ::  ::
::
::TODO  rename to hall
::TODO  master changes
::TODO  char57 comments as line comments when regarding code.
::TODO  avoid lark where possible
::TODO  document what user-facing messages actually mean!
::TODO  maybe have brokers accept reactions as well, redirect them to readers.
::      that way we can have foreign brokers react to our requests!
::TODO  ::> to :> etc.
::
::TODO  we can't do away with the default mailbox because we need it for things
::      like invite notifications etc. can we do better than request that apps
::      don't use it frivolously?
::
::TODO  crash on pokes/peers we do not expect
::
/?    151                                               ::<  hoon version
/-    talk                                              ::<  structures
/+    talk, time-to-id                                  ::<  libraries
/=    seed  /~  !>(.)
!:
::::
  ::
[. talk]
=>  ::>  ||
    ::>  ||  %arch
    ::>  ||
    ::>    data structures
    ::
    |%
    ++  state                                           ::>  broker state
      $:  stories/(map knot story)                      ::<  conversations
          outbox/(pair @ud (map @ud thought))           ::<  urbit outbox
          log/(map knot @ud)                            ::<  logged to clay
          nicks/(map ship knot)                         ::<  nicknames
          nik/(map (set partner) char)                  ::<  bound circle glyphs
          nak/(jug char (set partner))                  ::<  circle glyph lookup
      ==                                                ::
    ++  story                                           ::>  wire content
      $:  count/@ud                                     ::<  (lent grams)
          grams/(list telegram)                         ::<  all messages
          locals/group                                  ::<  local presence
          remotes/(map partner group)                   ::<  remote presence
          shape/config                                  ::<  configuration
          mirrors/(map circle config)                   ::<  remote config
          ::TODO  never gets updated.                   ::
          sequence/(map partner @ud)                    ::<  partners heard
          known/(map serial @ud)                        ::<  messages heard
      ==                                                ::
    ++  river  (pair point point)                       ::<  stream definition
    ++  point                                           ::>  stream endpoint
      $%  {$ud p/@ud}                                   ::<  by number
          {$da p/@da}                                   ::<  by date
      ==                                                ::
    ++  move  (pair bone card)                          ::<  all actions
    ++  lime                                            ::>  diff fruit
      $%  {$talk-prize prize}                           ::
          {$talk-rumor rumor}                           ::
          {$talk-reaction reaction}                     ::
      ==                                                ::
    ++  pear                                            ::>  poke fruit
      $%  {$talk-command command}                       ::
          {$write-comment spur ship cord}               ::
          {$write-fora-post spur ship cord cord}        ::
      ==                                                ::
    ++  card                                            ::>  general card
      $%  {$diff lime}                                  ::
          {$info wire ship term nori}                   ::
          {$peer wire dock path}                        ::
          {$poke wire dock pear}                        ::
          {$pull wire dock $~}                          ::
          {$quit $~}                                    ::
      ==                                                ::
    ++  weir                                            ::>  parsed wire
      $%  {$repeat num/@ud hos/ship nom/knot}           ::<  messaging wire
          {$friend nom/knot cir/circle}                 ::<  subscription wire
      ==                                                ::
    --
::
::>  ||
::>  ||  %work
::>  ||
::>    functional cores and arms.
::
|_  {bol/bowl state}
::
++  prep                                                ::<  prepare state
  ::>  adapts state.
  ::
  |=  old/(unit state)
  ^-  (quip move ..prep)
  ?~  old
    %-  f-bake  :-  %more
    ta-done:ta-init:ta
  [~ ..prep(+<+ u.old)]
::
::>  ||
::>  ||  %utility
::>  ||
::>    small utility functions.
::+|
::
++  strap  |*({k/* v/*} (~(put by *(map _k _v)) k v))   ::<  map key-value pair
::
::>  ||
::>  ||  %engines
::>  ||
::>    main cores.
::+|
::
++  ta                                                  ::<  per transaction
  ::>  thinker core, used for processing pokes into
  ::>  deltas.
  ::
  ::TODO  maybe rewrite all arms to produce (list delta)
  ::      instead of state? or nah?
  |_  ::>  moves: moves created by core operations.
      ::
      deltas/(list delta)
  ::
  ++  ta-done                                           ::<  resolve core
    ::>  produces the moves stored in ++ta's moves.
    ::>  they are produced in reverse order because
    ::>  ++ta-emil and ++ta-emit add them to the head of
    ::>  the {moves}.
    ::>  we don't produce any new state, because ++ta
    ::>  doesn't make any changes to it itself.
    ::
    ^-  (list delta)
    (flop deltas)
  ::
  ::>  ||
  ::>  ||  %emitters
  ::>  ||
  ::>    arms that create outward changes.
  ::+|
  ::
  ++  ta-delta                                          ::<  emit a delta
    ::>  adds a delta to the head of {deltas}.
    ::
    |=  dif/delta
    %_(+> deltas [dif deltas])
  ::
  ++  ta-deltas                                         ::<  emit delta list
    ::>  adds multiple deltas to the haad of {deltas}.
    ::>  flops to stay consistent with ++ta-delta.
    ::
    |=  dis/(list delta)
    %_(+> deltas (welp (flop dis) deltas))
  ::
  ++  ta-note                                           ::<  tell user
    ::>  sends {msg} as an %app message to the user's
    ::>  inbox.
    ::
    |=  msg/cord
    %^  ta-action  0  %phrase
    :-  [[%& our.bol (main our.bol)] ~ ~]
    [%app %talk-guardian msg]~
  ::
  ++  ta-evil                                           ::<  emit error
    ::>  tracing printf and crash.
    ::
    |=  msg/cord
    ~|  [%talk-ta-evil msg]
    !!
  ::
  ::>  ||
  ::>  ||  %data
  ::>  ||
  ::>    utility functions for data retrieval.
  ::+|
  ::
  ::TODO  functions for getting readers or followers of a specific story from
  ::      the subs in sup.bol.
  ::
  ++  ta-know                                           ::<  story monad
    ::>  produces a gill that takes a gate.
    ::>  if the story {nom} exists, calls the gate with
    ::>  a story core. if it doesn't, does nothing.
    ::
    |=  nom/knot
    |=  fun/$-(_so _ta)
    ^+  +>+>
    =+  pur=(~(get by stories) nom)
    ?~  pur
      ::TODO  crash instead?
      %-  ta-note
      (crip "unknown story '{(trip nom)}'")
    (fun ~(. so nom ~ u.pur))
  ::
  ::>  ||
  ::>  ||  %interaction-events
  ::>  ||
  ::>    arms that apply events we received.
  ::+|
  ::
  ++  ta-init                                           ::<  initialize app
    ::>  populate state on first boot.
    ::>  creates our default mailbox and journal.
    ::
    %+  roll
      ^-  (list {security knot cord})
      :~  [%brown (main our.bol) 'default home']
          [%green ~.public 'visible activity']
      ==
    |=  {{typ/security nom/knot des/cord} _ta}
    %+  ta-action  ost.bol
    [%create nom des typ]
  ::
  ++  ta-apply                                          ::<  apply command
    ::>  applies the command sent by {src}.
    ::
    |=  {src/ship cod/command}
    ^+  +>
    ?-  -.cod
      ::>  %review commands prompt us (as a circle host)
      ::>  to verify and distribute messages.
        $review
      (ta-think | src +.cod)
      ::>  %burden commands ask us to add the sender as a
      ::>  federator for the specified story, taking the
      ::>  state it sent into account.
        $burden
      (ta-burden src +.cod)
      ::>  %relief commands prompt us to relieve the
      ::>  specified federators of their duty.
        $relief
      (ta-relieve src +.cod)
    ==
  ::
  ++  ta-burden                                         ::<  accept federator
    ::>  adds {src} as a federator to story {nom},
    ::>  integrating its state into the story.
    ::
    |=  {src/ship nom/knot cof/lobby pes/crowd gaz/(list telegram)}
    %-  (ta-know nom)  |=  sor/_so  =<  so-done
    (so-burden:sor src cof pes gaz)
  ::
  ++  ta-relieve                                        ::<  remove federator
    ::>  removes {who} as federators from story {nom}.
    ::
    |=  {src/ship nom/knot who/(set ship)}
    %-  (ta-know nom)  |=  sor/_so  =<  so-done
    (so-relieve:sor src who)
  ::
  ++  ta-action                                         ::<  apply reader action
    ::>  performs action sent by a reader.
    ::
    ::delta-generation
    |=  {red/bone act/action}
    ^+  +>
    =<  work
    ::>  ||
    ::>  ||  %actions
    ::>  ||
    ::>    action processing core
    ::>
    ::>  ++work calls the appropriate action processing
    ::>  arm. most use ++affect to retrieve the affected
    ::>  story, reacting if it doesn't exist.
    |%
    ::  ||  %utility
    ::+|
    ++  work                                            ::<  perform action
      ^+  ..ta-action
      ::TODO  %.  +.act
      ::TODO  require deltas as product?
      ?-  -.act
        ::  circle configuration
        $create  (action-create +.act)
        $source  (action-source +.act)
        $depict  (action-depict +.act)
        $filter  (action-filter +.act)
        $permit  (action-permit +.act)
        $delete  (action-delete +.act)
        $enlist  (action-enlist +.act)
        $burden  (action-burden +.act)
        ::  messaging
        $convey  (action-convey +.act)
        $phrase  (action-phrase +.act)
        ::  personal metadata
        $status  (action-status +.act)
        ::  changing shared ui
        $glyph   (action-glyph +.act)
        $nick    (action-nick +.act)
      ==
    ::
    ++  react                                           ::<  new reaction
      ::>
      ::
      |=  rac/reaction
      (ta-delta %react red rac)
    ::
    ++  affect                                          ::<  delta to story
      ::>
      ::
      |=  {nom/knot dif/diff-story}
      ?:  (~(has by stories) nom)
        (impact nom dif)
      %-  react
      [%fail (crip "no story {(trip nom)}") `act]
    ::
    ++  impact                                          ::<  delta for story
      ::>
      ::
      |=  {nom/knot dif/diff-story}
      (ta-delta %story nom dif)
    ::
    ::>  ||  %circle-configuration
    ::+|
    ++  action-create                                   ::<  create story
      ::>  creates a story with the specified parameters.
      ::
      |=  {nom/knot des/cord typ/security}
      ^+  ..ta-action
      ?.  (~(has in stories) nom)
        %^  impact  nom  %new
        :*  [[%& our.bol nom] ~ ~]
            des
            [| |]
            [typ ~]
            [[our.bol ~ ~] [our.bol ~ ~]]
        ==
      %-  react
      [%fail (crip "{(trip nom)}: already exists") `act]
    ::
    ++  action-delete                                   ::<  delete + announce
      ::>  delete story {nom}, optionally announcing the
      ::>  event with message {mes}.
      ::
      |=  {nom/knot mes/(unit cord)}
      ^+  ..ta-action
      =.  ..ta-action  ::TODO  =?
        ?~  mes  ..ta-action
        %+  action-phrase
          [[%& our.bol nom] ~ ~]
        [%lin | u.mes]~
      (affect nom %remove ~)
    ::
    ++  action-depict                                   ::<  change description
      ::>  change description of story {nom} to {des}.
      ::
      |=  {nom/knot cap/cord}
      (affect nom %config [our.bol nom] %caption cap)
    ::
    ++  action-filter                                   ::<  change message rules
      ::>  replaces the story's current filter with the
      ::>  specified one.
      ::
      |=  {nom/knot fit/filter}
      (affect nom %config [our.bol nom] %filter fit)
    ::
    ++  action-permit                                   ::<  invite/banish
      ::>  invite to/banish from story {nom} all {sis}.
      ::
      |=  {nom/knot inv/? sis/(set ship)}
      =+  soy=(~(get by stories) nom)
      ?~  soy
        %^  ta-delta  %react  red
        [%fail (crip "no story {(trip nom)}") `act]
      =/  wyt/?  ?=(?($white $green) sec.con.shape.u.soy)
      =/  add/?  =(inv wyt)
      (affect nom %config [our.bol nom] %permit add sis)
    ::
    ++  action-source                                   ::<  un/sub p to/from r
      ::>  add/remove {pas} as sources for story {nom}.
      ::
      |=  {nom/knot sub/? pas/(set partner)}
      (affect nom %config [our.bol nom] %source sub pas)
    ::
    ++  action-enlist                                   ::<  dis/allow federation
      ::>  adds {sis} to story {nom}'s list of allowed
      ::>  federators.
      ::
      |=  {nom/knot fed/? sis/(set ship)}
      (affect nom %config [our.bol nom] %federal fed | sis)
    ::
    ++  action-burden                                   ::<  help federate
      ::>  starts federating the specified circle. create
      ::>  it locally if it doesn't yet exist.
      ::
      ::TODO  make deltas instead.
      ::TODO  ...but we'll still need to broadcast this move???
      |=  {hos/ship nom/knot}
      ^+  ..ta-action
      ::  update federation config.
      ::  we don't use the specialized so-arms for this because followers will
      ::  get notified once we receive updates from {hos} anyway.
      =+  new=(~(has by stories) nom)
      =.  ..ta-action  ::TODO  =?
        ?.  new  ..ta-action
        %^  impact   nom   %new
        :*  [[%& hos nom] ~ ~]
            *cord
            *filter
            *control
            [[hos ~ ~] [hos ~ ~]]
        ==
      =.  ..ta-action  ::TODO  =?
        ?:  new  ..ta-action
        (affect nom %config [our.bol nom] %federal & | [hos ~ ~])
      =.  ..ta-action  ::TODO  =?
        ?:  new  ..ta-action
        (affect nom %config [our.bol nom] %federal & & [hos ~ ~])
      ::TODO  shouldn't src-adding be included in %fed & & application?
      =.  ..ta-action  ::TODO  =?
        ?:  new  ..ta-action
        (affect nom %config [our.bol nom] %source & [[%& hos nom] ~ ~])
      ::  send %burden command with story's current state.
      (ta-delta %bear [hos nom])
    ::
    ::>  ||  %messaging
    ::+|
    ++  action-convey                                   ::<  post exact
      ::>  sends the messages provided in the action.
      ::
      |=  tos/(list thought)
      (ta-think & our.bol tos)
    ::
    ++  action-phrase                                   ::<  post easy
      ::>  sends the message contents provided in the
      ::>  action, constructing the audience, generating a
      ::>  serial and setting a timestamp.
      ::
      |=  {pas/(set partner) ses/(list speech)}
      ^+  ..ta-action
      =-  (ta-think & our.bol tos)
      |-  ^-  tos/(list thought)
      ?~  ses  ~
      =^  sir  eny.bol  (uniq eny.bol)
      :_  $(ses t.ses)
      :+  sir
        %-  ~(gas by *audience)
        %+  turn  (~(tap in pas))
        |=(p/partner [p *envelope %pending])
      [now.bol ~ i.ses]
    ::
    ::>  ||  %personal-metadata
    ::+|
    ++  action-status                                   ::<  our status update
      ::>  for every story in the set, update our status.
      ::TODO  accept (set circle). for locals, do directly.
      ::      for remotes, send command.
      ::TODO  split interface into action-presence and
      ::      action-human.
      ::
      |=  {nos/(set knot) sat/status}
      ^+  ..ta-action
      %-  ~(rep in nos)
      |=  {k/knot _ta}
      (affect k %status [%& our.bol k] our.bol %full sat)
    ::
    ::>  ||  %changing-shared-ui
    ::+|
    ++  action-nick                                     ::<  new identity
      ::>  assigns a new local identity ("nickname") to the
      ::>  target ship.
      ::
      |=  {who/ship nic/cord}
      ^+  ..ta-action
      ?.  =((~(get by nicks) who) `nic)  ..ta-action    ::<  no change
      (ta-delta %nick who nic)
    ::
    ++  action-glyph                                    ::<  bind a glyph
      ::>  un/bind glyph {lif} to partners {pas}.
      ::
      |=  {lif/char pas/(set partner) bin/?}
      (ta-delta %glyph bin lif pas)
    --
  ::
  ::>  ||
  ::>  ||  %subscription-events
  ::>  ||
  ::>    arms that react to subscription events.
  ::+|
  ::
  ++  ta-subscribe                                      ::<  listen to
    ::>  add her to a presence list if applicable.
    ::
    |=  {her/ship pax/path}
    ^+  +>
    ::  weird subscription path.
    ::TODO  catch earlier, just pass nom instead of path?
    ::  also check story existence earlier maybe?
    ?.  ?=({$circle @ta *} pax)  +>
    %-  (ta-know i.pax)  |=  sor/_so  =<  so-done
    (so-attend:sor her %hear [~ ~])
  ::
  ++  ta-leave                                          ::<  subscription failed
    ::>  removes {cir} from story {nom}'s followers.
    ::
    |=  {nom/knot cir/circle}
    %-  (ta-know nom)  |=  sor/_so  =<  so-done
    (so-leave:sor %& cir)
  ::
  ++  ta-cancel                                         ::<  unsubscribe
    ::>  drops {src}'s subscription. deduce the right way
    ::>  to do this from the subscription path {pax}.
    ::
    |=  {src/ship pax/path}
    ^+  +>
    ::TODO  catch earlier, just pass nom?
    ?.  ?=({$circle @ta *} pax)  +>
    ::  set ship status to %gone.
    %-  (ta-know i.pax)  |=  sor/_so  =<  so-done
    (so-absent:sor src)
  ::
  ++  ta-hear                                           ::<  apply rumor
    ::>
    ::
    |=  {det/knot src/partner dif/rumor}
    ^+  +>
    ?+  -.dif
      ~&([%ignoring-rumor -.dif] +>)
      ::
        $circle
      ?.  ?=($& -.src)
        ~&([%unexpected-rumor -.dif src] +>)
      %-  (ta-know det)  |=  sor/_so  =<  so-done
      (so-hear-circle:sor p.src dif.dif)
    ==
  ::
  ::>  ||
  ::>  ||  %messaging
  ::>  ||
  ::>    arms for sending and processing messages.
  ::+|
  ::
  ++  ta-think                                          ::<  publish or review
    ::>  consumes each thought.
    ::
    |=  {pub/? aut/ship tos/(list thought)}
    ^+  +>
    ?~  tos  +>
    $(tos t.tos, +> (ta-consume pub aut i.tos))
  ::
  ++  ta-consume                                        ::<  to each audience
    ::>  conducts thought {tot} to each partner in its audience.
    ::
    |=  {pub/? aut/ship tot/thought}
    =+  aud=(~(tap by aud.tot))
    |-  ^+  +>.^$
    ?~  aud  +>.^$
    $(aud t.aud, +>.^$ (ta-conduct pub aut p.i.aud tot))
  ::
  ++  ta-conduct                                        ::<  thought to partner
    ::>  either publishes or records a thought.
    ::
    |=  {pub/? aut/ship pan/partner tot/thought}
    ^+  +>
    ?-  -.pan
        $&                                              ::<  circle partner
      ?:  pub
        ?.  (team our.bol aut)
          %-  ta-note
          (crip "strange author {(scow %p aut)}")
        =.  aut  our.bol
        ?:  =(aut hos.p.pan)
          (ta-record nom.p.pan hos.p.pan tot)
        (ta-transmit p.pan tot)
      ?.  =(our.bol hos.p.pan)  +>
      (ta-record nom.p.pan aut tot)
      ::
        $|  !!                                          ::<  passport partner
    ==
  ::
  ++  ta-record                                         ::<  add to story
    ::>  add or update telegram {gam} in story {nom}.
    ::
    |=  {nom/knot gam/telegram}
    %-  (ta-know nom)  |=  sor/_so  =<  so-done
    (so-learn:sor gam)
  ::
  ++  ta-transmit                                       ::<  send message
    ::>  sends thought {tot} to {cir}.
    ::>  stores it to the outbox to await confirmation.
    ::
    |=  {cir/circle tot/thought}
    ^+  +>
    (ta-delta %out cir tot ~)
  ::
  ::>  ||
  ::>  ||  %stories
  ::>  ||
  ::>    arms for modifying stories.
  ::+|
  ::
  ++  so                                                ::<  story core
    ::>  story core, used for doing work on a story.
    ::
    |_  ::>  nom: story name in {stories}.
        ::>  acs: talk actions issued due to changes.
        ::>  story is faceless to ease data access.
        ::
        $:  nom/knot
            acs/(list action)
            story
        ==
    ::
    ++  so-done                                         ::<  apply changes
      ::>  put changed story back into the map and apply
      ::>  actions.
      ::
      ^+  +>
      ::?:  =(`+<+> (~(get by stories) nom))
      ::  ::TODO  tmp
      ::  ~&(%illegal-change-story !!)
      =.  acs  (flop acs)
      |-  ^+  +>+
      ?~  acs  +>+
      =.  +>+  (ta-action ost.bol i.acs)
      $(acs t.acs)
    ::
    ::>  ||
    ::>  ||  %emitters
    ::>  ||
    ::>    arms that create outward changes.
    ::+|
    ::
    ++  so-act                                          ::<  send action
      ::>  stores a talk action.
      ::
      |=  act/action
      ^+  +>
      +>(acs [act acs])
    ::
    ++  so-note                                         ::<  tell user
      ::>  sends {msg} as an %app message to the user's
      ::>  inbox.
      ::
      |=  msg/cord
      ^+  +>
      %+  so-act  %phrase
      :-  [[%& our.bol (main our.bol)] ~ ~]
      [%app %talk-guardian msg]~
    ::
    ++  so-delta                                        ::<  send delta
      ::>
      ::
      |=  dif/delta
      ^+  +>
      +>(deltas [dif deltas])
    ::
    ++  so-delta-our                                    ::<  send delta of us
      ::>  adds a delta about this story.
      ::
      |=  dif/diff-story
      ^+  +>
      (so-delta %story nom dif)
    ::
    ::>  ||
    ::>  ||  %data
    ::>  ||
    ::>    utility functions for data retrieval.
    ::+|
    ::
    ++  so-pan  [%& our.bol nom]                        ::<  us as partner
    ++  so-cir  [our.bol nom]                           ::<  us as circle
    ::
    ++  so-right                                        ::<  is federator?
      ::>  checks whether partner {pan} has authority
      ::>  over this story.
      ::
      |=  pan/partner
      ?&  ?=($& -.pan)
          =(nom nom.p.pan)
          (~(has in fes.fed.shape) hos.p.pan)
      ==
    ::
    ::>  ||
    ::>  ||  %interaction-events
    ::>  ||
    ::>    arms that apply events we received.
    ::+|
    ::
    ++  so-hear-circle                                  ::<  accept circle rumor
      ::>
      ::
      |=  {src/circle dif/diff-story}
      ^+  +>
      ?.  (~(has in src.shape) [%& src])
        ~&([%unexpected-rumor -.dif src] +>)
      ::  rumor from federator? apply to us.
      =.  src  ::TODO  =?
        ?:  (so-right [%& src])  so-cir
        src
      ?-  -.dif
        $new      $(dif [%config src %full con.dif])
        $grams    (so-lesson gaz.dif)
        $config   ::  ignore foreign mirrors.
                  ?.  |(=(src cir.dif) =(src so-cir))  +>
                  (so-delta-our dif)
        $status   ::  ignore foreign remotes.
                  ?.  |(=([%& src] pan.dif) =(src so-cir))  +>
                  (so-delta-our dif)
        $remove   (so-delta-our %config src %remove ~)
      ==
    ::
    ++  so-burden                                       ::<  accept federator
      ::>  if {src} is allowed to, have it federate this
      ::>  story.
      ::>  starts by assimilating {src}'s state into our
      ::>  own (giving priority to local state), removing
      ::>  redundant data, then sending updated state to
      ::>  all followers.
      ::
      |=  {src/ship cof/lobby pes/crowd gaz/(list telegram)}
      ^+  +>
      ::  continue if permitted and not yet done.
      ?.  (~(has in may.fed.shape) src)  +>
      ?:  (~(has in fes.fed.shape) src)  +>
      ::  assimilate config.
      =.  +>
        =+  nec=shape
        ::  adopt security list if they're similar.
        =.  ses.con.nec  ::TODO  =?
          ?.  .=  ?=(?($white $green) sec.con.nec)
                  ?=(?($white $green) sec.con.loc.cof)
            ses.con.nec
          (~(uni in ses.con.nec) ses.con.loc.cof)
        =.  fes.fed.nec
          (~(put in fes.fed.nec) src)
        =.  src.nec
          (~(put in src.nec) [%& src nom])
        ::TODO  maybe do more granular deltas later.
        (so-delta-our %config so-cir %full nec)
      ::  assimilate presence and remotes.
      ::TODO!!!  just delta my shit up famalam.
      ::TODO  should totally just make an arm that calculates deltas given
      ::      old and new inputs.
      ::=.  locals   (~(uni by loc.pes) locals)
      ::=.  remotes  (~(uni by rem.pes) remotes)
      ::=.  mirrors  (~(uni by rem.cof) mirrors)
      ::::  remove redundant remotes.
      ::=.  remotes
      ::  %-  ~(gas by *_remotes)
      ::  %+  murn  (~(tap by remotes))
      ::  |=  {p/partner g/group}
      ::  ^-  (unit {partner group})
      ::  ?:  ?&  ?=($& -.p)
      ::          =(nom.p.p nom)
      ::          (~(has in fes.fed.shape) hos.p.p)
      ::      ==
      ::    ~
      ::  `[p g]
      ::::  remove redundant mirrors.
      ::=.  mirrors
      ::  %-  ~(gas by *_mirrors)
      ::  %+  murn  (~(tap by mirrors))
      ::  |=  {c/circle f/config}
      ::  ^-  (unit {circle config})
      ::  ?:  ?&  =(nom.c nom)
      ::          (~(has in fes.fed.shape) hos.c)
      ::      ==
      ::    ~
      ::  `[c f]
      ::  finally, learn all grams.
      (so-lesson gaz)
    ::
    ++  so-relieve                                      ::<  remove federator
      ::>  if {src} is allowed to, removes {who} as
      ::>  federators from this story.
      ::
      |=  {src/ship who/(set ship)}
      ^+  +>
      ?.  (~(has in fes.fed.shape) src)  +>
      =+  wos=(~(uni in fes.fed.shape) who)
      ?~  wos  +>.$
      =.  +>.$
        (so-delta-our %config so-cir %federal | & wos)
      %-  so-delta-our
      :+  %config  so-cir
      :+  %source  |
      %-  ~(run in `(set ship)`wos)  ::TODO?  why need to cast?
      |=(s/ship [%& s nom])
    ::
    ::>  ||
    ::>  ||  %changes
    ::>  ||
    ::>    arms that make miscelaneous changes to this story.
    ::+|
    ::
    ++  so-sources                                      ::<  change source
      ::>  adds or removes {pas} from our sources.
      ::
      |=  {add/? pas/(set partner)}
      ^+  +>
      =/  sus/(set partner)
        %.  src.shape
        ?:(add ~(dif in pas) ~(int in pas))
      ?~  sus  +>.$
      (so-delta-our %config so-cir %source add sus)
    ::
    ++  so-depict                                       ::<  change description
      ::>  modifies our caption.
      ::
      |=  cap/cord
      ^+  +>
      ?:  =(cap cap.shape)  +>
      (so-delta-our %config so-cir %caption cap)
    ::
    ++  so-filter                                       ::<  change message rules
      ::>  modifies our filter.
      ::
      |=  fit/filter
      ^+  +>
      ?:  =(fit fit.shape)  +>
      (so-delta-our %config so-cir %filter fit)
    ::
    ++  so-federate                                     ::<  change federators
      ::>  adds or removes sis as active/allow
      ::>  ({fed} y/n) federators.
      ::
      |=  {add/? fed/? sis/(set ship)}
      =+  ses=?:(fed fes.fed.shape may.fed.shape)
      =/  sus/(set ship)
        %.  ses
        ?:(add ~(dif in sis) ~(int in sis))
      ?~  sus  +>.$
      ::  we also take care of the %src delta because we
      ::  want to keep delta application as simple as
      ::  possible.
      =.  +>.$  ::TODO  =?
        ?.  fed  +>.$
        %-  so-delta-our
        :+  %config  so-cir
        :+  %source  add
        %-  ~(run in `(set ship)`sus)  ::TODO  weird casting need, depends on ?~
        |=  s/ship  [%& s nom]
      (so-delta-our %config so-cir %federal add fed sus)
    ::
    ++  so-delete                                       ::<  delete story
      ::>  deletes this story. removes it from {stories}
      ::>  and unsubscribes from all src.
      ::
      (so-delta-our %remove ~)
    ::
    ++  so-attend                                       ::<  add local presence
      ::>  add {her} status to this story's presence map.
      ::
      |=  {her/ship sat/status}
      ^+  +>
      ?:  =(`sat (~(get by locals) her))  +>.$
      (so-delta-our %status so-pan her %full sat)
    ::
    ++  so-absent                                       ::<  del local presence
      ::>
      ::
      |=  her/ship
      ^+  +>
      ?.  (~(has by locals) her)  +>
      (so-delta-our %status so-pan her %remove ~)
    ::
    ::>  ||
    ::>  ||  %subscriptions
    ::>  ||
    ::>    arms for starting and ending subscriptions
    ::+|
    ::
    ++  so-leave                                        ::<  unsub from source
      ::>  delete {pan} from our sources.
      ::
      |=  pan/partner
      ^+  +>
      ?.  (~(has in src.shape) pan)  +>
      (so-delta-our %config so-cir %source | [pan ~ ~])
    ::
    ++  so-start                                        ::<  subscribe follower
      ::>  called upon subscribe. deduces the range of
      ::>  {her} subscription from {pax}, then sends
      ::>  the currently available part of it.
      ::
      |=  {her/ship pax/path}
      ^+  +>
      ::  read permissions
      ?.  (so-visible her)
        =.  +>  (so-delta %quit ost.bol)
        %-  so-note  %-  crip
        "so-start permission denied {(scow %p her)}"
      ::  find grams range
      =/  ruv/(unit river)
        ::  collapse unit list
        %+  biff
          %-  zl:jo
          %+  turn  pax
          ;~(biff slay |=(a/coin `(unit dime)`?~(-.a a ~)))
        |=  paf/(list dime)
        ?~  paf
          $(paf [%ud (sub (max 64 count) 64)]~)
        ?~  t.paf
          $(t.paf [%da (dec (bex 128))]~)
        ?.  ?=({{?($ud $da) @} {?($ud $da) @} $~} paf)
          ~
        `[[?+(- . $ud .)]:i.paf [?+(- . $ud .)]:i.t.paf]  ::XX arvo issue #366
      ?~  ruv
        =.  +>.$  (so-delta %quit ost.bol)
        %-  so-note  %-  crip
        "so-start malformed path {~(ram re >pax<)}"
      (so-first-grams u.ruv)
    ::
    ++  so-first-grams                                  ::<  beginning of stream
      ::>  find all grams that fall within the river and
      ::>  send them in a grams report to {ost.bol}.
      ::
      |=  riv/river
      ^+  +>
      =;  lab/{dun/? end/@u zeg/(list telegram)}
          ?.  dun.lab  +>.$
          (so-delta %quit ost.bol)
      =+  [end=count gaz=grams dun=| zeg=*(list telegram)]
      |-  ^-  (trel ? @ud (list telegram))
      ?~  gaz  [dun end zeg]
      ?:  ?-  -.q.riv                                   ::  after the end
            $ud  (lte p.q.riv end)
            $da  (lte p.q.riv wen.sam.tot.i.gaz)
          ==
        ::  if past the river, continue back, mark as done.
        $(end (dec end), gaz t.gaz, dun &)
      ?:  ?-  -.p.riv                                   ::  before the start
            $ud  (lth end p.p.riv)
            $da  (lth wen.sam.tot.i.gaz p.p.riv)
          ==
        ::  if before the river, we're done searching.
        [dun end zeg]
      ::  if in the river, add this gram and continue.
      $(end (dec end), gaz t.gaz, zeg [i.gaz zeg])
    ::
    ::>  ||
    ::>  ||  %messaging
    ::>  ||
    ::>    arms for adding to this story's messages.
    ::+|
    ::
    ++  so-sane                                         ::<  sanitize
      ::>  sanitize %lin speech according to our settings.
      ::
      |=  tot/thought
      ^-  thought
      ?.  ?=({$lin *} sep.sam.tot)  tot
      %_  tot
          msg.sep.sam
        %-  crip
        %-  tufa
        %+  turn  (tuba (trip msg.sep.sam.tot))
        |=  a/@c
        ::  always replace control characters.
        ?:  |((lth a 32) =(a `@c`127))
          `@`'?'
        ::  if desired, remove uppercasing.
        ?:  ?&  !cus.fit.shape
                (gte a 'A')
                (lte a 'Z')
            ==
          (add a 32)
        ::  if desired, replace non-ascii characters.
        ?:  ?&  !utf.fit.shape
                (gth a 127)
            ==
          `@`'?'
        a
      ==
    ::
    ++  so-lesson                                       ::<  learn messages
      ::>  learn all telegrams in a list.
      ::
      |=  gaz/(list telegram)
      ^+  +>
      ?~  gaz  +>
      $(gaz t.gaz, +> (so-learn i.gaz))
    ::
    ++  so-learn                                        ::<  save/update message
      ::>  store an incoming telegram, updating if it
      ::>  already exists.
      ::
      |=  gam/telegram
      ^+  +>
      ::  check for write permissions.
      ?.  (so-admire aut.gam)  +>.$
      ::  clean up the message to conform to our rules.
      =.  tot.gam  (so-sane tot.gam)
      =.  aud.tot.gam
        ::>  if we are in the audience, mark as received.
        =+  ole=(~(get by aud.tot.gam) [%& our.bol nom])
        ?^  ole  (~(put by aud.tot.gam) [%& our.bol nom] -.u.ole %received)
        ::>  federated circles need to pretend ~src/nom
        ::>  is also ~our/nom.
        ::TODO  pass src through explicitly instead of
        ::      relying on src.bol.
        =+  ole=(~(get by aud.tot.gam) [%& src.bol nom])
        ?~  ole  aud.tot.gam
        ::>  as described above, fake src into our.
        =.  aud.tot.gam  (~(del by aud.tot.gam) [%& src.bol nom])
        (~(put by aud.tot.gam) [%& our.bol nom] -.u.ole %received)
      (so-delta-our %grams [gam ~])
    ::
    ::>  ||
    ::>  ||  %permissions
    ::>  ||
    ::>    arms relating to story permissions.
    ::+|
    ::
    ++  so-secure                                       ::<  change security mode
      ::>  changes our security mode.
      ::
      |=  sec/security
      ^+  +>
      ?:  =(sec sec.con.shape)  +>
      (so-delta-our %config so-cir %secure sec)
    ::
    ++  so-permit                                       ::<  invite/banish
      ::>  update config to dis/allow ships permission.
      ::
      |=  {inv/? sis/(set ship)}
      ^+  +>
      ::>  wyt:  whitelist?
      ::>  add:  add to list?
      =/  wyt/?  ?=(?($white $green) sec.con.shape)
      =/  add/?  =(inv wyt)
      =.  +>.$
        %-  so-act
        :-  %phrase
        %-  ~(rep in sis)
        |=  {s/ship a/(set partner) t/(list speech)}
        :-  (~(put in a) [%& s (main s)])
        [[%inv inv so-cir] t]
      (so-delta-our %config so-cir %permit [add sis])
    ::
    ++  so-admire                                       ::<  accept from
      ::>  checks {her} write permissions.
      ::
      |=  her/ship
      ^-  ?
      ?-  sec.con.shape
        $black  !(~(has in ses.con.shape) her)         ::<  channel, blacklist
        $white  (~(has in ses.con.shape) her)          ::<  village, whitelist
        $green  (~(has in ses.con.shape) her)          ::<  journal, whitelist
        $brown  !(~(has in ses.con.shape) her)         ::<  mailbox, blacklist
      ==
    ::
    ++  so-visible                                      ::<  display to
      ::>  checks {her} read permissions.
      ::
      |=  her/ship
      ^-  ?
      ?-  sec.con.shape
        $black  !(~(has in ses.con.shape) her)         ::<  channel, blacklist
        $white  (~(has in ses.con.shape) her)          ::<  village, whitelist
        $green  &                                       ::<  journal, all
        $brown  (team our.bol her)                      ::<  mailbox, our team
      ==
    --
  --
::
++  da                                                  ::<  delta application
  ::>  core for doing things, mostly applying deltas to
  ::>  application state, but also dealing with events
  ::>  that aren't pokes.
  ::>  where appropriate, creates moves. those get
  ::>  produced when finalizing with ++da-done.
  ::
  |_  ::>  moves: moves created by core operations.
      ::
      moves/(list move)
  ::
  ++  da-done                                           ::<  resolve core
    ::>  produces the moves stored in ++da's moves.
    ::>  they are produced in reverse order because
    ::>  ++da-emil and ++da-emit add them to the head of
    ::>  the {moves}.
    ::
    ^-  (quip move +>)
    [(flop moves) +>]
  ::
  ::>  ||
  ::>  ||  %emitters
  ::>  ||
  ::>    arms that create outward changes.
  ::+|
  ::
  ++  da-emil                                           ::<  emit move list
    ::>  adds multiple moves to the head of {moves}.
    ::>  flops to stay consistent with ++ta-emit.
    ::
    |=  mol/(list move)
    %_(+> moves (welp (flop mol) moves))
  ::
  ++  da-emit                                           ::<  emit a move
    ::>  adds a move to the head of {moves}.
    ::
    |=  mov/move
    %_(+> moves [mov moves])
  ::
  ++  da-react                                          ::<  send reaction
    ::>  sends a talk-reaction diff to a reader.
    ::
    ::TODO  send the delta instead! (remove bone from delta: always ost.bol?)
    |=  {red/bone rac/reaction}
    %-  da-emit
    ::TODO  is diff the way to react to a poke?
    [red %diff %talk-reaction rac]
  ::
  ++  da-bear                                           ::<  share burden
    ::>
    ::
    |=  cir/circle
    =+  soy=(~(got by stories) nom.cir)
    %-  da-emit
    :*  ost.bol
        %poke
        /burden
        [hos.cir %talk-guardian]
        :*  %talk-command
            %burden
            nom.cir
            [shape.soy mirrors.soy]
            [locals.soy remotes.soy]
            grams.soy
        ==
    ==
  ::
  ::>  ||
  ::>  ||  %data
  ::>  ||
  ::>    utility functions for data retrieval.
  ::+|
  ::
  ::TODO  functions for getting readers or followers of a specific story from
  ::      the subs in sup.bol.
  ::      but maybe just on the outer core...
  ::
  ::>  ||
  ::>  ||  %change-application
  ::>  ||
  ::>    arms that change the application state.
  ::+|
  ::
  ++  da-change                                         ::<  apply delta
    ::>  modifies application state according to the
    ::>  change specified in {dif}.
    ::
    |=  dif/delta
    ^+  +>
    ?-  -.dif
        $more
      |-  ^+  +>.^$
      ?~  mor.dif  +>.^$
      $(+>.^$ ^$(dif i.mor.dif), mor.dif t.mor.dif)
      ::
      $out    (da-change-out +.dif)
      $done   (da-change-done (~(tap by don.dif)))
      $glyph  (da-change-glyph +.dif)
      $nick   (da-change-nick +.dif)
      $story  (da-change-story +.dif)
      $bear   (da-bear +.dif)
      $react  (da-react +.dif)
      $quit   (da-emit [ost.dif %quit ~])
    ==
  ::
  ++  da-change-out                                     ::<  outgoing messages
    ::>
    ::
    |=  {cir/circle out/(list thought)}
    ^+  +>
    ?~  out  +>
    =.  +>
      %+  da-emit  ost.bol
      :*  %poke
          /repeat/(scot %ud p.outbox)/(scot %p hos.cir)/[nom.cir]
          [hos.cir %talk-guardian]
          [%talk-command %review i.out ~]
      ==
    $(p.outbox +(p.outbox), q.outbox (~(put by q.outbox) p.outbox i.out))
  ::
  ++  da-change-done                                    ::<  sent & receives msgs
    ::>
    ::
    |=  don/(list {num/@ud who/partner gud/?})
    ^+  +>
    ?~  don  +>
    =+  oot=(~(get by q.outbox) num.i.don)
    ?~  oot  ~|([%da-change-done-none num.i.don] !!)
    =.  aud.u.oot
      =+  olg=(~(got by aud.u.oot) who.i.don)
      %+  ~(put by aud.u.oot)  who.i.don
      :-  -.olg
      ?:(gud.i.don %received %rejected)
    =.  +>.$
      +>.$  ::TODO!!!  da-think??????????
    $(q.outbox (~(del by q.outbox) num.i.don))
    ::|=  {num/@ud pan/partner fal/(unit tang)}
    ::=+  oot=(~(get by q.outbox) num)
    ::?~  oot  ~|([%ta-repeat-none num] !!)
    ::=.  q.outbox  (~(del by q.outbox) num)
    ::=.  aud.u.oot
    ::  =+  olg=(~(got by aud.u.oot) pan)
    ::  %+  ~(put by aud.u.oot)  pan
    ::  :-  -.olg
    ::  ?~  fal  %received
    ::  ~>  %slog.[0 u.fal]
    ::  %rejected
    ::(ta-think | our.bol u.oot ~)
  ::
  ++  da-change-glyph                                   ::<  un/bound glyph
    ::>
    ::
    |=  {bin/? gyf/char pas/(set partner)}
    ^+  +>
    ?:  bin
      %_  +>
        nik  (~(put by nik) pas gyf)
        nak  (~(put ju nak) gyf pas)
      ==
    =/  ole/(list (set partner))
      ?.  =(pas ~)  [pas ~]
      (~(tap in (~(get ju nak) gyf)))
    |-  ^+  +>.^$
    ?~  ole  +>.^$
    %_  $
      nik  (~(del by nik) i.ole)
      nak  (~(del ju nak) gyf i.ole)
      ole  t.ole
    ==
  ::
  ++  da-change-nick                                    ::<  changed nickname
    ::>
    ::
    |=  {who/ship nic/cord}
    ^+  +>
    ?:  =('' nic)
      $(nicks (~(del by nicks) who))
    $(nicks (~(put by nicks) who nic))
  ::
  ::>  ||
  ::>  ||  %stories
  ::>  ||
  ::>    arms for modifying stories.
  ::+|
  ::
  ++  da-change-story                                   ::<  apply circle delta
    ::>
    ::
    |=  {nom/knot dif/diff-story}
    ^+  +>
    ::TODO  just ~(got by stories) everywhere in ++da, the
    ::      relevant checks should be made when constructing
    ::      the deltas.
    ?+  -.dif
      sa-done:(~(sa-change sa nom (~(got by stories) nom)) dif)
      ::
      $new      (da-create nom +.dif)
      $remove   (da-delete nom)
    ==
  ::
  ++  da-create                                         ::<  configure story
    ::>  creates story {nom} with config {con}.
    ::
    |=  {nom/knot con/config}
    ^+  +>
    ::  if it's a whitelisted circle, put us in it.
    =.  ses.con.con  ::TODO  =?
      ?:  ?=(?($white $green) sec.con.con)
        [our.bol ~ ~]
      ses.con.con
    ::  also ensure we're listed as a federator.
    =.  may.fed.con
      (~(put in may.fed.con) our.bol)
    =.  fes.fed.con
      (~(put in fes.fed.con) our.bol)
    =<  sa-done
    %-  ~(sa-change sa nom *story)
    [%config [our.bol nom] %full con]
  ::
  ++  da-delete                                         ::<  delete story
    ::>  calls the story core to delete story {nom}.
    ::
    |=  nom/knot
    ^+  +>
    =.  +>
      %-  da-emil
      ~(sa-delete sa nom (~(got by stories) nom))
    +>(stories (~(del by stories) nom))
  ::
  ++  sa                                                ::<  story delta core
    ::>  story core, used for doing work on a story.
    ::
    |_  ::>  nom: story name in {stories}.
        ::>  story is faceless to ease data access.
        ::
        $:  nom/knot
            story
        ==
    ::
    ++  sa-done                                         ::<  apply changes
      ::>  put changed story back into the map.
      ::
      +>(stories (~(put by stories) nom +<+))
    ::
    ::>  ||
    ::>  ||  %emitters
    ::>  ||
    ::>    arms that create outward changes.
    ::+|
    ::
    ++  sa-emil                                         ::<  emit move list
      ::>  adds multiple moves to the head of {moves}.
      ::>  flops to stay consistent with ++ta-emit.
      ::
      |=  mol/(list move)
      %_(+> moves (welp (flop mol) moves))
    ::
    ++  sa-emit                                         ::<  emit a move
      ::>  adds a move to the head of {moves}.
      ::
      |=  mov/move
      %_(+> moves [mov moves])
    ::
    ++  sa-sauce                                        ::<  play cards
      ::>  cards to moves.
      ::
      |=  {ost/bone cub/(list card)}
      ^-  (list move)
      (flop (turn cub |=(a/card [ost a])))
    ::
    ::>  ||
    ::>  ||  %delta-application
    ::>  ||
    ::>    arms for applying deltas.
    ::+|
    ::
    ++  sa-delete                                       ::<  deletion of story
      ::>
      ::
      (sa-abjure (~(tap in src.shape)))
    ::
    ++  sa-change                                       ::<  apply circle delta
      ::>
      ::>  we don't do checks for federation here, this
      ::>  should have happened during delta generation.
      ::
      |=  dif/diff-story
      ^+  +>
      %.  dif
      ?+  -.dif
        sa-change-local
        ::
          $config
        ?:  =(cir.dif [our.bol nom])
          sa-change-local
        sa-change-remote
        ::
          $status
        ?:  =(pan.dif [%& our.bol nom])
          sa-change-local
        sa-change-remote
      ==
    ::
    ++  sa-change-local                                 ::<  apply our delta
      ::>
      ::
      |=  dif/diff-story
      ^+  +>
      ?+  -.dif
        ~&([%unexpected-delta-local -.dif] !!)
        ::
          $grams
        |-  ^+  +>.^$
        ?~  gaz.dif  +>.^$
        =.  +>.^$  (sa-change-grams i.gaz.dif)
        $(gaz.dif t.gaz.dif)
        ::
          $config
        =.  +>
          %-  sa-emil
          (sa-config-effects shape dif.dif)
        +>(shape (change-config shape dif.dif))
        ::
          $status
        %_  +>
            locals
          %+  ~(put by locals)  who.dif
          %+  change-status
            (fall (~(get by locals) who.dif) *status)
          dif.dif
        ==
      ==
    ::
    ++  sa-change-grams                                 ::<  save/update message
      ::>
      ::
      |=  gam/telegram
      ^+  +>
      =+  old=(~(get by known) uid.tot.gam)
      ?~  old
        ::  new message
        %_  +>.$
          grams    [gam grams]
          count    +(count)
          known    (~(put by known) uid.tot.gam count)
        ==
      ::  changed message
      =+  dex=(sub count u.old)
      %_  +>.$
        grams    %+  welp
                 (scag (dec dex) grams)
                 [gam (slag dex grams)]
      ==
    ::
    ++  sa-change-remote                                ::<  apply remote's delta
      ::>
      ::
      |=  dif/diff-story
      ^+  +>
      ?+  -.dif
        ~&([%unexpected-delta-remote -.dif] !!)
        ::
          $config
        ?:  ?=($remove -.dif.dif)
          +>(mirrors (~(del by mirrors) cir.dif))
        =/  new/config
          %+  change-config
          (fall (~(get by mirrors) cir.dif) *config)
          dif.dif
        +>.$(mirrors (~(put by mirrors) cir.dif new))
        ::
          $status
        %_  +>.$
            remotes
          %+  ~(put by remotes)  pan.dif
          =+  ole=(fall (~(get by remotes) pan.dif) *group)
          ?:  ?=($remove -.dif.dif)  (~(del by ole) who.dif)
          =+  old=(fall (~(get by ole) who.dif) *status)
          (~(put by ole) who.dif (change-status old dif.dif))
        ==
      ==
    ::
    ++  sa-config-effects                               ::<  config side-effects
      ::>
      ::
      |=  {old/config dif/diff-config}
      ^-  (list move)
      ?+  -.dif  ~
        $source   (sa-source-effects src.old +.dif)
        $permit   (sa-permit-effects sec.con.old ses.con.old +.dif)
        ::
          $federal
        ?.  fed.dif  ~
        %^  sa-source-effects  src.old  add.dif
        %-  ~(run in sis.dif)
        |=  s/ship  [%& s nom]
        ::
          $full
        =*  new  cof.dif
        ::  deal with subscription changes.
        =/  sem
          .=  ?=(?($white $green) sec.con.new)
              ?=(?($white $green) sec.con.old)
        ;:  weld
          (sa-source-effects src.old | (~(dif in src.old) src.new))
          (sa-source-effects src.old & (~(dif in src.new) src.old))
          ?.  sem  ~
          %^  sa-permit-effects  sec.con.new  ses.con.old
          [| (~(dif in ses.con.old) ses.con.new)]
          ?.  sem  ~
          %^  sa-permit-effects  sec.con.new  ses.con.old
          [& (~(dif in ses.con.new) ses.con.old)]
          ::TODO  maybe do federal source changes, but also take above source
          ::      changes into account: don't do doubles!
        ==
      ==
    ::
    ++  sa-source-effects                               ::<  un/subscribe
      ::>
      ::
      |=  {old/(set partner) add/? pas/(set partner)}
      ^-  (list move)
      =/  sus/(set partner)
        %.  old
        ?:(add ~(dif in pas) ~(int in pas))
      %.  (~(tap in `(set partner)`sus))  ::TODO  *need* to cast?
      ?:(add sa-acquire sa-abjure)
    ::
    ++  sa-permit-effects                               ::<  notify permitted
      ::>
      ::
      |=  {sec/security old/(set ship) add/? sis/(set ship)}
      ^-  (list move)
      =/  wyt  ?=(?($white $green) sec)
      =/  inv  =(wyt add)
      ?:  inv
        ::TODO  %inv & speeches
        ~
      ::TODO  %inv | speeches
      (sa-eject sis)
    ::
    ::>  ||
    ::>  ||  %subscriptions
    ::>  ||
    ::>    arms for starting and ending subscriptions
    ::+|
    ::
    ++  sa-acquire                                      ::<  subscribe us
      ::>  subscribes this story to each partner.
      ::
      |=  pas/(list partner)
      %+  sa-sauce  0  ::  subscription is caused by this app
      %-  zing
      %+  turn  pas
      |=  pan/partner
      ^-  (list card)
      ::>  subscribe starting at the last message we got,
      ::>  or if we haven't gotten any yet, messages
      ::>  from up to a day ago.
      =+  num=(~(get by sequence) pan)
      =+  old=(sub now.bol ~d1)                         :: XX full backlog
      =+  ini=?^(num (scot %ud u.num) (scot %da old))
      ?-  -.pan
          $|  !!                                        ::<  passport partner
        ::
          $&                                            ::<  circle partner
        :_  ~
        :*  %peer
            /circle/[nom]/(scot %p hos.p.pan)/[nom.p.pan]
            [hos.p.pan %talk-guardian]
            /circle/[nom.p.pan]/[ini]
        ==
      ==
    ::
    ++  sa-abjure                                       ::<  unsubscribe us
      ::>  unsubscribes this story from each partner.
      ::
      |=  pas/(list partner)
      %+  sa-sauce  0  ::  subscription is caused by this app
      %-  zing
      %+  turn  pas
      |=  pan/partner
      ^-  (list card)
      ?-  -.pan
          $|  !!                                        ::<  passport partner
        ::
          $&                                            ::<  circle partner
        :_  ~
        :*  %pull
            /friend/show/[nom]/(scot %p hos.p.pan)/[nom.p.pan]
            [hos.p.pan %talk-guardian]
            ~
        ==
      ==
    ::
    ++  sa-eject                                        ::<  unsubscribe ships
      ::>  removes ships {sis} from {followers}.
      ::
      |=  sis/(set ship)
      ^-  (list move)
      %+  turn  (~(tap in (sa-unearth sis)))
      |=  {b/bone}
      [b %quit ~]
    ::
    ++  sa-unearth                                      ::<  ships' bones
      ::>  find the bones in {followers} that belong to
      ::>  a ship in {sis}.
      ::
      |=  sis/(set ship)
      ^-  (set bone)
      %-  ~(rep in sup.bol)
      |=  {{b/bone s/ship p/path} c/(set bone)}
      ?.  ?&  (~(has in sis) s)
              ?=({@tas *} p)
              =(i.p nom)
          ==
        c
      (~(put in c) b)
    --
--
::
::
::>  ||
::>  ||  %wire-parsing
::>  ||
::+|
::
++  etch                                                ::<  parse wire
  ::>  parses {wir}} to obtain either %friend with story
  ::>  and circle or %repeat with message number,
  ::>  source ship and story.
  ::
  |=  wir/wire
  ^-  weir
  ?+    -.wir  !!
      $friend
    ?>  ?=({$show @ @ @ $~} t.wir)
    :^    %friend
        i.t.t.wir
      (slav %p i.t.t.t.wir)
    i.t.t.t.t.wir
    ::
      $repeat
    ?>  ?=({@ @ @ $~} t.wir)
    :^    %repeat
        (slav %ud i.t.wir)
      (slav %p i.t.t.wir)
    i.t.t.t.wir
  ==
::
++  etch-friend                                         ::<  parse /friend wire
  ::>  parses a /friend wire, call a gate with the result.
  ::
  |=  $:  wir/wire
          $=  fun
          $-  {nom/knot cir/circle}
              {(list move) _.}
      ==
  =+  wer=(etch wir)
  ?>(?=($friend -.wer) (fun nom.wer cir.wer))
::
++  etch-repeat                                         ::<  parse /repeat wire
  ::>  parses a /repeat wire, call gate with the result.
  ::
  |=  $:  wir/wire
          $=  fun
          $-  {num/@ud src/ship nom/knot}
              {(list move) _.}
      ==
  =+  wer=(etch wir)
  ?>(?=($repeat -.wer) (fun num.wer hos.wer nom.wer))
::
::>  ||
::>  ||  %new-events
::>  ||
::+|
::TODO  make use of ++prey for filtering subs?
++  f-bake                                              ::<  apply state delta
  ::>  applies a change to the application state,
  ::>  producing side-effects.
  ::
  |=  dif/delta
  ^-  (quip move +>)
  =^  mos  +>.$
    da-done:(da-change:da dif)
  :_  +>.$
  :(welp mos (affection dif))
::
++  g-query                                             ::<  query on state
  ::>
  ::
  |=  weg/(list coin)
  ::TODO  how would the system know how to parse the path?
  ::      should we define that ourselves?
  ::  ...i just want to cast to ++query if i can.
  ::TODO  should return (unit prize)? ie for /circle/non-existing
  ^-  prize
  ?~  weg  ~&(%empty-query !!)
  ?:  =(i.weg [%$ %tas %reader])
    [%reader nak nicks]
  ?:  =(i.weg [%$ %tas %friend])
    :-  %friend
    %-  ~(gas in *(set circle))
    %+  murn
      =-  (~(tap in src.shape.-))
      (~(got by stories) (main our.bol))
    |=  p/partner
    ^-  (unit circle)
    ?.  ?=($& -.p)  ~
    [~ p.p]
  ?:  ?&  =(i.weg [%$ %tas %circle])
          ?=(^ t.weg)
          ?=({$$ p/$ta q/@ta} i.t.weg)
      ==
    :-  %circle
    =+  soy=(~(got by stories) +>.i.t.weg)
    :+  grams.soy  ::TODO  get using specified range.
      [shape.soy mirrors.soy]
    [locals.soy remotes.soy]
  ~&(%invalid-query !!)
::
++  i-change                                            ::<  delta to rumor
  ::>
  ::
  ::TODO  probably want to do "affected by" checks for every bone,
  ::  and just construct the rumor once.
  |=  {weg/(list coin) dif/delta}
  ^-  (unit rumor)
  ?~  weg  ~&(%empty-query !!)
  ?:  =(i.weg [%$ %tas %reader])
    ::  changes to shared ui state apply.
    ?+  -.dif  ~
      $glyph  `[%reader dif]
      $nick   `[%reader dif]
    ==
  ?:  =(i.weg [%$ %tas %friend])
    ::  new or removed local stories apply.
    ::TODO  include mailbox sources. check privacy flags.
    ?.  ?=($story -.dif)  ~
    =/  add/(unit ?)
      ?+  -.dif.dif  ~
        $new      `&
        $remove   `|
      ==
    ?~  add  ~
    `[%friend u.add [our.bol nom.dif]]
  ?:  ?&  =(i.weg [%$ %tas %circle])
          ?=(^ t.weg)
          ?=({$$ p/$ta q/@ta} i.t.weg)
      ==
    ?.  ?=($story -.dif)  ~
    ?.  =(+>.i.t.weg nom.dif)  ~
    `[%circle dif.dif]
  ~&(%invalid-query !!)
::
++  affection                                           ::<  rumors to interested
  ::>
  ::
  ::TODO  probably want to do "affected by" checks for every bone,
  ::  and just construct the rumor once.
  |=  dif/delta
  ^-  (list move)
  %+  murn  (~(tap by sup.bol))
  |=  {b/bone s/ship p/path}
  ^-  (unit move)
  =+  rum=(i-change (tmp-parse-path p) dif)
  ::TODO  %quit bones that are done with their subscription.
  ::  ...but that would also require a ta-cancel call to remove
  ::  them from the presence list! how do?
  ?~  rum  ~
  `[b %diff %talk-rumor u.rum]
::
++  tmp-parse-path                                      ::<  ...
  ::>
  ::
  |=  pax/path
  ^-  (list coin)
  ?~  pax  ~
  :-  [%$ %tas `@tas`i.pax]
  ?.  =(%circle `@tas`i.pax)  ~
  ?~  t.pax  ~&(%invalid-circle-path !!)
  :-  [%$ %ta `@ta`i.t.pax]
  ~
  ::=+  tmp=((hard range) t.t.pax)
  ::?~  tmp  ~
  :::-  hed.u.tmp
  ::?~  t.u.tmp  ~
  ::[tal.u.t.u.tmp ~]
::
++  tmp-parse-peer-path                                 ::<  ...
  ::>
  ::
  |=  pax/path
  ^-  (pair knot circle)
  ?.  ?=({$circle @ta @ta @ta *} pax)
    ~&(%invalid-peer-path !!)
  :-  i.t.pax
  :-  (slav %p i.t.t.pax)
  i.t.t.t.pax
::
++  leak                                                ::<  visible to
  ::>
  ::
  |=  {who/ship weg/(list coin)}
  ^-  ?
  ::TODO
  ::?:  ?=({$reader *} pax)
  ::  ?.  (team our.bol her)
  ::    %-  ta-note
  ::    (crip "foreign reader {(scow %p her)}")
  ::  (ta-welcome ost.bol t.pax)
  ::::  weird subscription path.
  ::?.  ?=({@ *} pax)
  ::  (ta-evil %bad-path)
  ::=+  pur=(~(get by stories) i.pax)
  ::?~  pur
  ::  ::TODO  send this to the subscriber! make them unsub!
  ::  %-  ta-note
  ::  (crip "bad subscribe story '{(trip i.pax)}'")
  ::=+  soy=~(. so i.pax `(list action)`~ u.pur)        ::  nest-fail if no cast
  ::::  she needs read permissions to subscribe.
  ::?.  (so-visible:soy her)
  ::  (ta-evil %no-story)
  &
::
::>  ||
::>  ||  %poke-events
::>  ||
::+|
::
++  poke-talk-command                                   ::<  accept command
  ::>  incoming talk command. process it and update logs.
  ::
  |=  cod/command
  ^-  (quip move +>)
  =^  mos  +>.$
    %-  f-bake  :-  %more
    ta-done:(ta-apply:ta src.bol cod)
  =^  mow  +>.$
    log-all-to-file
  [(welp mos mow) +>.$]
::
++  poke-talk-action                                    ::<  accept action
  ::>  incoming talk action. process it.
  ::
  |=  act/action
  ^-  (quip move +>)
  ?.  (team src.bol our.bol)
    %-  f-bake  :-  %more
    =<  ta-done
    %-  ta-note:ta  %-  crip
    "talk-action stranger {(scow %p src.bol)}"
  %-  f-bake  :-  %more
  ta-done:(ta-action:ta ost.bol act)
::
::>  ||
::>  ||  %subscription-events
::>  ||
::+|
::
++  diff-talk-rumor                                     ::<  accept rumor
  ::>
  ::
  |=  {wir/wire dif/rumor}
  ^-  (quip move +>)
  =^  mos  +>.$
    %-  f-bake  :-  %more
    ::TODO  parse wire to get source and target of change
    =+  res=(tmp-parse-peer-path wir)
    ta-done:(ta-hear:ta p.res [%& q.res] dif)
  =^  mow  +>.$
    log-all-to-file
  [(welp mos mow) +>.$]
::
++  peer                                                ::<  accept subscription
  ::>  incoming subscription on {pax}.
  ::
  |=  pax/path
  ^-  (quip move +>)
  ?:  ?=({$sole *} pax)  ~&(%talk-broker-no-sole !!)
  =+  qer=(tmp-parse-path pax)
  ?.  (leak src.bol qer)  ~&(%peer-invisible !!)
  =^  mos  +>.$
    %-  f-bake  :-  %more
    ta-done:(ta-subscribe:ta src.bol pax)
  :_  +>.$
  :_  mos
  [ost.bol %diff %talk-prize (g-query qer)]

::
++  pull                                                ::<  unsubscribe
  ::>  unsubscribes.
  ::
  |=  pax/path
  ^-  (quip move +>)
  %-  f-bake  :-  %more
  ta-done:(ta-cancel:ta src.bol pax)
::
++  reap-friend                                         ::<  subscription n/ack
  ::>  if subscribing failed, update state to reflect
  ::>  that.
  ::
  ::TODO  this should deal with /reader subscriptions too.
  |=  {wir/wire fal/(unit tang)}
  ^-  (quip move +>)
  ?~  fal  [~ +>]
  %+  etch-friend  [%friend wir]
  |=  {nom/knot cir/circle}
  =.  u.fal  [>%reap-friend-fail nom cir< u.fal]
  %-  (slog (flop u.fal))
  %-  f-bake  :-  %more
  ta-done:(ta-leave:ta nom cir)
::
++  quit-friend                                         ::<  dropped subscription
  ::>  gall dropped our subscription. resubscribe.
  ::
  |=  wir/wire
  ^-  (quip move +>)
  %+  etch-friend  [%friend wir]
  |=  {nom/knot cir/circle}
  ::TODO  do better
  :_  +>.^$  :_  ~
  :*  0
      %peer
      /circle/[nom]/(scot %p hos.cir)/[nom.cir]
      [hos.cir %talk-guardian]
      /circle/[nom.cir]/(sub now.bol ~h1)
  ==
::
++  coup-repeat                                         ::<  message n/ack
  ::>  ack from ++ta-transmit. mark the message as
  ::>  received or rejected.
  ::
  |=  {wir/wire fal/(unit tang)}
  ^-  (quip move +>)
  %+  etch-repeat  [%repeat wir]
  |=  {num/@ud src/ship nom/knot}
  (f-bake %done (strap num [%& src nom] ?=($~ fal)))
::
::>  ||
::>  ||  %logging
::>  ||
::+|
::
++  poke-talk-save                                      ::<  save as log
  ::>  stores the telegrams of story {nom} in a log file,
  ::>  to be re-loaded by ++poke-talk-load.
  ::
  |=  nom/knot
  ^-  (quip move +>)
  =/  paf/path
    /(scot %p our.bol)/home/(scot %da now.bol)/talk/[nom]/talk-telegrams
  =+  grams:(~(got by stories) nom)
  :_  +>.$
  :_  ~
  :*  ost.bol
      %info
      /jamfile
      our.bol
      (foal paf [%talk-telegrams !>(-)])
  ==
::
++  poke-talk-load                                      ::<  load from log
  ::>  loads the telegrams of story {nom} into our state,
  ::>  as saved in ++poke-talk-save.
  ::
  |=  nom/knot
  ^-  (quip move +>)
  =/  grams
    .^  (list telegram)
        %cx
        /(scot %p our.bol)/home/(scot %da now.bol)/talk/[nom]/talk-telegrams
    ==
  =+  soy=(~(got by stories) nom)
  :-  ~
  %=  +>.$
      stories
    %+  ~(put by stories)  nom
    soy(grams grams, count (lent grams))
  ==
::
++  poke-talk-log                                       ::<  start logging
  ::>  starts logging story {nom}'s messages.
  ::
  |=  nom/knot
  ~&  %talk-poke-log
  ^-  (quip move +>)
  :-  [(log-to-file nom) ~]
  %=  +>.$
      log
    %+  ~(put by log)  nom
    count:(~(got by stories) nom)
  ==
::
++  poke-talk-unlog                                     ::<  stop logging
  ::>  stops logging story {nom}'s messages.
  ::
  |=  nom/knot
  ^-  (quip move +>)
  :-  ~
  +>.$(log (~(del by log) nom))
::
++  log-all-to-file                                     ::<  update stories logs
  ::>  for every story we're logging, (over)writes all
  ::>  their grams to log files if new ones have arrived.
  ::
  ::TODO  re-enable and test.
  ^-  (quip move .)
  ?:  &  [~ .]  ::  XXX!!!!
  :_  %_  .
          log
        %-  ~(urn by log)
        |=  {nom/knot len/@ud}
        count:(~(got by stories) nom)
      ==
  %+  murn  (~(tap by log))
  |=  {nom/knot len/@ud}
  ^-  (unit move)
  ?:  (gte len count:(~(got by stories) nom))
    ~
  `(log-to-file nom)
::
++  log-to-file                                         ::<  update story log
  ::>  logs all grams of story {nom} to a file.
  ::
  |=  nom/knot
  ^-  move
  =+  ^-  paf/path
      =+  day=(year %*(. (yore now.bol) +.t +:*tarp))
      %+  tope  [our.bol %home da+now.bol]
      /talk-telegrams/(scot %da day)/[nom]/talk
  =+  grams:(~(got by stories) nom)
  [ost.bol %info /jamfile our.bol (foal paf [%talk-telegrams !>(-)])]
--
