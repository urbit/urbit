::                                                      ::  ::
::::  /hoon/talk/app                                    ::  ::
  ::                                                    ::  ::
::
::TODO  rename to hall
::TODO  master changes
::TODO  char57 comments as line comments when regarding code.
::TODO  document what user-facing messages actually mean!
::TODO  maybe have brokers accept reactions as well, redirect them to readers.
::      that way we can have foreign brokers react to our requests!
::TODO  document gate samples fully.
::TODO  ::> to :> etc.
::
::TODO  crash on pokes/peers we do not expect
::
::TODO  for using moons as tmp identities for friends: stories may want to keep
::      lists of moons (or just ships in general?) that we define as "standalone"
::      so that the "convert to true identity" doesn't happen for them.
::
::TODO/REVIEW  rename porch/floor/court to inbox?
::
/?    151                                               ::<  hoon version
/-    talk                                              ::<  structures
/+    talk, time-to-id                                  ::<  libraries
/=    seed  /~  !>(.)
!:
::::
  ::
=,  talk
=>  ::>  ||
    ::>  ||  %arch
    ::>  ||
    ::>    data structures
    ::
    |%
    ++  state                                           ::>  broker state
      $:  stories/(map naem story)                      ::<  conversations
          outbox/(map serial tracking)                  ::<  sent messages
          log/(map naem @ud)                            ::<  logged to clay
          nicks/(map ship nick)                         ::<  local nicknames
          binds/(jug char (set circle))                 ::<  circle glyph lookup
      ==                                                ::
    ++  story                                           ::>  wire content
      $:  count/@ud                                     ::<  (lent grams)
          grams/(list telegram)                         ::<  all messages
          locals/group                                  ::<  local status
          remotes/(map circle group)                    ::<  remote status
          shape/config                                  ::<  configuration
          mirrors/(map circle config)                   ::<  remote config
          sequence/(map circle @ud)                     ::<  last-heard p circle
          known/(map serial @ud)                        ::<  messages heard
          inherited/_|                                  ::<  from parent?
      ==                                                ::
    ++  move  (pair bone card)                          ::<  all actions
    ++  lime                                            ::>  diff fruit
      $%  {$talk-prize prize}                           ::
          {$talk-rumor rumor}                           ::
      ==                                                ::
    ++  pear                                            ::>  poke fruit
      $%  {$talk-command command}                       ::
          {$write-comment spur ship cord}               ::
          {$write-fora-post spur ship cord cord}        ::
      ==                                                ::
    ++  card                                            ::>  general card
      $%  {$diff lime}                                  ::
          {$info wire ship term nori:clay}              ::
          {$peer wire dock path}                        ::
          {$poke wire dock pear}                        ::
          {$pull wire dock $~}                          ::
          {$quit $~}                                    ::
      ==                                                ::
    ++  weir                                            ::>  parsed wire
      $%  {$repeat cir/circle ses/(list serial)}        ::<  messaging wire
          {$circle nom/naem src/source}                 ::<  subscription wire
      ==                                                ::
    --
::
::>  ||
::>  ||  %work
::>  ||
::>    functional cores and arms.
::
|_  {bol/bowl:gall state}
::
++  prep                                                ::<  prepare state
  ::>  adapts state.
  ::
  |=  old/(unit state)
  ^-  (quip move _..prep)
  ?~  old
    %-  pre-bake
    ta-done:ta-init:ta
  [~ ..prep(+<+ u.old)]
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
  |_  ::>  deltas: deltas created by core operations.
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
    |=  det/delta
    %_(+> deltas [det deltas])
  ::
  ++  ta-deltas                                         ::<  emit delta list
    ::>  adds multiple deltas to the head of {deltas}.
    ::>  flops to stay consistent with ++ta-delta.
    ::
    |=  des/(list delta)
    %_(+> deltas (welp (flop des) deltas))
  ::
  ++  ta-note                                           ::<  tell user
    ::>  sends {msg} as an %app message to the user's
    ::>  inbox.
    ::
    |=  msg/cord
    %^  ta-action  0  %phrase
    :-  [[our.bol (main our.bol)] ~ ~]
    [%app dap.bol msg]~
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
  ++  ta-know                                           ::<  story monad
    ::>  produces a gill that takes a gate.
    ::>  if the story {nom} exists, calls the gate with
    ::>  a story core. if it doesn't, does nothing.
    ::
    |=  nom/naem
    |=  fun/$-(_so _ta)
    ^+  +>+>
    =+  pur=(~(get by stories) nom)
    ?~  pur
      %-  ta-evil
      (crip "no story '{(trip nom)}'")
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
    ::  create default circles.
    =>  %+  roll
          ^-  (list {security naem cord})
          :~  [%brown (main our.bol) 'default home']
              [%green %public 'visible activity']
          ==
        |=  {{typ/security nom/naem des/cord} _ta}
        %+  ta-action  ost.bol
        [%create nom des typ]
    %-  ta-deltas
    ::  if needed, subscribe to our parent's /burden.
    =+  sen=(above our.bol)
    ?:  ?|  !=(%czar (clan:title sen))
            =(sen our.bol)
            =(%pawn (clan:title our.bol))
        ==
      ~
    [%init ~]~
  ::
  ++  ta-apply                                          ::<  apply command
    ::>  applies the command sent by {src}.
    ::
    |=  {src/ship cod/command}
    ^+  +>
    ?-  -.cod
      ::>  %publish commands prompt us (as a circle host)
      ::>  to verify and distribute messages.
      $publish  (ta-think | src +.cod)
      ::>  %present commands are used to ask us to set
      ::>  someone's status in the indicated stories.
      $present  (ta-present src +.cod)
      ::>  %bearing commands are used by our children to
      ::>  let us know they're bearing our /burden. we
      ::>  need to watch them to allow changes to go up.
      $bearing  (ta-observe src)
    ==
  ::
  ++  ta-present                                        ::<  update a status
    ::>
    ::
    |=  {who/ship nos/(set naem) dif/diff-status}
    ^+  +>
    %-  ta-deltas
    %-  ~(rep in nos)
    |=  {n/naem l/(list delta)}
    :_  l
    [%story n %status [our.bol n] who dif]
  ::
  ++  ta-action                                         ::<  apply reader action
    ::>  performs action sent by a reader.
    ::
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
    ::>  story, crashing if it doesn't exist.
    |%
    ::  ||  %utility
    ::+|
    ++  work                                            ::<  perform action
      ^+  ..ta-action
      ::TODO  require deltas as product!
      ?-  -.act
        ::  circle configuration
        $create  (action-create +.act)
        $source  (action-source +.act)
        $depict  (action-depict +.act)
        $filter  (action-filter +.act)
        $permit  (action-permit +.act)
        $delete  (action-delete +.act)
        ::  messaging
        $convey  (action-convey +.act)
        $phrase  (action-phrase +.act)
        ::  personal metadata
        $notify  (action-notify +.act)
        $naming  (action-naming +.act)
        ::  changing shared ui
        $glyph   (action-glyph +.act)
        $nick    (action-nick +.act)
      ==
    ::
    ++  affect                                          ::<  delta to story
      ::>  store a delta about a story. if the story
      ::>  does not exist, crash.
      ::
      |=  {nom/naem det/delta-story}
      ?:  (~(has by stories) nom)
        (impact nom det)
      (ta-evil (crip "no story {(trip nom)}"))
    ::
    ++  impact                                          ::<  delta for story
      ::>  Store a delta about a story.
      ::
      |=  {nom/naem det/delta-story}
      (ta-delta %story nom det)
    ::
    ++  present                                         ::<  send status update
      ::>
      ::
      |=  {cis/(set circle) dif/diff-status}
      ^+  ..ta-action
      =/  cic
        ^-  (jug ship naem)
        %-  ~(rep in cis)
        |=  {c/circle m/(jug ship naem)}
        (~(put ju m) hos.c nom.c)
      =.  ..ta-action  ::TODO  =?
        ?.  (~(has by cic) our.bol)  ..ta-action
        %-  ~(rep in (~(get ju cic) our.bol))
        |=  {n/naem _ta}
        (affect n %status [our.bol n] our.bol dif)
      =.  cic  (~(del by cic) our.bol)
      %-  ta-deltas
      %-  ~(rep by cic)
      |=  {{h/ship s/(set naem)} l/(list delta)}
      :_  l
      [%present h s dif]
    ::
    ::>  ||  %circle-configuration
    ::+|
    ++  action-create                                   ::<  create story
      ::>  creates a story with the specified parameters.
      ::
      |=  {nom/naem des/cord typ/security}
      ^+  ..ta-action
      ?.  (~(has in stories) nom)
        %^  impact  nom  %new
        :*  [[[our.bol nom] ~] ~ ~]
            des
            *filter
            :-  typ
            ?.  ?=(?($white $green) typ)  ~
            [our.bol ~ ~]
        ==
      (ta-evil (crip "{(trip nom)}: already exists"))
    ::
    ++  action-delete                                   ::<  delete + announce
      ::>  delete story {nom}, optionally announcing the
      ::>  event with message {mes}.
      ::
      |=  {nom/naem mes/(unit cord)}
      ^+  ..ta-action
      =.  ..ta-action  ::TODO  =?
        ?~  mes  ..ta-action
        %+  action-phrase
          [[our.bol nom] ~ ~]
        [%lin | u.mes]~
      (affect nom %remove ~)
    ::
    ++  action-depict                                   ::<  change description
      ::>  change description of story {nom} to {des}.
      ::
      |=  {nom/naem cap/cord}
      (affect nom %config [our.bol nom] %caption cap)
    ::
    ++  action-filter                                   ::<  change message rules
      ::>  replaces the story's current filter with the
      ::>  specified one.
      ::
      |=  {nom/naem fit/filter}
      (affect nom %config [our.bol nom] %filter fit)
    ::
    ++  action-permit                                   ::<  invite/banish
      ::>  invite to/banish from story {nom} all {sis}.
      ::
      |=  {nom/naem inv/? sis/(set ship)}
      =+  soy=(~(get by stories) nom)
      ?~  soy
        (ta-evil (crip "no story {(trip nom)}"))
      so-done:(~(so-permit so nom ~ u.soy) inv sis)
    ::
    ++  action-source                                   ::<  un/sub p to/from r
      ::>  add/remove {pos} as sources for story {nom}.
      ::
      |=  {nom/naem sub/? srs/(set source)}
      =+  soy=(~(get by stories) nom)
      ?~  soy
        (ta-evil (crip "no story {(trip nom)}"))
      so-done:(~(so-sources so nom ~ u.soy) sub srs)
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
      ::>  action generating a serial and setting a
      ::>  timestamp.
      ::
      |=  {cis/(set circle) ses/(list speech)}
      ^+  ..ta-action
      =-  (ta-think & our.bol tos)
      |-  ^-  tos/(list thought)
      ?~  ses  ~
      =^  sir  eny.bol  (uniq eny.bol)
      :_  $(ses t.ses)
      [sir cis [now.bol i.ses]]
    ::
    ::>  ||  %personal-metadata
    ::+|
    ::
    ++  action-notify                                   ::<  our presence update
      ::>
      ::
      |=  {cis/(set circle) pes/presence}
      ^+  ..ta-action
      (present cis %presence pes)
    ::
    ++  action-naming                                   ::<  our name update
      ::>
      ::
      |=  {cis/(set circle) man/human}
      ^+  ..ta-action
      (present cis %human %full man)
    ::
    ::>  ||  %changing-shared-ui
    ::+|
    ++  action-nick                                     ::<  new identity
      ::>  assigns a new local identity ("nickname") to the
      ::>  target ship.
      ::
      |=  {who/ship nic/nick}
      ^+  ..ta-action
      ?.  =((~(get by nicks) who) `nic)  ..ta-action    ::<  no change
      (ta-delta %nick who nic)
    ::
    ++  action-glyph                                    ::<  bind a glyph
      ::>  un/bind glyph {lif} to circles {cis}.
      ::
      |=  {lif/char cis/(set circle) bin/?}
      (ta-delta %glyph bin lif cis)
    --
  ::
  ::>  ||
  ::>  ||  %subscription-events
  ::>  ||
  ::>    arms that react to subscription events.
  ::+|
  ::
  ++  ta-observe                                        ::<  watch burden bearer
    ::>  subscribe to a child who is bearing our burden.
    ::
    |=  who/ship
    ^+  +>
    ?.  =(our.bol (above who))
      ~&([%not-our-bearer who] +>)
    (ta-delta %observe who)
  ::
  ++  ta-subscribe                                      ::<  listen to
    ::>  add her to a status list if applicable.
    ::
    |=  {her/ship qer/query}
    ^+  +>
    ?+  -.qer
      +>
      ::
        $burden
      (ta-observe her)
      ::
        $circle
      %-  (ta-know nom.qer)  |=  sor/_so  =<  so-done
      (so-attend:sor her %hear [~ ~])
    ==
  ::
  ++  ta-cancel                                         ::<  forget
    ::>  drops {src}'s subscription. deduce the right way
    ::>  to do this from the subscription path {pax}.
    ::
    |=  {src/ship nom/naem}
    ^+  +>
    ::  set ship status to %gone.
    %-  (ta-know nom)  |=  sor/_so  =<  so-done
    (so-absent:sor src)
  ::
  ++  ta-greet                                          ::<  subscription success
    ::>  store a started subscription as source.
    ::
    |=  {nom/naem src/source}
    %-  (ta-know nom)  |=  sor/_so  =<  so-done
    (so-greet:sor src)
  ::
  ++  ta-leave                                          ::<  subscription failed
    ::>  removes {src} from story {nom}'s sources.
    ::
    |=  {nom/naem src/source}
    %-  (ta-know nom)  |=  sor/_so  =<  so-done
    (so-leave:sor src)
  ::
  ++  ta-take                                           ::<  apply prize
    ::>  for a %burden prize, bear the burden in a new
    ::>  or existing story.
    ::>  for a %circle prize, use ++so to accept it.
    ::
    |=  {wir/wire piz/prize}
    ^+  +>
    ?+  -.piz
      ~&([%ignoring-prize -.piz] +>)
      ::
        $burden
      %+  roll  ~(tap by sos.piz)
      |=  {{n/naem b/burden} _..ta-take}
      =<  so-done
      (~(so-bear so n ~ (fall (~(get by stories) n) *story)) b)
      ::
        $circle
      =+  wer=(etch wir)
      ?>  ?=($circle -.wer)
      %-  (ta-know nom.wer)  |=  sor/_so  =<  so-done
      (so-take:sor cir.src.wer +.piz)
    ==
  ::
  ++  ta-hear                                           ::<  apply rumor
    ::>  apply changes from a rumor to our state.
    ::>  for %burden, authoratively apply the story
    ::>  diff. if it's a new one, bear it.
    ::>  for %circle, apply the story diff normally.
    ::
    |=  {wir/wire rum/rumor}
    ^+  +>
    ?+  -.rum
      ~&([%ignoring-rumor -.rum] +>)
      ::
        $burden
      ?+  -.rum.rum
        %-  (ta-know nom.rum)  |=  sor/_so  =<  so-done
        (so-hear:sor & [our.bol nom.rum] rum.rum)
        ::
          $new
        =<  so-done
        %-  ~(so-bear so nom.rum ~ (fall (~(get by stories) nom.rum) *story))
        [~ [cof.rum.rum ~] [~ ~]]
      ==
      ::
        $circle
      =+  wer=(etch wir)
      ?>  ?=($circle -.wer)
      %-  (ta-know nom.wer)  |=  sor/_so  =<  so-done
      (so-hear:sor | cir.src.wer rum.rum)
    ==
  ::
  ++  ta-repeat                                         ::<  message delivered
    ::>  message got delivered. if an error was returned
    ::>  mark the message as rejected. if not, received.
    ::
    |=  {who/circle ses/(list serial) fal/(unit tang)}
    ^+  +>
    ~?  ?=(^ fal)  u.fal
    =-  (ta-delta %done who ses -)
    ?~  fal  %accepted
    ~>(%slog.[0 u.fal] %rejected)
  ::
  ++  ta-resub                                          ::<  subscription dropped
    ::>  when a subscription gets dropped by gall, we
    ::>  resubscribe.
    ::
    |=  {nom/naem src/source}
    ^+  +>
    %-  (ta-know nom)  |=  sor/_so  =<  so-done
    (so-resub:sor src)
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
    ::>  conducts thought {tot} to each circle in its audience.
    ::
    |=  {pub/? aut/ship tot/thought}
    =+  aud=~(tap in aud.tot)
    |-  ^+  +>.^$
    ?~  aud  +>.^$
    $(aud t.aud, +>.^$ (ta-conduct pub aut i.aud tot))
  ::
  ++  ta-conduct                                        ::<  thought to circle
    ::>  either publishes or records a thought.
    ::
    |=  {pub/? aut/ship cir/circle tot/thought}
    ^+  +>
    ?:  pub
      ?.  (team:title our.bol aut)
        %-  ta-note
        (crip "strange author {(scow %p aut)}")
      =.  aut  our.bol
      ?:  =(aut hos.cir)
        (ta-record nom.cir hos.cir tot)
      (ta-transmit cir tot)
    ?.  =(our.bol hos.cir)  +>
    (ta-record nom.cir aut tot)
  ::
  ++  ta-record                                         ::<  add to story
    ::>  add or update telegram {gam} in story {nom}.
    ::
    |=  {nom/naem gam/telegram}
    %-  (ta-know nom)  |=  sor/_so  =<  so-done
    (so-learn:sor gam)
  ::
  ++  ta-transmit                                       ::<  send message
    ::>  sends thought {tot} to {cir}.
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
        $:  nom/naem
            acs/(list action)
            story
        ==
    ::
    ++  so-done                                         ::<  apply changes
      ::>  put changed story back into the map and apply
      ::>  actions.
      ::TODO  update inline docs
      ::TODO  maybe produce list of actions, apply in ++ta
      ::
      ^+  +>
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
      :-  [[our.bol (main our.bol)] ~ ~]
      [%app dap.bol msg]~
    ::
    ++  so-delta                                        ::<  send delta
      ::>  store delta in ++ta core.
      ::
      ::TODO  maybe keep our own list of deltas, produce in done
      |=  det/delta
      ^+  +>
      +>(deltas [det deltas])
    ::
    ++  so-deltas                                       ::<  send delta list
      ::>  store multiple deltas in ++ta core.
      ::
      |=  des/(list delta)
      %_(+> deltas (welp (flop des) deltas))
    ::
    ++  so-delta-our                                    ::<  send delta of us
      ::>  adds a delta about this story.
      ::
      |=  det/delta-story
      ^+  +>
      (so-delta %story nom det)
    ::
    ++  so-deltas-our                                   ::<  send deltas of us
      ::>  adds multiple deltas about this story.
      ::
      |=  des/(list delta-story)
      ^+  +>
      %-  so-deltas
      %+  turn  des
      |=  d/delta-story
      [%story nom d]
    ::
    ::>  ||
    ::>  ||  %data
    ::>  ||
    ::>    utility functions for data retrieval.
    ::+|
    ::
    ++  so-cir  [our.bol nom]                           ::<  us as circle
    ::
    ::>  ||
    ::>  ||  %interaction-events
    ::>  ||
    ::>    arms that apply events we received.
    ::+|
    ::
    ++  so-take                                         ::<  accept circle prize
      ::>  apply the prize as if it were rumors.
      ::
      |=  {src/circle nes/(list envelope) cos/lobby pes/crowd}
      ^+  +>
      =.  +>.$
        (so-hear | src %config src %full loc.cos)
      =.  +>.$
        =+  los=~(tap by loc.pes)
        |-
        ?~  los  +>.^$
        =.  +>.^$
          (so-hear | src %status src p.i.los %full q.i.los)
        $(los t.los)
        ::TODO  ideally you'd just do this, but that runtime errors...
        ::%-  ~(rep in loc.pes)
        ::|=  {{w/ship s/status} _+>.$}
        ::(so-hear | src %status src w %full s)
      (so-unpack src nes)
    ::
    ++  so-hear                                         ::<  accept circle rumor
      ::>  apply changes from a rumor to this story.
      ::
      |=  {bur/? src/circle rum/rumor-story}
      ::TODO  tall-form gate comments like this for everything?
      ::|=  $:  ::>  bur: whether the story is inherited
      ::        ::>  src: story to change
      ::        ::>  rum: change to this story
      ::        ::
      ::        bur/?
      ::        src/circle
      ::        rum/rumor-story
      ::    ==
      ^+  +>
      ?-  -.rum
        $new      ?:  =(src so-cir)
                    (so-config-full ~ cof.rum)
                  $(rum [%config src %full cof.rum])
        $bear     (so-bear bur.rum)
        $gram     (so-open src nev.rum)
        $config   ::  full changes to us need to get split up.
                  ?:  &(=(cir.rum so-cir) ?=($full -.dif.rum))
                    (so-config-full `shape cof.dif.rum)
                  ::  remotes are fine.
                  ?:  =(src cir.rum)
                    (so-delta-our rum)
                  ::  remotes of remotes are not.
                  ~?  =(src so-cir)  %but-src-is-us
                  +>
        $status   ::  ignore foreign remotes.
                  ?.  |(=(src cir.rum) =(src so-cir))  +>
                  (so-delta-our rum)
        $remove   (so-delta-our %config src %remove ~)
      ==
    ::
    ++  so-bear                                         ::<  accept burden
      ::>  add what was pushed down from above to our
      ::>  state. in case of conflict, existing data is
      ::>  overwritten.
      ::
      |=  {gaz/(list telegram) cos/lobby pes/crowd}
      ^+  +>
      =*  self  +>
      ::  local config
      =.  self
        (so-config-full `shape loc.cos)
      ::  remote config
      =.  self
        %+  roll  ~(tap by rem.cos)
        |=  {{r/circle c/config} _self}
        (so-delta-our %config r %full c)
      ::  local status
      =.  self
        %+  roll  ~(tap by loc.pes)
        |=  {{w/ship s/status} _self}
        (so-delta-our %status so-cir w %full s)
      ::  remote status
      =.  self
        %+  roll  ~(tap by rem.pes)
        |=  {{c/circle g/group} _self}
        %+  roll  ~(tap by g)
        |=  {{w/ship s/status} _self}
        (so-delta-our %status c w %full s)
      ::  telegrams
      =.  self
        %-  so-deltas-our
        %+  turn  gaz
        |=  t/telegram
        ^-  delta-story
        :-  %gram
        ::  in audience, replace above with us.
        =-  t(aud -)
        =+  (~(del in aud.t) [(above our.bol) nom])
        (~(put in -) so-cir)
      ::  inherited flag
      (so-delta-our %inherited &)
    ::
    ::>  ||
    ::>  ||  %changes
    ::>  ||
    ::>    arms that make miscellaneous changes to this story.
    ::+|
    ::
    ++  so-config-full                                  ::<  split full config
      ::>  split a %full config delta up into multiple
      ::>  smaller ones, for easier application.
      ::TODO  mention side-effects
      ::
      |=  {old/(unit config) cof/config}
      ^+  +>
      %-  so-deltas
      %+  turn
        %+  weld
          ^-  (list delta-story)
          ?~  old  ~
          ::TODO?  what to do about src?
          :~  ::[%follow | src.u.old]
              [%config so-cir %permit | ses.con.u.old]
          ==
        ^-  (list delta-story)
        :~  ::[%follow & src.cof]
            [%config so-cir %caption cap.cof]
            [%config so-cir %filter fit.cof]
            [%config so-cir %secure sec.con.cof]
            [%config so-cir %permit & ses.con.cof]
        ==
      |=  d/delta-story
      [%story nom d]
    ::
    ++  so-sources                                      ::<  change source
      ::>  adds or removes {srs} from our sources,
      ::>  skipping over ones we already (don't) have.
      ::
      |=  {add/? srs/(set source)}
      ^+  +>
      =/  sus/(set source)
        %.  src.shape
        ?:(add ~(dif in srs) ~(int in srs))
      ~&  [%sources src.shape]
      ~&  [%change add sus]
      ::  we only make a delta for removals here,
      ::  because we don't need to wait for ++reap when
      ::  pulling subscriptions.
      =?  +>.$  !add
        =+  sos=~(tap in sus)
        |-  ^+  +>.^$
        ?~  sos  +>.^$
        =.  +>.^$  (so-delta-our %config so-cir %source | i.sos)
        $(sos t.sos)
        ::TODO  ideally below, but unexplained runtime error at `so-delta-our`
        ::%+  roll  ~(tap in sus)
        ::|=  {src/source _+>.$}
        ::^+  +>.^$
        ::(so-delta-our %config so-cir %source | src)
      ?~  sus  +>.$
      (so-delta-our %follow add sus)
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
    ++  so-delete                                       ::<  delete story
      ::>  deletes this story. removes it from {stories}
      ::>  and unsubscribes from all src.
      ::
      (so-delta-our %remove ~)
    ::
    ++  so-attend                                       ::<  add local status
      ::>  add {her} status to this story's status map.
      ::
      |=  {her/ship sat/status}
      ^+  +>
      ?:  =(`sat (~(get by locals) her))  +>.$
      (so-delta-our %status so-cir her %full sat)
    ::
    ++  so-absent                                       ::<  del local status
      ::>  remove {her} from our status map.
      ::
      |=  her/ship
      ^+  +>
      ?.  (~(has by locals) her)  +>
      (so-delta-our %status so-cir her %remove ~)
    ::
    ::>  ||
    ::>  ||  %subscriptions
    ::>  ||
    ::>    arms for starting and ending subscriptions
    ::+|
    ::
    ++  so-greet                                        ::<  subscription started
      ::>  store a started subscription as source.
      ::
      |=  src/source
      ^+  +>
      ?:  (~(has in src.shape) src)  +>
      (so-delta-our %config so-cir %source & src)
    ::
    ++  so-leave                                        ::<  subscription ended
      ::>  delete {src} from our sources.
      ::
      |=  src/source
      ^+  +>
      ?.  (~(has in src.shape) src)  +>
      (so-delta-our %config so-cir %source | src)
    ::
    ++  so-resub                                        ::<  subscription revived
      ::>  re-subscribe to a dropped subscription.
      ::>  if it was already active, we continue where
      ::>  we left off.
      ::
      |=  src/source
      ^+  +>
      =-  (so-delta-our %follow & [[cir.src -] ~ ~])
      ^-  range
      ?.  (~(has by sequence) cir.src)  ran.src
      =-  `[[%ud (~(got by sequence) cir.src)] -]
      ?~  ran.src  ~
      tal.u.ran.src
    ::
    ++  so-first-grams                                  ::<  beginning of stream
      ::>  find all grams that fall within the range.
      ::
      ::TODO  strip unused vars.
      |=  ran/range
      ^-  (list telegram)
      =+  [num=0 gaz=grams zeg=*(list telegram)]
      ::  fill in empty ranges to select all grams.
      =.  ran
        ?~  ran  `[[%ud 0] `[%ud count]]
        ?~  tal.u.ran  `[hed.u.ran `[%ud count]]
        ran
      ::  never fails, but compiler needs it.
      ?>  &(?=(^ ran) ?=(^ tal.u.ran))
      %-  flop
      |-  ^-  (list telegram)
      ?~  gaz  zeg
      ?:  ?-  -.u.tal.u.ran                             ::  after the end
            $ud  (lth +.u.tal.u.ran num)
            $da  (lth +.u.tal.u.ran wen.i.gaz)
          ==
        ::  if past the river, we're done searching.
        zeg
      ?:  ?-  -.hed.u.ran                               ::  before the start
            $ud  (lth num +.hed.u.ran)
            $da  (lth wen.i.gaz +.hed.u.ran)
          ==
        ::  if before the river, continue onward.
        $(num +(num), gaz t.gaz)
      ::  if in the river, add this gram and continue.
      $(num +(num), gaz t.gaz, zeg [i.gaz zeg])
    ::
    ++  so-in-range                                     ::<  place in range?
      ::>  produces two booleans: whether we're
      ::>  currently in the range, and whether the range
      ::>  has passed.
      ::TODO  to deal with changed messages, we'll want
      ::      to be able to pass in a num.
      ::
      |=  ran/range
      ^-  {in/? done/?}
      ?~  ran  [& |]
      =/  min
        ?-  -.hed.u.ran
          $ud  (gth count +.hed.u.ran)
          $da  (gth now.bol +.hed.u.ran)
        ==
      ?~  tal.u.ran
        [min |]
      =-  [&(min -) !-]
      ?-  -.u.tal.u.ran
        $ud  (gte +(+.u.tal.u.ran) count)
        $da  (gte +.u.tal.u.ran now.bol)
      ==
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
      |=  sep/speech
      ^-  speech
      ?.  ?=($lin -.sep)  sep
      %_  sep
          msg
        %-  crip
        %-  tufa
        %+  turn  (tuba (trip msg.sep))
        |=  a/@c
        ::  always replace control characters.
        ?:  |((lth a 32) =(a `@c`127))
          `@`'?'
        ::  if desired, remove uppercasing.
        ?:  ?&  !cas.fit.shape
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
    ++  so-unpack                                       ::<  process envelopes
      ::>  learn telegrams from list of envelopes and
      ::>  update the sequence of the source if needed.
      ::
      |=  {src/circle nes/(list envelope)}
      ^+  +>
      =.  +>  (so-lesson (turn nes tail))
      =/  num
        %+  roll  nes
        |=  {nev/envelope max/@ud}
        ?:((gth num.nev max) num.nev max)
      ?.  (gth num (fall (~(get by sequence) src) 0))
        +>.$
      (so-delta-our %sequent src num)
    ::
    ++  so-open                                         ::<  process envelope
      ::>  learn telegram from envelope and update the
      ::>  sequence of the source if needed.
      ::
      |=  {src/circle nev/envelope}
      ^+  +>
      =.  +>  (so-learn gam.nev)
      ?.  (gth num.nev (fall (~(get by sequence) src) 0))
        +>
      (so-delta-our %sequent src num.nev)
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
      ?.  (so-admire aut.gam)  +>
      ::  clean up the message to conform to our rules.
      =.  sep.gam  (so-sane sep.gam)
      (so-delta-our %gram gam)
    ::
    ::>  ||
    ::>  ||  %permissions
    ::>  ||
    ::>    arms relating to story permissions.
    ::+|
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
      =/  sus/(set ship)
        %.  ses.con.shape
        ?:(add ~(dif in sis) ~(int in sis))
      =.  +>.$
        %-  so-act
        :-  %phrase
        %-  ~(rep in sus)
        |=  {s/ship a/(set circle) t/(list speech)}
        :-  (~(put in a) [s (main s)])
        [[%inv inv so-cir] t]
      (so-delta-our %config so-cir %permit [add sus])
    ::
    ++  so-admire                                       ::<  accept from
      ::>  checks {her} write permissions.
      ::
      |=  her/ship
      ^-  ?
      ?-  sec.con.shape
        $black  !(~(has in ses.con.shape) her)          ::<  channel, blacklist
        $white  (~(has in ses.con.shape) her)           ::<  village, whitelist
        $green  (~(has in ses.con.shape) her)           ::<  journal, whitelist
        $brown  !(~(has in ses.con.shape) her)          ::<  mailbox, blacklist
      ==
    ::
    ++  so-visible                                      ::<  display to
      ::>  checks {her} read permissions.
      ::
      |=  her/ship
      ^-  ?
      ?-  sec.con.shape
        $black  !(~(has in ses.con.shape) her)          ::<  channel, blacklist
        $white  (~(has in ses.con.shape) her)           ::<  village, whitelist
        $green  &                                       ::<  journal, all
        $brown  (team:title our.bol her)                      ::<  mailbox, our team
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
    ^-  (quip move _+>)
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
  ++  da-present                                        ::<  send %present cmd
    ::>
    ::
    |=  {hos/ship nos/(set naem) dif/diff-status}
    ^+  +>
    %-  da-emit
    :*  ost.bol
        %poke
        /present
        [hos dap.bol]
        [%talk-command %present nos dif]
    ==
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
    |=  det/delta
    ^+  +>
    ?-  -.det
      $out      (da-change-out +.det)
      $done     (da-change-done +.det)
      $glyph    (da-change-glyph +.det)
      $nick     (da-change-nick +.det)
      $story    (da-change-story +.det)
      $init     da-init
      $observe  (da-observe +.det)
      $present  (da-present +.det)
      $quit     (da-emit [ost.det %quit ~])
    ==
  ::
  ++  da-init                                           ::<  startup side-effects
    ::>  apply %init delta, querying the /burden of the
    ::>  ship above us.
    ::
    %-  da-emit
    :*  0
        %peer
        /burden
        [(above our.bol) dap.bol]
        /burden/(scot %p our.bol)
    ==
  ::
  ++  da-observe                                        ::<  watch burden bearer
    ::>  apply %observe delta, querying the /report of
    ::>  {who} below us.
    ::
    |=  who/ship
    %-  da-emit
    :*  0
        %peer
        /report/(scot %p who)
        [who dap.bol]
        /report
    ==
  ::
  ++  da-change-out                                     ::<  outgoing messages
    ::>  apply an %out delta, sending a message.
    ::
    |=  {cir/circle out/(list thought)}
    ^+  +>
    =+  ses=(turn out head)
    =.  outbox
      ::  for every serial, add %pending state.
      %+  roll  ses
      |=  {s/serial o/_outbox}
      =.  o  ?~(o outbox o)  ::TODO  =?
      =+  t=(fall (~(get by o) s) *tracking)
      %+  ~(put by o)  s
      (~(put by t) cir %pending)
    %+  da-emit  ost.bol
    :*  %poke
        /repeat/(scot %p hos.cir)/[nom.cir]/(scot %ud (jam ses))
        [hos.cir dap.bol]
        [%talk-command %publish out]
    ==
  ::
  ++  da-change-done                                    ::<  delivered messages
    ::>  apply a %done delta, setting new delivery state
    ::>  for messages.
    ::
    |=  {cir/circle ses/(list serial) res/delivery}
    ^+  +>
    %_  +>
        outbox
      ::  for every serial, set new delivery state.
      %-  ~(gas by outbox)
      %+  turn  ses
      |=  s/serial
      :-  s
      %+  ~(put by (~(got by outbox) s))
      cir  res
    ==
  ::
  ++  da-change-glyph                                   ::<  un/bound glyph
    ::>  apply a %glyph delta, un/binding a glyph to/from
    ::>  a set of circles.
    ::
    |=  {bin/? gyf/char cis/(set circle)}
    ^+  +>
    ?:  bin
      %_  +>
        binds  (~(put ju binds) gyf cis)
      ==
    =/  ole/(list (set circle))
      ?.  =(cis ~)  [cis ~]
      ~(tap in (~(get ju binds) gyf))
    |-  ^+  +>.^$
    ?~  ole  +>.^$
    %_  $
      binds  (~(del ju binds) gyf i.ole)
      ole  t.ole
    ==
  ::
  ++  da-change-nick                                    ::<  changed nickname
    ::>  apply a %nick delta, setting a nickname for a
    ::>  ship.
    ::
    |=  {who/ship nic/nick}
    ^+  +>
    +>(nicks (change-nicks nicks who nic))
  ::
  ::>  ||
  ::>  ||  %stories
  ::>  ||
  ::>    arms for modifying stories.
  ::+|
  ::
  ++  da-change-story                                   ::<  apply circle delta
    ::>  apply a %story delta, redirecting the delta
    ::>  itself to ++sa-change.
    ::>  in case of a new or deleted story, specialized
    ::>  arms are called.
    ::
    |=  {nom/naem det/delta-story}
    ^+  +>
    ?+  -.det
      =<  sa-done
      %.  det
      =+  (fall (~(get by stories) nom) *story)
      ~(sa-change sa nom -)
      ::
      $new      (da-create nom +.det)
      $bear     ~&(%unexpected-unsplit-bear +>)
      $remove   (da-delete nom)
    ==
  ::
  ++  da-create                                         ::<  configure story
    ::>  creates story {nom} with config {con}.
    ::
    |=  {nom/naem cof/config}
    ^+  +>
    =<  sa-done
    %-  ~(sa-change sa nom *story)
    [%config [our.bol nom] %full cof]
  ::
  ++  da-delete                                         ::<  delete story
    ::>  calls the story core to delete story {nom}.
    ::
    |=  nom/naem
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
        $:  nom/naem
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
    ::>  ||  %data
    ::>  ||
    ::>    utility functions for data retrieval.
    ::+|
    ::
    ++  sa-cir  [our.bol nom]
    ::
    ::>  ||
    ::>  ||  %delta-application
    ::>  ||
    ::>    arms for applying deltas.
    ::+|
    ::
    ++  sa-delete                                       ::<  deletion of story
      ::>  apply a %remove story delta, unsubscribing
      ::>  this story from all its active sources.
      ::
      (sa-abjure src.shape)
    ::
    ++  sa-change                                       ::<  apply circle delta
      ::>  figure out whether to apply a %story delta to
      ::>  local or remote data.
      ::
      |=  det/delta-story
      ^+  +>
      %.  det
      ?:  ?&  ?=(?($config $status) -.det)
              !=(cir.det sa-cir)
          ==
        sa-change-remote
      sa-change-local
    ::
    ++  sa-change-local                                 ::<  apply our delta
      ::>  apply a %story delta to local data.
      ::
      |=  det/delta-story
      ^+  +>
      ?+  -.det
        ~&([%unexpected-delta-local -.det] !!)
        ::
          $inherited
        +>(inherited ihr.det)
        ::
          $follow
        (sa-emil (sa-follow-effects sub.det srs.det))
        ::
          $sequent
        +>(sequence (~(put by sequence) cir.det num.det))
        ::
        ::
          $gram
        (sa-change-gram gam.det)
        ::
          $config
        =.  +>
          %-  sa-emil
          (sa-config-effects shape dif.det)
        +>(shape (change-config shape dif.det))
        ::
          $status
        %_  +>
            locals
          %+  ~(put by locals)  who.det
          %+  change-status
            (fall (~(get by locals) who.det) *status)
          dif.det
        ==
      ==
    ::
    ++  sa-change-gram                                  ::<  save/update message
      ::>  apply a %gram delta, either appending or
      ::>  updating a message.
      ::
      |=  gam/telegram
      ^+  +>
      ::TODO  move "known" logic up into ++so? that way,
      ::      we can attach message numbers to changes.
      =+  old=(~(get by known) uid.gam)
      ?~  old
        ::  new message
        %_  +>.$
          grams    (welp grams [gam ~])
          count    +(count)
          known    (~(put by known) uid.gam count)
        ==
      ::  changed message
      %_  +>.$
        grams    %+  welp
                 (scag (dec u.old) grams)
                 [gam (slag u.old grams)]
      ==
    ::
    ++  sa-change-remote                                ::<  apply remote's delta
      ::>  apply a story diff to remote data.
      ::
      |=  det/delta-story
      ^+  +>
      ?+  -.det
        ~&([%unexpected-delta-remote -.det] !!)
        ::
          $config
        ?:  ?=($remove -.dif.det)
          +>(mirrors (~(del by mirrors) cir.det))
        =/  new/config
          %+  change-config
          (fall (~(get by mirrors) cir.det) *config)
          dif.det
        +>.$(mirrors (~(put by mirrors) cir.det new))
        ::
          $status
        %_  +>.$
            remotes
          %+  ~(put by remotes)  cir.det
          =+  ole=(fall (~(get by remotes) cir.det) *group)
          ?:  ?=($remove -.dif.det)  (~(del by ole) who.det)
          =+  old=(fall (~(get by ole) who.det) *status)
          (~(put by ole) who.det (change-status old dif.det))
        ==
      ==
    ::
    ++  sa-config-effects                               ::<  config side-effects
      ::>  apply side-effects for a %config delta.
      ::
      |=  {old/config dif/diff-config}
      ^-  (list move)
      ?+  -.dif  ~
        $permit   (sa-permit-effects sec.con.old ses.con.old +.dif)
        ::NOTE  when doing a lone %secure, calculate the
        ::      necessary %permit deltas alongside it.
      ==
    ::
    ++  sa-follow-effects                               ::<  un/subscribe
      ::>  apply side-effects for a %follow delta,
      ::>  un/subscribing this story to/from {cos}.
      ::
      |=  {sub/? srs/(set source)}
      ^-  (list move)
      %.  srs
      ?:(sub sa-acquire sa-abjure)
    ::
    ++  sa-permit-effects                               ::<  notify permitted
      ::>  apply side-effects for a %permit delta,
      ::>  kicking the subscriptions of {sis} if they
      ::>  are being banished.
      ::
      |=  {sec/security old/(set ship) add/? sis/(set ship)}
      ^-  (list move)
      =/  wyt  ?=(?($white $green) sec)
      =/  inv  =(wyt add)
      ?:  inv  ~
      =/  sus/(set ship)
        %.  ses.con.shape
        ?:(add ~(dif in sis) ~(int in sis))
      (sa-eject sus)
    ::
    ::>  ||
    ::>  ||  %subscriptions
    ::>  ||
    ::>    arms for starting and ending subscriptions
    ::+|
    ::
    ++  sa-acquire                                      ::<  subscribe us
      ::>  subscribes this story to each circle.
      ::
      |=  srs/(set source)
      %+  sa-sauce  0  ::  subscription is caused by this app
      %-  zing
      %+  turn  ~(tap in srs)
      |=  {cir/circle ran/range}
      ^-  (list card)  ::TODO  just (unit card) is enough.
      ?:  =(cir sa-cir)  ~                              ::  ignore self-subs
      [(circle-peer nom cir ran) ~]
    ::
    ++  sa-abjure                                       ::<  unsubscribe us
      ::>  unsubscribes this story from each circle.
      ::
      |=  srs/(set source)
      %+  sa-sauce  0  ::  subscription is caused by this app
      %-  zing
      %+  turn  ~(tap in srs)
      |=  {cir/circle ran/range}
      ^-  (list card)
      :_  ~
      =+  rap=(range-to-path ran)
      :*  %pull
          (welp /circle/[nom]/(scot %p hos.cir)/[nom.cir] rap)
          [hos.cir dap.bol]
          ~
      ==
    ::
    ++  sa-eject                                        ::<  unsubscribe ships
      ::>  removes ships {sis} from {followers}.
      ::
      |=  sis/(set ship)
      ^-  (list move)
      %+  turn  ~(tap in (sa-unearth sis))
      |=  {b/bone}
      [b %quit ~]
    ::
    ++  sa-unearth                                      ::<  ships' bones
      ::>  find the bones in {sup.bol} that belong to
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
  ::>  parses {wir} to obtain either %circle with story
  ::>  and circle or %repeat with message number, source
  ::>  ship, story and serials.
  ::
  |=  wir/wire
  ^-  weir
  ?+    -.wir  !!
      $circle
    ?>  ?=({@ @ @ *} t.wir)
    :^    %circle
        i.t.wir
      [(slav %p i.t.t.wir) i.t.t.t.wir]
    (path-to-range t.t.t.t.wir)
    ::
      $repeat
    ?>  ?=({@ @ @ $~} t.wir)
    :+  %repeat
      [(slav %p i.t.wir) i.t.t.wir]
    ((list serial) (cue (slav %ud i.t.t.t.wir)))
  ==
::
++  etch-circle                                         ::<  parse /circle wire
  ::>  parses a /circle wire, call a gate with the
  ::>  result.
  ::
  |=  $:  wir/wire
          $=  fun
          $-  {nom/naem src/source}
              {(list move) _.}
      ==
  =+  wer=(etch wir)
  ?>(?=($circle -.wer) (fun nom.wer src.wer))
::
++  etch-repeat                                         ::<  parse /repeat wire
  ::>  parses a /repeat wire, call gate with the result.
  ::
  |=  $:  wir/wire
          $=  fun
          $-  {cir/circle ses/(list serial)}
              {(list move) _.}
      ==
  =+  wer=(etch wir)
  ?>(?=($repeat -.wer) (fun cir.wer ses.wer))
::
++  circle-peer                                         ::<  /circle peer card
  ::>  constructs a %peer move to subscribe {nom} to a
  ::>  source.
  ::
  |=  {nom/naem source}
  ^-  card
  =+  rap=(range-to-path ran)
  :*  %peer
      (welp /circle/[nom]/(scot %p hos.cir)/[nom.cir] rap)
      [hos.cir dap.bol]
      (welp /circle/[nom.cir] rap)
  ==
::
::>  ||
::>  ||  %new-events
::>  ||
::+|
++  bake                                                ::<  apply state delta
  ::>  applies a change to the application state,
  ::>  producing side-effects.
  ::
  |=  det/delta
  ^-  (quip move _+>)
  =^  mos  +>.$
    da-done:(da-change:da det)
  :_  +>.$
  ::TODO  move affection to pre-bake, but then station creation doesn't happen?
  (welp mos (affection det))
::
++  pre-bake                                            ::<  apply more deltas
  ::>  bake a list of deltas.
  ::
  |=  des/(list delta)
  ^-  (quip move _+>)
  =|  moz/(list move)
  |-  ^-  (quip move _+>.^$)
  ?~  des  [moz +>.^$]
  =^  mos  +>.^$  (bake i.des)
  $(moz (welp moz mos), des t.des)
  ::TODO  ideally you'd just do this, but that runtime errors on "bake"...
  ::%+  roll  des
  ::|=  {d/delta m/(list move) _+>.$}
  ::=^  mos  +>.^$  (bake d)
  ::[(welp m mos) +>.^$]
::
++  peek                                                ::<  query on state
  ::>  find the result (if any) for a given query.
  ::
  |=  qer/query
  ^-  (unit (unit prize))
  ?-  -.qer
      $reader
    ``[%reader binds nicks]
    ::
      $burden
    :+  ~  ~
    :-  %burden
    %-  ~(gas in *(map naem burden))
    %+  murn  ~(tap by stories)
    |=  {n/naem s/story}
    ^-  (unit (pair naem burden))
    ::  only auto-federate channels for now.
    ?.  ?=($black sec.con.shape.s)  ~
    :+  ~  n
    :+  grams.s
      [shape.s mirrors.s]
    [locals.s remotes.s]
    ::
      $report
    ::REVIEW  we just don't have a prize... but do have rumors.
    ::        or do we need [%report ~] ?
    ::TODO  gall note: need to be able to subscirbe to just changes... or just
    ::      data etc.
    ~
    ::
      $circle  ::REVIEW  should we send precs & config to out of range subs?
    =+  soy=(~(get by stories) nom.qer)
    ?~  soy  ~
    :+  ~  ~
    :-  %circle
    :+  %+  turn
          (~(so-first-grams so:ta nom.qer ~ u.soy) ran.qer)
        (cury gram-to-envelope nom.qer)
      [shape.u.soy mirrors.u.soy]
    [locals.u.soy remotes.u.soy]
  ==
::
++  dedicate                                            ::<  rumor-story to theirs
  ::>  modify a %story diff to make it about their ship
  ::>  instead of ours.
  ::
  |=  {who/ship nom/naem det/delta-story}
  ^-  rumor-story
  ?+  -.det  det
    ::
    ::TODO  ...
    $follow     !!
    $inherited  !!
    $sequent    !!
    ::
      $gram
    :-  %gram
    %+  gram-to-envelope  nom
    %_  gam.det
        aud
      %-  ~(run in aud.gam.det)
      |=  c/circle
      ::TODO  it probably isn't safe to do this for
      ::      all audience members hosted by us, even
      ::      if this is only called for burdens.
      ?.  =(hos.c our.bol)  c
      [who nom.c]
    ==
    ::
      $config
    ?.  =(hos.cir.det our.bol)  det
    det(cir [who nom.cir.det])
    ::
      $status
    ?.  =(hos.cir.det our.bol)  det
    det(cir [who nom.cir.det])
  ==
::
++  gram-to-envelope                                   ::<  wrap gram with nr
  ::>  deduce the initial msg number from a telegram
  ::>  for a given story. assumes both story and
  ::>  telegram are known.
  ::
  |=  {nom/naem gam/telegram}
  ^-  envelope
  :_  gam
  %.  uid.gam
  ~(got by known:(~(got by stories) nom))
::
++  feel                                                ::<  delta to rumor
  ::>  if the given delta changes the result of the given
  ::>  query, produce the relevant rumor.
  ::
  |=  {qer/query det/delta}
  ^-  (unit rumor)
  ?-  -.qer
      $reader
    ::  changes to shared ui state apply.
    ?+  -.det  ~
      $glyph  `[%reader det]
      $nick   `[%reader det]
    ==
    ::
      $burden
    ::?:  &(=(who.qer src.bol) did-they-send-a-burden)  ~
    ?.  ?=($story -.det)  ~
    ?:  ?=(?($follow $inherited $sequent) -.det.det)  ~
    ::  only burden channels for now.
    ?.  =(%black sec.con.shape:(~(got by stories) nom.det))  ~
    `[%burden nom.det (dedicate who.qer nom.det det.det)]
    ::
      $report
    ::  only send changes we didn't get from above.
    ?:  =(src.bol (above our.bol))  ~
    ::  only send story reports about grams and status.
    ?.  ?=($story -.det)  ~
    ?.  ?=(?($grams $status) -.det.det)  ~
    =+  soy=(~(got by stories) nom.det)
    ::  and only if the story is inherited.
    ?.  inherited.soy  ~
    ::  only burden channels for now.
    ?.  =(%black sec.con.shape.soy)  ~
    `[%burden nom.det (dedicate (above our.bol) nom.det det.det)]
    ::
      $circle
    ?.  ?=($story -.det)  ~
    ?.  =(nom.qer nom.det)  ~
    ?:  ?=(?($follow $burden) -.det.det)  ~             ::  internal-only delta
    =+  ren=(~(so-in-range so:ta nom.qer ~ (~(got by stories) nom.qer)) ran.qer)
    ::TODO  if done.ren, %quit bone
    ?.  in.ren  ~
    ::TODO  move up? we also check for $follow above, why?
    ?:  ?=(?($follow $inherited $sequent) -.det.det)  ~
    =/  rum/rumor-story
      ?+  -.det.det  det.det
          $gram
        [%gram (gram-to-envelope nom.det gam.det.det)]
      ==
    `[%circle rum]
  ==
::
++  affection                                           ::<  rumors to interested
  ::>  for a given delta, send rumors to all queries it
  ::>  affects.
  ::
  ::TODO?  %quit bones that are done with their subscription.
  ::  ...but that would also require a ta-cancel call to remove
  ::  them from the status list! how do?
  ::  should there be an ++away arm for gall to call?
  |=  det/delta
  ^-  (list move)
  ::  cache results for paths.
  =|  res/(map path (unit rumor))
  %+  murn  ~(tap by sup.bol)
  |=  {b/bone s/ship p/path}
  ^-  (unit move)
  =+  mur=(~(get by res) p)
  ?^  mur
    ?~  u.mur  ~
    `[b %diff %talk-rumor u.u.mur]
  =+  rum=(feel (path-to-query p) det)
  =.  res  (~(put by res) p rum)
  ?~  rum  ~
  `[b %diff %talk-rumor u.rum]
::
++  path-to-query                                       ::<  path, coins, query
  ::>  parse a path into a (list coin), then parse that
  ::>  into a query structure.
  ::
  |=  pax/path
  (coins-to-query (path-to-coins pax))
::
++  path-to-coins                                       ::<  path to coin list
  ::>  parse a path into a list of coins.
  ::
  |=  pax/path
  ^-  (list coin)
  %+  turn  `path`pax
  |=  a/@ta
  (need (slay a))
::
++  coins-to-query                                      ::<  coin list to query
  ::>  parse a list of coins into a query structure.
  ::
  ^-  $-((list coin) query)
  =>  depa
  |^  %-  af  :~
          [%reader ul]
          [%burden (at /[%p])]
          [%report ul]
          [%circle (al term rang)]
      ==
  ++  term  (do %tas)
  ++  rang  (mu (al plac (mu (un plac))))
  ++  plac  (or %da %ud)
  --
::
++  leak                                                ::<  visible to
  ::>  determine if the given query is visible to the
  ::>  ship.
  ::
  |=  {who/ship qer/query}
  ^-  ?
  ?-  -.qer
    $reader   (team:title our.bol who)
    $burden   ?&  =(who who.qer)
                  =(our.bol (above who))
              ==
    $report   =(who (above our.bol))
    ::
      $circle
    ?.  (~(has by stories) nom.qer)  |
    %.  who
    ~(so-visible so:ta nom.qer ~ (~(got by stories) nom.qer))
  ==
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
  ^-  (quip move _+>)
  =^  mos  +>.$
    %-  pre-bake
    ta-done:(ta-apply:ta src.bol cod)
  =^  mow  +>.$
    log-all-to-file
  [(welp mos mow) +>.$]
::
++  poke-talk-action                                    ::<  accept action
  ::>  incoming talk action. process it.
  ::
  |=  act/action
  ^-  (quip move _+>)
  ?.  (team:title src.bol our.bol)
    %-  pre-bake
    =<  ta-done
    %-  ta-note:ta  %-  crip
    "talk-action stranger {(scow %p src.bol)}"
  %-  pre-bake
  ta-done:(ta-action:ta ost.bol act)
::
::>  ||
::>  ||  %subscription-events
::>  ||
::+|
::
++  diff-talk-prize                                     ::<  accept prize
  ::>  accept a query result.
  ::
  |=  {wir/wire piz/prize}
  ^-  (quip move _+>)
  =^  mos  +>.$
    ::  this shouldn't be necessary, but see the TODO below.
    ?:  ?=($burden -.piz)
      %-  pre-bake
      =<  ta-done
      %+  roll  ~(tap by sos.piz)
      |=  {{n/naem b/burden} _ta}
      =<  so-done
      (~(so-bear so n ~ (fall (~(get by stories) n) *story)) b)
    %-  pre-bake
    ta-done:(ta-take:ta wir piz)  ::TODO  %ap-lame for %burden prize...
  =^  mow  +>.$
    log-all-to-file
  [(welp mos mow) +>.$]
::
++  diff-talk-rumor                                     ::<  accept rumor
  ::>  accept a query result change.
  ::
  |=  {wir/wire rum/rumor}
  ^-  (quip move _+>)
  =^  mos  +>.$
    %-  pre-bake
    ta-done:(ta-hear:ta wir rum)
  =^  mow  +>.$
    log-all-to-file
  [(welp mos mow) +>.$]
::
++  peer                                                ::<  accept subscription
  ::>  incoming subscription on {pax}.
  ::
  |=  pax/path
  ^-  (quip move _+>)
  ?:  ?=({$sole *} pax)  ~&(%talk-broker-no-sole !!)
  =+  qer=(path-to-query pax)
  ?.  (leak src.bol qer)  ~&(%peer-invisible !!)
  =^  mos  +>.$
    %-  pre-bake
    ta-done:(ta-subscribe:ta src.bol qer)
  :_  +>.$
  =+  piz=(peek qer)
  ?~  piz  ~&([%query-unavailable pax] mos)
  ?~  u.piz  ~&([%query-invalid pax] mos)
  :_  mos
  [ost.bol %diff %talk-prize u.u.piz]
::
++  pull                                                ::<  unsubscribe
  ::>  unsubscribes.
  ::
  |=  pax/path
  ^-  (quip move _+>)
  =+  qer=(path-to-query pax)
  ?.  ?=($circle -.qer)  [~ +>.$]
  %-  pre-bake
  ta-done:(ta-cancel:ta src.bol nom.qer)
::
++  reap                                                ::<  catch-all reap
  ::>  handle all remote errors.
  ::
  |=  {wir/wire fal/(unit tang)}
  ^-  (quip move _+>)
  ?~  fal  [~ +>]
  ~|  reap-fail+wir
  (mean u.fal)
::
++  reap-circle                                         ::<  subscription n/ack
  ::>  if subscribing failed, update state to reflect
  ::>  that.
  ::
  ::TODO  update to handle all subscription kinds.
  |=  {wir/wire fal/(unit tang)}
  ^-  (quip move _+>)
  %+  etch-circle  [%circle wir]
  |=  {nom/naem src/source}
  ?~  fal
    %-  pre-bake
    ta-done:(ta-greet:ta nom src)
  =.  u.fal  [>%reap-circle-fail nom src< u.fal]
  %-  (slog (flop u.fal))
  %-  pre-bake
  ta-done:(ta-leave:ta nom src)
::
++  quit-circle                                         ::<  dropped subscription
  ::>  gall dropped our subscription. resubscribe.
  ::
  ::TODO  update for all subscription kinds.
  |=  wir/wire
  ^-  (quip move _+>)
  %+  etch-circle  [%circle wir]
  |=  {nom/naem src/source}
  %-  pre-bake
  ta-done:(ta-resub:ta nom src)
::
++  coup-repeat                                         ::<  message n/ack
  ::>  ack from ++ta-transmit. mark the message as
  ::>  received or rejected.
  ::
  |=  {wir/wire fal/(unit tang)}
  ^-  (quip move _+>)
  %+  etch-repeat  [%repeat wir]
  |=  {cir/circle ses/(list serial)}
  %-  pre-bake
  ta-done:(ta-repeat:ta cir ses fal)
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
  |=  nom/naem
  ^-  (quip move _+>)
  =/  paf/path
    /(scot %p our.bol)/home/(scot %da now.bol)/talk/[nom]/talk-telegrams
  =+  grams:(~(got by stories) nom)
  :_  +>.$
  :_  ~
  :*  ost.bol
      %info
      /jamfile
      our.bol
      (foal:space:userlib paf [%talk-telegrams !>(-)])
  ==
::
++  poke-talk-load                                      ::<  load from log
  ::>  loads the telegrams of story {nom} into our state,
  ::>  as saved in ++poke-talk-save.
  ::
  |=  nom/naem
  ^-  (quip move _+>)
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
  |=  nom/naem
  ~&  %talk-poke-log
  ^-  (quip move _+>)
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
  |=  nom/naem
  ^-  (quip move _+>)
  :-  ~
  +>.$(log (~(del by log) nom))
::
++  log-all-to-file                                     ::<  update stories logs
  ::>  for every story we're logging, (over)writes all
  ::>  their grams to log files if new ones have arrived.
  ::
  ::TODO  re-enable and test.
  ^-  (quip move _.)
  ?:  &  [~ .]  ::  XXX!!!!
  :_  %_  .
          log
        %-  ~(urn by log)
        |=  {nom/naem len/@ud}
        count:(~(got by stories) nom)
      ==
  %+  murn  ~(tap by log)
  |=  {nom/naem len/@ud}
  ^-  (unit move)
  ?:  (gte len count:(~(got by stories) nom))
    ~
  `(log-to-file nom)
::
++  log-to-file                                         ::<  update story log
  ::>  logs all grams of story {nom} to a file.
  ::
  |=  nom/naem
  ^-  move
  =+  ^-  paf/path
      =+  day=(year %*(. (yore now.bol) +.t +:*tarp))
      %+  en-beam:format  [our.bol %home da+now.bol]
      /talk-telegrams/(scot %da day)/[nom]/talk
  =+  grams:(~(got by stories) nom)
  :*  ost.bol
      %info
      /jamfile
      our.bol
      (foal:space:userlib paf [%talk-telegrams !>(-)])
  ==
--
