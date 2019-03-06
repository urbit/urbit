::                                                      ::  ::
::::  /app/hall/hoon                                    ::  ::
  ::                                                    ::  ::
::
::TODO  document gate samples fully.
::
::TODO  for using moons as tmp identities for friends: stories may want to keep
::      lists of moons (or just ships in general?) that we define as "standalone"
::      so that the "convert to true identity" doesn't happen for them.
::
/-    hall-sur=hall                                     ::  structures
/+    hall-lib=hall, hall-legacy                        ::  libraries
/=    seed  /~  !>(.)
/=    filter-gram
      /^  $-({telegram:hall-sur bowl:gall} telegram:hall-sur)
      /|  /:  /%/filter  /!noun/
          /~  |=({t/telegram:hall-sur bowl:gall} t)
      ==
/=    custom-rules
      /^  (map knot $-({ship ?($r %w) bowl:gall} ?))
      /:  /%/rules  /_  /!noun/
::
::::
=,  hall-sur
=>  ::  #
    ::  #  %arch
    ::  #
    ::    data structures
    ::
    |%
    ::  #  %state
    ::    state data structures
    ++  state                                           ::  application state
      $:  stories/(map name story)                      ::  conversations
          outbox/(map serial tracking)                  ::  sent messages
          log/(map name @ud)                            ::  logged to clay
          nicks/(map ship nick)                         ::  local nicknames
          binds/(jug char audience)                     ::  circle glyph lookup
          public/(set circle)                           ::  publicly member of
          rir/wire                                      ::  current rumor wire
      ==                                                ::
    ++  story                                           ::  wire content
      $:  count/@ud                                     ::  (lent grams)
          grams/(list telegram)                         ::  all messages
          known/(map serial @ud)                        ::  messages heard
          sourced/(map circle (list @ud))               ::  heard from
          sequence/(map circle @ud)                     ::  last-heard p circle
          locals/group                                  ::  local status
          remotes/(map circle group)                    ::  remote status
          shape/config                                  ::  configuration
          mirrors/(map circle config)                   ::  remote config
          peers/(jar ship query)                        ::  subscribers
          inherited/_|                                  ::  from parent?
      ==                                                ::
    ::  #  %deltas
    ::    changes to state
    ++  delta                                           ::
      $%  ::  public state                              ::
          {$public add/? cir/circle}                    ::  show/hide membership
          ::  messaging state                           ::
          {$out cir/circle out/(list thought)}          ::  send msgs to circle
          $:  $done                                     ::  set delivery state
              cir/circle                                ::
              ses/(list serial)                         ::
              res/delivery                              ::
          ==                                            ::
          ::  shared ui state                           ::
          {$glyph diff-glyph}                           ::  un/bound glyph
          {$nick diff-nick}                             ::  changed nickname
          ::  story state                               ::
          {$story nom/name det/delta-story}             ::  change to story
          ::  side-effects                              ::
          {$init ~}                                    ::  initialize
          {$observe who/ship}                           ::  watch burden bearer
          $:  $present                                  ::  send %present cmd
              hos/ship                                  ::
              nos/(set name)                            ::
              dif/diff-status                           ::
          ==                                            ::
      ==                                                ::
    ++  delta-story                                     ::  story delta
      $%  {$inherited ihr/?}                            ::  inherited flag
          {$follow sub/? srs/(set source)}              ::  un/subscribe
          {$sequent cir/circle num/@ud}                 ::  update last-heard
          {$gram src/circle gam/telegram}               ::  new/changed msgs
          {$sourced src/circle num/@ud}                 ::  new heard-from
          diff-story                                    ::  both in & outward
      ==                                                ::
    ::  #  %out
    ::    outgoing data
    ++  move  (pair bone card)                          ::  all actions
    ++  lime                                            ::  diff fruit
      $%  {$hall-prize prize}                           ::
          {$hall-rumor rumor}                           ::
      ==                                                ::
    ++  pear                                            ::  poke fruit
      $%  {$hall-command command}                       ::
          {$hall-action action}                         ::TODO  see ++gentle-quit
      ==                                                ::
    ++  card                                            ::  general card
      $%  {$diff lime}                                  ::
          {$info wire term nori:clay}                   ::
          {$peer wire dock path}                        ::
          {$poke wire dock pear}                        ::
          {$pull wire dock ~}                          ::
          {$quit ~}                                    ::
      ==                                                ::
    ++  weir                                            ::  parsed wire
      $%  {$repeat cir/circle ses/(list serial)}        ::  messaging wire
          {$circle nom/name src/source}                 ::  subscription wire
      ==                                                ::
    --
::
::  #
::  #  %work
::  #
::    functional cores and arms.
::
~%  %hall-door  ..is  ~
|_  {bol/bowl:gall $1 state}
::
::  #  %transition
::    prep transition
::
++  prep
  ::  adapts state.
  ::
  =>  |%
      ++  states
        $%({$1 s/state} {$0 s/state-0})
      ::
      ++  state-0
        (cork state |=(a/state a(stories (~(run by stories.a) story-0))))
      ++  story-0
        %+  cork  story
        |=  a/story
        %=  a
          shape     *config-0
          mirrors   (~(run by mirrors.a) config-0)
          peers     (~(run by peers.a) |=(a/(list query) (turn a query-0)))
        ==
      ++  query-0
        $%  $:  $circle
                nom/name
                wer/(unit circle)
                wat/(set circle-data)
                ran/range-0
            ==
            query
        ==
      ++  config-0
        {src/(set source-0) cap/cord tag/tags fit/filter con/control}
      ++  source-0
        {cir/circle ran/range-0}
      ++  range-0
        %-  unit
        $:  hed/place-0
            tal/(unit place-0)
        ==
      ++  place-0
        $%  {$da @da}
            {$ud @ud}
            {$sd @sd}
        ==
      --
  =|  mos/(list move)
  |=  old/(unit states)
  ^-  (quip move _..prep)
  ?~  old
    %-  pre-bake
    ta-done:ta-init:ta
  ?-  -.u.old
      $1
    [mos ..prep(+<+ u.old)]
  ::
      $0
    =-  $(old `[%1 s.u.old(stories -)])
    |^  %-  ~(run by stories.s.u.old)
        |=  soy/story-0
        ^-  story
        %=  soy
          shape     (prep-config shape.soy)
          mirrors   (~(run by mirrors.soy) prep-config)
          peers     %-  ~(run by peers.soy)
                    |=  a/(list query-0)
                    ^-  (list query)
                    (murn a prep-query)
        ==
    ::
    ++  prep-config
      |=  cof/config-0
      ^-  config
      =.  src.cof
        %-  ~(gas in *(set source))
        (murn ~(tap in src.cof) prep-source)
      :*  src.cof
          cap.cof
          tag.cof
          fit.cof
          con.cof
          0
      ==

    ::
    ++  prep-source
      |=  src/source-0
      ^-  (unit source)
      =+  nan=(prep-range ran.src)
      ?~  nan
        ~&  [%forgetting-source src]
        ~
      `src(ran u.nan)
    ::
    ++  prep-query
      |=  que/query-0
      ^-  (unit query)
      ?.  ?=($circle -.que)  `que
      =+  nan=(prep-range ran.que)
      ?~  nan
        ~&  [%forgetting-query que]
        ~
      `que(ran u.nan)
    ::
    ++  prep-range
      |=  ran/range-0
      ^-  (unit range)
      ?~  ran  `ran
      ::  ranges with a relative end aren't stored because they end
      ::  immediately, so if we find one we can safely discard it.
      ?:  ?=({$~ {$sd @sd}} tal.u.ran)  ~
      ::  we replace relative range starts with the current date.
      ::  this is practically correct.
      ?:  ?=({$sd @sd} hed.u.ran)
        `ran(hed.u [%da now.bol])
      `ran
    --
  ==
::
::  #  %engines
::    main cores.
::
++  ta
  ::  thinker core, used for processing pokes into deltas.
  ::
  ~/  %hall-ta
  |_  ::  deltas: deltas created by core operations.
      ::
      deltas/(list delta)
  ::  #  %resolve
  ::
  ++  ta-done
    ::    resolve core
    ::
    ::  produces the moves stored in ++ta's moves.
    ::  they are produced in reverse order because
    ::  ++ta-emil and ++ta-emit add them to the head of
    ::  the {moves}.
    ::
    ::  we don't produce any new state, because ++ta
    ::  doesn't make any changes to it itself.
    ::
    ^-  (list delta)
    (flop deltas)
  ::
  ::  #
  ::  #  %emitters
  ::  #
  ::    arms that create outward changes.
  ::
  ++  ta-delta
    ::  adds a delta to the head of {deltas}.
    ::
    ~/  %hall-ta-delta
    |=  det/delta
    %_(+> deltas [det deltas])
  ::
  ++  ta-deltas
    ::    adds multiple deltas to the head of {deltas}.
    ::
    ::  flops to stay consistent with ++ta-delta.
    ::
    ~/  %hall-ta-deltas
    |=  des/(list delta)
    %_(+> deltas (welp (flop des) deltas))
  ::
  ++  ta-speak
    ::  sends {sep} as an %app message to the user's inbox.
    ::
    ~/  %hall-ta-speak
    |=  sep/speech
    %+  ta-action  %phrase
    :-  [[our.bol %inbox] ~ ~]
    [%app dap.bol sep]~
  ::
  ++  ta-grieve
    ::  sends a stack trace to the user's inbox.
    ::
    ~/  %hall-ta-grieve
    |=  {msg/tape fal/tang}
    %^  ta-speak  %fat
      [%name 'stack trace' %tank fal]
    [%lin | (crip msg)]
  ::
  ++  ta-note
    ::  sends {msg} to the user's inbox.
    ::
    ~/  %hall-ta-note
    |=  msg/tape
    (ta-speak %lin | (crip msg))
  ::
  ++  ta-evil
    ::  tracing printf and crash.
    ::
    ~/  %hall-ta-evil
    |=  msg/cord
    ~|  [%hall-ta-evil msg]
    !!
  ::
  ::  #
  ::  #  %data
  ::  #
  ::    utility functions for data retrieval.
  ::
  ++  ta-know
    ::    story monad
    ::
    ::  produces a gill that takes a gate. if the story
    ::  {nom} exists, calls the gate with a story core.
    ::  if it doesn't, does nothing.
    ::
    ~/  %hall-ta-know
    |=  nom/name
    |:  fun=^|(|:(so ta))
    ^+  +>+>
    =+  pur=(~(get by stories) nom)
    ?~  pur
      %-  ta-evil
      (crip "no story '{(trip nom)}'")
    (fun ~(. so nom ~ u.pur))
  ::
  ::  #
  ::  #  %interaction-events
  ::  #
  ::    arms that apply events we received.
  ::
  ++  ta-init
    ::    initialize app
    ::
    ::  populate state on first boot. creates our default mailbox and journal.
    ::
    ::  create default circles.
    =>  %+  roll
          ^-  (list {security name cord})
          :~  [%mailbox %inbox 'default home']
              [%journal %public 'visible activity']
              [%mailbox %i 'dm invites']
          ==
        |:  [[typ=*security nom=*name des=*cord] ta]
        (ta-action [%create nom des typ])
    %-  ta-deltas
    ::  if needed, subscribe to our parent's /burden.
    =+  sen=(above:hall-lib [our now our]:bol)
    ?:  ?|  !=(%czar (clan:title sen))
            =(sen our.bol)
            =(%pawn (clan:title our.bol))
        ==
      ~
    [%init ~]~
  ::
  ++  ta-apply
    ::  applies the command sent by {src}.
    ::
    ~/  %hall-ta-apply
    |=  {src/ship cod/command}
    ^+  +>
    ?-  -.cod
      ::  %publish commands prompt us (as a circle host)
      ::  to verify and distribute messages.
      $publish  (ta-think | src +.cod)
      ::  %present commands are used to ask us to set
      ::  someone's status in the indicated stories.
      $present  (ta-present src +.cod)
      ::  %bearing commands are used by our children to
      ::  let us know they're bearing our /burden. we
      ::  need to watch them to allow changes to go up.
      $bearing  (ta-observe src)  ::TODO  isn't this redundant with ta-subscribe?
    ==
  ::
  ++  ta-present
    ::    update a status
    ::
    ::  sets status for the indicated stories,
    ::  but only if they have write permission there.
    ::
    ~/  %hall-ta-present
    |=  {who/ship nos/(set name) dif/diff-status}
    ^+  +>
    =+  nol=~(tap in nos)
    |-
    ?~  nol  +>.^$
    =.  +>.^$
      ?.  (~(has by stories) i.nol)  +>.^$
      =+  soy=(~(got by stories) i.nol)
      so-done:(~(so-present so i.nol ~ soy) who dif)
    $(nol t.nol)
  ::
  ++  ta-action
    ::  performs action sent by a client.
    ::
    ~/  %hall-ta-action
    |=  act/action
    ^+  +>
    =<  work
    ::  #
    ::  #  %actions
    ::  #
    ::    action processing core
    ::
    ::  ++work calls the appropriate action processing
    ::  arm. most use ++affect to retrieve the affected
    ::  story, crashing if it doesn't exist.
    ~%  %hall-ta-action-inner  +  ~
    |%
    ::  #  %utility
    ++  work                                            ::  perform action
      ^+  ..ta-action
      ?-  -.act
        ::  circle configuration
        $create  (action-create +.act)
        $design  (action-design +.act)
        $source  (action-source +.act)
        $depict  (action-depict +.act)
        $filter  (action-filter +.act)
        $permit  (action-permit +.act)
        $delete  (action-delete +.act)
        $usage   (action-usage +.act)
        $read    (action-read +.act)
        $newdm   (action-newdm +.act)
        ::  messaging
        $convey  (action-convey +.act)
        $phrase  (action-phrase +.act)
        ::  personal metadata
        $notify  (action-notify +.act)
        $naming  (action-naming +.act)
        ::  changing shared ui
        $glyph   (action-glyph +.act)
        $nick    (action-nick +.act)
        ::  misc changes
        $public  (action-public +.act)
      ==
    ::
    ++  affect
      ::    delta to story
      ::
      ::  store a delta about a story. if the story
      ::  does not exist, crash.
      ::
      ~/  %hall-affect
      |=  {nom/name det/delta-story}
      ?:  (~(has by stories) nom)
        (impact nom det)
      (ta-evil (crip "no story {(trip nom)}"))
    ::
    ++  impact
      ::    delta for story
      ::
      ::  Store a delta about a story.
      ::
      ~/  %hall-impact
      |=  {nom/name det/delta-story}
      (ta-delta %story nom det)
    ::
    ++  present
      ::  send status update
      ::
      ~/  %hall-present
      |=  {aud/audience dif/diff-status}
      ^+  ..ta-action
      =/  cic
        ^-  (jug ship name)
        %-  ~(rep in aud)
        |=  {c/circle m/(jug ship name)}
        (~(put ju m) hos.c nom.c)
      =?  ..ta-action  (~(has by cic) our.bol)
        =+  nos=~(tap in (~(get ju cic) our.bol))
        (ta-present our.bol (~(get ju cic) our.bol) dif)
      =.  cic  (~(del by cic) our.bol)
      %-  ta-deltas
      %-  ~(rep by cic)
      |=  {{h/ship s/(set name)} l/(list delta)}
      :_  l
      [%present h s dif]
    ::
    ::  #  %circle-configuration
    ++  action-create
      ::  creates a story with the specified parameters.
      ::
      ~/  %hall-action-create
      |=  {nom/name des/cord typ/security}
      ^+  ..ta-action
      ?.  (~(has by stories) nom)
        %^  impact  nom  %new
        :*  [[[our.bol nom] ~] ~ ~]
            des
            ~
            *filter
            :-  typ
            ?.  ?=(?($village $journal) typ)  ~
            [our.bol ~ ~]
            0
        ==
      (ta-evil (crip "{(trip nom)}: already exists"))
    ::
    ++  action-design
      ::  creates a story with the specified config.
      ::
      ~/  %hall-action-design
      |=  {nom/name cof/config}
      ?.  (~(has by stories) nom)
        (impact nom %new cof)
      (ta-evil (crip "{(trip nom)}: already exists"))
    ::
    ++  action-delete
      ::    delete + announce
      ::
      ::  delete story {nom}, optionally announcing the
      ::  event with message {mes}.
      ::
      ~/  %hall-action-delete
      |=  {nom/name mes/(unit cord)}
      ^+  ..ta-action
      =?  ..ta-action  ?=(^ mes)
        %+  action-phrase
          [[our.bol nom] ~ ~]
        [%lin | u.mes]~
      (affect nom %remove ~)
    ::
    ++  action-depict
      ::  change description of story {nom} to {des}.
      ::
      ~/  %hall-depict
      |=  {nom/name cap/cord}
      (affect nom %config [our.bol nom] %caption cap)
    ::
    ++  action-filter
      ::    change message rules
      ::
      ::  replaces the story's current filter with the
      ::  specified one.
      ::
      ~/  %hall-action-filter
      |=  {nom/name fit/filter}
      (affect nom %config [our.bol nom] %filter fit)
    ::
    ++  action-permit
      ::  invite to/banish from story {nom} all {sis}.
      ::
      ~/  %hall-action-permit
      |=  {nom/name inv/? sis/(set ship)}
      =+  soy=(~(get by stories) nom)
      ?~  soy
        (ta-evil (crip "no story {(trip nom)}"))
      so-done:(~(so-permit so nom ~ u.soy) inv sis)
    ::
    ++  action-source
      ::  add/remove {pos} as sources for story {nom}.
      ::
      ~/  %hall-action-source
      |=  {nom/name sub/? srs/(set source)}
      =+  soy=(~(get by stories) nom)
      ?~  soy
        (ta-evil (crip "no story {(trip nom)}"))
      so-done:(~(so-sources so nom ~ u.soy) sub srs)
    ::
    ++  action-usage
      ::  add or remove usage tags.
      ::
      ~/  %hall-action-usage
      |=  {nom/name add/? tas/tags}
      =+  soy=(~(get by stories) nom)
      ?~  soy
        (ta-evil (crip "no story {(trip nom)}"))
      so-done:(~(so-usage so nom ~ u.soy) add tas)
    ::
    ++  action-read
      ::  set the read message number
      ::
      ~/  %hall-action-read
      |=  {nom/name red/@ud}
      =+  soy=(~(get by stories) nom)
      ?~  soy
        (ta-evil (crip "no story {(trip nom)}"))
      so-done:(~(so-read so nom ~ u.soy) red)
    ::
    ++  action-newdm
      ::  copy all behavior of create, permit, and source in that order
      ::
      ~/  %hall-action-newdm
      |=  sis/(set ship)
      ::  generate circle name from sis as a dot seperated list of ship names
      ::  in alphabetical order
      ::
      =/  nom/name
      %^  rsh  3  1
      %+  roll
        %+  sort  %+  turn  ~(tap in (~(put in sis) our.bol))
        |=  p/ship
        ^-  cord
        (scot %p p)
        aor
      |=  {p/cord nam/name}
      ^-  @tas
      (crip "{(trip `@t`nam)}.{(slag 1 (trip p))}")
      ::  if we've already created this circle, no-op
      ::
      ?:  (~(has in ~(key by stories)) nom)
        (ta-deltas ~)
      ::  check if we already have an invite to this dm group
      ::  or if we are creating it
      ::
      =/  inv=(list telegram)
      %+  skim  grams:(~(got by stories) %i)
      |=  g=telegram
      ^-  ?
      ?.  ?=({$inv *} sep.g)     %.n
      ?.  =(nom nom.cir.sep.g)   %.n
      ?.  (~(has in sis) aut.g)  %.n
      %.y
      ::
      =.  inv  %+  sort  inv
        |=  {a/telegram b/telegram}
        (lte wen.a wen.b)
      ::  create our side of the dm and source it to our inbox
      ::
      =/  dels/(list delta)
      :~  :*  %story
              %inbox
              %follow
              &
              [[[our.bol nom] ~] ~ ~]
          ==
          :*  %story
              nom
              %new
              [[[our.bol nom] ~] ~ ~]
              'dm'
              ~
              *filter
              [%village (~(put in sis) our.bol)]
              0
          ==
      ==
      ::  if we did initiate the dm, send out invites
      ::
      ?:  ?=(~ inv)
        =.  ..ta-action  (ta-deltas dels)
        %-  action-convey
        ^-  (list thought)
        %+  turn  ~(tap in (~(del in sis) our.bol))
        |=  a=ship
        ^-  thought
        :*  uid=(shaf a eny.bol)
            aud=(sy [a %i] ~)
            wen=now.bol
            sep=[%inv & [our.bol nom]]
        ==
      ::  if we did not initiate the dm, source to the initiators copy
      ::
      =.  dels
      :_  dels
      :*  %story
          nom
          %follow
          &
          [[[aut.i.inv nom] ~] ~ ~]
      ==
      (ta-deltas dels)
    ::
    ::  #  %messaging
    ++  action-convey
      ::    post exact
      ::
      ::  sends the messages provided in the action.
      ::
      ~/  %hall-action-convey
      |=  tos/(list thought)
      (ta-think & our.bol tos)
    ::
    ++  action-phrase
      ::    post easy
      ::
      ::  sends the message contents provided in the
      ::  action generating a serial and setting a
      ::  timestamp.
      ::
      ~/  %hall-action-phrase
      |=  {aud/audience ses/(list speech)}
      ^+  ..ta-action
      =-  (ta-think & our.bol tos)
      |-  ^-  tos/(list thought)
      ?~  ses  ~
      =^  sir  eny.bol  ~(uniq hall-lib bol)
      :_  $(ses t.ses)
      [sir aud [now.bol i.ses]]
    ::
    ::  #  %personal-metadata
    ::
    ++  action-notify
      ::    our presence update
      ::
      ::  notify the audience of our new presence state,
      ::  or tell them to remove us if {pes} is ~.
      ::
      ~/  %hall-action-notify
      |=  {aud/audience pes/(unit presence)}
      ^+  ..ta-action
      ?~  pes  (present aud %remove ~)
      (present aud %presence u.pes)
    ::
    ++  action-naming
      ::  our name update
      ::
      ~/  %hall-action-naming
      |=  {aud/audience man/human}
      ^+  ..ta-action
      (present aud %human %full man)
    ::
    ::  #  %changing-shared-ui
    ++  action-nick
      ::    new identity
      ::
      ::  assigns a new local identity ("nickname") to the
      ::  target ship.
      ::
      ~/  %hall-action-nick
      |=  {who/ship nic/nick}
      ^+  ..ta-action
      ?.  =((~(get by nicks) who) `nic)  ..ta-action    ::  no change
      (ta-delta %nick who nic)
    ::
    ++  action-glyph
      ::  un/bind glyph {lif} to audience {aud}.
      ::
      ~/  %hall-action-glyph
      |=  {lif/char aud/audience bin/?}
      (ta-delta %glyph bin lif aud)
    ::
    ++  action-public
      ::    show/hide membership
      ::
      ::  add or remove a circle from the public membership list.
      ::
      ~/  %hall-action-public
      |=  {add/? cir/circle}
      (ta-delta %public add cir)
    --
  ::
  ::  #
  ::  #  %subscription-events
  ::  #
  ::    arms that react to subscription events.
  ::
  ++  ta-observe
    ::    watch burden bearer
    ::
    ::  subscribe to a child who is bearing our burden.
    ::TODO  everyone should be able to bear if they so desire.
    ::
    ~/  %hall-ta-observe
    |=  who/ship
    ^+  +>
    ?.  =(our.bol (above:hall-lib our.bol now.bol who))
      ~&([%not-our-bearer who] +>)
    (ta-delta %observe who)
  ::
  ++  ta-subscribe
    ::    listen to
    ::
    ::  reaction to incoming subscriptions.
    ::
    ~/  %hall-ta-subscribe
    |=  {her/ship qer/query}
    ^+  +>
    ?+  -.qer  +>
      $burden   (ta-observe her)
      $circle   %+  ta-delta  %story
                [nom.qer %peer & her qer]
    ==
  ::
  ++  ta-greet
    ::    subscription success
    ::
    ::  store a started subscription as source.
    ::
    ~/  %hall-ta-greet
    |=  {nom/name src/source}
    %-  (ta-know nom)  |=  sor/_so  =<  so-done
    (so-greet:sor src)
  ::
  ++  ta-leave
    ::    subscription failed
    ::
    ::  removes {src} from story {nom}'s sources.
    ::
    ~/  %hall-ta-leave
    |=  {nom/name src/source}
    %-  (ta-know nom)  |=  sor/_so  =<  so-done
    (so-leave:sor src)
  ::
  ++  ta-take
    ::    apply prize
    ::
    ::  for a %burden prize, bear the burden in a new
    ::  or existing story.
    ::  for a %circle prize, use ++so to accept it.
    ::  for a %report prize, silently ignore.
    ::
    ~/  %hall-ta-take
    |=  {wir/wire piz/prize}
    ^+  +>
    ?+  -.piz  ~&([%ignoring-prize -.piz] +>)
        $report
      +>
    ::
        $burden
      =+  sos=~(tap by sos.piz)
      |-  ^+  ..ta-take
      ?~  sos  ..ta-take
      =.  ..ta-take
        =+  (fall (~(get by stories) p.i.sos) *story)
        =>  (~(so-bear so p.i.sos ~ -) q.i.sos)
        =.  acs  (flop acs)
        |-  ^+  ..ta-take
        ?~  acs  ..ta-take
        =.  ..ta-take  (ta-action i.acs)
        $(acs t.acs)
      $(sos t.sos)
      ::TODO  runtime error
      ::%+  roll  ~(tap by sos.piz)
      ::|=  {{n/name b/burden} _..ta-take}
      ::=+  (fall (~(get by stories) n) *story)
      ::so-done:(~(so-bear so n ~ -) b)
    ::
        $circle
      =+  wer=(etch wir)
      ?>  ?=($circle -.wer)
      %-  (ta-know nom.wer)  |=  sor/_so  =<  so-done
      (so-take:sor cir.src.wer +.piz)
    ==
  ::
  ++  ta-hear
    ::    apply rumor
    ::
    ::  apply changes from a rumor to our state.
    ::  for %burden, authoratively apply the story
    ::  diff. if it's a new one, bear it.
    ::  for %circle, apply the story diff normally.
    ::
    ~/  %hall-ta-hear
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
        =?  +>  !(~(has by stories) nom.rum)
          (ta-delta %story +.rum)
        =>  =+  (fall (~(get by stories) nom.rum) *story)
            %-  ~(so-bear so nom.rum ~ -)
            [~ [cof.rum.rum ~] [~ ~]]
        =.  acs  (flop acs)
        |-  ^+  +>+
        ?~  acs  +>+
        =.  +>+  (ta-action i.acs)
        $(acs t.acs)
        ::TODO  runtime error
        ::=<  so-done
        ::%-  ~(so-bear so nom.rum ~ (fall (~(get by stories) nom.rum) *story))
        ::[~ [cof.rum.rum ~] [~ ~]]
      ==
    ::
        $circle
      =+  wer=(etch wir)
      ?>  ?=($circle -.wer)
      %-  (ta-know nom.wer)  |=  sor/_so  =<  so-done
      (so-hear:sor | cir.src.wer rum.rum)
    ==
  ::
  ++  ta-repeat
    ::    message delivered
    ::
    ::  message got delivered. if an error was returned
    ::  mark the message as rejected. if not, received.
    ::
    ~/  %hall-ta-repeat
    |=  {who/circle ses/(list serial) fal/(unit tang)}
    ^+  +>
    ?~  fal
      (ta-delta %done who ses %accepted)
    =.  +>  (ta-delta %done who ses %rejected)
    =-  (ta-grieve - u.fal)
    %+  weld  "{(scow %ud (lent ses))} message(s) "
    "rejected by {(scow %p hos.who)}/{(trip nom.who)}"
  ::
  ++  ta-resub
    ::    subscription dropped
    ::
    ::  when a subscription gets dropped by gall, we
    ::  resubscribe.
    ::
    ~/  %hall-ta-resub
    |=  {nom/name src/source}
    ^+  +>
    %-  (ta-know nom)  |=  sor/_so  =<  so-done
    (so-resub:sor src)
  ::
  ::  #
  ::  #  %messaging
  ::  #
  ::    arms for sending and processing messages.
  ::
  ++  ta-think
    ::    publish or review
    ::
    ::  consumes each thought.
    ::
    ~/  %hall-ta-think
    |=  {pub/? aut/ship tos/(list thought)}
    ^+  +>
    ?~  tos  +>
    $(tos t.tos, +> (ta-consume pub aut i.tos))
  ::
  ++  ta-consume
    ::    to each audience
    ::
    ::  conducts thought {tot} to each circle in its audience.
    ::
    ~/  %hall-ta-consume
    |=  {pub/? aut/ship tot/thought}
    =+  aud=~(tap in aud.tot)
    |-  ^+  +>.^$
    ?~  aud  +>.^$
    $(aud t.aud, +>.^$ (ta-conduct pub aut i.aud tot))
  ::
  ++  ta-conduct
    ::    thought to circle
    ::
    ::  either publishes or records a thought.
    ::
    ~/  %hall-ta-conduct
    |=  {pub/? aut/ship cir/circle tot/thought}
    ^+  +>
    ?:  pub
      ?.  (team:title our.bol aut)
        (ta-note "strange author {(scow %p aut)}")
      =.  aut  our.bol
      ?:  =(aut hos.cir)
        ?:  (~(has by stories) nom.cir)
          (ta-record nom.cir hos.cir tot)
        ::TODO  avenue for abuse?
        (ta-note "have no story {(scow %tas nom.cir)}")
      (ta-transmit cir tot)
    ?.  =(our.bol hos.cir)  +>
    (ta-record nom.cir aut tot)
  ::
  ++  ta-record
    ::    add to story
    ::
    ::  add or update telegram {gam} in story {nom}.
    ::
    ~/  %hall-ta-record
    |=  {nom/name gam/telegram}
    %-  (ta-know nom)  |=  sor/_so  =<  so-done
    (so-learn:sor [our.bol nom] gam)
  ::
  ++  ta-transmit
    ::  sends thought {tot} to {cir}.
    ::
    ~/  %hall-ta-transmit
    |=  {cir/circle tot/thought}
    ^+  +>
    (ta-delta %out cir tot ~)
  ::
  ::  #
  ::  #  %stories
  ::  #
  ::    arms for modifying stories.
  ::
  ++  so
    ::  story core, used for doing work on a story.
    ::
    ~/  %hall-so
    |_  ::  nom: story name in {stories}.
        ::  acs: hall actions issued due to changes.
        ::  story is faceless to ease data access.
        ::
        $:  nom/name
            acs/(list action)
            story
        ==
    ::
    ::  #  %resolve
    ++  so-done
      ::  apply actions generated by story operations.
      ::TODO  maybe produce list of actions, apply in ++ta
      ::
      ^+  +>
      =.  acs  (flop acs)
      |-  ^+  +>+
      ?~  acs  +>+
      =.  +>+  (ta-action i.acs)
      $(acs t.acs)
    ::
    ::  #
    ::  #  %emitters
    ::  #
    ::    arms that create outward changes.
    ::
    ++  so-act
      ::  stores a hall action.
      ::
      ~/  %hall-so-act
      |=  act/action
      ^+  +>
      +>(acs [act acs])
    ::
    ++  so-note
      ::  sends {msg} as an %app message to the user's inbox.
      ::
      ~/  %hall-so-note
      |=  msg/cord
      ^+  +>
      %+  so-act  %phrase
      :-  [[our.bol %inbox] ~ ~]
      [%app dap.bol %lin | msg]~
    ::
    ++  so-delta
      ::  store delta in ++ta core.
      ::
      ~/  %hall-so-delta
      |=  det/delta
      ^+  +>
      +>(deltas [det deltas])
    ::
    ++  so-deltas
      ::  store multiple deltas in ++ta core.
      ::
      ~/  %hall-so-deltas
      |=  des/(list delta)
      %_(+> deltas (welp (flop des) deltas))
    ::
    ++  so-delta-our
      ::  adds a delta about this story.
      ::
      ~/  %hall-so-delta-our
      |=  det/delta-story
      ^+  +>
      (so-delta %story nom det)
    ::
    ++  so-deltas-our
      ::  adds multiple deltas about this story.
      ::
      ~/  %hall-so-deltas-our
      |=  des/(list delta-story)
      ^+  +>
      %-  so-deltas
      %+  turn  des
      |=  d/delta-story
      [%story nom d]
    ::
    ::  #
    ::  #  %data
    ::  #
    ::    utility functions for data retrieval.
    ::
    ++  so-cir  [our.bol nom]                           ::  us as circle
    ::
    ::  #
    ::  #  %interaction-events
    ::  #
    ::    arms that apply events we received.
    ::
    ++  so-take
      ::  apply the prize as if it were rumors.
      ::
      ~/  %hall-so-take
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
    ++  so-hear
      ::  apply changes from a rumor to this story.
      ::
      ~/  %hall-so-hear
      |=  {bur/? src/circle rum/rumor-story}
      ::TODO  tall-form gate comments like this for everything?
      ::|=  $:  ::  bur: whether the story is inherited
      ::        ::  src: story to change
      ::        ::  rum: change to this story
      ::        ::
      ::        bur/?
      ::        src/circle
      ::        rum/rumor-story
      ::    ==
      ^+  +>
      ?-  -.rum
        $bear     (so-bear bur.rum)
        $peer     (so-delta-our rum)
        $gram     (so-open src nev.rum)
        $remove   ::TODO  should also remove from {remotes}?
                  (so-delta-our %config src %remove ~)
      ::
          $new
        ?:  =(src so-cir)
          (so-config-full ~ cof.rum)
        $(rum [%config src %full cof.rum])
      ::
          $config
        ::  we only subscribe to remotes' configs.
        ?.  =(src cir.rum)
          ~!  %unexpected-remote-config-from-remote
          !!
        =/  old/(unit config)
          ?:  =(cir.rum so-cir)  `shape
          (~(get by mirrors) cir.rum)
        ::  ignore if it won't result in change.
        ?.  ?|  &(?=($remove -.dif.rum) ?=(^ old))
                ?=(~ old)
                !=(u.old (change-config:hall-lib u.old dif.rum))
            ==
          +>.$
        ::  full changes to us need to get split up.
        ?:  &(=(cir.rum so-cir) ?=($full -.dif.rum))
          (so-config-full `shape cof.dif.rum)
        (so-delta-our rum)
      ::
          $status
        ::  we only subscribe to remotes' locals.
        ?.  |(=(src cir.rum) =(src so-cir))
          ~!  %unexpected-remote-status-from-remote
          !!
        =/  old/(unit status)
          ?:  =(cir.rum so-cir)  (~(get by locals) who.rum)
          =-  (~(get by -) who.rum)
          (fall (~(get by remotes) cir.rum) *group)
        ::  ignore if it won't result in change.
        ?.  ?|  &(?=($remove -.dif.rum) ?=(^ old))
                ?=(~ old)
                !=(u.old (change-status:hall-lib u.old dif.rum))
            ==
          +>.$
        (so-delta-our rum)
      ==
    ::
    ++  so-bear
      ::    accept burden
      ::
      ::  add what was pushed down from above to our
      ::  state. in case of conflict, existing data is
      ::  overwritten.
      ::
      ::NOTE  we don't use ++roll here because of urbit/arvo#447.
      ::
      ~/  %hall-so-bear
      |=  {gaz/(list telegram) cos/lobby pes/crowd}
      ^+  +>
      =*  self  +>
      ::
      ::  local config
      =.  self
        (so-config-full `shape loc.cos)
      ::
      ::  remote config
      =.  self
        =+  rem=~(tap by rem.cos)
        |-  ^+  self
        ?~  rem  self
        =*  wer  p.i.rem
        =*  cof  q.i.rem
        ::  only make a delta if it actually changed.
        =?  self  !=(`cof (~(get by mirrors) wer))
          (so-delta-our %config wer %full cof)
        $(rem t.rem)
      ::
      ::  local status
      =.  self
        =+  sas=~(tap by loc.pes)
        |-  ^+  self
        ?~  sas  self
        =*  who  p.i.sas
        =*  sat  q.i.sas
        ::  only make a delta if it actually changed.
        =?  deltas  !=(`sat (~(get by locals) who))
          :_  deltas
          :^  %story  nom  %status
          [[our.bol nom] who %full sat]
        $(sas t.sas)
      ::
      ::  remote status
      =.  self
        =+  rem=~(tap by rem.pes)
        |-  ^+  self
        ?~  rem  self
        =*  wer  p.i.rem
        =*  gou  q.i.rem
        ::  only make deltas if it actually changed.
        =?  deltas  !=(`gou (~(get by remotes) wer))
          %+  welp  deltas
          =+  gop=~(tap by gou)
          =+  hav=(fall (~(get by remotes) wer) *group)
          =|  l/(list delta)
          |-  ^+  l
          ?~  gop  l
          =*  who  p.i.gop
          =*  sat  q.i.gop
          ::  only make a delta if it actually changed.
          =?  l  !=(`sat (~(get by hav) who))
            [[%story nom %status wer who %full sat] l]
          $(gop t.gop)
        $(rem t.rem)
      ::
      ::  telegrams
      =.  self
        %_  self
            deltas
          %+  welp  deltas
          %-  flop
          ^-  (list delta)
          %+  murn  gaz
          |=  t/telegram
          ^-  (unit delta)
          ::  in audience, replace above with us.
          ::TODO  this really should be done by the sender.
          =.  aud.t
            =+  dem=[(above:hall-lib [our now our]:bol) nom]
            ?.  (~(has in aud.t) dem)  aud.t
            =+  (~(del in aud.t) dem)
            (~(put in -) so-cir)
          =+  num=(~(get by known) uid.t)
          ?:  &(?=(^ num) =(t (snag u.num grams)))  ~
          ::TODO  this really should have sent us the message
          ::      src as well but that's not an easy fix.
          `[%story nom %gram [(above:hall-lib [our now our]:bol) nom] t]
        ==
      ::  inherited flag
      %_(self deltas [[%story nom %inherited &] deltas])
      ::TODO  runtime error
      ::(so-delta-our %inherited &)
    ::
    ::  #
    ::  #  %changes
    ::  #
    ::    arms that make miscellaneous changes to this story.
    ::
    ++  so-present
      ::  accept status diff
      ~/  %hall-so-present
      |=  {who/ship dif/diff-status}
      ^+  +>
      ::  only have presence if you have write permission.
      ?.  |((so-admire who) ?=($remove -.dif))  +>
      ::  ignore if it won't result in change,
      ::  or if it sets an impersonating handle.
      ?.  ?:  ?=($remove -.dif)  (~(has by locals) who)
          ?|  !(~(has by locals) who)
            ::
              =+  old=(~(got by locals) who)
              =+  new=(change-status:hall-lib - dif)
              ?&  !=(old new)
                ::
                  ?=  ~
                  (rush (fall han.man.new '') ;~(pfix sig fed:ag))
                  ::TODO  calling with %+ gives syntax error
              ==
          ==
        +>
      (so-delta-our %status so-cir who dif)
    ::
    ++  so-config-full
      ::    split full config
      ::
      ::  split a %full config delta up into multiple
      ::  smaller ones, for easier application.
      ::
      ~/  %hall-so-config-full
      |=  {old/(unit config) cof/config}
      ^+  +>
      ~?  &(?=(^ old) !=(src.u.old src.cof))
        %maybe-missing-src-changes
      %-  so-deltas
      =-  %+  turn  -
          |=  d/diff-config
          [%story nom [%config so-cir d]]
      ^-  (list diff-config)
      ::TODO  figure out how to deal with src changes here.
      ::      %follow will probably behave a bit iffy in some cases.
      ?~  old
        ::  if we have no previous config, all diffs apply.
        :~  [%caption cap.cof]
            [%usage & tag.cof]
            [%filter fit.cof]
            [%secure sec.con.cof]
            [%permit & sis.con.cof]
            [%read red.cof]
        ==
      =-  (murn - same)
      ^-  (list (unit diff-config))
      =*  col  u.old
      ::  if we have previous config, figure out the changes.
      :~  ?:  =(cap.col cap.cof)  ~
          `[%caption cap.cof]
        ::
          =+  gon=(~(dif in tag.col) tag.cof)
          ?~  gon  ~
          `[%usage | gon]
        ::
          =+  new=(~(dif in tag.cof) tag.col)
          ?~  new  ~
          `[%usage & new]
        ::
          ?:  =(fit.col fit.cof)  ~
          `[%filter fit.cof]
        ::
          ?:  =(sec.con.col sec.con.cof)  ~
          `[%secure sec.con.cof]
        ::
          =+  gon=(~(dif in sis.con.col) sis.con.cof)
          ?~  gon  ~
          `[%permit | gon]
        ::
          =+  new=(~(dif in sis.con.cof) sis.con.col)
          ?~  new  ~
          `[%permit & new]
      ==
    ::
    ++  so-sources
      ::    change source
      ::
      ::  adds or removes {srs} from our sources,
      ::  skipping over ones we already (don't) have.
      ::
      ~/  %hall-so-sources
      |=  {add/? srs/(set source)}
      ^+  +>
      =/  sus/(set source)
        %.  src.shape
        ?:(add ~(dif in srs) ~(int in srs))
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
    ++  so-depict
      ::    change description
      ::
      ::  modifies our caption.
      ::
      ~/  %hall-so-depict
      |=  cap/cord
      ^+  +>
      ?:  =(cap cap.shape)  +>
      (so-delta-our %config so-cir %caption cap)
    ::
    ++  so-usage
      ::  add or remove usage tags.
      ::
      ~/  %hall-so-usage
      |=  {add/? tas/tags}
      ^+  +>
      =/  sas/tags
        %.  tag.shape
        ?:(add ~(dif in tas) ~(int in tas))
      ?~  sas  +>.$
      (so-delta-our %config so-cir %usage add sas)
    ::
    ++  so-read
      ::  set the read message number in circle config
      ~/  %hall-so-read
      |=  {red/@ud}
      ^+  +>
      ?:  =(red red.shape)  +>
      (so-delta-our %config so-cir %read red)
    ::
    ++  so-filter
      ::    change message rules
      ::
      ::  modifies our filter.
      ::
      ~/  %hall-so-filter
      |=  fit/filter
      ^+  +>
      ?:  =(fit fit.shape)  +>
      (so-delta-our %config so-cir %filter fit)
    ::
    ++  so-delete
      ::    delete story
      ::
      ::  deletes this story. removes it from {stories}
      ::  and unsubscribes from all src.
      ::
      (so-delta-our %remove ~)
    ::
    ::  #
    ::  #  %subscriptions
    ::  #
    ::    arms for starting and ending subscriptions
    ::
    ++  so-greet
      ::    subscription started
      ::
      ::  store a started subscription as source.
      ::
      ~/  %hall-so-greet
      |=  src/source
      ^+  +>
      ?:  (~(has in src.shape) src)  +>
      (so-delta-our %config so-cir %source & src)
    ::
    ++  so-leave
      ::    subscription ended
      ::
      ::  delete {src} from our sources.
      ::
      ~/  %hall-so-leave
      |=  src/source
      ^+  +>
      ?.  (~(has in src.shape) src)  +>
      (so-delta-our %config so-cir %source | src)
    ::
    ++  so-resub
      ::    subscription revived
      ::
      ::  re-subscribe to a dropped subscription.
      ::  if it was already active, we continue where
      ::  we left off.
      ::
      ~/  %hall-so-resub
      |=  src/source
      ^+  +>
      =+  seq=(~(get by sequence) cir.src)
      =/  ner/range
        ?~  seq  ran.src
        =-  `[[%ud u.seq] -]
        ?~  ran.src  ~
        tal.u.ran.src
      ::  if our subscription changes or ends, remove
      ::  the original source.
      =?  +>.$  !=(ner ran.src)
        (so-delta-our %config so-cir %source | src)
      ::  if we're past the range, don't resubscribe.
      ?:  ?&  ?=(^ ran.src)
              ?=(^ tal.u.ran.src)
            ::
              ?-  -.u.tal.u.ran.src
                $sd   &
                $da   (gte now.bol +.u.tal.u.ran.src)
                $ud   ?&  ?=(^ seq)
                          (gte u.seq +.u.tal.u.ran.src)
                      ==
              ==
          ==
        +>.$
      (so-delta-our %follow & [[cir.src -] ~ ~])
    ::
    ++  so-first-grams
      ::    beginning of stream
      ::
      ::  find all grams that fall within the range.
      ::
      ~/  %hall-so-first-grams
      |=  ran/range
      ^-  (list telegram)
      =+  [num=0 gaz=grams zeg=*(list telegram)]
      ::  fill in empty ranges to select all grams,
      ::  and calculate absolutes for relative places.
      =.  ran
        ?~  ran  `[[%ud 0] `[%ud count]]
        =*  hed  hed.u.ran
        =?  hed  ?=($sd -.hed)
          [%ud (sub count (min count (abs:si +.hed)))]
        ?~  tal.u.ran  `[hed `[%ud count]]
        =*  tal  u.tal.u.ran
        =?  tal  ?=($sd -.tal)
          [%ud (sub count (min count (abs:si +.tal)))]
        ran
      ::  never fails, but compiler needs it.
      ?>  &(?=(^ ran) ?=(^ tal.u.ran))
      =*  hed  hed.u.ran
      =*  tal  u.tal.u.ran
      %-  flop
      |-  ^-  (list telegram)
      ?~  gaz  zeg
      ?:  ?-  -.u.tal.u.ran                             ::  after the end
            $ud  (lth +.u.tal.u.ran num)
            $da  (lth +.u.tal.u.ran wen.i.gaz)
            $sd  !!  ::  caught above
          ==
        ::  if past the range, we're done searching.
        zeg
      ?:  ?-  -.hed.u.ran                               ::  before the start
            $ud  (lth num +.hed.u.ran)
            $da  (lth wen.i.gaz +.hed.u.ran)
            $sd  !!  ::  caught above
          ==
        ::  if before the range, continue onward.
        $(num +(num), gaz t.gaz)
      ::  if in the range, add this gram and continue.
      $(num +(num), gaz t.gaz, zeg [i.gaz zeg])
    ::
    ++  so-in-range
      ::    place in range?
      ::
      ::  produces two booleans: whether we're
      ::  currently in the range, and whether the range
      ::  has passed.
      ::TODO  to deal with changed messages, we'll want
      ::      to be able to pass in a num.
      ::
      ~/  %hall-so-in-range
      |=  ran/range
      ^-  {in/? done/?}
      ?~  ran  [& |]
      =/  min
        ?-  -.hed.u.ran
          $sd  &  ::  relative is always in.
          $ud  (gth count +.hed.u.ran)
          $da  (gth now.bol +.hed.u.ran)
        ==
      ?~  tal.u.ran
        [min |]
      =-  [&(min -) !-]
      ?-  -.u.tal.u.ran
        $sd  |  ::  relative is always done.
        $ud  (gte +(+.u.tal.u.ran) count)
        $da  (gte +.u.tal.u.ran now.bol)
      ==
    ::
    ::  #
    ::  #  %messaging
    ::  #
    ::    arms for adding to this story's messages.
    ::
    ++  so-sane
      ::  sanitize %lin speech according to our settings.
      ::
      ~/  %hall-so-sane
      |=  sep/speech
      ^-  speech
      ?+  -.sep  sep
          ?($ire $fat $app)
        sep(sep $(sep sep.sep))
      ::
          $lin
        =-  sep(msg -)
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
    ++  so-unpack
      ::    process envelopes
      ::
      ::  learn telegrams from list of envelopes and
      ::  update the sequence of the source if needed.
      ::
      ~/  %hall-so-unpack
      |=  {src/circle nes/(list envelope)}
      ^+  +>
      =.  +>  (so-lesson src (turn nes tail))
      =/  num
        %+  roll  nes
        |=  {nev/envelope max/@ud}
        ?:((gth num.nev max) num.nev max)
      ?.  (gth num (fall (~(get by sequence) src) 0))
        +>.$
      (so-delta-our %sequent src num)
    ::
    ++  so-open
      ::    process envelope
      ::
      ::  learn telegram from envelope and update the
      ::  sequence of the source if needed.
      ::
      ~/  %hall-so-open
      |=  {src/circle nev/envelope}
      ^+  +>
      =.  +>  (so-learn src gam.nev)
      ?.  (gth num.nev (fall (~(get by sequence) src) 0))
        +>
      (so-delta-our %sequent src num.nev)
    ::
    ++  so-lesson
      ::  learn all telegrams in a list.
      ::
      ~/  %hall-so-lesson
      |=  {src/circle gaz/(list telegram)}
      ^+  +>
      ?~  gaz  +>
      $(gaz t.gaz, +> (so-learn src i.gaz))
    ::
    ++  so-learn
      ::    save/update message
      ::
      ::  store an incoming telegram, updating if it
      ::  already exists.
      ::
      ~/  %hall-so-learn
      |=  {src/circle gam/telegram}
      ^+  +>
      ::  check for write permissions.
      ::TODO  we want to !! instead of silently failing,
      ::      so that ++coup-repeat of the caller gets
      ::      an error. but the caller may not be the
      ::      author. if we check for that to be true,
      ::      can we guarantee it's not an older message
      ::      getting resent? does that matter? think.
      ?.  (so-admire aut.gam)  +>
      ::  clean up the message to conform to our rules.
      =.  sep.gam  (so-sane sep.gam)
      ::
      =.  gam  (filter-gram gam bol)
      ::  if we already have it, ignore.
      =+  old=(~(get by known) uid.gam)
      ?.  &(?=(^ old) =(gam (snag u.old grams)))
        (so-delta-our %gram src gam)
      =+  sed=(~(get by sourced) src)
      ?:  |(?=(~ sed) ?=(~ (find [u.old]~ u.sed)))
        (so-delta-our %sourced src u.old)
      +>.$
    ::
    ::  #
    ::  #  %permissions
    ::  #
    ::    arms relating to story permissions.
    ::
    ++  so-permit
      ::    invite/banish
      ::
      ::  update config to dis/allow ships permission.
      ::
      ~/  %hall-so-permit
      |=  {inv/? sis/(set ship)}
      ^+  +>
      ::  wyt:  whitelist?
      ::  add:  add to list?
      =/  wyt/?  ?=(?($village $journal) sec.con.shape)
      =/  add/?  =(inv wyt)
      =/  sus/(set ship)
        %.  sis.con.shape
        ?:(add ~(dif in sis) ~(int in sis))
      ?~  sus  +>.$
      ::  if banished, remove their presences.
      =?  +>.$  !inv
        %-  so-deltas-our
        %+  turn  ~(tap in `(set ship)`sus)
        |=  s/ship
        :+  %status  so-cir
        [s %remove ~]
      (so-delta-our %config so-cir %permit [add sus])
    ::
    ++  so-admire
      ::    accept from
      ::
      ::  checks {her} write permissions.
      ::
      ~/  %hall-so-admire
      |=  her/ship
      ^-  ?
      ?-  sec.con.shape
        $channel  !(~(has in sis.con.shape) her)        ::  blacklist
        $village  (~(has in sis.con.shape) her)         ::  whitelist
        $journal  (~(has in sis.con.shape) her)         ::  author whitelist
        $mailbox  !(~(has in sis.con.shape) her)        ::  author blacklist
        $custom                                         ::  custom rule
          =/  rul/$-({ship ?($r $w) bowl:gall} ?)
            (fall (~(get by custom-rules) nom) |=(* |))
          (rul her %w bol)
      ==
    ::
    ++  so-visible
      ::  checks {her} read permissions.
      ::
      ~/  %hall-so-visible
      |=  her/ship
      ^-  ?
      ?-  sec.con.shape
        $channel  !(~(has in sis.con.shape) her)        ::  blacklist
        $village  (~(has in sis.con.shape) her)         ::  whitelist
        $journal  &                                     ::  all
        $mailbox  (team:title our.bol her)              ::  our team
        $custom                                         ::  custom rule
          =/  rul/$-({ship ?($r $w) bowl:gall} ?)
            (fall (~(get by custom-rules) nom) |=(* |))
          (rul her %r bol)
      ==
    --
  --
::
++  da
  ::    delta application
  ::
  ::  core for doing things, mostly applying deltas to
  ::  application state, but also dealing with events
  ::  that aren't pokes.
  ::  where appropriate, creates moves. those get
  ::  produced when finalizing with ++da-done.
  ::
  ~/  %hall-da
  |_  ::  moves: moves created by core operations.
      ::
      moves/(list move)
  ::  #  %resolve
  ::
  ++  da-done
    ::    resolve core
    ::
    ::  produces the moves stored in ++da's moves.
    ::  they are produced in reverse order because
    ::  ++da-emil and ++da-emit add them to the head of
    ::  the {moves}.
    ::
    ^-  (quip move _+>)
    [(flop moves) +>]
  ::
  ::  #
  ::  #  %emitters
  ::  #
  ::    arms that create outward changes.
  ::
  ++  da-emil
    ::    emit move list
    ::
    ::  adds multiple moves to the head of {moves}.
    ::  flops to stay consistent with ++ta-emit.
    ::
    ~/  %hall-da-emil
    |=  mol/(list move)
    %_(+> moves (welp (flop mol) moves))
  ::
  ++  da-emit
    ::    emit a move
    ::
    ::  adds a move to the head of {moves}.
    ::
    ~/  %hall-da-emit
    |=  mov/move
    %_(+> moves [mov moves])
  ::
  ++  da-present
    ::  send %present cmd
    ::
    ~/  %hall-da-present
    |=  {hos/ship nos/(set name) dif/diff-status}
    ^+  +>
    %-  da-emit
    :*  ost.bol
        %poke
        /present
        [hos dap.bol]
        [%hall-command %present nos dif]
    ==
  ::
  ::  #
  ::  #  %change-application
  ::  #
  ::    arms that change the application state.
  ::
  ++  da-change
    ::    apply delta
    ::
    ::  modifies application state according to the
    ::  change specified in {dif}.
    ::
    ~/  %hall-da-change
    |=  det/delta
    ^+  +>
    ?-  -.det
      $public   (da-change-public +.det)
      $out      (da-change-out +.det)
      $done     (da-change-done +.det)
      $glyph    (da-change-glyph +.det)
      $nick     (da-change-nick +.det)
      $story    (da-change-story +.det)
      $init     da-init
      $observe  (da-observe +.det)
      $present  (da-present +.det)
    ==
  ::
  ++  da-init
    ::    startup side-effects
    ::
    ::  apply %init delta, querying the /burden of the
    ::  ship above us.
    ::
    (da-emit (wire-to-peer /burden))
  ::
  ++  da-observe
    ::    watch burden bearer
    ::
    ::  apply %observe delta, querying the /report of
    ::  {who} below us.
    ::
    ~/  %hall-da-observe
    |=  who/ship
    (da-emit (wire-to-peer /report/(scot %p who)))
  ::
  ++  da-change-public
    ::    show/hide membership
    ::
    ::  add/remove a circle to/from the public
    ::  membership list.
    ::
    ~/  %hall-da-change-public
    |=  {add/? cir/circle}
    ^+  +>
    =-  +>.$(public -)
    ?:  add  (~(put in public) cir)
    (~(del in public) cir)
  ::
  ++  da-change-out
    ::    outgoing messages
    ::
    ::  apply an %out delta, sending a message.
    ::
    ~/  %hall-da-change-out
    |=  {cir/circle out/(list thought)}
    ^+  +>
    =+  ses=(turn out head)
    =.  outbox
      ::  for every serial, add %pending state.
      %+  roll  ses
      |=  {s/serial o/_outbox}
      =?  o  ?=(~ o)  outbox
      =+  t=(fall (~(get by o) s) *tracking)
      %+  ~(put by o)  s
      (~(put by t) cir %pending)
    %+  da-emit  ost.bol
    :*  %poke
        /repeat/(scot %p hos.cir)/[nom.cir]/(scot %ud (jam ses))
        [hos.cir dap.bol]
        [%hall-command %publish out]
    ==
  ::
  ++  da-change-done
    ::    delivered messages
    ::
    ::  apply a %done delta, setting new delivery state
    ::  for messages.
    ::
    ~/  %hall-da-change-done
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
  ++  da-change-glyph
    ::    un/bound glyph
    ::
    ::  apply a %glyph delta, un/binding a glyph to/from
    ::  an audience.
    ::
    ~/  %hall-da-change-glyph
    |=  {bin/? gyf/char aud/audience}
    ^+  +>
    ?:  bin
      %_  +>
        binds  (~(put ju binds) gyf aud)
      ==
    =/  ole/(list audience)
      ?.  =(aud ~)  [aud ~]
      ~(tap in (~(get ju binds) gyf))
    |-  ^+  +>.^$
    ?~  ole  +>.^$
    %_  $
      binds  (~(del ju binds) gyf i.ole)
      ole  t.ole
    ==
  ::
  ++  da-change-nick
    ::    changed nickname
    ::
    ::  apply a %nick delta, setting a nickname for a
    ::  ship.
    ::
    ~/  %hall-da-change-nick
    |=  {who/ship nic/nick}
    ^+  +>
    +>(nicks (change-nicks:hall-lib nicks who nic))
  ::
  ::  #
  ::  #  %stories
  ::  #
  ::    arms for modifying stories.
  ::
  ++  da-change-story
    ::    apply circle delta
    ::
    ::  apply a %story delta, redirecting the delta
    ::  itself to ++sa-change.
    ::  in case of a new or deleted story, specialized
    ::  arms are called.
    ::
    ~/  %hall-da-change-story
    |=  {nom/name det/delta-story}
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
  ++  da-create
    ::    configure story
    ::
    ::  creates story {nom} with config {con}.
    ::
    ~/  %hall-da-create
    |=  {nom/name cof/config}
    ^+  +>
    =<  sa-done
    %-  ~(sa-change sa nom *story)
    [%config [our.bol nom] %full cof]
  ::
  ++  da-delete
    ::    delete story
    ::
    ::  calls the story core to delete story {nom}.
    ::
    ~/  %hall-da-delete
    |=  nom/name
    ^+  +>
    =.  +>
      %-  da-emil
      ~(sa-delete sa nom (~(got by stories) nom))
    +>(stories (~(del by stories) nom))
  ::
  ++  sa
    ::    story delta core
    ::
    ::  story core, used for doing work on a story.
    ::
    ~/  %hall-sa
    |_  ::  nom: story name in {stories}.
        ::  story is faceless to ease data access.
        ::
        $:  nom/name
            story
        ==
    ::  #  %resolve
    ::
    ++  sa-done
      ::    apply changes
      ::
      ::  put changed story back into the map.
      ::
      +>(stories (~(put by stories) nom +<+))
    ::
    ::  #
    ::  #  %emitters
    ::  #
    ::    arms that create outward changes.
    ::
    ++  sa-emil
      ::    emit move list
      ::
      ::  adds multiple moves to the head of {moves}.
      ::  flops to stay consistent with ++ta-emit.
      ::
      ~/  %hall-sa-emil
      |=  mol/(list move)
      %_(+> moves (welp (flop mol) moves))
    ::
    ++  sa-emit
      ::    emit a move
      ::
      ::  adds a move to the head of {moves}.
      ::
      ~/  %hall-sa-emit
      |=  mov/move
      %_(+> moves [mov moves])
    ::
    ++  sa-sauce
      ::  cards to moves.
      ::
      ~/  %hall-sa-sauce
      |=  {ost/bone cub/(list card)}
      ^-  (list move)
      (flop (turn cub |=(a/card [ost a])))
    ::
    ::  #
    ::  #  %data
    ::  #
    ::    utility functions for data retrieval.
    ::
    ++  sa-cir  [our.bol nom]
    ::
    ::  #
    ::  #  %delta-application
    ::  #
    ::    arms for applying deltas.
    ::
    ++  sa-delete
      ::    deletion of story
      ::
      ::  apply a %remove story delta, unsubscribing
      ::  this story from all its active sources.
      ::
      %+  weld
        (sa-abjure src.shape)
      (sa-eject ~(key by peers))
    ::
    ++  sa-change
      ::    apply circle delta
      ::
      ::  figure out whether to apply a %story delta to
      ::  local or remote data.
      ::
      ~/  %hall-sa-change
      |=  det/delta-story
      ^+  +>
      %.  det
      ?:  ?&  ?=(?($config $status) -.det)
              !=(cir.det sa-cir)
          ==
        sa-change-remote
      sa-change-local
    ::
    ++  sa-change-local
      ::    apply our delta
      ::
      ::  apply a %story delta to local data.
      ::
      ~/  %hall-sa-change-local
      |=  det/delta-story
      ^+  +>
      ?+  -.det
        ~&([%unexpected-delta-local -.det] !!)
      ::
          $inherited
        +>(inherited ihr.det)
      ::
          $peer
        ?:  add.det
          +>(peers (~(add ja peers) who.det qer.det))
        =+  qes=(~(get ja peers) who.det)
        =.  qes
          =+  res=(find ~[qer.det] qes)
          ?~  res  qes
          (oust [u.res 1] qes)
        ?~  qes  +>.$(peers (~(del by peers) who.det))
        +>.$(peers (~(put in peers) who.det qes))
      ::
          $follow
        (sa-emil (sa-follow-effects sub.det srs.det))
      ::
          $sequent
        +>(sequence (~(put by sequence) cir.det num.det))
      ::
          $gram
        (sa-change-gram +.det)
      ::
          $sourced
        (sa-add-gram-source +.det)
      ::
          $config
        =.  +>
          %-  sa-emil
          (sa-config-effects shape dif.det)
        +>(shape (change-config:hall-lib shape dif.det))
      ::
          $status
        %_  +>
            locals
          ?:  ?=($remove -.dif.det)
            (~(del by locals) who.det)
          %+  ~(put by locals)  who.det
          %+  change-status:hall-lib
            (fall (~(get by locals) who.det) *status)
          dif.det
        ==
      ==
    ::
    ++  sa-add-gram-source
      ::    remember message source
      ::
      ::  if it's not already known, make note of the
      ::  fact that message {num} was heard from {src}.
      ::
      ~/  %hall-sa-add-gram-source
      |=  {src/circle num/@ud}
      ^+  +>
      =-  +>.$(sourced -)
      =+  sed=(fall (~(get by sourced) src) ~)
      ?^  (find ~[num] sed)  sourced
      (~(put by sourced) src [num sed])
    ::
    ++  sa-change-gram
      ::    save/update message
      ::
      ::  apply a %gram delta, either appending or
      ::  updating a message.
      ::
      ~/  %hall-sa-change-gram
      |=  {src/circle gam/telegram}
      ^+  +>
      ::TODO  move "known" logic up into ++so? that way,
      ::      we can attach message numbers to changes.
      =+  old=(~(get by known) uid.gam)
      ?~  old
        ::  new message
        %.  [src count]
        %_  sa-add-gram-source
          grams    (welp grams [gam ~])
          count    +(count)
          known    (~(put by known) uid.gam count)
        ==
      ::  changed message
      %.  [src u.old]
      %_  sa-add-gram-source
        grams    %+  welp
                 (scag u.old grams)
                 [gam (slag +(u.old) grams)]
      ==
    ::
    ++  sa-change-remote
      ::    apply remote's delta
      ::
      ::  apply a story diff to remote data.
      ::
      ~/  %hall-sa-change-remote
      |=  det/delta-story
      ^+  +>
      ?+  -.det
        ~&([%unexpected-delta-remote -.det] !!)
      ::
          $config
        ?:  ?=($remove -.dif.det)
          +>(mirrors (~(del by mirrors) cir.det))
        =/  new/config
          %+  change-config:hall-lib
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
          (~(put by ole) who.det (change-status:hall-lib old dif.det))
        ==
      ==
    ::
    ++  sa-config-effects
      ::  apply side-effects for a %config delta.
      ::
      ~/  %hall-sa-change-effects
      |=  {old/config dif/diff-config}
      ^-  (list move)
      ?+  -.dif  ~
        $permit   (sa-permit-effects sec.con.old sis.con.old +.dif)
        ::NOTE  when doing a lone %secure, calculate the
        ::      necessary %permit deltas alongside it.
      ==
    ::
    ++  sa-follow-effects
      ::    un/subscribe
      ::
      ::  apply side-effects for a %follow delta,
      ::  un/subscribing this story to/from {cos}.
      ::
      ~/  %hall-sa-follow-effects
      |=  {sub/? srs/(set source)}
      ^-  (list move)
      %.  srs
      ?:(sub sa-acquire sa-abjure)
    ::
    ++  sa-permit-effects
      ::    notify permitted
      ::
      ::  apply side-effects for a %permit delta,
      ::  kicking the subscriptions of {sis} if they
      ::  are being banished.
      ::
      ~/  %hall-sa-permit-effects
      |=  {sec/security old/(set ship) add/? sis/(set ship)}
      ^-  (list move)
      =/  wyt  ?=(?($village $journal) sec)
      =/  inv  =(wyt add)
      ?:  inv  ~
      =/  sus/(set ship)
        %.  sis.con.shape
        ?:(add ~(dif in sis) ~(int in sis))
      (sa-eject sus)
    ::
    ::  #
    ::  #  %subscriptions
    ::  #
    ::    arms for starting and ending subscriptions
    ::
    ++  sa-acquire
      ::  subscribes this story to each circle.
      ::
      ~/  %hall-sa-aquire
      |=  srs/(set source)
      =-  (murn - same)
      %+  turn  ~(tap in srs)
      |=  {cir/circle ran/range}
      ^-  (unit move)
      ?:  =(cir sa-cir)  ~  ::  ignore self-subs
      =+  wat=~[%grams %config-l %group-l]
      `(wire-to-peer (circle-wire nom wat cir ran))
    ::
    ++  sa-abjure
      ::  unsubscribes this story from each circle.
      ::
      ~/  %hall-sa-abjure
      |=  srs/(set source)
      ^-  (list move)
      %+  turn  ~(tap in srs)
      |=  {cir/circle ran/range}
      ^-  move
      =/  wir
        %^  circle-wire  nom
          ~[%grams %config-l %group-l]
        [cir ran]
      [ost.bol %pull wir [hos.cir dap.bol] ~]
    ::
    ++  sa-eject
      ::  removes ships {sis} from {followers}.
      ::
      ~/  %hall-sa-eject
      |=  sis/(set ship)
      ^-  (list move)
      %-  zing
      %+  turn  ~(tap in sup.bol)
      |=  {b/bone s/ship p/path}
      ^-  (list move)
      ?.  ?&  (~(has in sis) s)
              ?=({$circle @tas *} p)
              =(i.t.p nom)
          ==
        ~
      (gentle-quit b s (path-to-query p))
    ::
    ++  sa-unearth
      ::    ships' bones
      ::
      ::  find the bones in {sup.bol} that belong to
      ::  a ship in {sis}.
      ::
      ~/  %hall-sa-unearth
      |=  sis/(set ship)
      ^-  (set bone)
      %-  ~(rep in sup.bol)
      |=  {{b/bone s/ship p/path} c/(set bone)}
      ?.  ?&  (~(has in sis) s)
              ?=({$circle @tas *} p)
              =(i.t.p nom)
          ==
        c
      (~(put in c) b)
    --
--
::
::
::  #
::  #  %wire-utility
::  #
::
++  circle-wire
  ::    /circle peer wire
  ::
  ::  constructs a /circle %peer path for subscribing
  ::  {nom} to a source.
  ::
  ~/  %hall-circle-wire
  |=  {nom/name wat/(list circle-data) source}
  ^-  wire
  ;:  weld
    /circle/[nom]/(scot %p hos.cir)/[nom.cir]
    (sort wat gth)  ::  consistence
    (range-to-path:hall-lib ran)
  ==
::
++  wire-to-peer
  ::    peer move from wire
  ::
  ::  builds the peer move associated with the wire.
  ::
  ~/  %hall-wire-to-peer
  |=  wir/wire
  ^-  move
  =+  tar=(wire-to-target wir)
  [ost.bol %peer wir [p.tar dap.bol] q.tar]
::
++  wire-to-target
  ::    ship+path from wire
  ::
  ::  parses {wir} to obtain the target ship and the
  ::  query path.
  ::
  ~/  %hall-wire-to-target
  |=  wir/wire
  ^-  (pair ship path)
  ?+  wir  ~&(wir !!)
      {$circle @ @ *}
    :-  (slav %p i.t.t.wir)
    (welp /circle t.t.t.wir)
  ::
      {$burden *}
    :-  (above:hall-lib [our now our]:bol)
    /burden/(scot %p our.bol)
  ::
      {$report @ *}
    :-  (slav %p i.t.wir)
    /report
  ==
::
++  etch
  ::    parse wire
  ::
  ::  parses {wir} to obtain either %circle with story
  ::  and circle or %repeat with message number, source
  ::  ship, story and serials.
  ::
  ~/  %hall-etch
  |=  wir/wire
  ^-  weir
  ?+    wir  !!
      {$circle @ @ @ *}
      :: $circle, us, host, target
    :^    %circle
        i.t.wir
      [(slav %p i.t.t.wir) i.t.t.t.wir]
    (path-to-range:hall-lib t.t.t.t.wir)
  ::
      {$repeat @ @ @ ~}
    :+  %repeat
      [(slav %p i.t.wir) i.t.t.wir]
    ((list serial) (cue (slav %ud i.t.t.t.wir)))
  ==
::
++  etch-circle
  ::    parse /circle wire
  ::
  ::  parses a /circle wire, call a gate with the
  ::  result.
  ::
  ~/  %hall-etch-circle
  |=  $:  wir/wire
          $=  fun
          $-  {nom/name src/source}
              {(list move) _.}
      ==
  =+  wer=(etch wir)
  ?>(?=($circle -.wer) (fun nom.wer src.wer))
::
++  etch-repeat
  ::  parses a /repeat wire, call gate with the result.
  ::
  ~/  %hall-etch-repeat
  |=  $:  wir/wire
          $=  fun
          $-  {cir/circle ses/(list serial)}
              {(list move) _.}
      ==
  =+  wer=(etch wir)
  ?>(?=($repeat -.wer) (fun cir.wer ses.wer))
::
++  gentle-quit
  ::    quit other, pull us
  ::
  ::  we want to gently pull our own subscriptions,
  ::  rather than quitting them, so that we may
  ::  differentiate between a gall/ames quit and a
  ::  foreign quit. but since wex.bol isn't filled,
  ::  we'll have to just guess at what the correct wire
  ::  wire is. this is truly terrible, but will have to
  ::  do for now.
  ::TODO  get rid of this once gall improves.
  ::      it needs to tell us the difference between
  ::      an app-caused quit and a queue-caused one.
  ::      (aka connected/disconnected/rejected state)
  ::
  ~/  %hall-gentle-quit
  |=  {bon/bone who/ship qer/query}
  ^-  (list move)
  ?.  ?=($circle -.qer)  ~
  ?.  =(who our.bol)  [bon %quit ~]~
  %-  zing
  %+  turn  ~(tap in ~(key by stories))
  |=  n/name
  ^-  (list move)
  :~  :^  ost.bol  %poke  /
      :+  [our.bol dap.bol]  %hall-action
      :^  %source  n  |
      [[[our.bol nom.qer] ran.qer] ~ ~]
    ::
      :^  ost.bol  %pull
        %^  circle-wire  n  ~(tap in wat.qer)
        [[our.bol nom.qer] ran.qer]
      [[our.bol dap.bol] ~]
  ==
::
::  #
::  #  %new-events
::  #
++  bake
  ::    apply state delta
  ::
  ::  applies a change to the application state,
  ::  producing side-effects.
  ::
  ~/  %hall-bake
  |=  det/delta
  ^-  (quip move _+>)
  da-done:(da-change:da det)
::
++  pre-bake
  ::  apply more deltas
  ::
  ~/  %hall-pre-bake
  |=  des/(list delta)
  ^-  (quip move _+>)
  =|  moz/(list move)
  |-  ^-  (quip move _+>.^$)
  ?~  des  [moz +>.^$]
  =^  mos  +>.^$  (bake i.des)
  $(moz :(welp moz mos (affection i.des)), des t.des)
  ::TODO  ideally you'd just do this, but that runtime errors on "bake"...
  ::%+  roll  des
  ::|=  {d/delta m/(list move) _+>.$}
  ::=^  mos  +>.^$  (bake d)
  ::[:(welp m mos (affection d)) +>.^$]
::
++  peek
  ~/  %hall-peek
  |=  pax/path
  ?>  ?=({$x *} pax)  ::  others unsupported.
  ^-  (unit (unit (pair mark prize)))
  =+  piz=(look (path-to-query t.pax))
  ?~  piz  ~
  ?~  u.piz  [~ ~]
  ``[%hall-prize u.u.piz]
::
++  look
  ::    query on state
  ::
  ::  find the result (if any) for a given query.
  ::
  ~/  %hall-look
  |=  qer/query
  ^-  (unit (unit prize))
  ?-  -.qer
      $client
    ``[%client binds nicks]
  ::
      $circles
    =-  ``[%circles -]
    %-  ~(gas in *(set name))
    %+  murn  ~(tap by stories)
    |=  {n/name s/story}
    ^-  (unit name)
    ?:((~(so-visible so:ta n ~ s) who.qer) `n ~)
  ::
      $public
    ``[%public public]
  ::
      $burden
    :+  ~  ~
    :-  %burden
    %-  ~(gas in *(map name burden))
    %+  murn  ~(tap by stories)
    |=  {n/name s/story}
    ^-  (unit (pair name burden))
    ::  only auto-federate channels for now.
    ?.  ?=($channel sec.con.shape.s)  ~
    :+  ~  n
    ::  share no more than the last 100, for performance reasons.
    :+  ?:  (lte count.s 100)  grams.s
        (slag (sub count.s 100) grams.s)
      [shape.s mirrors.s]
    [locals.s remotes.s]
  ::
      $report
    ::TODO  gall note: need to be able to subscirbe to just changes... or just
    ::      data etc.
    ``[%report ~]
  ::
      $peers
    =+  soy=(~(get by stories) nom.qer)
    ?~  soy  ~
    ``[%peers peers.u.soy]
  ::
      $circle  ::REVIEW  should we send precs & config to out of range subs?
    =+  soy=(~(get by stories) nom.qer)
    ?~  soy  ~
    :+  ~  ~
    :-  %circle
    :+  ?.  (~(has in wat.qer) %grams)  ~
        %+  turn
          =-  (~(so-first-grams so:ta nom.qer ~ -) ran.qer)
          ::TODO  this can be done more efficiently.
          ?~  wer.qer  u.soy
          =-  u.soy(grams -, count (lent -))
          ?.  (~(has by sourced.u.soy) u.wer.qer)  ~
          %+  turn
            %-  flop
            (~(got by sourced.u.soy) u.wer.qer)
          |=  n/@ud
          (snag n grams.u.soy)
        (cury gram-to-envelope nom.qer)
      :-  shape.u.soy
      ?.  (~(has in wat.qer) %config-r)  ~
      mirrors.u.soy
    :-  locals.u.soy
    ?.  (~(has in wat.qer) %group-r)  ~
    remotes.u.soy
  ==
::
++  dedicate
  ::    rumor-story to theirs
  ::
  ::  modify a %story diff to make it about their ship
  ::  instead of ours.
  ::
  ~/  %hall-dedicate
  |=  {who/ship nom/name det/delta-story}
  ^-  rumor-story
  ?+  -.det  det
    ::
    ::  internal-only changes.
    $follow     !!
    $inherited  !!
    $sequent    !!
    $sourced    !!
  ::
      $gram
    :+  %gram
      ?.  =(src.det [our.bol nom])
        src.det
      [who nom]
    %+  gram-to-envelope  nom
    %_  gam.det
        aud
      %-  ~(run in aud.gam.det)
      |=  c/circle
      ?.  =(c [our.bol nom])  c
      [who nom]
    ==
  ::
      $config
    ?.  =(cir.det [our.bol nom])
      det
    det(cir [who nom])
  ::
      $status
    ?.  =(cir.det [our.bol nom])
      det
    det(cir [who nom])
  ==
::
++  gram-to-envelope
  ::    wrap gram with nr
  ::
  ::  deduce the initial msg number from a telegram
  ::  for a given story. assumes both story and
  ::  telegram are known.
  ::
  ~/  %hall-gram-to-envelope
  |=  {nom/name gam/telegram}
  ^-  envelope
  :_  gam
  %.  uid.gam
  ~(got by known:(~(got by stories) nom))
::
++  circle-feel-story
  ::
  ~/  %hall-circle-feel-story
  |=  $:  wer/(unit circle)
          wat/(set circle-data)
          nom/name
          det/delta-story
      ==
  ^-  ?
  ?&
    ?~  wer  &
    ?+  -.det  &
      $gram     =(src.det u.wer)
      $config   =(cir.det u.wer)
      $status   =(cir.det u.wer)
    ==
  ::
    ?:  =(wat ~)  &
    ?+  -.det   |
      $gram     (~(has in wat) %grams)
      $new      (~(has in wat) %config-l)
      $remove   (~(has in wat) %config-l)
      $config   ?:  =(cir.det [our.bol nom])
                  (~(has in wat) %config-l)
                (~(has in wat) %config-r)
      $status   ?:  =(cir.det [our.bol nom])
                  (~(has in wat) %group-l)
                (~(has in wat) %group-r)
    ==
  ==
::
++  feel
  ::    delta to rumor
  ::
  ::  if the given delta changes the result of the given
  ::  query, produce the relevant rumor.
  ::
  ~/  %hall-feel
  |=  {qer/query det/delta}
  ^-  (unit rumor)
  ?-  -.qer
      $client
    ::  changes to shared ui state apply.
    ?+  -.det  ~
      $glyph  `[%client det]
      $nick   `[%client det]
    ==
  ::
      $circles
    ::NOTE  this is another case where having access to
    ::      the pre-delta state would be nice to have.
    ?.  ?=($story -.det)  ~
    =;  add/(unit ?)
      ?~  add  ~
      `[%circles u.add nom.det]
    ::REVIEW  this could be considered leaky, since it
    ::        doesn't check if {who} ever knew of {nom},
    ::        but does that matter? can't really check..
    ::  if the story got deleted, remove it from the circles listing.
    ?:  ?=($remove -.det.det)  `|
    =+  soy=(~(got by stories) nom.det)
    ::  if the story got created, or something about the read permissions set
    ::  for the subscriber changed, update the circles listing.
    =;  dif/?
      ?.  dif  ~
      ::  if the story just got created, don't send a remove rumor, because it
      ::  never showed up in the first place.
      =-  ?:(&(?=($new -.det.det) !-) ~ `-)
      ?|  (team:title our.bol who.qer)
          (~(so-visible so:ta nom.det ~ soy) who.qer)
      ==
    ?|  ?=($new -.det.det)
      ::
        ?&  ?=($config -.det.det)
            ?=($permit -.dif.det.det)
            ?=(?($channel $village) sec.con.shape.soy)
            (~(has in sis.dif.det.det) who.qer)
        ==
    ==
  ::
      $public
    ?.  ?=($public -.det)  ~
    `det
  ::
      $burden
    ?.  ?=($story -.det)  ~
    ?:  &(=(who.qer src.bol) =(rir /report/(scot %p src.bol)))  ~
    ?:  ?=(?($follow $inherited $sequent $sourced) -.det.det)  ~
    ::  only burden channels for now.
    ?.  (~(has by stories) nom.det)  ~
    ?.  =(%channel sec.con.shape:(~(got by stories) nom.det))  ~
    `[%burden nom.det (dedicate who.qer nom.det det.det)]
  ::
      $report
    ::  only send changes we didn't get from above.
    ?:  =(src.bol (above:hall-lib [our now our]:bol))  ~
    ::  only send story reports about grams and status.
    ?.  ?=($story -.det)  ~
    ?.  ?=(?($gram $status) -.det.det)  ~
    =+  soy=(~(got by stories) nom.det)
    ::  and only if the story is inherited.
    ?.  inherited.soy  ~
    ::  only burden channels for now.
    ?.  =(%channel sec.con.shape.soy)  ~
    `[%burden nom.det (dedicate (above:hall-lib [our now our]:bol) nom.det det.det)]
  ::
      $peers
    ?.  ?=($story -.det)      ~
    ?.  =(nom.qer nom.det)    ~
    ?.  ?=($peer -.det.det)   ~
    `[%peers +.det.det]
  ::
      $circle
    ?.  ?=($story -.det)                              ~
    ?.  =(nom.qer nom.det)                            ~
    ?.  %-  circle-feel-story
        [wer.qer wat.qer nom.det det.det]             ~
    ?.  ?|  ?=($remove -.det.det)
          ::
            =<  in  %.  ran.qer
            =+  soy=(~(got by stories) nom.qer)
            ~(so-in-range so:ta nom.qer ~ soy)
        ==
      ~
    ?.  ?=(?($gram $new $config $status $remove) -.det.det)   ~
    :+  ~  %circle
    ?+  det.det  det.det
        {$gram *}
      :+  %gram  src.det.det
      (gram-to-envelope nom.det gam.det.det)
    ==
  ==
::
++  affection
  ::    rumors to interested
  ::
  ::  for a given delta, send rumors to all queries it
  ::  affects.
  ::
  ~/  %hall-affection
  |=  det/delta
  ^-  (list move)
  ::  cache results for paths.
  =|  res/(map path (list move))
  %-  zing
  %+  turn  ~(tap by sup.bol)
  |=  {b/bone s/ship p/path}
  ^-  (list move)
  =+  mur=(~(get by res) p)
  ?^  mur  u.mur
  =-  =.  res  (~(put by res) p -)
      -
  =+  qer=(path-to-query p)
  %+  welp
    =+  rum=(feel qer det)
    ?~  rum  ~
    [b %diff %hall-rumor u.rum]~
  ?.  ?=($circle -.qer)  ~
  ::  kill the subscription if we forgot the story.
  ?.  (~(has by stories) nom.qer)  (gentle-quit b s qer)
  ::  kill the subscription if it's past its range.
  =-  ?:(done:- (gentle-quit b s qer) ~)
  %.  ran.qer
  =-  ~(so-in-range so:ta nom.qer ~ -)
  (~(got by stories) nom.qer)
::
++  path-to-query
  ::    path, coins, query
  ::
  ::  parse a path into a (list coin), then parse that
  ::  into a query structure.
  ::
  ~/  %hall-path-to-query
  |=  pax/path
  ?.  ?=({$circle @tas *} pax)
    (coins-to-query (path-to-coins pax))
  =/  qer/query  [%circle i.t.pax ~ ~ ~]
  ?>  ?=($circle -.qer)  ::  for type system.
  =+  pax=t.t.pax
  =+  ^-  {qer/query pax/path}
    ?.  ?=({@ @ *} pax)  [qer pax]
    =+  hos=(slaw %p i.pax)
    ?~  hos  [qer pax]
    :_  t.t.pax
    qer(wer `[u.hos i.t.pax])
  ?>  ?=($circle -.qer)
  |-  ^+  qer
  ?~  pax  qer
  ::TODO  can probably do this a bit better...
  ?+  i.pax
    qer(ran (path-to-range:hall-lib pax))
  ::
    circle-data   %_  $  pax  t.pax
                    wat.qer   (~(put in wat.qer) i.pax)
                  ==
    $group        %_  $  pax  t.pax
                    wat.qer   %-  ~(uni in wat.qer)
                              ^+  wat.qer
                              =/  dat=(list circle-data)  [%group-l %group-r ~]
                              (sy dat)
                  ==
    $config       %_  $  pax  t.pax
                    wat.qer   %-  ~(uni in wat.qer)
                              ^+  wat.qer
                              =/  dat=(list circle-data)  [%config-l %config-r ~]
                              (sy dat)
                  ==
  ==
::
++  path-to-coins
  ::    path to coin list
  ::
  ::  parse a path into a list of coins.
  ::
  ~/  %hall-path-to-coins
  |=  pax/path
  ^-  (list coin)
  %+  turn  `path`pax
  |=  a/@ta
  (need (slay a))
::
++  coins-to-query
  ::    coin list to query
  ::
  ::  parse a list of coins into a query structure.
  ::
  ^-  $-((list coin) query)
  =>  depa:hall-lib
  |^  %-  af  :~
          [%client ul]
          [%circles (at /[%p])]
          [%public ul]
          [%burden (at /[%p])]
          [%report ul]
      ==
  ++  term  (do %tas)
  ++  rang  (mu (al plac (mu (un plac))))
  ++  plac  (or %da %ud)
  --
::
++  leak
  ::    visible to
  ::
  ::  determine if the given query is visible to the
  ::  ship.
  ::
  ~/  %hall-leak
  |=  {who/ship qer/query}
  ^-  ?
  ?-  -.qer
    $client   (team:title our.bol who)
    $circles  =(who who.qer)
    $public   &
    $burden   ?&  =(who who.qer)
                  =(our.bol (above:hall-lib our.bol now.bol who))
              ==
    $peers    =(who our.bol)  ::TODO  or so-visible?
    $report   =(who (above:hall-lib [our now our]:bol))
  ::
      $circle
    ?.  (~(has by stories) nom.qer)  |
    %.  who
    ~(so-visible so:ta nom.qer ~ (~(got by stories) nom.qer))
  ==
::
::  #
::  #  %poke-events
::  #
::
++  poke-hall-command
  ::    accept command
  ::
  ::  incoming hall command. process it and update logs.
  ::
  ~/  %hall-poke-hall-command
  |=  cod/command
  ^-  (quip move _+>)
  =^  mos  +>.$
    %-  pre-bake
    ta-done:(ta-apply:ta src.bol cod)
  =^  mow  +>.$
    log-all-to-file
  [(welp mos mow) +>.$]
::
++  poke-hall-action
  ::    accept action
  ::
  ::  incoming hall action. process it.
  ::
  ~/  %hall-poke-hall-action
  |=  act/action
  ^-  (quip move _+>)
  ?.  (team:title our.bol src.bol)
    %-  pre-bake
    =<  ta-done
    %-  ta-note:ta
    "hall-action stranger {(scow %p src.bol)}"
  =^  mos  +>.$
    %-  pre-bake
    ta-done:(ta-action:ta act)
  =^  mow  +>.$
    log-all-to-file
  [(welp mos mow) +>.$]
::
::  #
::  #  %subscription-events
::  #
::
++  diff-hall-prize
  ::    accept prize
  ::
  ::  accept a query result.
  ::
  ~/  %hall-diff-hall-prize
  |=  {wir/wire piz/prize}
  ^-  (quip move _+>)
  =^  mos  +>.$
    %-  pre-bake
    =>  (ta-take:ta wir piz)
    (flop deltas)
    ::TODO  ideally this, but runtime error for %burden prize
    ::%-  pre-bake
    ::ta-done:(ta-take:ta wir piz)
  =^  mow  +>.$
    log-all-to-file
  [(welp mos mow) +>.$]
::
++  diff-hall-rumor
  ::    accept rumor
  ::
  ::  accept a query result change.
  ::
  ~/  %hall-diff-hall-rumor
  |=  {wir/wire rum/rumor}
  ^-  (quip move _+>)
  ::NOTE  to keep us from echoing changes back to their
  ::      sender, we want to know (in ++feel) if a delta
  ::      was caused by a rumor from a /report.
  ::      if gall worked as advertised, we'd use ost.bol
  ::      and wex.bol to find out, but wex is never set,
  ::      so we just keep track of the "current rumor
  ::      wire" instead.
  =.  rir  wir
  =^  mos  +>.$
    %-  pre-bake
    =>  (ta-hear:ta wir rum)
    (flop deltas)
    ::TODO  runtime error for %burden rumors.
    ::ta-done:(ta-hear:ta wir rum)
  =^  mow  +>.$
    log-all-to-file
  [(welp mos mow) +>.$]
::
++  peer
  ::    accept subscription
  ::
  ::  incoming subscription on {pax}.
  ::
  ~/  %hall-peer
  |=  pax/path
  ^-  (quip move _+>)
  ?:  ?=([%sole *] pax)  ~&(%hall-no-sole !!)
  =+  qer=(path-to-query pax)
  ?.  (leak src.bol qer)  ~&(%peer-invisible !!)
  =^  mos  +>.$
    %-  pre-bake
    ta-done:(ta-subscribe:ta src.bol qer)
  :_  +>.$
  =+  piz=(look qer)
  ?~  piz  ~&([%query-unavailable pax] mos)
  ?~  u.piz  ~&([%query-invalid pax] mos)
  :_  mos
  [ost.bol %diff %hall-prize u.u.piz]
::
++  pull
  ::  unsubscribes.
  ::
  ~/  %hall-pull
  |=  pax/path
  ^-  (quip move _+>)
  [~ +>]
::
++  pull-circle
  ::  someone ends a /circle subscription.
  ::
  ~/  %hall-pull-circle
  |=  pax/path
  ^-  (quip move _+>)
  %-  pre-bake
  =+  qer=(path-to-query %circle pax)
  ?>  ?=($circle -.qer)
  ?.  (~(has by stories) nom.qer)  ~
  [%story nom.qer %peer | src.bol qer]~
::
++  reap
  ::    subscription n/ack
  ::
  ::  update state to reflect subscription success
  ::
  ~/  %hall-reap
  |=  {wir/wire fal/(unit tang)}
  ^-  (quip move _+>)
  %-  pre-bake
  %+  welp
    ?.  ?=({$circle *} wir)  ~
    =+  wer=(etch wir)
    ?>  ?=($circle -.wer)
    =<  ta-done
    %.  [nom.wer src.wer]
    ?~  fal  ta-greet:ta
    ta-leave:ta
  ?~  fal  ~
  =<  ta-done
  =-  (ta-grieve:ta - u.fal)
  =+  (wire-to-target wir)
  %+  weld  "failed (re)subscribe to {(scow %p p)} on "
  %+  roll  q
  |=  {a/@ta b/tape}
  :(weld b "/" (trip a))
::
++  quit
  ::    dropped subscription
  ::
  ::  gall dropped out subscription. resubscribe.
  ::
  ~/  %hall-quit
  |=  wir/wire
  ^-  (quip move _+>)
  [[(wire-to-peer wir) ~] +>]
::
++  quit-circle
  ::    dropped circle sub
  ::
  ::  gall dropped our subscription. resubscribe.
  ::
  ~/  %hall-quit-circle
  |=  wir/wire
  ^-  (quip move _+>)
  %+  etch-circle  [%circle wir]
  |=  {nom/name src/source}
  %-  pre-bake
  ta-done:(ta-resub:ta nom src)
::
++  coup-repeat
  ::    message n/ack
  ::
  ::  ack from ++ta-transmit. mark the message as
  ::  received or rejected.
  ::
  ~/  %hall-coup-repeat
  |=  {wir/wire fal/(unit tang)}
  ^-  (quip move _+>)
  %+  etch-repeat  [%repeat wir]
  |=  {cir/circle ses/(list serial)}
  %-  pre-bake
  ta-done:(ta-repeat:ta cir ses fal)
::
::  #
::  #  %logging
::  #
::
++  poke-hall-save
  ::    save as log
  ::
  ::  stores the telegrams of story {nom} in a log file,
  ::  to be re-loaded by ++poke-hall-load.
  ::TODO  maybe update to also store sourced list.
  ::
  ~/  %hall-poke-hall-save
  |=  nom/name
  ^-  (quip move _+>)
  =/  paf/path
    /(scot %p our.bol)/home/(scot %da now.bol)/hall/[nom]/hall-telegrams
  =+  grams:(~(got by stories) nom)
  :_  +>.$
  :_  ~
  :*  ost.bol
      %info
      /jamfile
      (foal:space:userlib paf [%hall-telegrams !>(-)])
  ==
::
++  poke-load-legacy
  ::  loads legacy messages into the story {nom}.
  ::
  ~/  %hall-poke-hall-legacy
  |=  nom/name
  ^-  (quip move _+>)
  =/  jams/json
    .^  json
        %cx
        /(scot %p our.bol)/home/(scot %da now.bol)/hall/legacy-telegrams/json
    ==
  =+  grams=(from-json:hall-legacy jams)
  ~&  [%loaded (lent grams)]
  %-  pre-bake
  %+  turn  (flop grams)
  |=  t/telegram
  [%story nom %gram [our.bol nom] t]
::
++  poke-hall-load
  ::    load from log
  ::
  ::  loads the telegrams of story {nom} into our state,
  ::  as saved in ++poke-hall-save.
  ::
  ~/  %hall-poke-hall-load
  |=  nom/name
  ^-  (quip move _+>)
  =/  grams
    .^  (list telegram)
        %cx
        /(scot %p our.bol)/home/(scot %da now.bol)/hall/[nom]/hall-telegrams
    ==
  %-  pre-bake
  %+  turn  grams
  |=  t/telegram
  [%story nom %gram [our.bol nom] t]
::
++  poke-hall-log
  ::  starts logging story {nom}'s messages.
  ::
  ~/  %hall-poke-hall-log
  |=  nom/name
  ^-  (quip move _+>)
  :-  [(log-to-file nom) ~]
  %=  +>.$
      log
    %+  ~(put by log)  nom
    count:(~(got by stories) nom)
  ==
::
++  poke-hall-unlog
  ::  stops logging story {nom}'s messages.
  ::
  ~/  %hall-poke-hall-unlog
  |=  nom/name
  ^-  (quip move _+>)
  :-  ~
  +>.$(log (~(del by log) nom))
::
++  log-all-to-file
  ::    update stories logs
  ::
  ::  for every story we're logging, (over)writes all
  ::  their grams to log files if new ones have arrived.
  ::
  ^-  (quip move _.)
  :_  %_  .
          log
        %-  ~(urn by log)
        |=  {nom/name len/@ud}
        count:(~(got by stories) nom)
      ==
  %+  murn  ~(tap by log)
  |=  {nom/name len/@ud}
  ^-  (unit move)
  ?:  (gte len count:(~(got by stories) nom))
    ~
  `(log-to-file nom)
::
++  log-to-file
  ::  logs all grams of story {nom} to a file.
  ::
  ~/  %hall-log-all-to-file
  |=  nom/name
  ^-  move
  =+  ^-  paf/path
      =+  day=(year %*(. (yore now.bol) +.t +:*tarp))
      %+  en-beam:format  [our.bol %home da+now.bol]
      /hall-telegrams/(scot %da day)/[nom]/hall
  =+  grams:(~(got by stories) nom)
  :*  ost.bol
      %info
      /jamfile
      (foal:space:userlib paf [%hall-telegrams !>(-)])
  ==
::
::TODO  for debug purposes. remove eventually.
::  users beware, here be dragons.
++  poke-noun
  ~/  %hall-poke-noun
  |=  a/*
  ^-  (quip move _+>)
  ?:  ?=([%kill ship] a)
    :_  +>
    %-  ~(rep by sup.bol)
    |=  [[b=bone c=(pair ship path)] out=(list move)]
    ?:  =(+.a p.c)
      [[b %quit ~] out]
    out
  ?>  ?=(@t a)
  ?:  =(a 'check')
    ~&  'verifying message reference integrity...'
    =-  ~&(- [~ +>.$])
    %-  ~(urn by stories)
    |=  {n/name s/story}
    =+  %-  ~(rep by known.s)
      |=  {{u/serial a/@ud} k/@ud m/@ud}
      :-  ?:((gth a k) a k)
      ?:  =(u uid:(snag a grams.s))  m
      ~?  (lth m 3)
        :*  [%fake a u]
            [%prev uid:(snag (dec a) grams.s)]
            [%real uid:(snag a grams.s)]
            [%next uid:(snag +(a) grams.s)]
        ==
      +(m)
    :^  count=count.s
        lent=(lent grams.s)
      known=k
    mismatch=m
  ?:  =(a 'check subs')
    ~&  'here are all incoming non-circle subs'
    ~&  ^-  (list (pair ship path))
        %+  murn  ~(tap by sup.bol)
        |=  {b/bone s/ship p/path}
        ^-  (unit (pair ship path))
        ?:  ?=({$circle *} p)  ~
        `[s p]
    [~ +>]
  ?:  =(a 'check all subs')
    ~&  'here are all incoming subs'
    ~&  ^-  (list (pair ship path))
        %+  turn  ~(tap by sup.bol)
        |=  {b/bone s/ship p/path}
        ^-  (pair ship path)
        [s p]
    [~ +>]
  ?:  =(a 'rebuild')
    ~&  'rebuilding message references...'
    =-  [~ +>.$(stories -)]
    %-  ~(urn by stories)
    |=  {nom/name soy/story}
    =+  %+  roll  grams.soy
      |=  {t/telegram c/@ud k/(map serial @ud) s/(map circle (list @ud))}
      :+  +(c)  (~(put by k) uid.t c)
      =/  src/circle
        ?:  (~(has by aud.t) [our.bol nom])  [our.bol nom]
        ?~  aud.t  ~&(%strange-aud [our.bol %inbox])
        n.aud.t
      %+  ~(put by s)  src
      [c (fall (~(get by s) src) ~)]
    soy(count c, known k, sourced s)
  ?:  =(a 'refederate')
    ~&  'refederating. may take a while...'
    :_  +>
    =+  bov=(above:hall-lib [our now our]:bol)
    ?:  =(bov our.bol)  ~
    :~  [ost.bol %pull /burden [bov dap.bol] ~]
        (wire-to-peer /burden)
    ==
  ?:  =(a 'incoming')
    ~&  'incoming subscriptions (ignoring circle subs):'
    ~&  %+  skip  ~(tap by sup.bol)
        |=  {bone (pair ship path)}
        &(?=({$circle *} q) !?=({$circle $inbox *} q))
    [~ +>]
  ?:  =(a 'sources')
    ~&  'sources per story:'
    ~&  %-  ~(urn by stories)
        |=  {n/name s/story}
        [n src.shape.s]
    [~ +>]
  ?:  =(`0 (find "re-listen " (trip a)))
    ~&  're-listening'
    :_  +>
    :_  ~
    (wire-to-peer /report/(crip (slag 10 (trip a))))
  ::
  ::
  ?:  =(a 'kill-all-subs')
    :_  +>
    %-  ~(rep by sup.bol)
    |=  [[b=bone (pair ship path)] out=(list move)]
    [[b %quit ~] out]
  ::
  ?:  =(a 'kill-our-subs')
    :_  +>
    %-  ~(rep by sup.bol)
    |=  [[b=bone c=(pair ship path)] out=(list move)]
    ?:  =(our.bol p.c)
      [[b %quit ~] out]
    out
  ::
  ?:  =(a 'print')
    ~&  shape:(~(got by stories) %inbox)
    [~ +>]
  ::
  ?:  =(a 'print all')
    ~&  %+  turn  ~(tap by stories)
        |=  [n=name s=story]
        [n shape:s]
    [~ +>]
  ::
  [~ +>]
--
