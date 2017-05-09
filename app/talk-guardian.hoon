::                                                      ::  ::
::::  /hoon/talk/app                                    ::  ::
  ::                                                    ::  ::
::
::TODO  master changes
::TODO  char57 comments as line comments when regarding code.
::TODO  avoid lark where possible
::TODO  document what user-facing messages actually mean!
::TODO  ::> to :> etc.
::
::TODO  we can't do away with the default mailbox because we need it for things
::      like invite notifications etc. can we do better than request that apps
::      don't use it frivolously?
::
::TODO  ".. the importance of the CQRS pattern (command-query separation) behind
::      Urbit's separation of %poke and %peer. Pokes (messages) are one-way
::      commands, not queries. Peers (subscriptions) have no effect on the
::      server state."
::      but we *do* change state on-subscribe! is that a problem?
::
::TODO  crash on pokes/peers we do not expect
::TODO  keep both a folks and nicks maps. (actual profiles and local nicknames.)
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
      $:  stories/(map knot story)                      ::<  convercirions
          readers/(map bone (set knot))                 ::<  our message readers
          outbox/(pair @ud (map @ud thought))           ::<  urbit outbox
          log/(map knot @ud)                            ::<  logged to clay
          folks/(map ship human)                        ::<  human identities
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
          followers/(map bone river)                    ::<  subscribers
      ==                                                ::
    ++  river  (pair point point)                       ::<  stream definition
    ++  point                                           ::>  stream endpoint
      $%  {$ud p/@ud}                                   ::<  by number
          {$da p/@da}                                   ::<  by date
      ==                                                ::
    ++  move  (pair bone card)                          ::<  all actions
    ++  lime                                            ::>  diff fruit
      $%  {$talk-report report}                         ::
          {$talk-lowdown lowdown}                       ::
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
  ::>  for every transaction/event (poke, peer, etc.)
  ::>  talk-guardian receives, the ++ta transaction core
  ::>  is called.
  ::>  in processing transactions, ++ta may modify app
  ::>  state, or create moves. these moves get produced
  ::>  upon finalizing the core's work with ++ta-done.
  ::>  when making changes to stories, the ++so core is
  ::>  used.
  ::
  |_  ::>  moves: moves created by core operations.
      ::
      moves/(list move)
  ::
  ++  ta-done                                           ::<  resolve core
    ::>  produces the moves stored in ++ta's moves.
    ::>  they are produced in reverse order because
    ::>  ++ta-emil and ++ta-emit add them to the head of
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
  ++  ta-emil                                           ::<  emit move list
    ::>  adds multiple moves to the head of {moves}.
    ::>  flops to stay consistent with ++ta-emit.
    ::
    |=  mol/(list move)
    %_(+> moves (welp (flop mol) moves))
  ::
  ++  ta-emit                                           ::<  emit a move
    ::>  adds a move to the head of {moves}.
    ::
    |=  mov/move
    %_(+> moves [mov moves])
  ::
  ++  ta-inform                                         ::<  broadcast lowdown
    ::>  sends a talk-lowdown diff to all readers.
    ::
    |=  low/lowdown
    %-  ta-emil
    %-  ~(rep by readers)
    |=  {{b/bone *} l/(list move)}
    [[b %diff %talk-lowdown low] l]
  ::
  ++  ta-react                                          ::<  send reaction
    ::>  sends a talk-reaction diff to a reader.
    ::
    |=  {red/bone rac/reaction}
    %-  ta-emit
    [red %diff %talk-reaction rac]
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
  ++  ta-human                                          ::<  look up person
    ::>  get {her} identity. if we need to, create one.
    ::
    |=  her/ship
    ^-  (quid human +>)
    =^  who  folks
      =+  who=(~(get by folks) her)
      ?^  who  [u.who folks]
      =+  who=`human`[~ `(scot %p her)]                 ::  XX do right
      [who (~(put by folks) her who)]
    [who +>.$]
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
        $human   (action-human +.act)
        $glyph   (action-glyph +.act)
      ==
    ::
    ++  affect                                          ::<  affect story
      ::>  produces a gill that takes a gate.
      ::>  if the story {nom} exists, calls the gate with
      ::>  a story core and the story itself.
      ::>  if it doesn't, reacts accordingly.
      ::
      |=  nom/knot
      |=  fec/$-({_so story} _ta)
      ^+  ta
      =+  pur=(~(get by stories) nom)
      ?^  pur
        (fec ~(. so nom ~ u.pur) u.pur)
      %+  ta-react  red
      [%fail (crip "no story {(trip nom)}") `act]
    ::
    ::>  ||  %circle-configuration
    ::+|
    ++  action-create                                   ::<  create story
      ::>  creates a story with the specified parameters.
      ::
      |=  {nom/knot des/cord typ/security}
      ^+  ..ta-action
      ?.  (~(has in stories) nom)
        %+  ta-config  nom
        :*  [[%& our.bol nom] ~ ~]
            des
            [| |]
            [typ ~]
            [[our.bol ~ ~] [our.bol ~ ~]]
        ==
      (ta-react red %fail (crip "{(trip nom)}: already exists") `act)
    ::
    ++  action-delete                                   ::<  delete + announce
      ::>  delete story {nom}, optionally announcing the
      ::>  event with message {mes}.
      ::
      |=  {nom/knot mes/(unit cord)}
      ^+  ..ta-action
      %-  (affect nom)  |=  *
      =.  ..ta-action  ::TODO  =?
        ?~  mes  ..ta-action
        %+  action-phrase
          [[%& our.bol nom] ~ ~]
        [%lin | u.mes]~
      (ta-unconfig nom)
    ::
    ++  action-depict                                   ::<  change description
      ::>  change description of story {nom} to {des}.
      ::
      |=  {nom/knot des/cord}
      ^+  ..ta-action
      %-  (affect nom)  |=  {sor/_so soy/story}
      =.  cap.shape.soy  des
      (ta-config nom shape.soy)
    ::
    ++  action-filter                                   ::<  change message rules
      ::>  replaces the story's current filter with the
      ::>  specified one.
      ::
      |=  {nom/knot fit/filter}
      ^+  ..ta-action
      %-  (affect nom)  |=  {sor/_so soy/story}
      =.  fit.shape.soy  fit
      (ta-config nom shape.soy)
    ::
    ++  action-permit                                   ::<  invite/banish
      ::>  invite to/banish from story {nom} all {sis}.
      ::
      |=  {nom/knot inv/? sis/(set ship)}
      ^+  ..ta-action
      %-  (affect nom)  |=  {sor/_so *}  =<  so-done
      (so-permit:sor inv sis)
    ::
    ++  action-source                                   ::<  un/sub p to/from r
      ::>  add/remove {pas} as sources for story {nom}.
      ::
      |=  {nom/knot sub/? pas/(set partner)}
      ^+  ..ta-action
      %-  (affect nom)  |=  {sor/_so soy/story}
      =.  src.shape.soy
        %.  pas
        ?:  sub
          ~(uni in src.shape.soy)
        ~(dif in src.shape.soy)
      (ta-config nom shape.soy)
    ::
    ++  action-enlist                                   ::<  dis/allow federation
      ::>  adds {sis} to story {nom}'s list of allowed
      ::>  federators.
      ::
      |=  {nom/knot fed/? sis/(set ship)}
      ^+  ..ta-action
      %-  (affect nom)  |=  {sor/_so soy/story}
      ::TODO  maybe so-enlist, with message note and everything
      =.  may.fed.shape.soy
        %.  sis
        ?:  fed
          ~(uni in may.fed.shape.soy)
        ~(dif in may.fed.shape.soy)
      (ta-config nom shape.soy)
    ::
    ++  action-burden                                   ::<  help federate
      ::>  starts federating the specified circle. create
      ::>  it locally if it doesn't yet exist.
      ::
      |=  {hos/ship nom/knot}
      ^+  ..ta-action
      ::  update federation config.
      =+  soy=(fall (~(get by stories) nom) *story)
      =.  may.fed.shape.soy
        (~(put in may.fed.shape.soy) hos)
      =.  fes.fed.shape.soy
        (~(put in fes.fed.shape.soy) hos)
      =.  src.shape.soy
        (~(put in src.shape.soy) [%& hos nom])
      =.  ..ta-action
        (ta-config nom shape.soy)
      ::  send %burden command with story's current state.
      %-  ta-emit
      :*  ost.bol
          %poke
          /burden
          [hos %talk-guardian]
          :*  %talk-command
              %burden
              nom
              [shape.soy mirrors.soy]
              [locals.soy remotes.soy]
              grams.soy
          ==
      ==
    ::
    ::>  ||  %messaging
    ::+|
    ++  action-convey                                   ::<  post exact
      ::>  sends the messages provided in the action.
      ::
      |=  tos/(list thought)
      ^+  ..ta-action
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
      ::
      |=  {nos/(set knot) sat/status}
      ^+  ..ta-action
      %-  ~(rep in nos)
      |=  {k/knot _ta}
      %-  (affect k)  |=  {sor/_so *}
      so-done:(so-notify:sor our.bol sat)
    ::
    ::>  ||  %changing-shared-ui
    ::+|
    ++  action-human                                    ::<  new identity
      ::>  assigns a new local identity ("nickname") to the
      ::>  target ship.
      ::
      |=  {sip/ship nic/human}
      ^+  ..ta-action
      ?.  =((~(get by folks) sip) `nic)  ..ta-action    ::<  no change
      =.  folks
        ?~  han.nic  (~(del by folks) sip)
        (~(put by folks) sip nic)
      %+  ta-inform  %names
      ::TODO  think long and hard, do we need unit for delition or is a human
      ::      with [~ ~] good enough? if the latter, agent's $names will change.
      (strap sip ?~(han.nic ~ `nic))
    ::
    ++  action-glyph                                    ::<  bind a glyph
      ::>  un/bind glyph {lif} to partners {pas}.
      ::
      |=  {lif/char pas/(set partner) bin/?}
      =.  ..ta-action
        ?:  bin
          %=  ..ta-action                               ::<  bind
            nik  (~(put by nik) pas lif)
            nak  (~(put ju nak) lif pas)
          ==
        =/  ole/(set (set partner))                     ::<  unbind
          ?.  =(pas ~)  [pas ~ ~]
          (~(get ju nak) lif)
        |-  ^+  ..ta-action
        ?~  ole  ..ta-action
        =.  ..ta-action  $(ole l.ole)
        =.  ..ta-action  $(ole r.ole)
        %=  ..ta-action
          nik  (~(del by nik) pas)
          nak  (~(del ju nak) lif pas)
        ==
      (ta-inform %glyph nak)
    --
  ::
  ++  ta-diff-report                                    ::<  subscription update
    ::>  process a talk report from {cir} into story {nom}.
    ::
    |=  {nom/knot cir/circle ret/report}
    %-  (ta-know nom)  |=  sor/_so  =<  so-done
    (so-diff-report:sor cir ret)
  ::
  ::>  ||
  ::>  ||  %subscription-events
  ::>  ||
  ::>    arms that react to subscription events.
  ::+|
  ::
  ++  ta-subscribe                                      ::<  listen to
    ::>  start {her} susbcription on {pax}.
    ::>  for readers, we forward to ++ta-welcome.
    ::>  for foreign brokers, we check if their desired
    ::>  story exists and if they have permission to
    ::>  subscribe to it, before sending them all data.
    ::
    |=  {her/ship pax/path}
    ^+  +>
    ::  reader subscription.
    ?:  ?=({$reader *} pax)
      ?.  (team our.bol her)
        %-  ta-note
        (crip "foreign reader {(scow %p her)}")
      (ta-welcome ost.bol t.pax)
    ::  weird subscription path.
    ?.  ?=({@ *} pax)
      (ta-evil %bad-path)
    =+  pur=(~(get by stories) i.pax)
    ?~  pur
      ::TODO  send this to the subscriber! make them unsub!
      %-  ta-note
      (crip "bad subscribe story '{(trip i.pax)}'")
    =+  soy=~(. so i.pax `(list action)`~ u.pur)        ::  nest-fail if no cast
    ::  she needs read permissions to subscribe.
    ?.  (so-visible:soy her)
      (ta-evil %no-story)
      ::TODO?  or (so-sauce ost.bol [%quit ~]~) ?
    =^  who  +>.$  (ta-human her)
    ::  send current data to bring her up to date.
    =.  soy  (so-report-lobby:soy ost.bol ~ ~)
    =.  soy  (so-report-crowd:soy ost.bol ~ ~)
    =.  soy  (so-start:soy her t.pax)                   ::<  also adds story sub
    =.  soy  (so-notify:soy her %hear who)              ::<  add her status
    so-done:soy                                         ::<  apply story changes
  ::
  ++  ta-welcome                                        ::<  reader subscription
    ::>  brings new reader up to date. susbcribes it to the specified story,
    ::TODO  or shared ui state if no story was specified.
    ::TODO  maybe also send a list of knots again? (basically a %house lowdown)
    ::
    |=  {new/bone pax/path}
    =/  sor/knot
      ?:  ?=({@tas *} pax)  i.pax
      (main our.bol)                                    ::  default to mailbox
    ::>  new reader? send shared ui state.
    =.  +>.$  ::TODO  =?
      ?:  (~(has by readers) new)  +>.$
      %-  ta-emil
      :~  ::>  bound glyphs
          [new %diff %talk-lowdown %glyph nak]
          ::>  nicknames
          [new %diff %talk-lowdown %names (~(run by folks) some)]
      ==
    ::>  send story state.
    =.  +>.$  ::TODO  =?
      ?.  (~(has by stories) sor)  +>.$
      =+  soy=(~(got by stories) sor)
      %-  ta-emil
      :~  ::>  configurations
          :*  new  %diff  %talk-lowdown  %confs
            `shape.soy  (~(run by mirrors.soy) some)
          ==
          ::>  presences
          [new %diff %talk-lowdown %precs locals.soy remotes.soy]
          ::>  messages
          [new %diff %talk-lowdown %grams 0 grams.soy]
      ==
    ::>  add this subscription to {readers}.
    %=  +>.$
        readers
      %+  ~(put by readers)  new
      (~(put in (fall (~(get by readers) new) ~)) sor)
    ==
  ::
  ++  ta-retry                                          ::<  subscription resend
    ::>  re-subscribes {cir} to story {nom}.
    ::
    |=  {nom/knot cir/circle}
    %-  (ta-know nom)  |=  sor/_so  =<  so-done
    (so-acquire:sor [%& cir]~)
  ::
  ++  ta-quit                                           ::<  subscription failed
    ::>  removes {cir} from story {nom}'s followers.
    ::
    |=  {nom/knot cir/circle}
    %-  (ta-know nom)  |=  sor/_so  =<  so-done
    (so-quit:sor %& cir)
  ::
  ++  ta-cancel                                         ::<  unsubscribe
    ::>  drops {src}'s subscription. deduce the right way
    ::>  to do this from the subscription path {pax}.
    ::
    |=  {src/ship pax/path}
    ^+  +>
    ::  remove a reader subscription.
    ?:  ?=({$reader *} pax)
    =/  nom/knot
      ?:  ?=({@tas *} t.pax)  i.pax
      (main our.bol)  ::TODO  change when ++ta-welcome changes.
    =/  nes/(set knot)
      %.  nom
      ~(del in (~(got by readers) ost.bol))
    +>.$(readers (~(put by readers) ost.bol nes))
    ::  weird subscription path.
    ?.  ?=({@ @ *} pax)
      %-  ta-note
      (crip "ta-cancel weird path {~(ram re >pax<)}")
    ::  remove a regular subscription, set ship status to %gone.
    %-  (ta-know i.pax)  |=  sor/_so  =<  so-done
    (so-notify:so-cancel:sor src %gone *human)
  ::
  ::>  ||  %fora
  ::>    fora things.
  ::TODO  move outside of guardian...
  ::+|
  ::
  ++  ta-base-hart
    ::x  produces our ship's host desk's web address as a hart.
    ::
    .^(hart %e /(scot %p our.bol)/host/(scot %da now.bol))
  ::
  ++  ta-fora-post
    ::x  sends a fora post. if we don't have a channel for posts yet, create one
    ::
    |=  {pax/path sup/spur hed/@t txt/@t}
    ::x  tell %hood to submit a fora post.
    =.  ..ta-emit
      %+  ta-emit  ost.bol
      :*  %poke
          /fora-post
          [our.bol %hood]
          [%write-fora-post sup src.bol hed txt]
      ==
    =+  man=%posts
    ::x  if we have a %posts story, go ahead and consume.
    ?:  (~(has by stories) man)
      (ta-consume-fora-post man pax hed txt)
    ::x  if we have no %posts story, first create it, then consume.
    =;  new  (ta-consume-fora-post:new man pax hed txt)
    =.  ..ta-action
      %+  ta-action  ost.bol
      [%create man 'towards a community' %brown]
    ::x  send informative message to our mailbox.
    %^  ta-consume  &  our.bol
    :^    (shaf %init eny.bol)                          ::<  serial
        (my [[%& our.bol (main our.bol)] *envelope %pending] ~)  ::<  audience
    ::>  statement
      now.bol
    [~ %app %tree 'receiving forum posts, ;join %posts for details']
  ::
  ++  ta-consume-fora-post
    ::x  add a message for a fora post to the man story.
    ::
    |=  {man/knot pax/path hed/@t txt/@t}  ^+  +>
    =.  pax  (welp pax /posts/(crip "{<now.bol>}~"))
    %^  ta-consume  |
      src.bol
    :*  (shaf %comt eny.bol)
        (my [[%& our.bol man] *envelope %pending] ~)
        now.bol
        (sy /fora-post eyre+pax ~)
      :-  %mor  :~
        [%fat text+(lore txt) [%url [ta-base-hart `pax ~] ~]]
        [%app %tree (crip "forum post: '{(trip hed)}'")]
      ==
    ==
  ::
  ++  ta-comment
    ::x  sends a comment. if we don't have a channel for them yet, creates one.
    ::
    |=  {pax/path sup/spur txt/@t}
    =.  ..ta-emit
      %+  ta-emit  ost.bol
      :*  %poke
          /comment
          [our.bol %hood]
          [%write-comment sup src.bol txt]
      ==
    =+  man=%comments
    ?:  (~(has by stories) man)
      (ta-consume-comment man pax sup txt)
    =;  new  (ta-consume-comment:new man pax sup txt)
    =.  ..ta-action
      %+  ta-action  ost.bol
      [%create man 'letters to the editor' %brown]
    %^  ta-consume  &  our.bol
    :^    (shaf %init eny.bol)
        (my [[%& our.bol (main our.bol)] *envelope %pending] ~)
      now.bol
    [~ %app %tree 'receiving comments, ;join %comments for details']
  ::
  ++  ta-consume-comment
    ::x  adds a message for a comment to the man story.
    ::
    |=  {man/knot pax/path sup/spur txt/@t}  ^+  +>
    =+  nam=?~(sup "" (trip i.sup))                     :: file name
    =+  fra=(crip (time-to-id now.bol))                 :: url fragment
    %^  ta-consume  |
      src.bol
    :*  (shaf %comt eny.bol)
        (my [[%& our.bol man] *envelope %pending] ~)
        now.bol
        (sy /comment eyre+pax ~)
      :-  %mor  :~
        [%fat text+(lore txt) [%url [ta-base-hart `pax ~] `fra]]
        [%app %tree (crip "comment on /{nam}")]
      ==
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
    =.  +>
        %+  ta-emit  ost.bol
        :*  %poke
            /repeat/(scot %ud p.outbox)/(scot %p hos.cir)/[nom.cir]
            [hos.cir %talk-guardian]
            [%talk-command [%review tot ~]]
        ==
    +>(p.outbox +(p.outbox), q.outbox (~(put by q.outbox) p.outbox tot))
  ::
  ++  ta-coup-repeat                                    ::<  remove from outbox
    ::>  assemble partner and call ++ta-repeat.
    ::
    |=  {{num/@ud src/ship nom/knot} fal/(unit tang)}
    (ta-repeat num [%& src nom] fal)
  ::
  ++  ta-repeat                                         ::<  remove from outbox
    ::>  take message out of outbox, mark it as received
    ::>  or rejected, based on the existence of error
    ::>  message {fal}.
    ::
    |=  {num/@ud pan/partner fal/(unit tang)}
    =+  oot=(~(get by q.outbox) num)
    ?~  oot  ~|([%ta-repeat-none num] !!)
    =.  q.outbox  (~(del by q.outbox) num)
    =.  aud.u.oot
      =+  olg=(~(got by aud.u.oot) pan)
      %+  ~(put by aud.u.oot)  pan
      :-  -.olg
      ?~  fal  %received
      ~>  %slog.[0 u.fal]
      %rejected
    (ta-think | our.bol u.oot ~)
  ::
  ::>  ||
  ::>  ||  %stories
  ::>  ||
  ::>    arms for modifying stories.
  ::+|
  ::
  ++  ta-config                                         ::<  configure story
    ::>  configures story {nom}, creating it if needed.
    ::>  if it's a whitelist config, and we're creating
    ::>  the story, makes sure we're in it.
    ::
    |=  {nom/knot con/config}
    ^+  +>
    ::>  neu:  is new story
    ::>  pur:  the story
    ::>  wyt:  will be white
    =+  :+  neu=!(~(has by stories) nom)
          pur=(fall (~(get by stories) nom) *story)
        wyt=?=(?($white $green) sec.con.con)
    =.  ses.con.con  ::TODO  =?
      ?:  &(neu wyt)  [our.bol ~ ~]
      ses.con.con
    so-done:(~(so-reform so nom ~ pur) con)
  ::
  ++  ta-unconfig                                       ::<  delete story
    ::>  calls the story core to delete story {nom}.
    ::
    |=  nom/knot
    ^+  +>
    %-  (ta-know nom)  |=  sor/_so  =<  so-done
    so-reform-gone:so
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
      =.  +>  +>(stories (~(put by stories) nom +<+>))
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
    ++  so-sauce                                        ::<  send backward
      ::>  cards to moves, prepend to {moves} reversed.
      ::
      |=  {ost/bone cub/(list card)}
      %_    +>.$
          moves
        (welp (flop (turn cub |=(a/card [ost a]))) moves)
      ==
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
    ++  so-inform                                       ::<  send lowdown
      ::>  sends lowdown to all interested readers.
      ::
      |=  low/lowdown
      =.  moves
        %+  weld
          ^-  (list move)
          %+  murn  (~(tap by readers))
          |=  {b/bone s/(set knot)}
          ^-  (unit move)
          ?.  (~(has in s) nom)  ~
          `[b %diff %talk-lowdown low]
        moves
      +>.$
    ::
    ++  so-report                                       ::<  send update
      ::>  sends report to all bones.
      ::
      |=  {bos/(set bone) ret/report}
      ^+  +>
      ?~  bos  +>
      =.  +>  $(bos l.bos)
      =.  +>  $(bos r.bos)
      (so-sauce n.bos [%diff %talk-report ret]~)
    ::
    ++  so-report-crowd                                 ::<  presence update
      ::>  send local and remote presences in a report.
      ::
      |=  bos/(set bone)
      (so-report bos %crowd locals so-remotes)
    ::
    ++  so-report-lobby                                 ::<  config update
      ::>  send local and remote configs in a report.
      ::
      |=  bos/(set bone)
      (so-report bos %lobby shape mirrors)
    ::
    ::>  ||
    ::>  ||  %data
    ::>  ||
    ::>    utility functions for data retrieval.
    ::+|
    ::
    ++  so-followers                                    ::<  follower bones
      ::>  turns the keys of {followers} into a set.
      ::
      ^-  (set bone)
      %-  ~(gas in *(set bone))
      %+  turn  (~(tap by followers))
      |=  {b/bone *}  b
    ::
    ++  so-unearth                                      ::<  ships' bones
      ::>  find the bones in {followers} that belong to
      ::>  a ship in {sis}.
      ::
      |=  sis/(set ship)
      ^-  (set bone)
      %-  ~(rep in sup.bol)
      |=  {{b/bone s/ship p/path} c/(set bone)}
      ?.  ?&  (~(has in sis) s)
              (~(has by followers) b)
              ?=({@tas *} p)
              =(i.p nom)
          ==
        c
      (~(put in c) b)
    ::
    ++  so-remotes                                      ::<  remote presences
      ::>  produces {remotes}, with all our local
      ::>  presences replaced by the versions from their
      ::>  stories.
      ::
      %-  ~(urn by remotes)                             ::  XX performance
      |=  {pan/partner gop/group}
      ^-  group
      ?.  &(?=($& -.pan) =(our.bol hos.p.pan))  gop
      =+  soy=(~(get by stories) nom.p.pan)
      ?~  soy  gop
      locals.u.soy
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
        (so-reform nec)
      ::  assimilate presence and remotes.
      =.  locals   (~(uni by loc.pes) locals)
      =.  remotes  (~(uni by rem.pes) remotes)
      =.  mirrors  (~(uni by rem.cof) mirrors)
      ::  remove redundant remotes.
      =.  remotes
        %-  ~(gas by *_remotes)
        %+  murn  (~(tap by remotes))
        |=  {p/partner g/group}
        ^-  (unit {partner group})
        ?:  ?&  ?=($& -.p)
                =(nom.p.p nom)
                (~(has in fes.fed.shape) hos.p.p)
            ==
          ~
        `[p g]
      ::  remove redundant mirrors.
      =.  mirrors
        %-  ~(gas by *_mirrors)
        %+  murn  (~(tap by mirrors))
        |=  {c/circle f/config}
        ^-  (unit {circle config})
        ?:  ?&  =(nom.c nom)
                (~(has in fes.fed.shape) hos.c)
            ==
          ~
        `[c f]
      ::  send updates to followers.
      =.  +>.$  (so-report-crowd so-followers)
      =.  +>.$  (so-report-lobby so-followers)
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
      =+  nes=shape
      =.  fes.fed.nes
        (~(dif in fes.fed.nes) who)
      =.  src.nes
        %-  ~(dif in src.nes)
        ^-  (set partner)
        %-  ~(run in who)
        |=(s/ship [%& s nom])
      ?:  =(nes shape)  +>.$
      ::TODO  propogate %relief to other federators.
      (so-reform nes)
    ::
    ++  so-diff-report                                  ::<  process update
      ::>  process a talk report from {cir}.
      ::>  if we didn't expect it, ignore.
      ::
      |=  {cir/circle ret/report}
      ^+  +>
      ?:  =(cir [our.bol nom])  +>
      ?.  (~(has in src.shape) [%& cir])
        %-  so-note
        %-  crip  ;:  weld
          "so-diff unexpected "
          (scow %p hos.cir)  "/"  (trip nom.cir)
          " %"  (scow %tas -.ret)
          " in "  (scow %tas nom)
        ==
      ?-  -.ret
        $lobby  (so-lobby cir +.ret)
        $crowd  (so-remind [%& cir] +.ret)
        $grams  (so-lesson gaz.ret)
      ==
    ::
    ++  so-lobby                                        ::<  update config
      ::>  add circle's config to our remote config map.
      ::
      ::TODO  when do we care about rem?
      |=  {cir/circle con/config rem/(map circle config)}
      ^+  +>
      ::  if they're a federator, set new shape.
      =.  +>  ::TODO  =?
        ?.  ?&  =(nom nom.cir)
                (~(has in fes.fed.shape) hos.cir)
            ==
          +>
        ::  to prevent removal of federators during initial setup, only allow
        ::  federator deletion through explicit action.
        =.  fes.fed.con  (~(uni in fes.fed.con) fes.fed.shape)
        =.  may.fed.con  (~(uni in may.fed.con) may.fed.shape)
        =.  src.con  ::TODO  is this needed?
          %-  ~(uni in src.con)
          ^-  (set partner)
          %-  ~(run in fes.fed.con)
          |=  s/ship
          ^-  partner
          [%& s nom]
        (so-reform con)
      =+  old=mirrors
      =.  mirrors  (~(put by mirrors) cir con)
      ?:  =(mirrors old)  +>.$
      =.  +>.$  (so-inform %confs `shape (strap cir `con))
      (so-report-lobby so-followers)
    ::
    ++  so-remind                                       ::<  remote presence
      ::>  adds {tay}'s {loc} to our remote presence map,
      ::>  after merging with {rem}.
      ::>  if this changes anything, send a report.
      ::
      |=  {pan/partner loc/group rem/(map partner group)}
      ::  if they're a federator, set new locals.
      =/  buk  (~(uni by remotes) rem)
      =.  buk  (~(put by buk) pan loc)
      ::  seperate federation presences and true remotes.
      =/  nep/crowd
        %+  roll  (~(tap by buk))
        |=  {{p/partner g/group} c/crowd}
        ?:  (so-right p)
          [(~(uni by loc.c) g) rem.c]
        [loc.c (~(put by rem.c) p g)]
      ::  ensure we have the latest.
      =.  loc.nep
        ?.  (so-right pan)  (~(uni by locals) loc.nep)
        (~(uni by (~(uni by locals) loc.nep)) loc)
      ::  if nothing changed, we're done.
      ?:  ?&  =(loc.nep locals)
              =(rem.nep remotes)
          ==
        +>.$
      ::  reader update.
      =.  +>.$  ::TODO  =?
        %^  so-inform  %precs
        (~(dif in loc.nep) locals)  ::  locals
        ::  per-partner diff for remotes.
        %-  ~(urn by rem.nep)
        |=  {p/partner a/group}
        =+  o=(~(get by remotes) p)
        ?~(o a (~(dif in a) u.o))
      ::  subscriber update.
      (so-report-crowd(locals loc.nep, remotes rem.nep) so-followers)
    ::
    ::>  ||
    ::>  ||  %changes
    ::>  ||
    ::>    arms that make miscelaneous changes to this story.
    ::+|
    ::
    ++  so-reform                                       ::<  reconfigure
      ::>  changes the config of this story and notify
      ::>  our followers.
      ::>  subscribes to new sources, unsubs from removed
      ::>  ones.
      ::
      |=  cof/config
      ?:  =(cof shape)  +>
      =.  +>.$  (so-inform %confs `cof ~)
      =/  dif/(pair (list partner) (list partner))
          =+  old=`(list partner)`(~(tap in src.shape) ~)
          =+  new=`(list partner)`(~(tap in src.cof) ~)
          :-  (skip new |=(a/partner (~(has in src.shape) a)))
          (skip old |=(a/partner (~(has in src.cof) a)))
      =.  +>.$  (so-acquire p.dif)
      =.  +>.$  (so-abjure q.dif)
      =.  shape  cof
      (so-report-lobby so-followers)
    ::
    ++  so-reform-gone                                  ::<  delete story
      ::>  deletes this story. removes it from {stories}
      ::>  and unsubscribes from all src.
      ::
      =.  stories  (~(del by stories) nom)
      =.  .  (so-inform %confs ~ ~)
      =.  .  (so-report-lobby so-followers)
      (so-abjure (~(tap in src.shape)))
    ::
    ++  so-notify                                       ::<  local presence
      ::>  add {her} status to this story's presence map.
      ::>  if this changes it, send a report.
      ::
      |=  {her/ship sat/status}
      ^+  +>
      =/  nol  (~(put by locals) her sat)
      ?:  =(nol locals)  +>.$
      =.  +>.$  (so-inform %precs (strap her sat) ~)
      (so-report-crowd(locals nol) so-followers)
    ::
    ::>  ||
    ::>  ||  %subscriptions
    ::>  ||
    ::>    arms for starting and ending subscriptions
    ::+|
    ::
    ++  so-acquire                                      ::<  subscribe us
      ::>  subscribes this story to each partner.
      ::
      |=  pas/(list partner)
      %+  so-sauce  0  ::  subscription is caused by this app
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
            /friend/show/[nom]/(scot %p hos.p.pan)/[nom.p.pan]
            [hos.p.pan %talk-guardian]
            /[nom.p.pan]/[ini]
        ==
      ==
    ::
    ++  so-abjure                                       ::<  unsubscribe us
      ::>  unsubscribes this story from each partner.
      ::
      |=  pas/(list partner)
      %+  so-sauce  0  ::  subscription is caused by this app
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
    ++  so-quit                                         ::<  subscription failed
      ::>  delete {pan} from our subscriptions, then send
      ::>  an updated capal report.
      ::
      |=  pan/partner
      ^+  +>
      ?.  (~(has in src.shape) pan)  +>
      =.  src.shape  (~(del in src.shape) pan)
      =.  +>  (so-inform %confs `shape ~)
      (so-report-lobby so-followers)
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
        =.  +>
          (so-sauce ost.bol [%quit ~]~)
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
        =.  +>.$
          (so-sauce ost.bol [%quit ~]~)
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
          =.  +>.$
            (so-sauce ost.bol [[%diff %talk-report %grams end.lab zeg.lab] ~])
          ?:  dun.lab
            (so-sauce ost.bol [[%quit ~] ~])
          +>.$(followers (~(put by followers) ost.bol riv))
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
    ++  so-cancel                                       ::<  unsubscribe follower
      ::>  removes {ost.bol} from our followers.
      ::
      .(followers (~(del by followers) ost.bol))
    ::
    ++  so-eject                                        ::<  unsubscribe ships
      ::>  removes ships {sis} from {followers}.
      ::
      |=  sis/(set ship)
      %=  +>
          followers
        %-  ~(rep in (so-unearth sis))
        |=  {b/bone f/_followers}
        =.  f  ?~(f followers f)  ::TODO  =?
        (~(del by f) b)
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
    ++  so-refresh                                      ::<  update to listeners
      ::>  called when messages get added or changed.
      ::>  calculates the changes and sends them to all
      ::>  followers.
      ::>  any followers that are no longer interested
      ::>  get removed.
      ::
      |=  {num/@ud gam/telegram}
      ^+  +>
      ::>  notify the interested readers.
      =.  +>  (so-inform %grams num gam ~)
      ::>  notify only the followers who are currently interested.
      =/  moy
        ^-  (pair (list bone) (list move))
        %-  ~(rep in followers)
        |=  {f/(pair bone river) r/(pair (list bone) (list move))}
        ::  after the end: unsubscribe
        ?:  ?-  -.q.q.f
              $ud  (lte p.q.q.f num)
              $da  (lte p.q.q.f wen.sam.tot.gam)
            ==
          [[p.f p.r] [[p.f %quit ~] q.r]]
        ::  before the start: ignore
        ?:  ?-  -.p.q.f
              $ud  (gth p.p.q.f num)
              $da  (gth p.p.q.f wen.sam.tot.gam)
            ==
          r
        ::  in the river: send gram
        :-  p.r
        [[p.f %diff %talk-report %grams num gam ~] q.r]
      =.  moves  (welp q.moy moves)
      |-  ^+  +>.^$
      ?~  p.moy  +>.^$
      $(p.moy t.p.moy, followers (~(del by followers) i.p.moy))
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
      =+  old=(~(get by known) uid.tot.gam)
      ?~  old
        (so-append gam)      ::<  add
      (so-revise u.old gam)  ::<  modify
    ::
    ++  so-append                                       ::<  append message
      ::>  add gram to our story, send report to subs.
      ::
      |=  gam/telegram
      ^+  +>
      %+  %=  so-refresh
            grams  [gam grams]
            count  +(count)
            known  (~(put by known) uid.tot.gam count)
          ==
        count
      gam
    ::
    ++  so-revise                                       ::<  revise message
      ::>  modify gram in our story, send report to subs.
      ::
      |=  {num/@ud gam/telegram}
      ::>  dex: index in grams list to insert message at.
      =+  dex=(sub count num)
      ?:  =(gam (snag (dec dex) grams))  +>.$           ::<  no change
      =.  grams
        %+  welp
        (scag (dec dex) grams)
        [gam (slag dex grams)]
      (so-refresh num gam)
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
      =.  +>.$  ::TODO  =?
        ?:  inv  +>.$
        (so-eject sis)
      =.  +>.$
        %-  so-act
        :-  %phrase
        %-  ~(rep in sis)
        |=  {s/ship a/(set partner) t/(list speech)}
        :-  (~(put in a) [%& s (main s)])
        [[%inv inv [our.bol nom]] t]
      %-  so-reform
      %=  shape
          ses.con
        %.  sis
        ?:  add
          ~(uni in ses.con.shape)
        ~(dif in ses.con.shape)
      ==
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
    =<  ta-done
    %-  ta-note:ta  %-  crip
    "talk-action stranger {(scow %p src.bol)}"
  ta-done:(ta-action:ta ost.bol act)
::
++  poke-talk-comment                                   ::<  do comment
  ::>  sends a comment.
  ::
  |=  {pax/path sup/spur txt/@t}
  ^-  (quip move +>)
  ta-done:(ta-comment:ta pax sup txt)
::
++  poke-talk-fora-post                                 ::<  do fora post
  ::>  sends a fora post.
  ::
  |=  {pax/path sup/spur hed/@t txt/@t}
  ^-  (quip move +>)
  ta-done:(ta-fora-post:ta pax sup hed txt)
::
::>  ||
::>  ||  %subscription-events
::>  ||
::+|
::
++  diff-talk-report                                    ::<  accept report
  ::>  incoming talk-report. process it and update logs.
  ::
  |=  {wir/wire ret/report}
  ^-  (quip move +>)
  =^  mos  +>.$
    %+  etch-friend  wir
    |=  {nom/knot cir/circle}
    ta-done:(ta-diff-report:ta nom cir ret)
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
  ta-done:(ta-subscribe:ta src.bol pax)
::
++  pull                                                ::<  unsubscribe
  ::>  unsubscribes.
  ::
  |=  pax/path
  ^-  (quip move +>)
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
  ta-done:(ta-quit:ta nom cir)
::
++  quit-friend                                         ::<  dropped subscription
  ::>  gall dropped our subscription. resubscribe.
  ::
  |=  wir/wire
  ^-  (quip move +>)
  %+  etch-friend  [%friend wir]
  |=  {nom/knot cir/circle}
  ta-done:(ta-retry:ta nom cir)
::
++  coup-repeat                                         ::<  message n/ack
  ::>  ack from ++ta-transmit. mark the message as
  ::>  received or rejected.
  ::
  |=  {wir/wire fal/(unit tang)}
  ^-  (quip move +>)
  %+  etch-repeat  [%repeat wir]
  |=  {num/@ud src/ship nom/knot}
  ta-done:(ta-coup-repeat:ta [num src nom] fal)
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
