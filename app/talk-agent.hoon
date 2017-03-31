::                                                      ::  ::
::::  /hoon/talk-agent/app                              ::  ::
  ::                                                    ::  ::
::
::TODO  guardian's todo's apply here too
::TODO  if we use a new structure for sending messages broker->reader, then we
::      can be safe in not doing permission checks.
::TODO  actually send update moves when needed
::TODO  process reports
::TODO  init, general subscribe to broker.
::TODO?  do we even need multiple shells?
::TODO  rename cores. pa->ta (transaction), ta->pa (partner), etc.
::
/?    310                                               ::  hoon version
/-    talk, sole                                        ::  structures
/+    talk, sole, time-to-id, twitter                   ::  libraries
/=    seed  /~  !>(.)
!:
::
::::
  ::
[. talk sole]
=>  |%                                                  ::  data structures
    ++  chattel                                         ::  full state
      $:  tales/(map knot tale)                         ::  conversations
          folks/(map ship human)                        ::  human identities
          nik/(map (set partner) char)                  ::  bound station glyphs
          nak/(jug char (set partner))                  ::  station glyph lookup
          cli/shell                                     ::  interaction state
      ==                                                ::
    ++  tale                                            ::  user-facing story
      $:  count/@ud                                     ::  (lent grams)
          grams/(list telegram)                         ::  all history
          locals/atlas                                  ::  presence
          shape/config                                  ::  configuration
          known/(map serial @ud)                        ::  messages heard
      ==                                                ::
    ::TODO  do away with most of shell state, it's largely a mirror of tale..?
    ++  shell                                           ::  console session
      $:  id/bone                                       ::  identifier
          count/@ud                                     ::  messages shown
          say/sole-share                                ::  console state
          active/(set partner)                          ::  active targets
          settings/(set knot)                           ::  frontend settings
      ==                                                ::
    ::                                                  ::
    ++  move  (pair bone card)                          ::  all actions
    ++  lime                                            ::  diff fruit
      $%  {$talk-report report}                         ::
          {$sole-effect sole-effect}                    ::
      ==                                                ::
    ++  pear                                            ::  poke fruit
      $%  {$talk-command command}                       ::
          {$talk-update update}                         ::
      ==                                                ::
    ++  card                                            ::  general card
      $%  {$diff lime}                                  ::
          {$poke wire dock pear}                        ::
          {$peer wire dock path}                        ::
      ==                                                ::
    ++  work                                            ::  interface action
      $%  {$number p/$@(@ud {@u @ud})}                  ::  relative/absolute
          {$help $~}                                    ::  print usage info
          {$who p/where}                                ::  presence
          {$what p/$@(char (set partner))}              ::  show bound glyph
          {$bind p/char q/(unit where)}                 ::
          {$join p/where}                               ::
          {$leave p/where}                              ::
          {$say p/(list speech)}                        ::
          {$eval p/cord q/twig}                         ::
          {$invite p/knot q/(list partner)}             ::  whitelist add
          {$banish p/knot q/(list partner)}             ::  blacklist add
          {$block p/knot q/(list partner)}              ::  blacklist add
          {$author p/knot q/(list partner)}             ::  whitelist add
          {$nick p/(unit ship) q/(unit cord)}           ::
          {$set p/knot}                                 ::
          {$unset p/knot}                               ::
          {$target p/where q/(unit work)}               ::  set active targets
          ::  {$destroy p/knot}                         ::
          {$create p/posture q/knot r/cord}             ::
          {$probe p/station}                            ::
      ==                                                ::
    ++  weir                                            ::  parsed wire
      $%  {$repeat p/@ud q/@p r/knot}                   ::
          {$friend p/knot q/station}                    ::
      ==                                                ::
    ++  where  (set partner)                            ::  non-empty audience
    ++  glyphs  `wall`~[">=+-" "}),." "\"'`^" "$%&@"]     :: station char pool'
    ++  broker                                          ::  broker ship and name
      |=  our/ship
      :_  %talk-guardian
      ?.  =((clan our) %earl)
        our
      (sein our)
    --
::
|_  {hid/bowl chattel}
::
++  ra                                                  ::  per transaction
  ::x  gets called when talk gets poked or otherwise prompted/needs to perform
  ::x  an action.
  ::x  arms generally modify state, and store moves in ++ra's moves. these get
  ::x  produced when calling ++ra-abet.
  ::x  in applying commands and making reports, it uses ++pa for story work.
  ::
  ::x  moves: moves storage, added to by ++ra-emit and -emil, produced by -abed.
  |_  moves/(list move)
  ::
  ++  ra-abet                                           ::  resolve core
    ::x  produces the moves stored in ++ra's moves.
    ::x  sole-effects get special treatment to become a single move.
    ::
    ^+  [*(list move) +>]
    :_  +>
    ::x  seperate our sole-effects from other moves.
    =+  ^=  yop
        |-  ^-  (pair (list move) (list sole-effect))
        ?~  moves  [~ ~]
        =+  mor=$(moves t.moves)
        ?:  ?&  =(id.cli p.i.moves)
                ?=({$diff $sole-effect *} q.i.moves)
            ==
          [p.mor [+>.q.i.moves q.mor]]
        [[i.moves p.mor] q.mor]
    ::x  flop moves, flop and squash sole-effects into a %mor.
    =+  :*  moz=(flop p.yop)
            ^=  foc  ^-  (unit sole-effect)
            ?~  q.yop  ~
            ?~(t.q.yop `i.q.yop `[%mor (flop `(list sole-effect)`q.yop)])
        ==
    ?~  foc
      moz
    ?~  id.cli
      ~&  %no-sole
      moz
    ::x  produce moves or sole-effects and moves.
    [[id.cli %diff %sole-effect u.foc] moz]
  ::
  ++  ra-diff-talk-lowdown
    ::x  process a talk lowdown
    ::
    |=  low/lowdown
    ^+  +>
    ?-  -.low
    $glyph  (ra-diff-talk-lowdown-glyph +.low)
    $names  (ra-diff-talk-lowdown-names +.low)
    $tales  (ra-diff-talk-lowdown-tales +.low)
    $precs  (ra-diff-talk-lowdown-precs +.low)
    $grams  (ra-diff-talk-lowdown-grams +.low)
    ==
  ::
  ++  ra-diff-talk-lowdown-glyph
    ::x  apply new set of glyph bindings.
    ::
    |=  nek/_nak
    ^+  +>
    ?:  =(nek nak)
      +>
    =.  nak  nek
    =.  nik  %-  ~(gas by *(map (set partner) char))
             =-  (zing `(list (list {(set partner) char}))`-)
             %+  turn  (~(tap by nek))
             |=  {a/char b/(set (set partner))}
             (turn (~(tap by b)) |=(c/(set partner) [c a]))
    sh-abet:~(sh-prod sh ~ cli (main our.hid))  ::TODO  mailbox not used
  ::
  ++  ra-diff-talk-lowdown-names
    ::x  apply new local identities.
    ::
    |=  nas/(map ship (unit human))
    ^+  +>
    %=  +>  folks
      %-  ~(gas by *(map ship human))
      %+  murn
      =<  $
      %~  tap  by
          %-  ~(uni by nas)
          ^-  _nas
          (~(run by folks) some)
      ==
      |=  {s/ship h/(unit human)}
      ?~  h  ~
      (some [s u.h])
    ==
  ::  ::
  ++  ra-diff-talk-lowdown-tales
    ::x  apply tale configs.
    ::
    |=  tals/(map knot (unit config))
    ^+  +>
    ::TODO  for every changed config, (sh-low-config oldconfig newconfig)
    ::      maybe something else for new/removed tales?
    %=  +>  tales
      %+  roll  (~(tap by tals))
      |=  {t/(pair knot (unit config)) tas/_tales}
      ~&  [%r-new-config-for p.t]
      =.  tas  ?~(tas tales tas)  ::x  start with our current tales.
      =+  tal=(fall (~(get by tas) p.t) *tale)
      ?~  q.t  (~(del by tas) p.t)
      (~(put by tas) p.t tal(shape u.q.t))
    ==
  ::
  ++  ra-diff-talk-lowdown-precs
    ::x  apply new presence.
    ::
    |=  {man/knot pes/atlas}
    ^+  +>
    =+  tal=(~(get by tales) man)
    ?~  tal  ~&([%know-no-tale man] +>.$)
    =.  +>.$  sh-abet:(~(sh-low-precs sh ~ cli man) locals.u.tal pes)
    +>.$(tales (~(put by tales) man u.tal(locals pes)))
  ::
  ++  ra-diff-talk-lowdown-grams
    ::x  apply new grams
    ::
    |=  {man/knot num/@ud gams/(list telegram)}
    ^+  +>
    =+  tal=(~(get by tales) man)
    ?~  tal  ~&([%know-no-tale man] +>.$)
    =.  +>.$  sh-abet:(~(sh-low-grams sh ~ cli man) num gams)
    pa-abet:(~(pa-lesson pa man u.tal) gams)
  ::
  ++  ra-emil                                           ::  ra-emit move list
    ::x  adds multiple moves to the core's list. flops to emulate ++ra-emit.
    ::
    |=  mol/(list move)
    %_(+> moves (welp (flop mol) moves))
  ::
  ++  ra-emit                                           ::  emit a move
    ::x  adds a move to the core's list.
    ::
    |=  mov/move
    %_(+> moves [mov moves])
  ::
  ++  ra-sole
    ::x  applies sole-action.
    ::
    |=  act/sole-action
    ^+  +>
    ?.  =(id.cli ost.hid)
      ~&  %strange-sole
      !!
    ::TODO  but there's not actually a tale to specify. mailbox is "fake"!
    sh-abet:(~(sh-sole sh ~ cli (main our.hid)) act)
  ::
  ++  ra-console
    ::x  make a shell for her.
    ::
    |=  {her/ship pax/path}
    ~&  [%ra-console her pax ost.hid]
    ^+  +>
    ::x  get story from the path, default to standard mailbox.
    =/  man/knot
      ?+  pax  !!
        $~        (main her)
        {@ta $~}  i.pax
      ==
    =/  she/shell
      %*(. *shell id ost.hid, active (sy [%& our.hid man] ~))
    sh-abet:~(sh-prod sh ~ she man)
  ::
  ++  ra-init
    ::x  populate state on first boot. subscribes to our broker.
    ::
    ~&  [%r-ra-init ost.hid]
    %-  ra-emit
    :*  ost.hid
        %peer
        /
        (broker our.hid)
        /
    ==
  ::
  ++  pa                                                ::  story core
    ::x  tale core, used for doing work on a story.
    ::x  as always, an -abet arms is used for applying changes to the state.
    ::x  ++pa-watch- arms get called by ++ra-subscribe to add a subscriber.
    ::x  bones are used to identify subscribers (source event identifiers)
    ::
    |_  ::x  man: the knot identifying the story in stories.
        ::x  story doesn't get a face because ease of use
        ::
        $:  man/knot
            tale
        ==
    ++  pa-abet
      ::x  apply/fold changes back into the stories map.
      ::
      ^+  +>
      +>(tales (~(put by tales) man `tale`+<+))
    ::
    ++  pa-lesson                                       ::  learn multiple
      ::x  learn all telegrams in a list.
      ::
      |=  gaz/(list telegram)
      ^+  +>
      ?~  gaz  +>
      $(gaz t.gaz, +> (pa-learn i.gaz))
    ::
    ++  pa-learn                                        ::  learn message
      ::x  store an incoming telegram, modifying audience to say we received it.
      ::x  update existing telegram if it already exists.
      ::
      |=  gam/telegram
      ^+  +>
      ::x  if author isn't allowed to write here, reject.
      ::TODO  we shouldn't need to do permission checks anymore, all we need to
      ::      do is make sure the grams were sent to us by our broker.
      ::      do this earlier up in the chain.
      =.  q.q.gam
        ::x  if we are in the audience, mark us as having received it.
        =+  ole=(~(get by q.q.gam) [%& our.hid man])
        ?^  ole  (~(put by q.q.gam) [%& our.hid man] -.u.ole %received)
        ::  for fedearted stations, pretend station src/foo is also our/foo
        ::  XX pass src through explicitly instead of relying on implicit
        ::     value in hid from the subscription to src/foo
        =+  ole=(~(get by q.q.gam) [%& src.hid man])
        ?~  ole  q.q.gam
        ::x  as described above, fake src into our.
        =.  q.q.gam  (~(del by q.q.gam) [%& src.hid man])
        (~(put by q.q.gam) [%& our.hid man] -.u.ole %received)
      =+  old=(~(get by known) p.q.gam)
      ?~  old
        (pa-append gam)      ::x  add
      (pa-revise u.old gam)  ::x  modify
    ::
    ++  pa-append                                       ::  append new gram
      ::x  add gram to our story, and update our subscribers.
      ::
      |=  gam/telegram
      ^+  +>
      %=  +>
        grams  [gam grams]
        count  +(count)
        known  (~(put by known) p.q.gam count)
      ==
    ::
    ++  pa-revise                                       ::  revise existing gram
      ::x  modify a gram in our story, and update our subscribers.
      ::
      |=  {num/@ud gam/telegram}
      =+  way=(sub count num)
      ?:  =(gam (snag (dec way) grams))
        +>.$                                            ::  no change
      =.  grams  (welp (scag (dec way) grams) [gam (slag way grams)])
      +>.$
    ::
    ++  pa-remind                                       ::  presence update
      ::x  merge local and remote presences, and store them in locals.
      ::
      ::TODO  verify if this now does what it's supposed to.
      |=  {loc/atlas rem/(map partner atlas)}
      ::x  remove this story from the presence map, since it's in local already.
      =.  rem  (~(del by rem) %& our.hid man)  :: superceded by local data
      ::TODO  stop using timed, just do uni here.
      =/  pres
        %+  roll  (~(tap by rem))
        |=  {r/(pair partner atlas) l/atlas}
        =.  l  ?~(l loc l)  ::TODO  =?
        (~(uni by l) q.r)
      +>.$(locals pres)
    ::
    ++  pa-cabal
      ::x  update the tale's config.
      ::
      |=  {cuz/station con/config ham/(map station config)}
      ^+  +>
      +>(shape con)
    --
  ::
  ++  sh                                                ::  per console
    ::x  shell core, responsible for doing things with console sessions,
    ::x  like parsing input, acting based on input, showing output, keeping
    ::x  track of settings and other frontend state.
    ::x  important arms include ++sh-repo which is used to apply reports, and
    ::x  ++sh-sole which gets called upon cli prompt interaction.
    ::x  any talk commands the core's arms want to have executed get put into
    ::x  coz. the stored commands get applied upon calling ++sh-abet.
    ::
    |_  $:  ::x  coz: talk commands storage, applied by ++sh-abet.
            ::x  she: console session state used in this core.
            ::
            coz/(list command)
            she/shell
            man/knot
        ==
    ++  sh-scad                                         ::  command parser
      ::x  builds a core with parsers for talk-cli, and produces its work arm.
      ::x  ++work uses those parsers to parse the current talk-cli prompt input
      ::x  and produce a work item to be executed by ++sh-work.
      ::
      =<  work
      |%
      ++  expr                                          ::  [cord twig]
        |=  tub/nail  %.  tub
        %+  stag  (crip q.tub)
        wide:(vang & [&1:% &2:% (scot %da now.hid) |3:%])
      ::
      ++  dare                                          ::  @dr
        %+  sear
          |=  a/coin
          ?.  ?=({$$ $dr @} a)  ~
          (some `@dr`+>.a)
        nuck:so
      ::
      ++  ship  ;~(pfix sig fed:ag)                     ::  ship
      ++  shiz                                          ::  ship set
        %+  cook
          |=(a/(list ^ship) (~(gas in *(set ^ship)) a))
        (most ;~(plug com (star ace)) ship)
      ::
      ++  pasp                                          ::  passport
        ;~  pfix  pat
          ;~  pose
            (stag %twitter ;~(pfix (jest 't') col urs:ab))
          ==
        ==
      ::
      ++  stan                                          ::  station
        ;~  pose
          (cold [our.hid (main our.hid)] col)
          ;~(pfix cen (stag our.hid sym))
          ;~(pfix fas (stag (sein our.hid) sym))
        ::
          %+  cook
            |=  {a/@p b/(unit term)}
            [a ?^(b u.b (main a))]
          ;~  plug
            ship
            (punt ;~(pfix fas urs:ab))
          ==
        ==
      ::
      ++  parn                                          ::  partner
        ;~  pose
          (stag %& stan)
          (stag %| pasp)
        ==
      ++  partners-flat                                 ::  collapse mixed list
        |=  a/(list (each partner (set partner)))
        ^-  (set partner)
        ?~  a  ~
        ?-  -.i.a
          $&  (~(put in $(a t.a)) p.i.a)
          $|  (~(uni in $(a t.a)) p.i.a)
        ==
      ::
      ++  para                                          ::  partners alias
        %+  cook  partners-flat
        %+  most  ;~(plug com (star ace))
        (pick parn (sear sh-glyf glyph))
      ::
      ++  parz                                          ::  non-empty partners
        %+  cook  ~(gas in *(set partner))
        (most ;~(plug com (star ace)) parn)
      ::
      ++  nump                                          ::  number reference
        ;~  pose
          ;~(pfix hep dem:ag)
          ;~  plug
            (cook lent (plus (just '0')))
            ;~(pose dem:ag (easy 0))
          ==
          (stag 0 dem:ag)
        ==
      ::
      ++  pore                                          ::  posture
        ;~  pose
          (cold %black (jest %channel))
          (cold %white (jest %village))
          (cold %green (jest %journal))
          (cold %brown (jest %mailbox))
        ==
      ::
      ++  message
        ;~  pose
          ;~(plug (cold %eval hax) expr)
        ::
          %+  stag  %say
          %+  most  (jest '•')
          ;~  pose
            (stag %url aurf:urlp)
            :(stag %lin | ;~(pfix pat text))
            :(stag %lin & ;~(less sem hax text))
          ==
        ==
      ::
      ++  nick  (cook crip (stun [1 14] low))           ::  nickname
      ++  text  (cook crip (plus (shim ' ' '~')))       ::  bullets separating
      ++  glyph  (mask "/\\\{(<!?{(zing glyphs)}")      ::  station postfix
      ++  setting
        %-  perk  :~
          %noob
          %quiet
          %showtime
        ==
      ++  work
        %+  knee  *^work  |.  ~+
        =-  ;~(pose ;~(pfix sem -) message)
        ;~  pose
          ;~  (glue ace)  (perk %create ~)
            pore
            ;~(pfix cen sym)
            qut
          ==
        ::
          ;~(plug (perk %who ~) ;~(pose ;~(pfix ace para) (easy ~)))
          ;~(plug (perk %bind ~) ;~(pfix ace glyph) (punt ;~(pfix ace para)))
          ;~((glue ace) (perk %join ~) para)
          ;~((glue ace) (perk %leave ~) para)
          ;~((glue ace) (perk %what ~) ;~(pose parz glyph))
        ::
          ;~  plug  (perk %nick ~)
            ;~  pose
              ;~  plug
                (cook some ;~(pfix ace ship))
                (cold (some '') ;~(pfix ace sig))
              ==
              ;~  plug
                ;~  pose
                  (cook some ;~(pfix ace ship))
                  (easy ~)
                ==
                ;~  pose
                  (cook some ;~(pfix ace nick))
                  (easy ~)
                ==
              ==
            ==
          ==
        ::
          ;~(plug (perk %set ~) ;~(pose ;~(pfix ace setting) (easy %$)))
          ;~(plug (perk %unset ~) ;~(pfix ace setting))
        ::
          ;~(plug (perk %help ~) (easy ~))
          (stag %number nump)
          (stag %target ;~(plug para (punt ;~(pfix ace message))))
          (stag %number (cook lent (star sem)))
        ==
      --
    ++  sh-abet
      ::x  applies talk commands (in reverse order, because ++sh-tell adds them
      ::x  to coz as [new coz], but you want to apply them in the order
      ::x  [coz new]). produces an updated context for the ++sh core.
      ::
      ^+  +>  ::x  points to ++sh's |_ core's context.
      %.  ^-  (list move)
        %-  flop
        %+  turn  coz
        |=  c/command
        ^-  move
        :*  ost.hid
            %poke
            /reader/command
            (broker our.hid)
            [%talk-command c]
        ==
      ra-emil(cli she)
    ::
    ++  sh-fact                                         ::  send console effect
      ::x  adds a console effect to ++ra's moves.
      ::
      |=  fec/sole-effect
      ^+  +>
      +>(moves :_(moves [id.she %diff %sole-effect fec]))
    ::
    ++  sh-update
      ::x  adds a talk-update to ++ra's moves
      ::
      |=  dup/update
      ^+  +>
      ::TODO  is ost.hid okay here? or do we want to store bone of broker?
      ~&  [%r-update-to ost.hid]
      %=  +>
        moves  :_  moves
               :*  ost.hid
                   %poke
                   /reader/update
                   (broker our.hid)
                   [%talk-update dup]
               ==
      ==
    ::
    ++  sh-prod                                         ::  show prompt
      ::x  make and store a move to modify the cli prompt, displaying audience.
      ::
      ^+  .
      %+  sh-fact  %pro
      :+  &  %talk-line
      ^-  tape
      =/  rew/(pair (pair @t @t) (set partner))
          [['[' ']'] active.she]
      =+  cha=(~(get by nik) q.rew)
      ?^  cha  ~[u.cha ' ']
      :: ~&  [rew nik nak]
      =+  por=~(te-prom te man q.rew)
      (weld `tape`[p.p.rew por] `tape`[q.p.rew ' ' ~])
    ::
    ++  sh-pact                                         ::  update active aud
      ::x  change currently selected audience to lix, updating prompt.
      ::
      |=  lix/(set partner)
      ^+  +>
      =+  act=(sh-pare lix)  ::x  ensure we can see what we send.
      ?~  act  ~|(%no-audience !!)  ::TODO  can't happen, remove.
      ?:  =(active.she act)  +>.$
      sh-prod(active.she act)
    ::
    ++  sh-pare                                         ::  adjust target list
      ::x  if the audience paz does not contain a partner we're subscribed to,
      ::x  add our mailbox to the audience (so that we can see our own message).
      ::
      |=  paz/(set partner)
      ?:  (sh-pear paz)  paz
      (~(put in paz) [%& our.hid (main our.hid)])
    ::
    ++  sh-pear                                         ::  hearback
      ::x  produces true if any partner is included in our subscriptions,
      ::x  aka, if we hear messages sent to paz.
      ::
      |=  paz/(set partner)
      ?~  paz  |
      ::TODO  hoon does short-circuiting, right? do the has first, then recurse.
      ?|  $(paz l.paz)
          $(paz r.paz)
          (~(has in sources.shape:(~(got by tales) man)) `partner`n.paz)
      ==
    ::
    ++  sh-pest                                         ::  report listen
      ::x  updates audience to be tay, only if tay is not a village/%white.
      ::x?  why exclude village (invite-only?) audiences from this?
      ::
      ::TODO  does this still do the correct thing?
      |=  tay/partner
      ^+  +>
      ?.  ?=($& -.tay)  +>  ::x  if partner is a passport, do nothing.
      =+  tal=(~(get by tales) q.p.tay)
      ?.  |(?=($~ tal) !?=($white p.cordon.shape.u.tal))
        +>.$
      (sh-pact [tay ~ ~])
    ::
    ++  sh-rend                                         ::  print on one line
      ::x  renders a telegram as a single line, adds it as a console move,
      ::x  and updates the selected audience to match the telegram's.
      ::
      |=  gam/telegram
      =+  lin=~(tr-line tr man settings.she gam)
      (sh-fact %txt lin)
    ::
    ++  sh-numb                                         ::  print msg number
      ::x  does as it says on the box.
      ::
      |=  num/@ud
      ^+  +>
      =+  bun=(scow %ud num)
      %+  sh-fact  %txt
      (runt [(sub 13 (lent bun)) '-'] "[{bun}]")
    ::
    ++  sh-glyf                                         ::  decode glyph
      ::x  gets the partner(s) that match a glyph.
      ::x?  why (set partner)? it seems like it only ever returns a single one.
      ::TODO should produce a set when ambiguous.
      ::
      |=  cha/char  ^-  (unit (set partner))
      =+  lax=(~(get ju nak) cha)
      ?:  =(~ lax)  ~  ::x  no partner.
      ?:  ?=({* $~ $~} lax)  `n.lax  ::x  single partner.
      ::x  in case of multiple partners, pick the most recently active one.
      =+  grams=grams:(~(got by tales) man)  ::TODO  probably no longer correct.
      |-  ^-  (unit (set partner))
      ?~  grams  ~
      ::x  get first partner from a telegram's audience.
      =+  pan=(silt (turn (~(tap by q.q.i.grams)) head))
      ?:  (~(has in lax) pan)  `pan
      $(grams t.grams)
    ::
    ++  sh-repo-atlas-diff
      ::x  calculates the difference between two atlasses (presence lists).
      ::
      |=  {one/atlas two/atlas}
      =|  $=  ret
          $:  old/(list (pair ship status))
              new/(list (pair ship status))
              cha/(list (pair ship status))
          ==
      ^+  ret
      =.  ret
        =+  eno=(~(tap by one))
        |-  ^+  ret
        ?~  eno  ret
        =.  ret  $(eno t.eno)
        ?:  =(%gone p.q.i.eno)  ret
        =+  unt=(~(get by two) p.i.eno)
        ?~  unt
          ret(old [i.eno old.ret])
        ?:  =(%gone p.u.unt)
          ret(old [i.eno old.ret])
        ?:  =(q.i.eno u.unt)  ret
        ret(cha [[p.i.eno u.unt] cha.ret])
      =.  ret
        =+  owt=(~(tap by two))
        |-  ^+  ret
        ?~  owt  ret
        =.  ret  $(owt t.owt)
        ?:  =(%gone p.q.i.owt)  ret
        ?.  (~(has by one) p.i.owt)
          ret(new [i.owt new.ret])
        ?:  =(%gone p:(~(got by one) p.i.owt))
          ret(new [i.owt new.ret])
        ret
      ret
    ::
    ++  sh-set-diff
      ::x  calculates the difference between two sets,
      ::x  returning what was lost in old and what was gained in new.
      ::
      |*  {one/(set *) two/(set *)}
      :-  ^=  old  (~(tap in (~(dif in one) two)))
          ^=  new  (~(tap in (~(dif in two) one)))
    ::
    ++  sh-puss
      ::x  posture as text.
      ::
      |=  a/posture  ^-  tape
      ?-  a
        $black  "channel"
        $brown  "mailbox"
        $white  "village"
        $green  "journal"
      ==
    ::
    ++  sh-repo-config-exceptions
      ::x  used by ++sh-repo-config-show to aid in printing info to cli.
      ::
      |=  {pre/tape por/posture old/(list ship) new/(list ship)}
      =+  out=?:(?=(?($black $brown) por) "try " "cut ")
      =+  inn=?:(?=(?($black $brown) por) "ban " "add ")
      =.  +>.$
          |-  ^+  +>.^$
          ?~  old  +>.^$
          =.  +>.^$  $(old t.old)
          (sh-note :(weld pre out " " (scow %p i.old)))
      =.  +>.$
          |-  ^+  +>.^$
          ?~  new  +>.^$
          =.  +>.^$  $(new t.new)
          (sh-note :(weld pre out " " (scow %p i.new)))
      +>.$
    ::
    ++  sh-repo-config-sources
      ::x  used by ++sh-repo-config-show to aid in printing info to cli,
      ::x  pertaining to the un/subscribing to partners.
      ::
      |=  {pre/tape old/(list partner) new/(list partner)}
      ^+  +>
      =.  +>.$
          |-  ^+  +>.^$
          ?~  old  +>.^$
          =.  +>.^$  $(old t.old)
          (sh-note (weld pre "off {~(ta-full ta man i.old)}"))
      =.  +>.$
          |-  ^+  +>.^$
          ?~  new  +>.^$
          =.  +>.^$  $(new t.new)
          (sh-note (weld pre "hey {~(ta-full ta man i.new)}"))
      +>.$
    ::
    ++  sh-repo-config-show
      ::x  prints config changes to the cli.
      ::
      |=  {pre/tape laz/config loc/config}
      ^+  +>
      =.  +>.$
        ?:  =(caption.loc caption.laz)  +>.$
        (sh-note :(weld pre "cap " (trip caption.loc)))
      =.  +>.$
          %+  sh-repo-config-sources
            (weld (trip man) ": ")
          (sh-set-diff sources.laz sources.loc)
      ?:  !=(p.cordon.loc p.cordon.laz)
        =.  +>.$  (sh-note :(weld pre "but " (sh-puss p.cordon.loc)))
        %^    sh-repo-config-exceptions
            (weld (trip man) ": ")
          p.cordon.loc
        [~ (~(tap in q.cordon.loc))]
      %^    sh-repo-config-exceptions
          (weld (trip man) ": ")
        p.cordon.loc
      (sh-set-diff q.cordon.laz q.cordon.loc)
    ::
    ++  sh-low-config
      ::x  updates the current shell's cabal and prints changes to cli.
      ::
      |=  {old/config new/config}
      ^+  +>
      (sh-repo-config-show "" old new)
    ::
    ++  sh-note                                         ::  shell message
      ::x  prints a txt to cli in talk's format.
      ::
      |=  txt/tape
      ^+  +>
      (sh-fact %txt (runt [14 '-'] `tape`['|' ' ' (scag 64 txt)]))
    ::
    ++  sh-spaz                                         ::  print status
      ::x  gets the presence of a status.
      ::
      |=  saz/status
      ^-  tape
      ['%' (trip p.saz)]
    ::
    ++  sh-repo-group-diff-here                         ::  print atlas diff
      ::x  prints presence notifications.
      ::
      |=  $:  pre/tape
            $=  cul
            $:  old/(list (pair ship status))
                new/(list (pair ship status))
                cha/(list (pair ship status))
            ==
          ==
      ?:  (~(has in settings.she) %quiet)
        +>.$
      =.  +>.$
          |-  ^+  +>.^$
          ?~  old.cul  +>.^$
          =.  +>.^$  $(old.cul t.old.cul)
          (sh-note (weld pre "bye {(scow %p p.i.old.cul)}"))
      =.  +>.$
          |-  ^+  +>.^$
          ?~  new.cul  +>.^$
          =.  +>.^$  $(new.cul t.new.cul)
          %-  sh-note
          (weld pre "met {(scow %p p.i.new.cul)} {(sh-spaz q.i.new.cul)}")
      =.  +>.$
          |-  ^+  +>.^$
          ?~  cha.cul  +>.^$
          %-  sh-note
          (weld pre "set {(scow %p p.i.cha.cul)} {(sh-spaz q.i.cha.cul)}")
      +>.$
    ::
    ++  sh-low-precs
      ::x  print presence changes
      ::
      |=  {old/atlas new/atlas}
      ^+  +>
      =+  dif=(sh-repo-atlas-diff old new)
      (sh-repo-group-diff-here "" dif)
    ::
    ++  sh-low-gram
      ::x  renders telegram: increase gram count and print the gram.
      ::x  every fifth gram, prints the number.
      ::
      |=  {num/@ud gam/telegram}
      ^+  +>
      ?:  =(num count.she)
        =.  +>  ?:(=(0 (mod num 5)) (sh-numb num) +>)
        (sh-rend(count.she +(num)) gam)
      ?:  (gth num count.she)
        =.  +>  (sh-numb num)
        (sh-rend(count.she +(num)) gam)
      +>
    ::
    ++  sh-low-grams                                    ::  apply telegrams
      ::x  renders telegrams.
      ::
      |=  {num/@ud gaz/(list telegram)}
      ^+  +>
      ?~  gaz  +>
      $(gaz t.gaz, num +(num), +> (sh-low-gram num i.gaz))
    ::
    ++  sh-sane-chat                                    ::  sanitize chatter
      ::x  (for chat messages) sanitizes the input buffer and splits it into
      ::x  multiple lines ('•').
      ::
      |=  buf/(list @c)
      ^-  (list sole-edit)
      ?~  buf  ~
      =+  isa==(i.buf (turf '@'))
      =+  [[pre=*@c cur=i.buf buf=t.buf] inx=0 brk=0 len=0 new=|]
      =*  txt  -<
      |^  ^-  (list sole-edit)
          ?:  =(cur (turf '•'))
            ?:  =(pre (turf '•'))
              [[%del inx] ?~(buf ~ $(txt +.txt))]
            ?:  new
              [(fix ' ') $(cur `@c`' ')]
            newline
          ?:  =(cur `@`' ')
            =.  brk  ?:(=(pre `@`' ') brk inx)
            ?.  =(64 len)  advance
            :-  (fix(inx brk) (turf '•'))
            ?:  isa
              [[%ins +(brk) (turf '@')] newline(new &)]
            newline(new &)
          ?:  =(64 len)
            =+  dif=(sub inx brk)
            ?:  (lth dif 64)
              :-  (fix(inx brk) (turf '•'))
              ?:  isa
                [[%ins +(brk) (turf '@')] $(len dif, new &)]
              $(len dif, new &)
            [[%ins inx (turf '•')] $(len 0, inx +(inx), new &)]
          ?:  |((lth cur 32) (gth cur 126))
            [(fix '?') advance]
          ?:  &((gte cur 'A') (lte cur 'Z'))
            [(fix (add 32 cur)) advance]
          advance
      ::
      ++  advance  ?~(buf ~ $(len +(len), inx +(inx), txt +.txt))
      ++  newline  ?~(buf ~ $(len 0, inx +(inx), txt +.txt))
      ++  fix  |=(cha/@ [%mor [%del inx] [%ins inx `@c`cha] ~])
      --
    ::
    ++  sh-sane                                         ::  sanitize input
      ::x  parses cli prompt input using ++sh-scad and sanitizes when invalid.
      ::
      |=  {inv/sole-edit buf/(list @c)}
      ^-  {lit/(list sole-edit) err/(unit @u)}
      =+  res=(rose (tufa buf) sh-scad)
      ?:  ?=($| -.res)  [[inv]~ `p.res]
      :_  ~
      ?~  p.res  ~
      =+  wok=u.p.res
      |-  ^-  (list sole-edit)
      ?+  -.wok  ~
        $target  ?~(q.wok ~ $(wok u.q.wok))
        $say  |-  ::  XX per line
              ?~  p.wok  ~
              ?:  ?=($lin -.i.p.wok)
                (sh-sane-chat buf)
              $(p.wok t.p.wok)
      ==
    ::
    ++  sh-slug                                         ::  edit to sanity
      ::x  corrects invalid prompt input.
      ::
      |=  {lit/(list sole-edit) err/(unit @u)}
      ^+  +>
      ?~  lit  +>
      =^  lic  say.she
          (~(transmit sole say.she) `sole-edit`?~(t.lit i.lit [%mor lit]))
      (sh-fact [%mor [%det lic] ?~(err ~ [%err u.err]~)])
    ::
    ++  sh-stir                                         ::  apply edit
      ::x  called when typing into the talk prompt. applies the change and does
      ::x  sanitizing.
      ::
      |=  cal/sole-change
      ^+  +>
      =^  inv  say.she  (~(transceive sole say.she) cal)
      =+  fix=(sh-sane inv buf.say.she)
      ?~  lit.fix
        +>.$
      ?~  err.fix
        (sh-slug fix)                 :: just capital correction
      ?.  &(?=($del -.inv) =(+(p.inv) (lent buf.say.she)))
        +>.$                          :: allow interior edits, deletes
      (sh-slug fix)
    ::
    ++  sh-lame                                         ::  send error
      ::x  just puts some text into the cli.
      ::
      |=  txt/tape
      (sh-fact [%txt txt])
    ::
    ++  sh-whom                                         ::  current audience
      ::x  produces the currently selected audience for this shell.
      ::
      ^-  audience
      %-  ~(gas by *audience)
      %+  turn  (~(tap in active.she))
      |=(a/partner [a *envelope %pending])
    ::
    ++  sh-tell                                         ::  add command
      ::x  adds talk command to core state. these get applied with ++sh-abet.
      ::
      |=  cod/command
      %_(+> coz [cod coz])
    ::
    ++  sh-twig-head  ^-  vase                          ::  eval data
      ::x  makes a vase of environment data to evaluate against (#-messages).
      ::
      !>(`{our/@p now/@da eny/@uvI}`[our.hid now.hid (shas %eny eny.hid)])
    ::
    ++  sh-work                                         ::  do work
      ::x  implements worker arms for different talk commands.
      ::x  all worker arms must produce updated state/context.
      ::
      |=  job/work
      ^+  +>
      =+  roy=(~(got by tales) man)
      =<  work
      |%
      ++  work
        ?-  -.job
          $number  (number +.job)
          $leave   (leave +.job)
          $join    (join +.job)
          $eval    (eval +.job)
          $who     (who +.job)
          $what    (what +.job)
          $bind    (bind +.job)
          $invite  (invite +.job)
          $banish  (banish +.job)
          $author  (author +.job)
          $block   (block +.job)
          $create  (create +.job)
          $nick    (nick +.job)
          $set     (wo-set +.job)
          $unset   (unset +.job)
          $target  (target +.job)
          $probe   (probe +.job)
          $help    help
          $say     (say +.job)
        ==
      ::
      ++  activate                                      ::  from %number
        |=  gam/telegram
        ^+  ..sh-work
        =+  tay=~(. tr man settings.she gam)
        =.  ..sh-work  (sh-fact tr-fact:tay)
        sh-prod(active.she tr-pals:tay)
      ::
      ++  help
        (sh-fact %txt "see http://urbit.org/docs/using/messaging/")
      ::
      ++  glyph
        |=  idx/@
        =<  cha
        %+  reel  glyphs
        |=  {all/tape ole/{cha/char num/@}}
        =+  new=(snag (mod idx (lent all)) all)
        =+  num=~(wyt in (~(get ju nak) new))
        ?~  cha.ole  [new num]
        ?:  (lth num.ole num)
          ole
        [new num]
      ::
      ++  set-glyph
        |=  {cha/char lix/(set partner)}
        =:  nik  (~(put by nik) lix cha)
            nak  (~(put ju nak) cha lix)
          ==
        (sh-update %bind cha lix)
      ::
      ++  join                                          ::  %join
        |=  pan/(set partner)
        ^+  ..sh-work
        =.  ..sh-work
          =+  (~(get by nik) pan)
          ?^  -  (sh-note "has glyph {<u>}")
          =+  cha=(glyph (mug pan))
          (sh-note:(set-glyph cha pan) "new glyph {<cha>}")
        =.  ..sh-work
          sh-prod(active.she pan)
        =+  loc=shape:(~(got by tales) man)
        ::x  change local mailbox config to include subscription to pan.
        %^  sh-tell  %design  man
        `loc(sources (~(uni in sources.loc) pan))
      ::
      ++  leave                                          ::  %leave
        |=  pan/(set partner)
        ^+  ..sh-work
        =+  loc=shape:(~(got by tales) man)
        ::x  change local mailbox config to exclude subscription to pan.
        %^  sh-tell  %design  man
        `loc(sources (~(dif in sources.loc) pan))
      ::
      ++  what                                          ::  %what
        |=  qur/$@(char (set partner))  ^+  ..sh-work
        ?^  qur
          =+  cha=(~(get by nik) qur)
          (sh-fact %txt ?~(cha "none" [u.cha]~))
        =+  pan=(~(tap in (~(get ju nak) qur)))
        ?:  =(~ pan)  (sh-fact %txt "~")
        =<  (sh-fact %mor (turn pan .))
        |=(a/(set partner) [%txt <a>]) ::  XX ~(te-whom te man.she a)
      ::
      ++  who                                          ::  %who
        |=  pan/(set partner)  ^+  ..sh-work
        ::TODO  clever use of =< and . take note!
        ::TODO  test/fix/update
        =<  (sh-fact %mor (murn (sort (~(tap by tales) ~) aor) .))
        |=  {nam/knot tal/tale}
        ~
        ::=<  (sh-fact %mor (murn (sort (~(tap by q.owners.she) ~) aor) .))
        ::|=  {pon/partner alt/atlas}  ^-  (unit sole-effect)
        ::?.  |(=(~ pan) (~(has in pan) pon))  ~
        ::=-  `[%tan rose+[", " `~]^- leaf+~(ta-full ta man.she pon) ~]
        ::=<  (murn (sort (~(tap by alt)) aor) .)
        ::|=  {a/ship b/presence c/human}  ^-  (unit tank)
        ::::TODO  print human names.
        ::?-  b
        ::  $gone  ~
        ::  $hear  `>a<
        ::  $talk  `>a<      ::  XX difference
        ::==
      ::
      ++  bind                                          ::  %bind
        |=  {cha/char pan/(unit (set partner))}  ^+  ..sh-work
        ?~  pan  $(pan `active.she)
        =+  ole=(~(get by nik) u.pan)
        ?:  =(ole [~ cha])  ..sh-work
        (sh-note:(set-glyph cha u.pan) "bound {<cha>} {<u.pan>}")
      ::
      ++  invite                                        ::  %invite
        |=  {nom/knot tal/(list partner)}
        ^+  ..sh-work
        !!
      ::
      ++  block                                         ::  %block
        |=  {nom/knot tal/(list partner)}
        ^+  ..sh-work
        !!
      ::
      ++  author                                        ::  %author
        |=  {nom/knot tal/(list partner)}
        ^+  ..sh-work
        !!
      ::
      ++  banish                                        ::  %banish
        |=  {nom/knot tal/(list partner)}
        ^+  ..sh-work
        !!
      ::
      ++  create                                        ::  %create
        |=  {por/posture nom/knot txt/cord}
        ^+  ..sh-work
        ?:  (~(has in tales) nom)
          (sh-lame "{(trip nom)}: already exists")
        =.  ..sh-work
            ::x  create new config for channel.
            %^  sh-tell  %design  nom
            :-  ~
            :+  [[%& our.hid nom] ~ ~]
              (end 3 64 txt)
            [por ~]
        (join [[%& our.hid nom] ~ ~])
      ::
      ++  reverse-folks
        |=  nym/knot
        ^-  (list ship)
        %+  murn  (~(tap by folks))
        |=  {p/ship q/human}
        ?~  hand.q  ~
        ?.  =(u.hand.q nym)  ~
        [~ u=p]
      ::
      ++  nick                                          ::  %nick
        |=  {her/(unit ship) nym/(unit cord)}
        ^+  ..sh-work
        ::x  no arguments
        ?:  ?=({$~ $~} +<)
          %+  sh-fact  %mor
          %+  turn  (~(tap by folks))
          |=  {p/ship q/human}
          :-  %txt
          ?~  hand.q
            "{<p>}:"
          "{<p>}: {<u.hand.q>}"
        ::x  unset nickname
        ?~  nym
          ?>  ?=(^ her)
          =+  asc=(~(get by folks) u.her)
          %+  sh-fact  %txt
          ?~  asc  "{<u.her>} unbound"
          ?~  hand.u.asc  "{<u.her>}:"
          "{<u.her>}: {<u.hand.u.asc>}"
        ::x  get nickname
        ?~  her
          %+  sh-fact  %mor
          %+  turn  (reverse-folks u.nym)
          |=  p/ship
          [%txt "{<p>}: {<u.nym>}"]
        %.  [%human u.her [true=~ hand=nym]]
        %=  sh-update
          folks  ?~  u.nym
                   (~(del by folks) u.her)  ::x  unset nickname
                 (~(put by folks) u.her [true=~ hand=nym])  ::x  set nickname
        ==
      ::
      ++  wo-set                                        ::  %set
        |=  seg/knot
        ^+  ..sh-work
        ?~  seg
          %+  sh-fact  %mor
          %+  turn  (~(tap in settings.she))
          |=  s/knot
          [%txt (trip s)]
        %=  ..sh-work
          settings.she  (~(put in settings.she) seg)
        ==
      ::
      ++  unset                                         ::  %unset
        |=  neg/knot
        ^+  ..sh-work
        %=  ..sh-work
          settings.she  (~(del in settings.she) neg)
        ==
      ::
      ++  target                                        ::  %target
        |=  {pan/(set partner) woe/(unit ^work)}
        ^+  ..sh-work
        =.  ..sh-pact  (sh-pact pan)
        ?~(woe ..sh-work work(job u.woe))
      ::
      ++  number                                        ::  %number
        ::TODO  !!!!!
        ::TODO  mistakes were made. turns out we need multiple shells after all!
        ::TODO  but that seems a bit hacky, surely we can do better than it was?
        ::TODO  ...just storing the telegrams in the order they were printed
        ::      doesn't seem clean enough though.
        ::TODO  !!!!!
        |=  num/$@(@ud {p/@u q/@ud})
        ^+  ..sh-work
        =+  roy=(~(got by tales) man)  ::TODO  see above
        |-
        ?@  num
          ?:  (gte num count.roy)
            (sh-lame "{(scow %s (new:si | +(num)))}: no such telegram")
          =.  ..sh-fact  (sh-fact %txt "? {(scow %s (new:si | +(num)))}")
          (activate (snag num grams.roy))
        ?.  (gth q.num count.roy)
          ?~  count.roy
            (sh-lame "0: no messages")
          =+  msg=(deli (dec count.roy) num)
          =.  ..sh-fact  (sh-fact %txt "? {(scow %ud msg)}")
          (activate (snag (sub count.roy +(msg)) grams.roy))
        (sh-lame "…{(reap p.num '0')}{(scow %ud q.num)}: no such telegram")
      ::
      ++  deli                                          ::  find number
        |=  {max/@ud nul/@u fin/@ud}  ^-  @ud
        =+  dog=|-(?:(=(0 fin) 1 (mul 10 $(fin (div fin 10)))))
        =.  dog  (mul dog (pow 10 nul))
        =-  ?:((lte - max) - (sub - dog))
        (add fin (sub max (mod max dog)))
      ::
      ++  probe                                         ::  inquire
        |=  cuz/station
        ^+  ..sh-work
        ~&  [%probe cuz]
        ..sh-work
      ::
      ++  eval                                          ::  run
        |=  {txt/cord exe/twig}
        =>  |.([(sell (slap (slop sh-twig-head seed) exe))]~)
        =+  tan=p:(mule .)
        (say [%fat tank+tan exp+txt] ~)
      ::
      ++  say                                           ::  publish
        |=  sep/(list speech)
        ^+  ..sh-work
        =-  ..sh-work(coz ?~(tot coz :_(coz [%publish tot])))
        |-  ^-  tot/(list thought)
        ?~  sep  ~
        =^  sir  ..sh-work  sh-uniq
        [[sir sh-whom [now.hid ~ i.sep]] $(sep t.sep)]
      --
    ::
    ++  sh-done                                         ::  apply result
      ::x  called upon hitting return in the prompt. if input is invalid,
      ::x  ++sh-slug is called. otherwise, the appropriate work is done
      ::x  and the entered command (if any) gets displayed to the user.
      ::
      =+  fix=(sh-sane [%nop ~] buf.say.she)
      ?^  lit.fix
        (sh-slug fix)
      =+  jub=(rust (tufa buf.say.she) sh-scad)
      ?~  jub  (sh-fact %bel ~)
      %.  u.jub
      =<  sh-work
      =+  buf=buf.say.she
      =^  cal  say.she  (~(transmit sole say.she) [%set ~])
      %-  sh-fact
      :*  %mor
          [%nex ~]
          [%det cal]
          ?.  ?=({$';' *} buf)  ~
          :_  ~
          [%txt (runt [14 '-'] `tape`['|' ' ' (tufa `(list @)`buf)])]
      ==
    ::
    ++  sh-sole                                         ::  apply edit
      ::x  applies sole action.
      ::
      |=  act/sole-action
      ^+  +>
      ?-  -.act
        $det  (sh-stir +.act)
        $clr  ..sh-sole :: (sh-pact ~) :: XX clear to PM-to-self?
        $ret  sh-done
      ==
    ::
    ++  sh-uniq
      ::x  generates a new serial.
      ::
      ^-  {serial _.}
      [(shaf %serial eny.hid) .(eny.hid (shax eny.hid))]
    --
  --
::
++  sn                                                  ::  station render core
  ::x  used in both station and ship rendering.
  ::
  ::x  man: mailbox.
  ::x  one: the station.
  |_  {man/knot one/station}
  ++  sn-best                                           ::  best to show
    ::x  returns true if one is better to show, false otherwise.
    ::x  prioritizes: our > main > size.
    ::TODO  maybe simplify. (lth (xeb (xeb p.one)) (xeb (xeb p.two)))
    ::
    |=  two/station
    ^-  ?
    ::x  the station that's ours is better.
    ?:  =(our.hid p.one)
      ?:  =(our.hid p.two)
        ?<  =(q.one q.two)
        ::x  if both stations are ours, the main story is better.
        ?:  =((main p.one) q.one)  %&
        ?:  =((main p.two) q.two)  %|
        ::x  if neither are, pick the "larger" one.
        (lth q.one q.two)
      %&
    ::x  if one isn't ours but two is, two is better.
    ?:  =(our.hid p.two)
      %|
    ?:  =(p.one p.two)
      ::x  if they're from the same ship, pick the "larger" one.
      (lth q.one q.two)
    ::x  when in doubt, pick one if its ship is "smaller" than its channel.
    ::x?  i guess you want this to be consistent across (a b) and (b a), but
    ::x  this still seems pretty arbitrary.
    (lth p.one q.one)
  ::
  ++  sn-curt                                           ::  render name in 14
    ::x  prints a ship name in 14 characters. left-pads with spaces.
    ::x  mup signifies "are there other targets besides this one"
    ::
    |=  mup/?
    ^-  tape
    =+  raw=(cite p.one)
    (runt [(sub 14 (lent raw)) ' '] raw)
  ::
  ++  sn-nick
    ::x  get nick for ship, or shortname if no nick. left-pads with spaces.
    ::
    |.  ^-  tape
    =+  nym=(~(get by folks) p.one)
    ?~  nym
      (sn-curt |)
    ?~  hand.u.nym
      (sn-curt |)
    =+  raw=(trip u.hand.u.nym)
    =+  len=(sub 14 (lent raw))
    (weld (reap len ' ') raw)
  ::
  ++  sn-phat                                           ::  render accurately
    ::x  prints a station fully, but still taking "shortcuts" where possible:
    ::x  ":" for local mailbox, "~ship" for foreign mailbox,
    ::x  "%channel" for local station, "/channel" for parent station.
    ::
    ^-  tape
    ?:  =(p.one our.hid)
      ?:  =(q.one man)
        ":"
      ['%' (trip q.one)]
    ?:  =(p.one (sein our.hid))
      ['/' (trip q.one)]
    =+  wun=(scow %p p.one)
    ?:  =(q.one (main p.one))
      wun
    :(welp wun "/" (trip q.one))
  --
::
++  ta                                                  ::  partner core
  ::x  used primarily for printing partners.
  ::
  ::x  man: mailbox.
  ::x  one: the partner.
  |_  {man/knot one/partner}
  ++  ta-beat                                           ::  more relevant
    ::x  returns true if one is better to show, false otherwise.
    ::x  prefers stations over passports. if both are stations, sn-best. if both
    ::x  are passports, pick the "larger" one, if they're equal, content hash.
    ::
    |=  two/partner  ^-  ?
    ?-    -.one
        $&
      ?-  -.two
        $|  %&
        $&  (~(sn-best sn man p.one) p.two)
      ==
    ::
        $|
      ?-  -.two
        $&  %|
        $|  ?:  =(-.p.two -.p.one)
              (lth (mug +.p.one) (mug +.p.two))
            (lth -.p.two -.p.one)
      ==
    ==
  ++  ta-best                                           ::  most relevant
    ::x  picks the most relevant partner.
    ::
    |=(two/partner ?:((ta-beat two) two one))
  ::
  ++  ta-sigh                                            ::  assemble label
    ::x  prepend pre to yiz, omitting characters of yiz to stay within len.
    ::
    |=  {len/@ud pre/tape yiz/cord}
    ^-  tape
    =+  nez=(trip yiz)
    =+  lez=(lent nez)
    ?>  (gth len (lent pre))
    =.  len  (sub len (lent pre))
    ?.  (gth lez len)
      =.  nez  (welp pre nez)
      ?.  (lth lez len)  nez
      (runt [(sub len lez) '-'] nez)
    :(welp pre (scag (dec len) nez) "+")
  ::
  ++  ta-full  (ta-show ~)                              ::  render full width
  ++  ta-show                                           ::  render partner
    ::x  renders a partner as text.
    ::
    |=  moy/(unit ?)
    ^-  tape
    ?-    -.one
    ::x render station as glyph if we can.
        $&
      ?~  moy
        =+  cha=(~(get by nik) one ~ ~)
        =-  ?~(cha - "'{u.cha ~}' {-}")
        ~(sn-phat sn man p.one)
      (~(sn-curt sn man p.one) u.moy)
    ::
    ::x  render passport.
        $|
      =+  ^=  pre  ^-  tape
          ?-  -.p.one
            $twitter  "@t:"
          ==
      ?~  moy
        (weld pre (trip p.p.one))
      =.  pre  ?.(=(& u.moy) pre ['*' pre])
      (ta-sigh 14 pre p.p.one)
    ==
  --
::
++  te                                                  ::  audience renderer
  ::x  used for representing audiences (sets of partners) as tapes.
  ::
  ::  man: mailbox.
  ::  lix: members of the audience.
  |_  {man/knot lix/(set partner)}
  ++  te-best  ^-  (unit partner)
    ::x  pick the most relevant partner.
    ::
    ?~  lix  ~
    :-  ~
    |-  ^-  partner
    =+  lef=`(unit partner)`te-best(lix l.lix)
    =+  rit=`(unit partner)`te-best(lix r.lix)
    =.  n.lix  ?~(lef n.lix (~(ta-best ta man n.lix) u.lef))
    =.  n.lix  ?~(rit n.lix (~(ta-best ta man n.lix) u.rit))
    n.lix
  ::
  ++  te-deaf  ^+  .                                    ::  except for self
    ::x  remove ourselves from the audience.
    ::
    .(lix (~(del in lix) `partner`[%& our.hid man]))
  ::
  ++  te-maud  ^-  ?                                    ::  multiple audience
    ::x  checks if there's multiple partners in the audience via pattern match.
    ::
    =.  .  te-deaf
    !?=($@($~ {* $~ $~}) lix)
  ::
  ++  te-prom  ^-  tape                                 ::  render targets
    ::x  render all partners, ordered by relevance.
    ::
    =.  .  te-deaf
    =+  ^=  all
        %+  sort  `(list partner)`(~(tap in lix))
        |=  {a/partner b/partner}
        (~(ta-beat ta man a) b)
    =+  fir=&
    |-  ^-  tape
    ?~  all  ~
    ;:  welp
      ?:(fir "" " ")
      (~(ta-show ta man i.all) ~)
      $(all t.all, fir |)
    ==
  ::
  ++  te-whom                                           ::  render sender
    ::x  render sender as the most relevant partner.
    ::
    (~(ta-show ta man (need te-best)) ~ te-maud)
  ::
  ++  ta-dire                                           ::  direct message
    ::x  returns true if partner is a mailbox of ours.
    ::
    |=  pan/partner  ^-  ?
    ?&  ?=($& -.pan)
        =(p.p.pan our.hid)
    ::
        =+  sot=(~(get by tales) q.p.pan)
        &(?=(^ sot) ?=($brown p.cordon.shape.u.sot))
    ==
  ::
  ++  te-pref                                           ::  audience glyph
    ::x  get the glyph that corresponds to the audience, with a space appended.
    ::x  if it's a dm to us, use :. if it's a dm by us, use ;. complex, use *.
    ::
    ^-  tape
    =+  cha=(~(get by nik) lix)
    ?^  cha  ~[u.cha ' ']
    ?.  (lien (~(tap by lix)) ta-dire)
      "* "
    ?:  ?=({{$& ^} $~ $~} lix)
      ": "
    "; "
  --
::
++  tr                                                  ::  telegram renderer
  ::x  responsible for converting telegrams and everything relating to them to
  ::x  text to be displayed in the cli.
  ::
  |_  $:  ::x  man: story.
          ::x  sef: settings flags.
          ::x  telegram:
          ::x   who: author.
          ::x   thought:
          ::x    sen: unique identifier.
          ::x    aud: audience.
          ::x    statement:
          ::x     wen: timestamp.
          ::x     bou: complete aroma.
          ::x     sep: message contents.
          ::
          man/knot
          sef/(set knot)
          who/ship
          sen/serial
          aud/audience
          wen/@da
          bou/bouquet
          sep/speech
      ==
  ++  tr-fact  ^-  sole-effect                          ::  activate effect
    ::x  produce sole-effect for printing message details.
    ::
    ~[%mor [%tan tr-meta] tr-body]
  ::
  ++  tr-line  ^-  tape                                 ::  one-line print
    ::x  crams a telegram into a single line by displaying a short ship name,
    ::x  a short representation of the gram, and an optional timestamp.
    ::
    =+  txt=(tr-text =(who our.hid))
    ?:  =(~ txt)  ""
    =+  ^=  baw
        ::  ?:  oug
        ::  ~(te-whom te man tr-pals)
        ?.  (~(has in sef) %noob)
          (~(sn-curt sn man [who (main who)]) |)
        (~(sn-nick sn man [who (main who)]))
    ?:  (~(has in sef) %showtime)
      =+  dat=(yore now.hid)
      =+  ^=  t
        |=  a/@  ^-  tape
        %+  weld
          ?:  (lth a 10)  "0"  ~
          (scow %ud a)
      =+  ^=  time  :(weld "~" (t h.t.dat) "." (t m.t.dat) "." (t s.t.dat))
      :(weld baw txt (reap (sub 67 (lent txt)) ' ') time)
    (weld baw txt)
  ::
  ++  tr-meta  ^-  tang
    ::x  build strings that display metadata, including message serial,
    ::x  timestamp, author and audience.
    ::
    =.  wen  (sub wen (mod wen (div wen ~s0..0001)))     :: round
    =+  hed=leaf+"{(scow %uv sen)} at {(scow %da wen)}"
    =+  =<  paz=(turn (~(tap by aud)) .)
        |=({a/partner *} leaf+~(ta-full ta man a))
    =+  bok=(turn (sort (~(tap in bou)) aor) smyt)
    [%rose [" " ~ ~] [hed >who< [%rose [", " "to " ~] paz] bok]]~
  ::
  ++  tr-body
    ::x  long-form display of message contents, specific to each speech type.
    ::
    |-  ^-  sole-effect
    ?+  -.sep  tan+[>sep<]~
      $exp  tan+~[leaf+"# {(trip p.sep)}"]
      $lin  tan+~[leaf+"{?:(p.sep "" "@ ")}{(trip q.sep)}"]
      $non  tan+~
      $app  tan+~[rose+[": " ~ ~]^~[leaf+"[{(trip p.sep)}]" leaf+(trip q.sep)]]
      $url  url+(crip (earf p.sep))
      $mor  mor+(turn p.sep |=(speech ^$(sep +<)))
      $fat  [%mor $(sep q.sep) tan+(tr-rend-tors p.sep) ~]
      $api
        :-  %tan
        :_  ~
        :+  %rose
          [": " ~ ~]
        :~  leaf+"[{(trip id.sep)} on {(trip service.sep)}]"
            leaf+(trip body.sep)
            leaf+(earf url.sep)
        ==
    ==
  ::
  ++  tr-rend-tors
    ::x  render an attachment.
    ::
    |=  a/torso  ^-  tang
    ?-  -.a
      $name  (welp $(a q.a) leaf+"={(trip p.a)}" ~)
      $tank  +.a
      $text  (turn (flop +.a) |=(b/cord leaf+(trip b)))
    ==
  ::
  ++  tr-pals
    ::x  strip delivery info from audience, producing a set of partners.
    ::
    ^-  (set partner)
    %-  ~(gas in *(set partner))
    (turn (~(tap by aud)) |=({a/partner *} a))
  ::
  ++  tr-chow
    ::x  truncate the txt to be of max len characters. if it does truncate,
    ::x  indicates it did so by appending a character.
    ::
    |=  {len/@u txt/tape}  ^-  tape
    ?:  (gth len (lent txt))  txt
    =.  txt  (scag len txt)
    |-
    ?~  txt  txt
    ?:  =(' ' i.txt)
      |-(['_' ?.(?=({$' ' *} t.txt) t.txt $(txt t.txt))])
    ?~  t.txt  "…"
    [i.txt $(txt t.txt)]
  ::
  ++  tr-both
    ::x  try to fit two tapes into a single line.
    ::
    |=  {a/tape b/tape}  ^-  tape
    ?:  (gth (lent a) 62)  (tr-chow 64 a)
    %+  weld  a
    (tr-chow (sub 64 (lent a)) "  {b}")
  ::
  ++  tr-text
    ::x  gets a tape representation of a message that fits within a single line.
    ::
    |=  oug/?
    ^-  tape
    ?+    -.sep  ~&(tr-lost+sep "")
        $mor
      ?~  p.sep  ~&(%tr-mor-empty "")
      |-  ^-  tape
      ?~  t.p.sep  ^$(sep i.p.sep)
      (tr-both ^$(sep i.p.sep) $(p.sep t.p.sep))
    ::
        $fat
      %+  tr-both  $(sep q.sep)
      ?+  -.p.sep  "..."
        $tank  ~(ram re %rose [" " `~] +.p.sep)
      ==
    ::
        $exp  (tr-chow 66 '#' ' ' (trip p.sep))
        $url  =+  ful=(earf p.sep)
              ?:  (gth 64 (lent ful))  ['/' ' ' ful]
              :+  '/'  '_'
              =+  hok=r.p.p.p.sep
              ~!  hok
              =-  (swag [a=(sub (max 64 (lent -)) 64) b=64] -)
              ^-  tape
              =<  ?:(?=($& -.hok) (reel p.hok .) +:(scow %if p.hok))
              |=({a/knot b/tape} ?~(b (trip a) (welp b '.' (trip a))))
    ::
        $lin
      =+  txt=(trip q.sep)
      ?:  p.sep
        =+  pal=tr-pals
        =.  pal  ?:  =(who our.hid)  pal
                 (~(del in pal) [%& who (main who)])
        (weld ~(te-pref te man pal) txt)
      (weld " " txt)
    ::
        $app
      (tr-chow 64 "[{(trip p.sep)}]: {(trip q.sep)}")
    ::
        $api
      (tr-chow 64 "[{(trip id.sep)}@{(trip service.sep)}]: {(trip summary.sep)}")
    ==
  --
::
++  peer
  ::x  incoming subscription on pax.
  ::
  |=  pax/path
  ~&  [%r-peer pax ost.hid src.hid]
  ^-  (quip move +>)
  ?.  (team src.hid our.hid)
    ~&  [%peer-talk-reader-stranger src.hid]
    [~ +>]
  ?.  ?=({$sole *} pax)
    ~&  [%peer-talk-reader-strange pax]
    [~ +>]
  ~&  [%r-peer-sole ost.hid]
  ra-abet:(ra-console:ra src.hid t.pax)
::
++  diff-talk-lowdown
  ::x  incoming talk-lowdown. process it.
  ::
  |=  {way/wire low/lowdown}
  ra-abet:(ra-diff-talk-lowdown:ra low)
::
++  poke-sole-action                                    ::  accept console
  ::x  incoming sole action. process it.
  ::
  |=  act/sole-action
  ra-abet:(ra-sole:ra act)
::
++  prep
  ::x  state adapter.
  ::
  |=  old/*::(unit chattel)
  ::^-  (quip move ..prep)
  ::?~  old
    ra-abet:ra-init:ra
  ::[~ ..prep(+<+ u.old)]
--
