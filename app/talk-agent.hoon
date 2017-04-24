::                                                      ::  ::
::::  /hoon/talk-agent/app                              ::  ::
  ::                                                    ::  ::
::
::TODO  guardian's todo's apply here too
::TODO  make sure glyphs get unbound when joins etc don't succeed.
::TODO  correct/clean up presence/config change notifications
::
::TODO  maybe keep track of received grams per partner, too?
::
::>  This reader implementation makes use of the mailbox
::>  for all its subscriptions and messaging. All
::>  lowdowns received are exclusively about the mailbox,
::>  since that's the only thing the reader ever
::>  subscribes to.
::
/?    310                                               ::<  hoon version
/-    talk, sole                                        ::<  structures
/+    talk, sole                                        ::<  libraries
/=    seed  /~  !>(.)
!:
::::
  ::
[. talk sole]
=>  ::>  ||
    ::>  ||  %arch
    ::>  ||
    ::>    data structures
    ::
    |%
    ++  chattel                                         ::>  reader state
      $:  ::  messaging state                           ::
          count/@ud                                     ::<  (lent grams)
          grams/(list telegram)                         ::<  all history
          known/(map serial @ud)                        ::<  messages heard
          sources/(set partner)                         ::<  our subscriptions
          ::  partner details                           ::
          remotes/(map partner atlas)                   ::<  remote presences
          mirrors/(map circle config)                   ::<  remote configs
          ::  ui state                                  ::
          folks/(map ship human)                        ::<  human identities
          nik/(map (set partner) char)                  ::<  bound circle glyphs
          nak/(jug char (set partner))                  ::<  circle glyph lookup
          cli/shell                                     ::<  interaction state
      ==                                                ::
    ++  shell                                           ::>  console session
      $:  id/bone                                       ::<  identifier
          count/@ud                                     ::<  messages shown
          say/sole-share                                ::<  console state
          active/(set partner)                          ::<  active targets
          settings/(set knot)                           ::<  frontend settings
      ==                                                ::
    ++  move  (pair bone card)                          ::<  all actions
    ++  lime                                            ::>  diff fruit
      $%  {$talk-report report}                         ::
          {$sole-effect sole-effect}                    ::
      ==                                                ::
    ++  pear                                            ::>  poke fruit
      $%  {$talk-command command}                       ::
          {$talk-action action}                         ::
      ==                                                ::
    ++  card                                            ::>  general card
      $%  {$diff lime}                                  ::
          {$poke wire dock pear}                        ::
          {$peer wire dock path}                        ::
      ==                                                ::
    ++  work                                            ::>  interface action
      $%  ::  circle management                         ::
          {$join p/where}                               ::<  subscribe to
          {$leave p/where}                              ::<  unsubscribe from
          {$create p/posture q/knot r/cord}             ::<  create circle
          {$delete p/knot q/(unit cord)}                ::<  delete circle
          {$depict p/knot q/cord}                       ::<  change description
          {$source p/knot q/(set partner)}              ::<  add source
          {$invite p/knot q/(set ship)}                 ::<  give permission
          {$banish p/knot q/(set ship)}                 ::<  deny permission
          ::  messaging                                 ::
          {$say p/(list speech)}                        ::<  send message
          {$eval p/cord q/twig}                         ::<  send #-message
          {$target p/where q/(unit work)}               ::<  set active targets
          ::  displaying info                           ::
          {$number p/$@(@ud {@u @ud})}                  ::<  relative/absolute
          {$who p/where}                                ::<  presence
          {$what p/$@(char (set partner))}              ::<  show bound glyph
          ::  ui settings                               ::
          {$bind p/char q/(unit where)}                 ::<  bind glyph
          {$unbind p/char q/(unit where)}               ::<  unbind glyph
          {$nick p/(unit ship) q/(unit cord)}           ::<  un/set/show nick
          {$set p/knot}                                 ::<  enable setting
          {$unset p/knot}                               ::<  disable setting
          ::  miscellaneous                             ::
          {$help $~}                                    ::<  print usage info
      ==                                                ::
    ++  where  (set partner)                            ::<  non-empty audience
    ++  glyphs  `wall`~[">=+-" "}),." "\"'`^" "$%&@"]   ::<  circle char pool '
    --
::
::>  ||
::>  ||  %work
::>  ||
::>    functional cores and arms.
::
|_  {bol/bowl chattel}
::
++  prep                                                ::<  prepare state
  ::>  adapts state.
  ::
  |=  old/(unit chattel)
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
++  broker                                              ::<  broker ship + name
  |=  our/ship
  :_  %talk-guardian
  ?.  =((clan our) %earl)
    our
  (sein our)
::
++  inbox                                               ::<  reader's circle
  ::>  produces the name of the circle used by this
  ::>  reader for all its operations
  (main our.bol)
::
::>  ||
::>  ||  %engines
::>  ||
::>    main cores.
::+|
::
++  ta                                                  ::  per transaction
  ::>  for every transaction/event (poke, peer etc.)
  ::>  talk-agent receives, the ++ta transaction core is
  ::>  called.
  ::>  in processing transactions, ++ta may modify app
  ::>  state, or create moves. these moves get produced
  ::>  upon finalizing the core's with with ++ta-done.
  ::>  when making changes to the shell, the ++sh core is
  ::>  used.
  ::
  |_  ::>  moves:  moves created by core operations.
      ::
      moves/(list move)
  ::
  ++  ta-done                                           ::<  resolve core
    ::>  produces the moves stored in ++ta's moves.
    ::>  %sole-effect moves get squashed into a %mor.
    ::
    ^+  [*(list move) +>]
    :_  +>
    ::  seperate our sole-effects from other moves.
    =/  yop
      |-  ^-  (pair (list move) (list sole-effect))
      ?~  moves  [~ ~]
      =+  mor=$(moves t.moves)
      ?:  ?&  =(id.cli p.i.moves)
              ?=({$diff $sole-effect *} q.i.moves)
          ==
        [p.mor [+>.q.i.moves q.mor]]
      [[i.moves p.mor] q.mor]
    ::  flop moves, flop and squash sole-effects into a %mor.
    =+  moz=(flop p.yop)
    =/  foc/(unit sole-effect)
      ?~  q.yop  ~
      ?~  t.q.yop  `i.q.yop                             ::<  single sole-effect
      `[%mor (flop q.yop)]                              ::<  more sole-effects
    ::  produce moves or sole-effects and moves.
    ?~  foc  moz
    ?~  id.cli  ~&(%reader-no-sole moz)
    [[id.cli %diff %sole-effect u.foc] moz]
  ::
  ::>  ||
  ::>  ||  %emitters
  ::>  ||
  ::>    arms that create outward changes.
  ::+|
  ::
  ++  ta-emil                                           ::<  emit move list
    ::>  adds multiple moves to the core's list.
    ::>  flops to emulate ++ta-emit.
    ::
    |=  mol/(list move)
    %_(+> moves (welp (flop mol) moves))
  ::
  ++  ta-emit                                           ::<  emit a move
    ::>  adds a move to the core's list.
    ::
    |=  mov/move
    %_(+> moves [mov moves])
  ::
  ::>  ||
  ::>  ||  %interaction-events
  ::>  ||
  ::>    arms that apply events we received.
  ::+|
  ::
  ++  ta-init                                           ::<  initialize app
    ::>  subscribes to our broker.
    ::
    %-  ta-emit
    :*  ost.bol
        %peer
        /                 ::<  return/diff path
        (broker our.bol)
        /reader/[inbox]   ::<  peer path
    ==
  ::
  ++  ta-reaction                                       ::<  apply reaction
    ::>  processes a talk reaction.
    ::
    |=  rac/reaction
    ^+  +>
    sh-done:(~(sh-reaction sh cli) rac)
  ::
  ++  ta-low                                            ::<  apply lowdown
    ::>  processes a talk lowdown
    ::
    |=  low/lowdown
    ^+  +>
    ?-  -.low
      $glyph  (ta-low-glyph +.low)
      $names  (ta-low-names +.low)
      $confs  (ta-low-confs +.low)
      $precs  (ta-low-precs +.low)
      $grams  (ta-low-grams +.low)
    ==
  ::
  ++  ta-low-glyph                                      ::<  apply changed glyphs
    ::>  applies new set of glyph bindings.
    ::
    |=  nek/_nak
    ^+  +>
    ?:  =(nek nak)  +>                                  ::  no change
    =.  nak  nek
    =.  nik
      %-  ~(gas by *(map (set partner) char))
      =-  (zing -)
      %+  turn  (~(tap by nek))
      |=  {a/char b/(set (set partner))}
      (turn (~(tap by b)) |=(c/(set partner) [c a]))
    sh-done:~(sh-prod sh cli)
  ::
  ++  ta-low-names                                      ::<  apply changed names
    ::>  applies new local identities.
    ::
    |=  nas/(map ship (unit human))
    ^+  +>
    %=  +>
        folks
      %-  ~(gas by *(map ship human))
      %+  murn
        =<  $
        %~  tap  by
            %.  nas
            ~(uni by `_nas`(~(run by folks) some))
        ==
      |=  {s/ship h/(unit human)}
      ?~(h ~ (some [s u.h]))
    ==
  ::
  ++  ta-low-confs                                      ::<  apply changed confs
    ::>  applies new circle configurations.
    ::>  because of how this reader only subscribes to
    ::>  the main mailbox, {coy} is always the mailbox's
    ::>  config.
    ::
    |=  {coy/(unit config) cofs/(map circle (unit config))}
    ^+  +>
    ::>  if possible, update {sources}. if we do, and we
    ::>  gain new ones, update the prompt. (this is to
    ::>  remove the mailbox from the audience after
    ::>  creating or joining a new circle.)
    ?~  coy  ~&(%mailbox-gone !!)
    =.  +>  ::TODO  =?
      ?~  (~(dif in src.u.coy) sources)  +>.$
      =<  sh-done
      %-  ~(sh-pact sh(sources src.u.coy) cli)
      (~(dif in src.u.coy) sources)
    =.  sources  src.u.coy
    =.  cofs  (~(put by cofs) [our.bol inbox] coy)
    ::  print changes for each config.
    =.  +>.$
      =<  sh-done
      %+  roll  (~(tap by cofs))
      |=  {{s/circle c/(unit config)} core/_sh}
      %^  ~(sh-low-config core cli)
      s  (~(get by mirrors) s)  c
    ::  apply config changes to {mirrors}.
    =.  mirrors
      %-  ~(gas by *_mirrors)
      %+  murn  (~(tap by cofs))
      |=  {s/circle c/(unit config)}
      ^-  (unit (pair circle config))
      ?~(c ~ `[s u.c])
    +>.$
  ::
  ++  ta-low-precs                                      ::<  apply changed precs
    ::>  applies new presences.
    ::>  other clients might care for {tas}, but we're
    ::>  only ever getting this for the mailbox, where
    ::>  we're the only ones present.
    ::
    |=  {tas/atlas pas/(map partner atlas)}
    ^+  +>
    =/  ner/_remotes                                    ::  per-partner uni
      %-  ~(urn by pas)
      |=  {p/partner a/atlas}
      =+  o=(~(get by remotes) p)
      ?~(o a (~(uni by u.o) a))
    =.  ner  (~(uni by remotes) ner)                    ::  fill in the gaps
    ?:  =(remotes ner)  +>.$                            ::  no change
    =.  +>.$
      =<  sh-done
      %+  ~(sh-low-rempe sh cli)
      remotes  ner
    +>.$(remotes ner)
  ::
  ++  ta-low-grams                                      ::<  apply messages
    ::>  applies new or changed telegrams.
    ::
    |=  {num/@ud gams/(list telegram)}
    ^+  +>
    =.  +>.$  (ta-lesson gams)
    =<  sh-done
    (~(sh-low-grams sh cli) num gams)
  ::
  ::>  ||
  ::>  ||  %messages
  ::>  ||
  ::>    storing and updating messages.
  ::+|
  ::
  ++  ta-lesson                                         ::<  learn messages
    ::>  learn all telegrams in a list.
    ::
    |=  gaz/(list telegram)
    ^+  +>
    ?~  gaz  +>
    $(gaz t.gaz, +> (ta-learn i.gaz))
  ::
  ++  ta-learn                                          ::<  save/update message
    ::>  store an incoming telegram, updating if it
    ::>  already exists.
    ::
    |=  gam/telegram
    ^+  +>
    =+  old=(~(get by known) uid.tot.gam)
    ?~  old
      (ta-append gam)      ::<  add
    (ta-revise u.old gam)  ::<  modify
  ::
  ++  ta-append                                         ::<  append message
    ::>  store a new telegram.
    ::
    |=  gam/telegram
    ^+  +>
    %=  +>
      grams  [gam grams]
      count  +(count)
      known  (~(put by known) uid.tot.gam count)
    ==
  ::
  ++  ta-revise                                         ::<  revise message
    ::>  modify a telegram we know.
    ::
    |=  {num/@ud gam/telegram}
    =+  way=(sub count num)
    ?:  =(gam (snag (dec way) grams))
      +>.$                                            ::  no change
    =.  grams  (welp (scag (dec way) grams) [gam (slag way grams)])
    +>.$
  ::
  ::>  ||
  ::>  ||  %console
  ::>  ||
  ::>    arms for shell functionality.
  ::+|
  ::
  ++  ta-console                                        ::<  initialize shell
    ::>  initialize the shell of this reader.
    ::
    ^+  .
    =/  she/shell
      %*(. *shell id ost.bol, active (sy [%& our.bol inbox] ~))
    sh-done:~(sh-prod sh she)
  ::
  ++  ta-sole                                           ::<  apply sole input
    ::>  applies sole-action.
    ::
    |=  act/sole-action
    ^+  +>
    ?.  =(id.cli ost.bol)
      ~&(%strange-sole !!)
    sh-done:(~(sh-sole sh cli) act)
  ::
  ++  sh                                                ::<  per console
    ::>  shell core, responsible for handling user input
    ::>  and the related actions, and outputting changes
    ::>  to the cli.
    ::
    |_  $:  ::>  she: console state.
            ::>  man: our mailbox
            ::
            she/shell
        ==
    ::
    ++  sh-done                                         ::<  resolve core
      ::>  stores changes to the cli.
      ::
      ^+  +>
      +>(cli she)
    ::
    ::>  ||
    ::>  ||  %emitters
    ::>  ||
    ::>    arms that create outward changes.
    ::+|
    ::
    ++  sh-fact                                         ::<  send console effect
      ::>  adds a console effect to ++ta's moves.
      ::
      |=  fec/sole-effect
      ^+  +>
      +>(moves [[id.she %diff %sole-effect fec] moves])
    ::
    ++  sh-act                                          ::<  send action
      ::>  adds an aaction to ++ta's moves.
      ::
      |=  act/action
      ^+  +>
      %=  +>
          moves
        :_  moves
        :*  ost.bol
            %poke
            /reader/action
            (broker our.bol)
            [%talk-action act]
        ==
      ==
    ::
    ::>  ||
    ::>  ||  %cli-interaction
    ::>  ||
    ::>    processing user input as it happens.
    ::+|
    ::
    ++  sh-sole                                         ::<  apply edit
      ::>  applies sole action.
      ::
      |=  act/sole-action
      ^+  +>
      ?-  -.act
        $det  (sh-edit +.act)
        $clr  ..sh-sole :: (sh-pact ~) :: XX clear to PM-to-self?
        $ret  sh-obey
      ==
    ::
    ++  sh-edit                                         ::<  apply sole edit
      ::>  called when typing into the cli prompt.
      ::>  applies the change and does sanitizing.
      ::
      |=  cal/sole-change
      ^+  +>
      =^  inv  say.she  (~(transceive sole say.she) cal)
      =+  fix=(sh-sane inv buf.say.she)
      ?~  lit.fix
        +>.$
      :: just capital correction
      ?~  err.fix
        (sh-slug fix)
      :: allow interior edits and deletes
      ?.  &(?=($del -.inv) =(+(p.inv) (lent buf.say.she)))
        +>.$
      (sh-slug fix)
    ::
    ++  sh-read                                         ::<  command parser
      ::>  parses the command line buffer. produces work
      ::>  items which can be executed by ++sh-work.
      ::
      =<  work
      ::>  ||  %parsers
      ::>    various parsers for command line input.
      |%
      ++  expr                                          ::<  [cord twig]
        |=  tub/nail  %.  tub
        %+  stag  (crip q.tub)
        wide:(vang & [&1:% &2:% (scot %da now.bol) |3:%])
      ::
      ++  dare                                          ::<  @dr
        %+  sear
          |=  a/coin
          ?.  ?=({$$ $dr @} a)  ~
          (some `@dr`+>.a)
        nuck:so
      ::
      ++  ship  ;~(pfix sig fed:ag)                     ::<  ship
      ++  shiz                                          ::<  ship set
        %+  cook
          |=(a/(list ^ship) (~(gas in *(set ^ship)) a))
        (most ;~(plug com (star ace)) ship)
      ::
      ++  pasp                                          ::<  passport
        ;~  pfix  pat
          ;~  pose
            (stag %twitter ;~(pfix (jest 't') col urs:ab))
          ==
        ==
      ::
      ++  stat                                          ::<  local circle
        ;~(pfix cen sym)
      ::
      ++  stan                                          ::<  circle
        ;~  pose
          (cold [our.bol inbox] col)
          ;~(pfix cen (stag our.bol sym))
          ;~(pfix fas (stag (sein our.bol) sym))
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
      ++  parn                                          ::<  partner
        ;~  pose
          (stag %& stan)
          (stag %| pasp)
        ==
      ::
      ++  partners-flat                                 ::<  collapse mixed list
        |=  a/(list (each partner (set partner)))
        ^-  (set partner)
        ?~  a  ~
        ?-  -.i.a
          $&  (~(put in $(a t.a)) p.i.a)
          $|  (~(uni in $(a t.a)) p.i.a)
        ==
      ::
      ++  para                                          ::<  partners alias
        %+  cook  partners-flat
        %+  most  ;~(plug com (star ace))
        (pick parn (sear sh-glyf glyph))
      ::
      ++  parz                                          ::<  non-empty partners
        %+  cook  ~(gas in *(set partner))
        (most ;~(plug com (star ace)) parn)
      ::
      ++  nump                                          ::<  number reference
        ;~  pose
          ;~(pfix hep dem:ag)
          ;~  plug
            (cook lent (plus (just '0')))
            ;~(pose dem:ag (easy 0))
          ==
          (stag 0 dem:ag)
        ==
      ::
      ++  pore                                          ::<  posture
        ;~  pose
          (cold %black (jest %channel))
          (cold %white (jest %village))
          (cold %green (jest %journal))
          (cold %brown (jest %mailbox))
        ==
      ::
      ++  message                                       ::<  exp, lin or url msg
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
      ++  nick  (cook crip (stun [1 14] low))           ::<  nickname
      ++  text  (cook crip (plus (shim ' ' '~')))       ::<  bullets separating
      ++  glyph  (mask "/\\\{(<!?{(zing glyphs)}")      ::<  circle postfix
      ++  setting                                       ::<  setting flag
        %-  perk  :~
          %noob
          %quiet
          %showtime
        ==
      ++  work                                          ::<  full input
        %+  knee  *^work  |.  ~+
        =-  ;~(pose ;~(pfix sem -) message)
        ;~  pose
          ::
          ::  circle management
          ::
          ;~((glue ace) (perk %join ~) para)
          ::
          ;~((glue ace) (perk %leave ~) para)
          ::
          ;~  (glue ace)  (perk %create ~)
            pore
            stat
            qut
          ==
          ::
          ;~  plug  (perk %delete ~)
            ;~(pfix ;~(plug ace cen) sym)
            ;~  pose
              (cook some ;~(pfix ace qut))
              (easy ~)
            ==
          ==
          ::
          ;~((glue ace) (perk %depict ~) stat qut)
          ::
          ;~((glue ace) (perk %source ~) stat parz)
          ::
          ;~((glue ace) (perk %invite ~) stat shiz)
          ::
          ;~((glue ace) (perk %banish ~) stat shiz)
          ::
          ::  displaying info
          ::
          ;~(plug (perk %who ~) ;~(pose ;~(pfix ace para) (easy ~)))
          ::
          ;~((glue ace) (perk %what ~) ;~(pose parz glyph))
          ::
          ::  ui settings
          ::
          ;~(plug (perk %bind ~) ;~(pfix ace glyph) (punt ;~(pfix ace para)))
          ::
          ;~(plug (perk %unbind ~) ;~(pfix ace glyph) (punt ;~(pfix ace para)))
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
          ::
          ;~(plug (perk %unset ~) ;~(pfix ace setting))
          ::
          ::  miscellaneous
          ::
          ;~(plug (perk %help ~) (easy ~))
          ::
          ::  (parsers below come last because they're easy to match)
          ::
          ::  messaging
          ::
          (stag %target ;~(plug para (punt ;~(pfix ace message))))
          ::
          ::  displaying info
          ::
          (stag %number nump)
          (stag %number (cook lent (star sem)))
        ==
      --
    ::
    ++  sh-sane-chat                                    ::<  sanitize chatter
      ::>  (for chat messages) sanitizes the input buffer
      ::>  and splits it into  multiple lines (by '•').
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
    ++  sh-sane                                         ::<  sanitize input
      ::>  parses cli prompt input using ++sh-read and
      ::>  sanitizes when invalid.
      ::
      |=  {inv/sole-edit buf/(list @c)}
      ^-  {lit/(list sole-edit) err/(unit @u)}
      =+  res=(rose (tufa buf) sh-read)
      ?:  ?=($| -.res)  [[inv]~ `p.res]
      :_  ~
      ?~  p.res  ~
      =+  wok=u.p.res
      |-  ^-  (list sole-edit)
      ?+  -.wok
        ~
        ::
          $target
        ?~(q.wok ~ $(wok u.q.wok))
        ::
          $say
        |-  ::  XX per line
        ?~  p.wok  ~
        ?:  ?=($lin -.i.p.wok)
          (sh-sane-chat buf)
        $(p.wok t.p.wok)
      ==
    ::
    ++  sh-slug                                         ::<  edit to sanity
      ::>  corrects invalid prompt input.
      ::
      |=  {lit/(list sole-edit) err/(unit @u)}
      ^+  +>
      ?~  lit  +>
      =^  lic  say.she
          (~(transmit sole say.she) `sole-edit`?~(t.lit i.lit [%mor lit]))
      (sh-fact [%mor [%det lic] ?~(err ~ [%err u.err]~)])
    ::
    ++  sh-obey                                         ::<  apply result
      ::>  called upon hitting return in the prompt. if
      ::>  input is invalid, ++sh-slug is called.
      ::>  otherwise, the appropriate work is done and
      ::>  the entered command (if any) gets displayed
      ::>  to the user.
      ::
      =+  fix=(sh-sane [%nop ~] buf.say.she)
      ?^  lit.fix
        (sh-slug fix)
      =+  jub=(rust (tufa buf.say.she) sh-read)
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
    ::>  ||
    ::>  ||  %user-action
    ::>  ||
    ::>    processing user actions.
    ::+|
    ::
    ++  sh-work                                         ::<  do work
      ::>  implements worker arms for different talk
      ::>  commands.
      ::>  worker arms must produce updated state.
      ::
      |=  job/work
      ^+  +>
      =<  work
      |%
      ::
      ::>  ||
      ::>  ||  %helpers
      ::>  ||
      ::+|
      ::
      ++  work                                          ::<  call correct worker
        ?-  -.job
          ::  circle management
          $join    (join +.job)
          $leave   (leave +.job)
          $create  (create +.job)
          $delete  (delete +.job)
          $depict  (depict +.job)
          $source  (source +.job)
          $invite  (invite +.job)
          $banish  (banish +.job)
          ::  messaging
          $say     (say +.job)
          $eval    (eval +.job)
          $target  (target +.job)
          ::  displaying info
          $number  (number +.job)
          $who     (who +.job)
          $what    (what +.job)
          ::  ui settings
          $bind    (bind +.job)
          $unbind  (unbind +.job)
          $nick    (nick +.job)
          $set     (wo-set +.job)
          $unset   (unset +.job)
          ::  miscelaneous
          $help    help
        ==
      ::
      ++  activate                                      ::<  from %number
        ::>  prints message details.
        ::
        |=  gam/telegram
        ^+  ..sh-work
        =+  tay=~(. tr settings.she gam)
        =.  ..sh-work  (sh-fact tr-fact:tay)
        sh-prod(active.she tr-pals:tay)
      ::
      ++  deli                                          ::<  find number
        ::>  gets absolute message number from relative.
        ::
        |=  {max/@ud nul/@u fin/@ud}
        ^-  @ud
        =+  dog=|-(?:(=(0 fin) 1 (mul 10 $(fin (div fin 10)))))
        =.  dog  (mul dog (pow 10 nul))
        =-  ?:((lte - max) - (sub - dog))
        (add fin (sub max (mod max dog)))
      ::
      ++  glyph                                         ::<  grab a glyph
        ::>  finds a new glyph for assignment.
        ::
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
      ++  set-glyph                                     ::<  new glyph binding
        ::>  applies glyph binding to our state and sends
        ::>  an action.
        ::
        |=  {cha/char pas/(set partner)}
        =:  nik  (~(put by nik) pas cha)
            nak  (~(put ju nak) cha pas)
        ==
        (sh-act %glyph cha pas &)
      ::
      ++  unset-glyph                                   ::<  old glyph binding
        ::>  removes either {pas} or all bindings on a
        ::>  glyph and sends an action.
        ::
        |=  {cha/char pas/(unit (set partner))}
        =/  ole/(set (set partner))
          ?^  pas  [u.pas ~ ~]
          (~(get ju nak) cha)
        =.  ..sh-work  (sh-act %glyph cha (fall pas ~) |)
        |-  ^+  ..sh-work
        ?~  ole  ..sh-work
        =.  ..sh-work  $(ole l.ole)
        =.  ..sh-work  $(ole r.ole)
        %=  ..sh-work
          nik  (~(del by nik) n.ole)
          nak  (~(del ju nak) cha n.ole)
        ==
      ::
      ++  reverse-folks                                 ::<  find by handle
        ::>  finds all ships whose handle matches {nym}.
        ::
        |=  nym/knot
        ^-  (list ship)
        %+  murn  (~(tap by folks))
        |=  {p/ship q/human}
        ?~  han.q  ~
        ?.  =(u.han.q nym)  ~
        [~ u=p]
      ::
      ++  twig-head                                       ::<  eval data
        ::>  makes a vase of environment data to evaluate
        ::>  against (for #-messages).
        ::
        ^-  vase
        !>  ^-  {our/@p now/@da eny/@uvI}
        [our.bol now.bol (shas %eny eny.bol)]
      ::
      ::>  ||
      ::>  ||  %circle-management
      ::>  ||
      ::+|
      ::
      ++  join                                          ::<  %join
        ::>  change local mailbox config to include
        ::>  subscriptions to {pas}.
        ::TODO  only bind glyph *after* we've
        ::      successfully joined.
        ::
        |=  pas/(set partner)
        ^+  ..sh-work
        =.  ..sh-work
          =+  (~(get by nik) pas)
          ?^  -  (sh-note "has glyph {<u>}")
          =+  cha=(glyph (mug pas))
          (sh-note:(set-glyph cha pas) "new glyph {<cha>}")
        =.  ..sh-work
          sh-prod(active.she pas)
        (sh-act %source inbox & pas)
      ::
      ++  leave                                         ::<  %leave
        ::>  change local mailbox config to exclude
        ::>  subscriptions to {pas}.
        ::
        |=  pas/(set partner)
        ^+  ..sh-work
        (sh-act %source inbox | pas)
      ::
      ++  create                                        ::<  %create
        ::>  creates circle {nom} with specified config.
        ::
        |=  {por/posture nom/knot txt/cord}
        ^+  ..sh-work
        ::TODO  simplify?
        ?:  (~(has in mirrors) [our.bol nom])
          (sh-lame "{(trip nom)}: already exists")
        =.  ..sh-work
          (sh-act %create nom txt por)
        (join [[%& our.bol nom] ~ ~])
      ::
      ++  delete                                        ::<  %delete
        ::>  deletes our circle {nom}, after optionally
        ::>  sending a last announce message {say}.
        ::
        |=  {nom/knot say/(unit cord)}
        ^+  ..sh-work
        (sh-act %delete nom say)
      ::
      ++  depict                                        ::<  %depict
        ::>  changes the description of {nom} to {txt}.
        ::
        |=  {nom/knot txt/cord}
        ^+  ..sh-work
        (sh-act %depict nom txt)
      ::
      ++  source                                        ::<  %source
        ::>  adds {pas} to {nom}'s src.
        ::
        |=  {nom/knot pas/(set partner)}
        ^+  ..sh-work
        (sh-act %source nom & pas)
      ::
      ++  invite                                        ::<  %invite
        ::>  invites {sis} to our circle {nom}.
        ::
        |=  {nom/knot sis/(set ship)}
        ^+  ..sh-work
        (sh-act %permit nom & sis)
      ::
      ++  banish                                        ::<  %banish
        ::>  banish {sis} from our circle {nom}.
        ::
        |=  {nom/knot sis/(set ship)}
        ^+  ..sh-work
        (sh-act %permit nom | sis)
      ::
      ::>  ||
      ::>  ||  %messaging
      ::>  ||
      ::+|
      ::
      ++  say                                           ::<  publish
        ::>  sends message.
        ::
        |=  sep/(list speech)
        ^+  ..sh-work
        (sh-act %phrase active.she sep)
      ::
      ++  eval                                          ::<  run
        ::>  executes {exe} and sends both its code and
        ::>  result.
        ::
        |=  {txt/cord exe/twig}
        =>  |.([(sell (slap (slop twig-head seed) exe))]~)
        =+  tan=p:(mule .)
        (say [%fat tank+tan exp+txt] ~)
      ::
      ++  target                                        ::<  %target
        ::>  sets messaging target, then execute {woe}.
        ::
        |=  {pan/(set partner) woe/(unit ^work)}
        ^+  ..sh-work
        =.  ..sh-pact  (sh-pact pan)
        ?~(woe ..sh-work work(job u.woe))
      ::
      ::>  ||
      ::>  ||  %displaying-info
      ::>  ||
      ::+|
      ::
      ++  who                                          ::<  %who
        ::>  prints presence lists for {pas} or all.
        ::
        |=  pas/(set partner)  ^+  ..sh-work
        ::TODO  clever use of =< and . take note!
        =<  (sh-fact %mor (murn (sort (~(tap by remotes) ~) aor) .))
        |=  {pon/partner alt/atlas}  ^-  (unit sole-effect)
        ?.  |(=(~ pas) (~(has in pas) pon))  ~
        =-  `[%tan rose+[", " `~]^- leaf+~(pr-full pr pon) ~]
        =<  (murn (sort (~(tap by alt)) aor) .)
        |=  {a/ship b/presence c/human}  ^-  (unit tank)
        =.  c
          ?.  =(han.c `(scot %p a))  c
          [tru.c ~]
        ?-  b
          $gone  ~
          $idle  `leaf+:(weld "idle " (scow %p a) " " (trip (fall han.c '')))
          $hear  `leaf+:(weld "hear " (scow %p a) " " (trip (fall han.c '')))
          $talk  `leaf+:(weld "talk " (scow %p a) " " (trip (fall han.c '')))
        ==
      ::
      ++  what                                          ::<  %what
        ::>  prints binding details. goes both ways.
        ::TODO  pretty-print
        ::
        |=  qur/$@(char (set partner))
        ^+  ..sh-work
        ?^  qur
          =+  cha=(~(get by nik) qur)
          (sh-fact %txt ?~(cha "none" [u.cha]~))
        =+  pan=(~(tap in (~(get ju nak) qur)))
        ?:  =(~ pan)  (sh-fact %txt "~")
        =<  (sh-fact %mor (turn pan .))
        |=(a/(set partner) [%txt <a>]) ::  XX ~(ar-whom ar a)
      ::
      ++  number                                        ::<  %number
        ::>  finds selected message, expand it.
        ::
        |=  num/$@(@ud {p/@u q/@ud})
        ^+  ..sh-work
        |-
        ?@  num
          ?:  (gte num count)
            (sh-lame "{(scow %s (new:si | +(num)))}: no such telegram")
          =.  ..sh-fact  (sh-fact %txt "? {(scow %s (new:si | +(num)))}")
          (activate (snag num grams))
        ?.  (gth q.num count)
          ?:  =(count 0)
            (sh-lame "0: no messages")
          =+  msg=(deli (dec count) num)
          =.  ..sh-fact  (sh-fact %txt "? {(scow %ud msg)}")
          (activate (snag (sub count +(msg)) grams))
        (sh-lame "…{(reap p.num '0')}{(scow %ud q.num)}: no such telegram")
      ::
      ::>  ||
      ::>  ||  %ui-settings
      ::>  ||
      ::+|
      ::
      ++  bind                                          ::<  %bind
        ::>  binds targets {pas} to the glyph {cha}.
        ::
        |=  {cha/char pas/(unit (set partner))}
        ^+  ..sh-work
        ?~  pas  $(pas `active.she)
        =+  ole=(~(get by nik) u.pas)
        ?:  =(ole [~ cha])  ..sh-work
        %.  "bound {<cha>} {<u.pas>}"
        sh-note:sh-prod:(set-glyph cha u.pas)
      ::
      ++  unbind                                        ::<  %unbind
        ::>  unbinds targets {pas} to glyph {cha}.
        ::
        |=  {cha/char pan/(unit (set partner))}
        ^+  ..sh-work
        ?.  ?|  &(?=(^ pan) (~(has by nik) u.pan))
                &(?=($~ pan) (~(has by nak) cha))
            ==
          ..sh-work
        %.  "unbound {<cha>}"
        sh-note:sh-prod:(unset-glyph cha pan)
      ::
      ++  nick                                          ::<  %nick
        ::>  either shows, sets or unsets nicknames
        ::>  depending on arguments.
        ::
        |=  {her/(unit ship) nym/(unit cord)}
        ^+  ..sh-work
        ::>  no arguments, show all
        ?:  ?=({$~ $~} +<)
          %+  sh-fact  %mor
          %+  turn  (~(tap by folks))
          |=  {p/ship q/human}
          :-  %txt
          ?~  han.q
            "{<p>}:"
          "{<p>}: {<u.han.q>}"
        ::>  show her nick
        ?~  nym
          ?>  ?=(^ her)
          =+  asc=(~(get by folks) u.her)
          %+  sh-fact  %txt
          ?~  asc  "{<u.her>} unbound"
          ?~  han.u.asc  "{<u.her>}:"
          "{<u.her>}: {<u.han.u.asc>}"
        ::>  show nick ship
        ?~  her
          %+  sh-fact  %mor
          %+  turn  (reverse-folks u.nym)
          |=  p/ship
          [%txt "{<p>}: {<u.nym>}"]
        %.  [%human u.her [true=~ hand=nym]]
        %=  sh-act
            folks
          ?~  u.nym
            ::>  unset nickname
            (~(del by folks) u.her)
          ::>  set nickname
          (~(put by folks) u.her [true=~ hand=nym])
        ==
      ::
      ++  wo-set                                        ::<  %set
        ::>  enables ui setting flag.
        ::
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
      ++  unset                                         ::<  %unset
        ::>  disables ui setting flag.
        ::
        |=  neg/knot
        ^+  ..sh-work
        %=  ..sh-work
          settings.she  (~(del in settings.she) neg)
        ==
      ::
      ::>  ||
      ::>  ||  %miscellaneous
      ::>  ||
      ::+|
      ::
      ++  help                                          ::<  %help
        ::>  prints help message
        ::
        (sh-fact %txt "see http://urbit.org/docs/using/messaging/")
      --
    ::
    ++  sh-pact                                         ::<  update active aud
      ::>  change currently selected audience to {lix}
      ::>  and update the prompt.
      ::
      |=  lix/(set partner)
      ^+  +>
      ::>  ensure we can see what we send.
      =+  act=(sh-pare lix)
      ?:  =(active.she act)  +>.$
      sh-prod(active.she act)
    ::
    ++  sh-pare                                         ::<  adjust target list
      ::>  if the audience {paz} does not contain a
      ::>  partner we're subscribed to, add our mailbox
      ::>  to the audience (so that we can see our own
      ::>  message).
      ::
      |=  paz/(set partner)
      ?:  (sh-pear paz)  paz
      (~(put in paz) [%& our.bol inbox])
    ::
    ++  sh-pear                                         ::<  hearback
      ::>  produces true if any partner is included in
      ::>  our subscriptions, meaning, we hear messages
      ::>  sent to {paz}.
      ::
      |=  paz/(set partner)
      ?~  paz  |
      ?|  (~(has in sources) `partner`n.paz)
          $(paz l.paz)
          $(paz r.paz)
      ==
    ::
    ++  sh-pest                                         ::<  report listen
      ::>  updates audience to be {tay}, only if {tay} is
      ::>  not a village/%white.
      ::TODO  why exclude village (invite-only?) audiences from this?
      ::TODO  only used in config change printing, maybe delete.
      ::
      |=  tay/partner
      ^+  +>
      ::>  if partner is a passport, ignore.
      ?.  ?=($& -.tay)  +>
      =+  cof=(~(get by mirrors) +.tay)
      ?.  |(?=($~ cof) !?=($white sec.con.u.cof))
        +>.$
      (sh-pact [tay ~ ~])
    ::
    ++  sh-glyf                                         ::<  decode glyph
      ::>  finds the partner(s) that match a glyph.
      ::TODO  should maybe return full set, not latest,
      ::      if ambiguous.
      ::
      |=  cha/char  ^-  (unit (set partner))
      =+  lax=(~(get ju nak) cha)
      ::>  no partner.
      ?:  =(~ lax)  ~
      ::>  single partner.
      ?:  ?=({* $~ $~} lax)  `n.lax
      ::>  in case of multiple partners, pick the most recently active one.
      |-  ^-  (unit (set partner))
      ?~  grams  ~
      ::>  get first partner from a telegram's audience.
      =+  pan=(silt (turn (~(tap by aud.tot.i.grams)) head))
      ?:  (~(has in lax) pan)  `pan
      $(grams t.grams)
    ::
    ::>  ||
    ::>  ||  %differs
    ::>  ||
    ::>    arms that calculate differences between datasets.
    ::+|
    ::
    ++  sh-atlas-diff                                   ::<  atlas diff parts
      ::>  calculates the difference between two presence
      ::>  lists, producing lists of removed, added and
      ::>  changed presences.
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
        ?:  =(%gone pec.q.i.eno)  ret
        =+  unt=(~(get by two) p.i.eno)
        ?~  unt
          ret(old [i.eno old.ret])
        ?:  =(%gone pec.u.unt)
          ret(old [i.eno old.ret])
        ?:  =(q.i.eno u.unt)  ret
        ret(cha [[p.i.eno u.unt] cha.ret])
      =.  ret
        =+  owt=(~(tap by two))
        |-  ^+  ret
        ?~  owt  ret
        =.  ret  $(owt t.owt)
        ?:  =(%gone pec.q.i.owt)  ret
        ?.  (~(has by one) p.i.owt)
          ret(new [i.owt new.ret])
        ?:  =(%gone pec:(~(got by one) p.i.owt))
          ret(new [i.owt new.ret])
        ret
      ret
    ::
    ++  sh-rempe-diff                                   ::<  remotes diff
      ::>  calculates the difference between two remote
      ::>  presence maps, producing a list of removed,
      ::>  added and changed presences maps.
      ::
      |=  {one/(map partner atlas) two/(map partner atlas)}
      =|  $=  ret
          $:  old/(list (pair partner atlas))
              new/(list (pair partner atlas))
              cha/(list (pair partner atlas))
          ==
      ^+  ret
      =.  ret
        =+  eno=(~(tap by one))
        |-  ^+  ret
        ?~  eno  ret
        =.  ret  $(eno t.eno)
        =+  unt=(~(get by two) p.i.eno)
        ?~  unt
          ret(old [i.eno old.ret])
        ?:  =(q.i.eno u.unt)  ret
        ret(cha [[p.i.eno u.unt] cha.ret])
      =.  ret
        =+  owt=(~(tap by two))
        |-  ^+  ret
        ?~  owt  ret
        =.  ret  $(owt t.owt)
        ?:  (~(has by one) p.i.owt)
          ret
        ret(new [i.owt new.ret])
      ret
    ::
    ++  sh-remco-diff                                   ::<  config diff parts
      ::>  calculates the difference between two config
      ::>  maps, producing lists of removed, added and
      ::>  changed configs.
      ::
      |=  {one/(map circle config) two/(map circle config)}
      =|  $=  ret
          $:  old/(list (pair circle config))
              new/(list (pair circle config))
              cha/(list (pair circle config))
          ==
      ^+  ret
      =.  ret
        =+  eno=(~(tap by one))
        |-  ^+  ret
        ?~  eno  ret
        =.  ret  $(eno t.eno)
        =+  unt=(~(get by two) p.i.eno)
        ?~  unt
          ret(old [i.eno old.ret])
        ?:  =(q.i.eno u.unt)  ret
        ret(cha [[p.i.eno u.unt] cha.ret])
      =.  ret
        =+  owt=(~(tap by two))
        |-  ^+  ret
        ?~  owt  ret
        =.  ret  $(owt t.owt)
        ?:  (~(has by one) p.i.owt)
          ret
        ret(new [i.owt new.ret])
      ret
    ::
    ++  sh-set-diff                                     ::<  set diff
      ::>  calculates the difference between two sets,
      ::>  procuding lists of removed and added items.
      ::
      |*  {one/(set *) two/(set *)}
      :-  ^=  old  (~(tap in (~(dif in one) two)))
          ^=  new  (~(tap in (~(dif in two) one)))
    ::
    ::>  ||
    ::>  ||  %printers
    ::>  ||
    ::>    arms for printing data to the cli.
    ::+|
    ::
    ++  sh-reaction                                     ::<  apply reaction
      ::>  renders a reaction to the cli.
      ::
      |=  rac/reaction
      (sh-lame (trip wat.rac))
    ::
    ++  sh-lame                                         ::<  send error
      ::>  just puts some text into the cli as-is.
      ::
      |=  txt/tape
      (sh-fact [%txt txt])
    ::
    ++  sh-note                                         ::<  shell message
      ::>  left-pads {txt} with heps and prints it.
      ::
      |=  txt/tape
      ^+  +>
      (sh-fact %txt (runt [14 '-'] `tape`['|' ' ' (scag 64 txt)]))
    ::
    ++  sh-prod                                         ::<  show prompt
      ::>  makes and stores a move to modify the cli
      ::>  prompt to display the current audience.
      ::
      ^+  .
      %+  sh-fact  %pro
      :+  &  %talk-line
      ^-  tape
      =/  rew/(pair (pair @t @t) (set partner))
          [['[' ']'] active.she]
      =+  cha=(~(get by nik) q.rew)
      ?^  cha  ~[u.cha ' ']
      =+  por=~(ar-prom ar q.rew)
      (weld `tape`[p.p.rew por] `tape`[q.p.rew ' ' ~])
    ::
    ++  sh-rend                                         ::<  print on one line
      ::>  renders a telegram as a single line, adds it
      ::>  as a console move.
      ::
      |=  gam/telegram
      =+  lin=~(tr-line tr settings.she gam)
      (sh-fact %txt lin)
    ::
    ++  sh-numb                                         ::<  print msg number
      ::>  prints a message number, left-padded by heps.
      ::
      |=  num/@ud
      ^+  +>
      =+  bun=(scow %ud num)
      %+  sh-fact  %txt
      (runt [(sub 13 (lent bun)) '-'] "[{bun}]")
    ::
    ++  sh-puss                                         ::<  readable posture
      ::>  renders a security posture.
      ::
      |=  a/posture  ^-  tape
      ?-  a
        $black  "channel"
        $brown  "mailbox"
        $white  "village"
        $green  "journal"
      ==
    ::
    ++  sh-spaz                                         ::<  render status
      ::>  gets the presence of {saz} as a tape.
      ::
      |=  sat/status
      ^-  tape
      ['%' (trip pec.sat)]
    ::
    ++  sh-show-precs                                   ::<  print atlas diff
      ::>  prints presence changes to the cli.
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
    ++  sh-show-permits                                 ::<  show permits
      ::>  prints invite/banish effects to the cli.
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
    ++  sh-show-sources                                 ::<  show sources
      ::>  prints subscription changes to the cli.
      ::
      |=  {pre/tape old/(list partner) new/(list partner)}
      ^+  +>
      =.  +>.$
          |-  ^+  +>.^$
          ?~  old  +>.^$
          =.  +>.^$  $(old t.old)
          (sh-note (weld pre "off {~(pr-full pr i.old)}"))
      =.  +>.$
          |-  ^+  +>.^$
          ?~  new  +>.^$
          =.  +>.^$  $(new t.new)
          (sh-note (weld pre "hey {~(pr-full pr i.new)}"))
      +>.$
    ::
    ++  sh-show-config                                  ::<  show config
      ::>  prints config changes to the cli.
      ::
      |=  {pre/tape laz/config loc/config}
      ^+  +>
      =.  +>.$
        ?:  =(cap.loc cap.laz)  +>.$
        (sh-note :(weld pre "cap " (trip cap.loc)))
      =.  +>.$
          %+  sh-show-sources
            (weld (trip inbox) ": ")
          (sh-set-diff src.laz src.loc)
      ?:  !=(sec.con.loc sec.con.laz)
        =.  +>.$  (sh-note :(weld pre "but " (sh-puss sec.con.loc)))
        %^    sh-show-permits
            (weld (trip inbox) ": ")
          sec.con.loc
        [~ (~(tap in ses.con.loc))]
      %^    sh-show-permits
          (weld (trip inbox) ": ")
        sec.con.loc
      (sh-set-diff ses.con.laz ses.con.loc)
    ::
    ++  sh-low-config                                   ::<  do show config
      ::>  prints a circle's config changes to the cli.
      ::
      |=  {sat/circle old/(unit config) new/(unit config)}
      ^+  +>
      ?~  old  ~&([%new-conf sat] +>)
      ?~  new  ~&([%del-conf sat] +>)  ::TODO  tmp
      %^  sh-show-config
        (weld ~(cr-phat cr sat) ": ")
      u.old  u.new
    ::
    ++  sh-low-remco                                    ::TODO  delete me
      ::>  prints changes to remote configs to the cli
      ::
      |=  {ole/(map circle config) neu/(map circle config)}
      ^+  +>
      =+  (sh-remco-diff ole neu)
      =.  +>.$
          |-  ^+  +>.^$
          ?~  new  +>.^$
          =.  +>.^$  $(new t.new)
          =.  +>.^$  (sh-pest [%& p.i.new])
          %+  sh-show-config
            (weld ~(cr-phat cr p.i.new) ": ")
          [*config q.i.new]
      =.  +>.$
          |-  ^+  +>.^$
          ?~  cha  +>.^$
          =.  +>.^$  $(cha t.cha)
          %+  sh-show-config
            (weld ~(cr-phat cr p.i.cha) ": ")
          [(~(got by ole) `circle`p.i.cha) q.i.cha]
      +>.$
    ::
    ++  sh-low-rempe                                    ::<  show remotes
      ::>  prints remote presence changes to the cli.
      ::
      |=  {old/(map partner atlas) new/(map partner atlas)}
      =+  day=(sh-rempe-diff old new)
      ?:  (~(has in settings.she) %quiet)
        +>.$
      =.  +>.$
          |-  ^+  +>.^$
          ?~  old.day  +>.^$
          =.  +>.^$  $(old.day t.old.day)
          (sh-note (weld "not " (~(pr-show pr p.i.old.day) ~)))
      =.  +>.$
          |-  ^+  +>.^$
          ?~  new.day  +>.^$
          =.  +>.^$  $(new.day t.new.day)
          =.  +>.^$
              (sh-note (weld "new " (~(pr-show pr p.i.new.day) ~)))
          (sh-show-precs "--" ~ (~(tap by q.i.new.day)) ~)
      =.  +>.$
          |-  ^+  +>.^$
          ?~  cha.day  +>.^$
          =.  +>.^$  $(cha.day t.cha.day)
          =.  +>.^$
              (sh-note (weld "for " (~(pr-show pr p.i.cha.day) ~)))
          =+  yez=(~(got by old) p.i.cha.day)
          %+  sh-show-precs  "--"
          (sh-atlas-diff yez q.i.cha.day)
      +>.$
    ::
    ++  sh-low-precs                                    ::<  show presence
      ::>  prints presence changes to the cli.
      ::
      |=  {old/atlas new/atlas}
      ^+  +>
      =+  dif=(sh-atlas-diff old new)
      (sh-show-precs "" dif)
    ::
    ++  sh-low-gram                                     ::<  show telegram
      ::>  prints the telegram. every fifth message,
      ::>  print the message number also.
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
    ++  sh-low-grams                                    ::<  do show telegrams
      ::>  prints multiple telegrams.
      ::
      |=  {num/@ud gaz/(list telegram)}
      ^+  +>
      ?~  gaz  +>
      $(gaz t.gaz, num +(num), +> (sh-low-gram num i.gaz))
    ::
    --
  --
::
::>  ||
::>  ||  %renderers
::>  ||
::>    rendering cores.
::+|
::
++  cr                                                  ::<  circle renderer
  ::>  used in both circle and ship rendering.
  ::
  |_  ::>  one: the circle.
      ::
      one/circle
  ::
  ++  cr-best                                           ::<  best to show
    ::>  returns true if one is better to show, false
    ::>  otherwise. prioritizes: our > main > size.
    ::TODO  maybe simplify. (lth (xeb (xeb hos.one)) (xeb (xeb hos.two)))
    ::
    |=  two/circle
    ^-  ?
    ::  the circle that's ours is better.
    ?:  =(our.bol hos.one)
      ?:  =(our.bol hos.two)
        ?<  =(nom.one nom.two)
        ::  if both circles are ours, the main story is better.
        ?:  =((main hos.one) nom.one)  %&
        ?:  =((main hos.two) nom.two)  %|
        ::  if neither are, pick the "larger" one.
        (lth nom.one nom.two)
      %&
    ::  if one isn't ours but two is, two is better.
    ?:  =(our.bol hos.two)
      %|
    ?:  =(hos.one hos.two)
      ::  if they're from the same ship, pick the "larger" one.
      (lth nom.one nom.two)
    ::  when in doubt, pick one if its ship is "smaller" than its channel.
    (lth hos.one nom.one)
  ::
  ++  cr-curt                                           ::<  render name in 14
    ::>  prints a ship name in 14 characters. left-pads
    ::>  with spaces. {mup} signifies "are there other
    ::>  targets besides this one?"
    ::
    |=  mup/?
    ^-  tape
    =+  raw=(cite hos.one)
    (runt [(sub 14 (lent raw)) ' '] raw)
  ::
  ++  cr-nick                                           ::<  nick or name in 14
    ::>  get nick for ship, or shortname if no nick.
    ::>  left-pads with spaces.
    ::
    |.  ^-  tape
    =+  nym=(~(get by folks) hos.one)
    ?~  nym
      (cr-curt |)
    ?~  han.u.nym
      (cr-curt |)
    =+  raw=(trip u.han.u.nym)
    =+  len=(sub 14 (lent raw))
    (weld (reap len ' ') raw)
  ::
  ++  cr-phat                                           ::<  render accurately
    ::>  prints a circle fully, but still taking
    ::>  "shortcuts" where possible:
    ::>  ":" for local mailbox, "~ship" for foreign
    ::>  mailbox, "%channel" for local circle,
    ::>  "/channel" for parent circle.
    ::
    ^-  tape
    ?:  =(hos.one our.bol)
      ?:  =(nom.one inbox)
        ":"
      ['%' (trip nom.one)]
    ?:  =(hos.one (sein our.bol))
      ['/' (trip nom.one)]
    =+  wun=(scow %p hos.one)
    ?:  =(nom.one (main hos.one))
      wun
    :(welp wun "/" (trip nom.one))
  --
::
++  pr                                                  ::<  partner renderer
  ::>  used primarily for printing partners.
  ::
  |_  ::>  one: the partner
      ::
      one/partner
  ::
  ++  pr-beat                                           ::<  more relevant
    ::>  returns true if one is better to show, false
    ::>  otherwise. prefers circles over passports.
    ::>  if both are circles, ++cr-best.
    ::>  if both are passports, pick the "larger" one.
    ::>  if they're equal, content hash.
    ::
    |=  two/partner  ^-  ?
    ?-  -.one
        $&
      ?-  -.two
        $|  %&
        $&  (~(cr-best cr p.one) p.two)
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
  ::
  ++  pr-best                                           ::<  most relevant
    ::>  picks the most relevant partner.
    ::
    |=(two/partner ?:((pr-beat two) two one))
  ::
  ++  pr-sigh                                           ::<  assemble label
    ::>  prepend {pre} to {yiz}, omitting characters of
    ::>  {yiz} to stay within {len} characters.
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
  ++  pr-full  (pr-show ~)                              ::<  render full width
  ::
  ++  pr-show                                           ::<  render partner
    ::>  renders a partner as text.
    ::
    |=  moy/(unit ?)
    ^-  tape
    ?-  -.one
      ::  render circle (as glyph if we can).
        $&
      ?~  moy
        =+  cha=(~(get by nik) one ~ ~)
        =-  ?~(cha - "'{u.cha ~}' {-}")
        ~(cr-phat cr p.one)
      (~(cr-curt cr p.one) u.moy)
      ::  render passport.
        $|
      =/  pre  ^-  tape
        ?-  -.p.one
          $twitter  "@t:"
        ==
      ?~  moy
        (weld pre (trip p.p.one))
      =.  pre  ?.(u.moy pre ['*' pre])
      (pr-sigh 14 pre p.p.one)
    ==
  --
::
++  ar                                                  ::<  audience renderer
  ::>  used for representing audiences (sets of partners)
  ::>  as tapes.
  ::
  |_  ::>  lix: members of the audience.
      ::
      lix/(set partner)
  ::
  ++  ar-best                                           ::<  most relevant
    ::>  find the most relevant partner in the set.
    ::
    ^-  (unit partner)
    ?~  lix  ~
    :-  ~
    |-  ^-  partner
    =+  lef=`(unit partner)`ar-best(lix l.lix)
    =+  rit=`(unit partner)`ar-best(lix r.lix)
    =.  n.lix  ?~(lef n.lix (~(pr-best pr n.lix) u.lef))
    =.  n.lix  ?~(rit n.lix (~(pr-best pr n.lix) u.rit))
    n.lix
  ::
  ++  ar-deaf                                           ::<  except for self
    ::>  remove ourselves from the audience.
    ::
    ^+  .
    .(lix (~(del in lix) `partner`[%& our.bol inbox]))
  ::
  ++  ar-maud                                           ::<  multiple audience
    ::>  checks if there's multiple partners in the
    ::>  audience via pattern matching.
    ::
    ^-  ?
    =.  .  ar-deaf
    !?=($@($~ {* $~ $~}) lix)
  ::
  ++  ar-prom                                           ::<  render targets
    ::>  render all partners, ordered by relevance.
    ::
    ^-  tape
    =.  .  ar-deaf
    =/  all
      %+  sort  `(list partner)`(~(tap in lix))
      |=  {a/partner b/partner}
      (~(pr-beat pr a) b)
    =+  fir=&
    |-  ^-  tape
    ?~  all  ~
    ;:  welp
      ?:(fir "" " ")
      (~(pr-show pr i.all) ~)
      $(all t.all, fir |)
    ==
  ::
  ++  ar-whom                                           ::<  render sender
    ::>  render sender as the most relevant partner.
    ::
    (~(pr-show pr (need ar-best)) ~ ar-maud)
  ::
  ++  ar-dire                                           ::<  direct message
    ::>  returns true if partner is a mailbox of ours.
    ::
    |=  pan/partner  ^-  ?
    ?&  ?=($& -.pan)
        =(hos.p.pan our.bol)
        =+  sot=(~(get by mirrors) +.pan)
        &(?=(^ sot) ?=($brown sec.con.u.sot))
    ==
  ::
  ++  ar-pref                                           ::<  audience glyph
    ::>  get the glyph that corresponds to the audience,
    ::>  with a space appended. for mailbox messages and
    ::>  complex audiences, use reserved "glyphs".
    ::
    ^-  tape
    =+  cha=(~(get by nik) lix)
    ?^  cha  ~[u.cha ' ']
    ?.  (lien (~(tap by lix)) ar-dire)
      "* "
    ?:  ?=({{$& ^} $~ $~} lix)
      ": "
    "; "
  --
::
++  tr                                                  ::<  telegram renderer
  ::>  responsible for converting telegrams and
  ::>  everything relating to them to text to be
  ::>  displayed in the cli.
  ::
  |_  $:  ::>  sef: settings flags.
          ::>  \ telegram
          ::>   who: author.
          ::>   \ thought
          ::>    sen: unique identifier.
          ::>    aud: audience.
          ::>    \ statement
          ::>     wen: timestamp.
          ::>     bou: complete aroma.
          ::>     sep: message contents.
          ::
          sef/(set knot)
          who/ship
          sen/serial
          aud/audience
          wen/@da
          bou/bouquet
          sep/speech
      ==
  ::
  ++  tr-fact                                           ::<  activate effect
    ::>  produces sole-effect for printing message
    ::>  details.
    ::
    ^-  sole-effect
    ~[%mor [%tan tr-meta] tr-body]
  ::
  ++  tr-line                                           ::<  one-line print
    ::>  crams a telegram into a single line by
    ::>  displaying a short ship name, a short
    ::>  representation of the gram, and an optional
    ::>  timestamp.
    ::
    ^-  tape
    =+  txt=(tr-text =(who our.bol))
    ?:  =(~ txt)  ""
    =/  baw
      ::  ?:  oug
      ::  ~(ar-whom ar tr-pals)
      ?.  (~(has in sef) %noob)
        (~(cr-curt cr [who (main who)]) |)
      (~(cr-nick cr [who (main who)]))
    ?:  (~(has in sef) %showtime)
      =+  dat=(yore now.bol)
      =/  t
        |=  a/@
        %+  weld
        ?:  (lth a 10)  "0"  ~
        (scow %ud a)
      =/  time
        ;:  weld
          "~"  (t h.t.dat)
          "."  (t m.t.dat)
          "."  (t s.t.dat)
        ==
      :(weld baw txt (reap (sub 67 (lent txt)) ' ') time)
    (weld baw txt)
  ::
  ++  tr-meta                                           ::<  metadata
    ::>  builds string that display metadata, including
    ::>  message serial, timestamp, author and audience.
    ::
    ^-  tang
    =.  wen  (sub wen (mod wen (div wen ~s0..0001)))    :: round
    =+  hed=leaf+"{(scow %uv sen)} at {(scow %da wen)}"
    =/  paz
      %+  turn  (~(tap by aud))
      |=  {a/partner *}
      leaf+~(pr-full pr a)
    =+  bok=(turn (sort (~(tap in bou)) aor) smyt)
    [%rose [" " ~ ~] [hed >who< [%rose [", " "to " ~] paz] bok]]~
  ::
  ++  tr-body                                           ::<  message content
    ::>  long-form display of message contents, specific
    ::>  to each speech type.
    ::
    |-  ^-  sole-effect
    ?+  -.sep
      tan+[>sep<]~
      ::
        $non
      tan+~
      ::
        $lin
      tan+~[leaf+"{?:(pat.sep "" "@ ")}{(trip msg.sep)}"]
      ::
        $url
      url+(crip (earf url.sep))
      ::
        $exp
      tan+~[leaf+"# {(trip exp.sep)}"]
      ::
        $fat
      [%mor $(sep sep.sep) tan+(tr-tors tac.sep) ~]
      ::
        $inv
      :-  %tan
      :_  ~
      :-  %leaf
      %+  weld
        ?:  inv.sep
          "you have been invited to "
        "you have been banished from "
      ~(cr-phat cr sat.sep)
      ::
        $mor
      mor+(turn ses.sep |=(speech ^$(sep +<)))
      ::
        $app
      tan+~[rose+[": " ~ ~]^~[leaf+"[{(trip app.sep)}]" leaf+(trip msg.sep)]]
      ::
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
  ++  tr-tors                                           ::<  attachment
    ::>  renders an attachment.
    ::
    |=  a/torso
    ^-  tang
    ?-  -.a
      $name  (welp $(a tac.a) leaf+"={(trip nom.a)}" ~)
      $tank  +.a
      $text  (turn (flop +.a) |=(b/cord leaf+(trip b)))
    ==
  ::
  ++  tr-pals                                           ::<  aud w/o delivery
    ::>  strip delivery info from audience, producing a
    ::>  plain set of partners.
    ::
    ^-  (set partner)
    %-  ~(gas in *(set partner))
    %+  turn  (~(tap by aud))
    |=({a/partner *} a)
  ::
  ++  tr-chow                                           ::<  truncate
    ::>  truncates the {txt} to be of max {len}
    ::>  characters. if it does truncate, indicates it
    ::>  did so by appending _ or ….
    ::
    |=  {len/@u txt/tape}
    ^-  tape
    ?:  (gth len (lent txt))  txt
    =.  txt  (scag len txt)
    |-
    ?~  txt  txt
    ?:  =(' ' i.txt)
      |-(['_' ?.(?=({$' ' *} t.txt) t.txt $(txt t.txt))])
    ?~  t.txt  "…"
    [i.txt $(txt t.txt)]
  ::
  ++  tr-both                                           ::<  two tapes one line
    ::>  attempts to fit two tapes into a 64-char line.
    ::
    |=  {a/tape b/tape}
    ^-  tape
    ?:  (gth (lent a) 62)  (tr-chow 64 a)
    %+  weld  a
    (tr-chow (sub 64 (lent a)) "  {b}")
  ::
  ++  tr-text                                           ::<  one line contents
    ::>  renders a single-line version of the message.
    ::
    |=  oug/?
    ^-  tape
    ?+  -.sep
      ~&(tr-lost+sep "")
      ::
        $mor
      ?~  ses.sep  ~&(%tr-mor-empty "")
      |-  ^-  tape
      ?~  t.ses.sep  ^$(sep i.ses.sep)
      (tr-both ^$(sep i.ses.sep) $(ses.sep t.ses.sep))
      ::
        $fat
      %+  tr-both  $(sep sep.sep)
      ?+  -.tac.sep  "..."
        $tank  ~(ram re %rose [" " `~] +.tac.sep)
      ==
      ::
        $exp
      (tr-chow 66 '#' ' ' (trip exp.sep))
      ::
        $url
      =+  ful=(earf url.sep)
      ?:  (gth 64 (lent ful))  ['/' ' ' ful]
      :+  '/'  '_'
      =+  hok=r.p.p.url.sep
      ~!  hok
      =-  (swag [a=(sub (max 64 (lent -)) 64) b=64] -)
      ^-  tape
      =<  ?:  ?=($& -.hok)
            (reel p.hok .)
          +:(scow %if p.hok)
      |=  {a/knot b/tape}
      ?~  b  (trip a)
      (welp b '.' (trip a))
      ::
        $lin
      =+  txt=(trip msg.sep)
      ?:  pat.sep
        =+  pal=tr-pals
        =.  pal  ?:  =(who our.bol)  pal  ::TODO  =?
                 (~(del in pal) [%& who (main who)])
        (weld ~(ar-pref ar pal) txt)
      (weld " " txt)
      ::
        $inv
      %+  weld
        ?:  inv.sep
          " invited you to "
        " banished you from "
      ~(cr-phat cr sat.sep)
      ::
        $app
      (tr-chow 64 "[{(trip app.sep)}]: {(trip msg.sep)}")
      ::
        $api
      %+  tr-chow  64
      %+  weld
        "[{(trip id.sep)}@{(trip service.sep)}]: "
      (trip summary.sep)
    ==
  --
::
::>  ||
::>  ||  %events
::>  ||
::+|
::
++  peer                                                ::<  accept subscription
  ::>  incoming subscription on pax.
  ::
  |=  pax/path
  ^-  (quip move +>)
  ?.  (team src.bol our.bol)
    ~&  [%peer-talk-reader-stranger src.bol]
    [~ +>]
  ?.  ?=({$sole *} pax)
    ~&  [%peer-talk-reader-strange pax]
    [~ +>]
  ta-done:ta-console:ta
::
++  diff-talk-lowdown                                   ::<  accept lowdown
  ::>  incoming talk-lowdown. process it.
  ::>  we *could* use the wire to identify what story
  ::>  subscription our lowdown is coming from, but
  ::>  since we only ever subscribe to a single story,
  ::>  we don't bother.
  ::
  |=  {way/wire low/lowdown}
  ta-done:(ta-low:ta low)
::
++  diff-talk-reaction                                  ::<  accept reaction
  ::>  incoming talk reaction. process it.
  ::
  |=  {way/wire rac/reaction}
  ?.  =(src.bol -:(broker our.bol))
    ~&  [%diff-reaction-stranger src.bol]
    [~ +>]
  ta-done:(ta-reaction:ta rac)
::
++  poke-sole-action                                    ::<  accept console
  ::>  incoming sole action. process it.
  ::
  |=  act/sole-action
  ta-done:(ta-sole:ta act)
--
