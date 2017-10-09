::                                                      ::  ::
::::  /hoon/talk-agent/app                              ::  ::
  ::                                                    ::  ::
::
::TODO  master changes, incl %notify
::TODO  guardian's todo's apply here too
::TODO  make sure glyphs only get bound when joins succeed
::      ...this is a bit troublesome, because failed joins don't actually
::      unsubscribe us.
::TODO  maybe keep track of received grams per circle, too?
::
::TODO  for delta model:
::      3) split into delta creation and application, as with hall.
::
::>  This reader implementation makes use of the mailbox
::>  for all its subscriptions and messaging. All
::>  rumors received are exclusively about the mailbox,
::>  since that's the only thing the reader ever
::>  subscribes to.
::
/?    151                                               ::<  hoon version
/-    talk, sole                                        ::<  structures
/+    talk, sole                                        ::<  libraries
/=    seed  /~  !>(.)
!:
::::
  ::
=,  talk
=,  sole
=>  ::>  ||
    ::>  ||  %arch
    ::>  ||
    ::>    data structures
    ::
    |%
    ++  state                                           ::>  reader state
      $:  ::  messaging state                           ::
          count/@ud                                     ::<  (lent grams)
          grams/(list telegram)                         ::<  all history
          known/(map serial @ud)                        ::<  messages heard
          sources/(set circle)                          ::<  our subscriptions
          ::  circle details                            ::
          remotes/(map circle group)                    ::<  remote presences
          mirrors/(map circle config)                   ::<  remote configs
          ::  ui state                                  ::
          nicks/(map ship nick)                         ::<  human identities
          nik/(map (set circle) char)                   ::<  bound circle glyphs
          nak/(jug char (set circle))                   ::<  circle glyph lookup
          cli/shell                                     ::<  interaction state
      ==                                                ::
    ++  shell                                           ::>  console session
      $:  id/bone                                       ::<  identifier
          latest/@ud                                    ::<  latest shown msg num
          say/sole-share                                ::<  console state
          active/(set circle)                           ::<  active targets
          settings/(set term)                           ::<  frontend settings
      ==                                                ::
    ++  move  (pair bone card)                          ::<  all actions
    ++  lime                                            ::>  diff fruit
      $%  {$sole-effect sole-effect}                    ::
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
          {$join p/(map circle range)}                  ::<  subscribe to
          {$leave p/where}                              ::<  unsubscribe from
          {$create p/security q/naem r/cord}            ::<  create circle
          {$delete p/naem q/(unit cord)}                ::<  delete circle
          {$depict p/naem q/cord}                       ::<  change description
          {$filter p/naem q/? r/?}                      ::<  change message rules
          {$invite p/naem q/(set ship)}                 ::<  give permission
          {$banish p/naem q/(set ship)}                 ::<  deny permission
          {$source p/naem q/(map circle range)}         ::<  add source
          ::  personal metadata
          {$attend p/(set circle) q/presence}           ::<  set our presence
          {$name p/(set circle) q/human}                ::<  set our name
          ::  messaging                                 ::
          {$say p/(list speech)}                        ::<  send message
          {$eval p/cord q/twig}                         ::<  send #-message
          {$target p/where q/(unit work)}               ::<  set active targets
          {$reply p/$@(@ud {@u @ud}) q/(list speech)}   ::<  reply to
          ::  displaying info                           ::
          {$number p/$@(@ud {@u @ud})}                  ::<  relative/absolute
          {$who p/where}                                ::<  presence
          {$what p/$@(char (set circle))}               ::<  show bound glyph
          ::  ui settings                               ::
          {$bind p/char q/(unit where)}                 ::<  bind glyph
          {$unbind p/char q/(unit where)}               ::<  unbind glyph
          {$nick p/(unit ship) q/(unit cord)}           ::<  un/set/show nick
          {$set p/term}                                 ::<  enable setting
          {$unset p/term}                               ::<  disable setting
          ::  miscellaneous                             ::
          {$help $~}                                    ::<  print usage info
      ==                                                ::
    ++  where  (set circle)                             ::<  non-empty audience
    ++  glyphs  `wall`~[">=+-" "}),." "\"'`^" "$%&@"]   ::<  circle char pool '
    ++  termwidth  80  ::TODO  put in setting or something?
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
  ^-  dock
  :_  %talk-guardian
  (true-self our.bol)
::
++  inbox                                               ::<  reader's circle name
  ::>  produces the name of the circle used by this
  ::>  reader for all its operations
  ^-  naem
  (main our.bol)
::
++  incir                                               ::<  reader's circle
  ::>  ++inbox, except a full circle.
  ^-  circle
  :_  inbox
  (true-self our.bol)
::
++  renum                                               ::<  gram i# by serial
  ::>  find the grams list index for gram with serial.
  |=  ser/serial
  ^-  (unit @ud)
  =+  num=(~(get by known) ser)
  ?~  num  ~
  `(sub count +(u.num))
::
++  recall                                              ::<  gram by serial
  ::>  find a known gram with serial {ser}.
  |=  ser/serial
  ^-  (unit telegram)
  =+  num=(renum ser)
  ?~  num  ~
  `(snag u.num grams)
::
++  nik-from-nak                                        ::<  nik from nak
  ::>
  ::
  ::TODO  ...we really should rename these.
  |=  nek/_nak
  ^+  nik
  %-  ~(gas by *(map (set circle) char))
  =-  (zing -)
  %+  turn  ~(tap by nek)
  |=  {a/char b/(set (set circle))}
  (turn ~(tap by b) |=(c/(set circle) [c a]))
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
    %-  ta-emil
    ^-  (list move)
    :~  :*  ost.bol
            %peer
            /
            broker
            /reader
        ==
        :*  ost.bol
            %peer
            /
            broker
            /circle/[inbox]
        ==
    ==
  ::
  ++  ta-take                                           ::<  accept prize
    ::>
    ::
    |=  piz/prize
    ^+  +>
    ?+  -.piz  +>
      ::
        $reader
      %=  +>
        nak     gys.piz
        nik     (nik-from-nak gys.piz)
        nicks   nis.piz
      ==
      ::
        $circle
      %.  nes.piz
      %=  ta-unpack
        sources   src.loc.cos.piz
        mirrors   (~(put by rem.cos.piz) incir loc.cos.piz)
        remotes   (~(put by rem.pes.piz) incir loc.pes.piz)
      ==
    ==
  ::
  ++  ta-hear                                           ::<  apply change
    ::>
    ::
    |=  rum/rumor
    ^+  +>
    ?+  -.rum  +>
      ::
        $reader
      ?-  -.rum.rum
          $glyph
        (ta-change-glyph +.rum.rum)
        ::
          $nick
        +>(nicks (change-nicks nicks who.rum.rum nic.rum.rum))
      ==
      ::
        $circle
      (ta-change-circle rum.rum)
    ==
  ::
  ++  ta-change-circle                                  ::<  apply circle change
    ::>
    ::
    |=  rum/rumor-story
    ^+  +>
    ?+  -.rum
        ~&([%unexpected-circle-rumor -.rum] +>)
      ::
        $gram
      (ta-learn gam.nev.rum)
      ::
        $config
      %=  +>
          sources
        ?.  ?&  ?=($source -.dif.rum)
                =(cir.rum incir)
            ==
          sources
        %.  cir.dif.rum
        ?:  add.dif.rum
          ~(put in sources)
        ~(del in sources)
        ::
          mirrors
        ?:  ?=($remove -.dif.rum)  (~(del by mirrors) cir.rum)
        %+  ~(put by mirrors)  cir.rum
        %+  change-config
          (fall (~(get by mirrors) cir.rum) *config)
        dif.rum
      ==
      ::
        $status
      %=  +>
          remotes
        %+  ~(put by remotes)  cir.rum
        =+  rem=(fall (~(get by remotes) cir.rum) *group)
        ?:  ?=($remove -.dif.rum)  (~(del by rem) who.rum)
        %+  ~(put by rem)  who.rum
        %+  change-status
          (fall (~(get by rem) who.rum) *status)
        dif.rum
      ==
    ==
  ::
  ++  ta-change-glyph                                   ::<  apply changed glyphs
    ::>  applies new set of glyph bindings.
    ::
    |=  {bin/? gyf/char pas/(set circle)}
    ^+  +>
    =+  nek=(change-glyphs nak bin gyf pas)
    ?:  =(nek nak)  +>.$                                ::  no change
    =.  nak  nek
    =.  nik  (nik-from-nak nek)
    sh-done:~(sh-prod sh cli)
  ::
  ::>  ||
  ::>  ||  %messages
  ::>  ||
  ::>    storing and updating messages.
  ::+|
  ::
  ++  ta-unpack                                         ::<  open envelopes
    ::>  the reader currently doesn't care about nums.
    ::
    |=  nes/(list envelope)
    ^+  +>
    (ta-lesson (turn nes tail))
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
    =+  old=(renum uid.gam)
    ?~  old
      (ta-append gam)      ::<  add
    (ta-revise u.old gam)  ::<  modify
  ::
  ++  ta-append                                         ::<  append message
    ::>  store a new telegram.
    ::
    |=  gam/telegram
    ^+  +>
    =:  grams  [gam grams]
        count  +(count)
        known  (~(put by known) uid.gam count)
    ==
    =<  sh-done
    (~(sh-gram sh cli) gam)
  ::
  ++  ta-revise                                         ::<  revise message
    ::>  modify a telegram we know.
    ::
    |=  {num/@ud gam/telegram}
    =+  old=(snag num grams)
    ?:  =(gam old)  +>.$                                ::  no change
    =.  grams  (welp (scag num grams) [gam (slag +(num) grams)])
    ?:  =(sep.gam sep.old)  +>.$                        ::  no worthy change
    =<  sh-done
    (~(sh-gram sh cli) gam)
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
      %*(. *shell id ost.bol, active (sy incir ~))
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
            broker
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
      ++  cire                                          ::<  local circle
        ;~(pfix cen sym)
      ::
      ++  circ                                          ::<  circle
        ;~  pose
          (cold incir col)
          ;~(pfix cen (stag our.bol sym))
          ;~(pfix fas (stag (sein:title our.bol) sym))
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
      ++  cirs                                          ::<  non-empty circles
        %+  cook  ~(gas in *(set circle))
        (most ;~(plug com (star ace)) circ)
      ::
      ++  pont                                          ::<  point for range
        ;~  pose
          ::TODO  support entering of @da and @dr,
          ::      convert all to @da. use sear with crub?
          ::      also allow "now"?
          (stag %ud dem:ag)
        ==
      ::
      ++  rang                                          ::<  subscription range
        =+  ;~  pose
              (cook some ;~(pfix fas pont))
              (easy ~)
            ==
        ;~  pose
          (cook some ;~(plug ;~(pfix fas pont) -))
          (easy ~)
        ==
      ::
      ++  sorz                                          ::<  non-empty sources
        %+  cook  ~(gas by *(map circle range))
        (most ;~(plug com (star ace)) ;~(plug circ rang))
      ::
      ++  pick                                          ::<  message reference
        ;~(pose nump (cook lent (star sem)))
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
      ++  pore                                          ::<  security
        ;~  pose
          (cold %black (jest %channel))
          (cold %white (jest %village))
          (cold %green (jest %journal))
          (cold %brown (jest %mailbox))
        ==
      ::
      ++  lobe                                          ::<  y/n loob
        ;~  pose
          (cold %& ;~(pose (jest 'y') (jest '&')))
          (cold %| ;~(pose (jest 'n') (jest '|')))
        ==
      ::
      ++  message                                       ::<  exp, lin or url msg
        ;~  pose
          ;~(plug (cold %eval hax) expr)
          (stag %say speeches)
        ==
      ::
      ++  speeches                                      ::<  lin or url msgs
        %+  most  (jest '•')
        ;~  pose
          (stag %url aurf:de-purl:html)
          :(stag %lin & ;~(pfix pat text))
          :(stag %lin | ;~(less sem hax text))
        ==
      ::
      ++  text                                          ::<  msg without break
        %+  cook  crip
        (plus ;~(less (jest '•') next))
      ::
      ++  nick  (cook crip (stun [1 14] low))           ::<  nickname
      ++  glyph  (mask "/\\\{(<!?{(zing glyphs)}")      ::<  circle postfix
      ++  setting                                       ::<  setting flag
        %-  perk  :~
          %noob  ::TODO  rename to nick
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
          ;~((glue ace) (perk %join ~) sorz)
          ::
          ;~((glue ace) (perk %leave ~) cirs)
          ::
          ;~  (glue ace)  (perk %create ~)
            pore
            cire
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
          ;~((glue ace) (perk %depict ~) cire qut)
          ::
          ;~((glue ace) (perk %filter ~) cire lobe lobe)
          ::
          ;~((glue ace) (perk %invite ~) cire shiz)
          ::
          ;~((glue ace) (perk %banish ~) cire shiz)
          ::
          ;~((glue ace) (perk %source ~) cire sorz)
          ::
          ::  personal metadata
          ::
          ;~  (glue ace)
            (perk %attend ~)
            cirs
            (perk %gone %idle %hear %talk ~)
          ==
          ::
          ;~  plug
            (perk %name ~)
            ;~(pfix ace cirs)
            ;~(pfix ace ;~(pose (cook some qut) (cold ~ sig)))
            ;~  pose
              ;~  pfix  ace
                %+  cook  some
                ;~  pose
                  ;~((glue ace) qut (cook some qut) qut)
                  ;~(plug qut (cold ~ ace) qut)
                ==
              ==
              ;~(pfix ace (cold ~ sig))
              (easy ~)
            ==
          ==
          ::
          ::  displaying info
          ::
          ;~(plug (perk %who ~) ;~(pose ;~(pfix ace cirs) (easy ~)))
          ::
          ;~((glue ace) (perk %what ~) ;~(pose cirs glyph))
          ::
          ::  ui settings
          ::
          ;~(plug (perk %bind ~) ;~(pfix ace glyph) (punt ;~(pfix ace cirs)))
          ::
          ;~(plug (perk %unbind ~) ;~(pfix ace glyph) (punt ;~(pfix ace cirs)))
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
          ::  (parsers below come last because they match quickly)
          ::
          ::  messaging
          ::
          (stag %target ;~(plug cirs (punt ;~(pfix ace message))))
          ::
          (stag %reply ;~(plug pick ;~(pfix ace speeches)))
          ::
          ::  displaying info
          ::
          (stag %number pick)
        ==
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
          ::TODO  for inv/ban, enl/ret etc, set bools here?
          ::  circle management
          $join    (join +.job)
          $leave   (leave +.job)
          $create  (create +.job)
          $delete  (delete +.job)
          $depict  (depict +.job)
          $filter  (filter +.job)
          $invite  (invite +.job)
          $banish  (banish +.job)
          $source  (source +.job)
          ::  personal metadata
          $attend  (attend +.job)
          $name    (name +.job)
          ::  messaging
          $say     (say +.job)
          $eval    (eval +.job)
          $target  (target +.job)
          $reply   (reply +.job)
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
        sh-prod(active.she aud.gam)
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
        |=  {cha/char pas/(set circle)}
        =:  nik  (~(put by nik) pas cha)
            nak  (~(put ju nak) cha pas)
        ==
        (sh-act %glyph cha pas &)
      ::
      ++  unset-glyph                                   ::<  old glyph binding
        ::>  removes either {pas} or all bindings on a
        ::>  glyph and sends an action.
        ::
        |=  {cha/char pas/(unit (set circle))}
        =/  ole/(set (set circle))
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
      ++  reverse-nicks                                 ::<  find by handle
        ::>  finds all ships whose handle matches {nym}.
        ::
        |=  nym/^nick
        ^-  (list ship)
        %+  murn  ~(tap by nicks)
        |=  {p/ship q/^nick}
        ?.  =(q nym)  ~
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
        ::
        |=  pos/(map circle range)
        ^+  ..sh-work
        =+  pas=~(key by pos)
        =.  ..sh-work
          =+  (~(get by nik) pas)
          ?^  -  (sh-note "has glyph {<u>}")
          =+  cha=(glyph (mug pas))
          (sh-note:(set-glyph cha pas) "new glyph {<cha>}")
        =.  ..sh-work
          sh-prod(active.she pas)
        (sh-act %source inbox & pos)
      ::
      ++  leave                                         ::<  %leave
        ::>  change local mailbox config to exclude
        ::>  subscriptions to {pas}.
        ::
        |=  pas/(set circle)
        ^+  ..sh-work
        =/  pos
          %-  ~(run in pas)
          |=(p/circle [p ~])
        (sh-act %source inbox | pos)
      ::
      ++  create                                        ::<  %create
        ::>  creates circle {nom} with specified config.
        ::
        |=  {sec/security nom/naem txt/cord}
        ^+  ..sh-work
        =.  ..sh-work
          (sh-act %create nom txt sec)
        (join [[[our.bol nom] ~] ~ ~])
      ::
      ++  delete                                        ::<  %delete
        ::>  deletes our circle {nom}, after optionally
        ::>  sending a last announce message {say}.
        ::
        |=  {nom/naem say/(unit cord)}
        ^+  ..sh-work
        (sh-act %delete nom say)
      ::
      ++  depict                                        ::<  %depict
        ::>  changes the description of {nom} to {txt}.
        ::
        |=  {nom/naem txt/cord}
        ^+  ..sh-work
        (sh-act %depict nom txt)
      ::
      ++  invite                                        ::<  %invite
        ::>  invites {sis} to our circle {nom}.
        ::
        |=  {nom/naem sis/(set ship)}
        ^+  ..sh-work
        (sh-act %permit nom & sis)
      ::
      ++  filter
        |=  {nom/naem cus/? utf/?}
        ^+  ..sh-work
        (sh-act %filter nom cus utf)
      ::
      ++  banish                                        ::<  %banish
        ::>  banish {sis} from our circle {nom}.
        ::
        |=  {nom/naem sis/(set ship)}
        ^+  ..sh-work
        (sh-act %permit nom | sis)
      ::
      ++  source                                        ::<  %source
        ::>  adds {pas} to {nom}'s src.
        ::
        |=  {nom/naem pos/(map circle range)}
        ^+  ..sh-work
        (sh-act %source nom & pos)
      ::
      ::>  ||
      ::>  ||  %personal-metadata
      ::>  ||
      ::+|
      ::
      ++  attend                                        ::<  set our presence
        ::>  sets our presence to {pec} in circles {cis}.
        ::
        |=  {cis/(set circle) pec/presence}
        ^+  ..sh-work
        (sh-act %notify cis pec)
      ::
      ++  name                                          ::<  set our name
        ::>  sets our name to {man} in circles {cis}.
        ::
        |=  {cis/(set circle) man/human}
        ^+  ..sh-work
        (sh-act %naming cis man)
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
        (say [%exp txt tan] ~)
      ::
      ++  target                                        ::<  %target
        ::>  sets messaging target, then execute {woe}.
        ::
        |=  {pan/(set circle) woe/(unit ^work)}
        ^+  ..sh-work
        =.  ..sh-pact  (sh-pact pan)
        ?~(woe ..sh-work work(job u.woe))
      ::
      ++  reply                                         ::<  %reply
        ::>  send a reply to the selected message.
        ::
        |=  {num/$@(@ud {p/@u q/@ud}) sep/(list speech)}
        ^+  ..sh-work
        ::TODO  =- (say (turn ... [%ire - s])) nest-fails on the - ???
        ::TODO  what's friendlier, reply-to-null or error?
        =/  ser/serial
          ?@  num
            ?:  (gte num count)  0v0
            uid:(snag num grams)
          ?:  (gth q.num count)  0v0
          ?:  =(count 0)  0v0
          =+  msg=(deli (dec count) num)
          uid:(snag (sub count +(msg)) grams)
        (say (turn sep |=(s/speech [%ire `serial`ser s])))
      ::
      ::>  ||
      ::>  ||  %displaying-info
      ::>  ||
      ::+|
      ::
      ++  who                                           ::<  %who
        ::>  prints presence lists for {pas} or all.
        ::
        |=  pas/(set circle)  ^+  ..sh-work
        =<  (sh-fact %mor (murn (sort ~(tap by remotes) aor) .))
        |=  {pon/circle gop/group}  ^-  (unit sole-effect)
        ?.  |(=(~ pas) (~(has in pas) pon))  ~
        =-  `[%tan rose+[", " `~]^- leaf+~(pr-full pr pon) ~]
        =<  (murn (sort ~(tap by gop) aor) .)
        |=  {a/ship b/presence c/human}  ^-  (unit tank)
        =.  c
          ?.  =(han.c `(scot %p a))  c
          [~ tru.c]
        ?-  b
          $gone  ~
          $idle  `leaf+:(weld "idle " (scow %p a) " " (trip (fall han.c '')))
          $hear  `leaf+:(weld "hear " (scow %p a) " " (trip (fall han.c '')))
          $talk  `leaf+:(weld "talk " (scow %p a) " " (trip (fall han.c '')))
        ==
      ::
      ++  what                                          ::<  %what
        ::>  prints binding details. goes both ways.
        ::
        |=  qur/$@(char (set circle))
        ^+  ..sh-work
        ?^  qur
          =+  cha=(~(get by nik) qur)
          (sh-fact %txt ?~(cha "none" [u.cha]~))
        =+  pan=~(tap in (~(get ju nak) qur))
        ?:  =(~ pan)  (sh-fact %txt "~")
        =<  (sh-fact %mor (turn pan .))
        |=(a/(set circle) [%txt ~(ar-prom ar a)])
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
        |=  {cha/char pas/(unit (set circle))}
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
        |=  {cha/char pan/(unit (set circle))}
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
        |=  {her/(unit ship) nym/(unit ^nick)}
        ^+  ..sh-work
        ::>  no arguments, show all
        ?:  ?=({$~ $~} +<)
          %+  sh-fact  %mor
          %+  turn  ~(tap by nicks)
          |=  {p/ship q/^nick}
          :-  %txt
          "{<p>}: {<q>}"
        ::>  show her nick
        ?~  nym
          ?>  ?=(^ her)
          =+  asc=(~(get by nicks) u.her)
          %+  sh-fact  %txt
          ?~  asc  "{<u.her>} unbound"
          "{<u.her>}: {<u.asc>}"
        ::>  show nick ship
        ?~  her
          %+  sh-fact  %mor
          %+  turn  (reverse-nicks u.nym)
          |=  p/ship
          [%txt "{<p>}: {<u.nym>}"]
        %.  [%nick u.her (fall nym '')]
        %=  sh-act
            nicks
          ?~  u.nym
            ::>  unset nickname
            (~(del by nicks) u.her)
          ::>  set nickname
          (~(put by nicks) u.her u.nym)
        ==
      ::
      ++  wo-set                                        ::<  %set
        ::>  enables ui setting flag.
        ::
        |=  seg/term
        ^+  ..sh-work
        ?~  seg
          %+  sh-fact  %mor
          %+  turn  ~(tap in settings.she)
          |=  s/term
          [%txt (trip s)]
        %=  ..sh-work
          settings.she  (~(put in settings.she) seg)
        ==
      ::
      ++  unset                                         ::<  %unset
        ::>  disables ui setting flag.
        ::
        |=  neg/term
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
      |=  lix/(set circle)
      ^+  +>
      ::>  ensure we can see what we send.
      =+  act=(sh-pare lix)
      ?:  =(active.she act)  +>.$
      sh-prod(active.she act)
    ::
    ++  sh-pare                                         ::<  adjust target list
      ::>  if the audience {cis} does not contain a
      ::>  circle we're subscribed to, add our mailbox
      ::>  to the audience (so that we can see our own
      ::>  message).
      ::
      |=  cis/(set circle)
      ?:  (sh-pear cis)  cis
      (~(put in cis) incir)
    ::
    ++  sh-pear                                         ::<  hearback
      ::>  produces true if any circle is included in
      ::>  our subscriptions, meaning, we hear messages
      ::>  sent to {cis}.
      ::
      |=  cis/(set circle)
      ?~  cis  |
      ?|  (~(has in sources) `circle`n.cis)
          $(cis l.cis)
          $(cis r.cis)
      ==
    ::
    ++  sh-glyf                                         ::<  decode glyph
      ::>  finds the circle(s) that match a glyph.
      ::
      |=  cha/char  ^-  (unit (set circle))
      =+  lax=(~(get ju nak) cha)
      ::>  no circle.
      ?:  =(~ lax)  ~
      ::>  single circle.
      ?:  ?=({* $~ $~} lax)  `n.lax
      ::>  in case of multiple circles, pick the most recently active one.
      |-  ^-  (unit (set circle))
      ?~  grams  ~
      ::>  get first circle from a telegram's audience.
      =+  pan=(silt ~(tap in aud.i.grams))
      ?:  (~(has in lax) pan)  `pan
      $(grams t.grams)
    ::
    ::>  ||
    ::>  ||  %differs
    ::>  ||
    ::>    arms that calculate differences between datasets.
    ::+|
    ::
    ++  sh-group-diff                                   ::<  group diff parts
      ::>  calculates the difference between two presence
      ::>  lists, producing lists of removed, added and
      ::>  changed presences.
      ::
      |=  {one/group two/group}
      =|  $=  ret
          $:  old/(list (pair ship status))
              new/(list (pair ship status))
              cha/(list (pair ship status))
          ==
      ^+  ret
      =.  ret
        =+  eno=~(tap by one)
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
        =+  owt=~(tap by two)
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
      |=  {one/(map circle group) two/(map circle group)}
      =|  $=  ret
          $:  old/(list (pair circle group))
              new/(list (pair circle group))
              cha/(list (pair circle group))
          ==
      ^+  ret
      =.  ret
        =+  eno=~(tap by one)
        |-  ^+  ret
        ?~  eno  ret
        =.  ret  $(eno t.eno)
        =+  unt=(~(get by two) p.i.eno)
        ?~  unt
          ret(old [i.eno old.ret])
        ?:  =(q.i.eno u.unt)  ret
        ret(cha [[p.i.eno u.unt] cha.ret])
      =.  ret
        =+  owt=~(tap by two)
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
        =+  eno=~(tap by one)
        |-  ^+  ret
        ?~  eno  ret
        =.  ret  $(eno t.eno)
        =+  unt=(~(get by two) p.i.eno)
        ?~  unt
          ret(old [i.eno old.ret])
        ?:  =(q.i.eno u.unt)  ret
        ret(cha [[p.i.eno u.unt] cha.ret])
      =.  ret
        =+  owt=~(tap by two)
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
      :-  ^=  old  ~(tap in (~(dif in one) two))
          ^=  new  ~(tap in (~(dif in two) one))
    ::
    ::>  ||
    ::>  ||  %printers
    ::>  ||
    ::>    arms for printing data to the cli.
    ::+|
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
      =/  rew/(pair (pair cord cord) (set circle))
          [['[' ']'] active.she]
      =+  cha=(~(get by nik) q.rew)
      ?^  cha  ~[u.cha ' ']
      =+  por=~(ar-prom ar q.rew)
      (weld `tape`[p.p.rew por] `tape`[q.p.rew ' ' ~])
    ::
    ++  sh-rend                                         ::<  print telegram
      ::>  prints a telegram as rendered by ++tr-rend.
      ::
      |=  gam/telegram
      ^+  +>
      =+  lis=~(tr-rend tr settings.she gam)
      ?~  lis  +>.$
      %+  sh-fact  %mor
      ::TODO?  we need to cast, but not if we do =(0 (lent lis)) above instead..
      %+  turn  `(list tape)`lis
      |=  t/tape
      [%txt t]
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
    ++  sh-cure                                         ::<  readable security
      ::>  renders a security kind.
      ::
      |=  a/security
      ^-  tape
      ?-  a
        $black  "channel"
        $brown  "mailbox"
        $white  "village"
        $green  "journal"
      ==
    ::
    ++  sh-scis                                         ::<  render status
      ::>  gets the presence of {saz} as a tape.
      ::
      |=  sat/status
      ^-  tape
      ['%' (trip pec.sat)]
    ::
    ++  sh-show-precs                                   ::<  print group diff
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
          (weld pre "met {(scow %p p.i.new.cul)} {(sh-scis q.i.new.cul)}")
      =.  +>.$
          |-  ^+  +>.^$
          ?~  cha.cul  +>.^$
          %-  sh-note
          (weld pre "set {(scow %p p.i.cha.cul)} {(sh-scis q.i.cha.cul)}")
      +>.$
    ::
    ++  sh-show-permits                                 ::<  show permits
      ::>  prints invite/banish effects to the cli.
      ::
      |=  {pre/tape sec/security old/(list ship) new/(list ship)}
      =+  out=?:(?=(?($black $brown) sec) "try " "cut ")
      =+  inn=?:(?=(?($black $brown) sec) "ban " "add ")
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
      |=  {pre/tape old/(list circle) new/(list circle)}
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
        =.  +>.$  (sh-note :(weld pre "but " (sh-cure sec.con.loc)))
        %^    sh-show-permits
            (weld (trip inbox) ": ")
          sec.con.loc
        [~ ~(tap in ses.con.loc)]
      %^    sh-show-permits
          (weld (trip inbox) ": ")
        sec.con.loc
      (sh-set-diff ses.con.laz ses.con.loc)
    ::
    ++  sh-config                                       ::<  do show config
      ::>  prints a circle's config changes to the cli.
      ::
      |=  {cir/circle old/(unit config) new/(unit config)}
      ^+  +>
      ::  new circle
      ?~  old
        ::  ++sh-show-rempe will notice a new circle.
        +>
      ::  removed circle
      ?~  new
        (sh-note (weld "rip " ~(cr-phat cr cir)))
      %^  sh-show-config
        (weld ~(cr-phat cr cir) ": ")
      u.old  u.new
    ::
    ++  sh-gram                                         ::<  show telegram
      ::>  prints the telegram. every fifth message,
      ::>  print the message number also.
      ::
      |=  gam/telegram
      ^+  +>
      ::TODO  is it cool to just assume all messages we print are already stored?
      =+  num=(~(got by known) uid.gam)
      =.  +>.$
        ::  if the number isn't directly after latest, print it always.
        ?.  =(num +(latest.she))
          (sh-numb num)
        ::  if the number is directly after latest, print every fifth.
        ?.  =(0 (mod num 5))  +>.$
        (sh-numb num)
      (sh-rend(latest.she num) gam)
    ::
    ++  sh-grams                                        ::<  do show telegrams
      ::>  prints multiple telegrams.
      ::
      |=  gaz/(list telegram)
      ^+  +>
      ?~  gaz  +>
      $(gaz t.gaz, +> (sh-gram i.gaz))
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
    ::
    |=  two/circle
    ^-  ?
    ::  the circle that's ours is better.
    ?:  =(our.bol hos.one)
      ?.  =(our.bol hos.two)  %&
      ?<  =(nom.one nom.two)
      ::  if both circles are ours, the main story is better.
      ?:  =((main hos.one) nom.one)  %&
      ?:  =((main hos.two) nom.two)  %|
      ::  if neither are, pick the "larger" one.
      (lth nom.one nom.two)
    ::  if one isn't ours but two is, two is better.
    ?:  =(our.bol hos.two)  %|
    ?:  =(hos.one hos.two)
      ::  if they're from the same ship, pick the "larger" one.
      (lth nom.one nom.two)
    ::  if they're from different ships, neither ours, pick hierarchically.
    (lth (xeb hos.one) (xeb hos.two))
  ::
  ++  cr-curt                                           ::<  render name in 14
    ::>  prints a ship name in 14 characters. left-pads
    ::>  with spaces. {mup} signifies "are there other
    ::>  targets besides this one?"
    ::
    |=  mup/?
    ^-  tape
    =+  raw=(cite:title hos.one)
    (runt [(sub 14 (lent raw)) ' '] raw)
  ::
  ++  cr-nick                                           ::<  nick or name in 14
    ::>  get nick for ship, or shortname if no nick.
    ::>  left-pads with spaces.
    ::
    |.  ^-  tape
    =+  nym=(~(get by nicks) hos.one)
    ?~  nym
      (cr-curt |)
    =+  raw=(trip u.nym)
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
    =+  wun=(scow %p hos.one)
    ?:  =(nom.one (main hos.one))
      wun
    ?:  =(hos.one (sein:title our.bol))
      ['/' (trip nom.one)]
    :(welp wun "/" (trip nom.one))
  --
::
::TODO  maybe merge with cr?
++  pr                                                  ::<  circle renderer
  ::>  used primarily for printing circles.
  ::
  |_  ::>  one: the circle
      ::
      one/circle
  ::
  ++  pr-beat                                           ::<  more relevant
    ::>  returns true if one is better to show, false
    ::>  otherwise. prefers circles over passports.
    ::>  if both are circles, ++cr-best.
    ::>  if both are passports, pick the "larger" one.
    ::>  if they're equal, content hash.
    ::
    |=  two/circle  ^-  ?
    (~(cr-best cr one) two)
  ::
  ++  pr-best                                           ::<  most relevant
    ::>  picks the most relevant circle.
    ::
    |=(two/circle ?:((pr-beat two) two one))
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
  ++  pr-show                                           ::<  render circle
    ::>  renders a circle as text.
    ::
    ::>  moy:  multiple circles in audience?
    |=  moy/(unit ?)
    ^-  tape
    ::  render circle (as glyph if we can).
    ?~  moy
      =+  cha=(~(get by nik) one ~ ~)
      =-  ?~(cha - "'{u.cha ~}' {-}")
      ~(cr-phat cr one)
    (~(cr-curt cr one) u.moy)
  --
::
++  ar                                                  ::<  audience renderer
  ::>  used for representing audiences (sets of circles)
  ::>  as tapes.
  ::
  |_  ::>  lix: members of the audience.
      ::
      lix/(set circle)
  ::
  ++  ar-best                                           ::<  most relevant
    ::>  find the most relevant circle in the set.
    ::
    ^-  (unit circle)
    ?~  lix  ~
    :-  ~
    |-  ^-  circle
    =+  lef=`(unit circle)`ar-best(lix l.lix)
    =+  rit=`(unit circle)`ar-best(lix r.lix)
    =.  n.lix  ?~(lef n.lix (~(pr-best pr n.lix) u.lef))
    =.  n.lix  ?~(rit n.lix (~(pr-best pr n.lix) u.rit))
    n.lix
  ::
  ++  ar-deaf                                           ::<  except for self
    ::>  remove ourselves from the audience.
    ::
    ^+  .
    .(lix (~(del in lix) `circle`incir))
  ::
  ++  ar-maud                                           ::<  multiple audience
    ::>  checks if there's multiple circles in the
    ::>  audience via pattern matching.
    ::
    ^-  ?
    =.  .  ar-deaf
    !?=($@($~ {* $~ $~}) lix)
  ::
  ++  ar-prom                                           ::<  render targets
    ::>  render all circles, ordered by relevance.
    ::
    ^-  tape
    =.  .  ar-deaf
    =/  all
      %+  sort  `(list circle)`~(tap in lix)
      |=  {a/circle b/circle}
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
    ::>  render sender as the most relevant circle.
    ::
    (~(pr-show pr (need ar-best)) ~ ar-maud)
  ::
  ++  ar-dire                                           ::<  direct message
    ::>  returns true if circle is a mailbox of ours.
    ::
    |=  cir/circle  ^-  ?
    ?&  =(hos.cir our.bol)
        =+  sot=(~(get by mirrors) cir)
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
    ?.  (lien ~(tap by lix) ar-dire)
      "* "
    ?:  ?=({^ $~ $~} lix)
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
          sef/(set term)
          who/ship
          sen/serial
          aud/audience
          wen/@da
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
  ++  tr-rend                                           ::<  render telegram
    ::>  renders a telegram.
    ::>  the first line will contain the author and
    ::>  optional timestamp.
    ::
    ^-  (list tape)
    =/  wyd
      %+  sub  termwidth                                ::  termwidth,
      %+  add  14                                       ::  minus author,
      ?:((~(has in sef) %showtime) 10 0)                ::  minus timestamp.
    =+  txs=(tr-text wyd)
    ?~  txs  ~
    ::  render the author.
    =/  nom/tape
      ?:  (~(has in sef) %noob)
        (~(cr-nick cr [who (main who)]))
      (~(cr-curt cr [who (main who)]) |)
    ::  regular indent.
    =/  den/tape
      (reap (lent nom) ' ')
    ::  timestamp, if desired.
    =/  tam/tape
      ?.  (~(has in sef) %showtime)  ""
      =+  dat=(yore now.bol)
      =/  t
        |=  a/@
        %+  weld
          ?:((lth a 10) "0" ~)
        (scow %ud a)
      =/  time
        ;:  weld
          "~"  (t h.t.dat)
          "."  (t m.t.dat)
          "."  (t s.t.dat)
        ==
      %+  weld
        (reap (sub +(wyd) (min wyd (lent (tuba i.txs)))) ' ')
      time
    %-  flop
    %+  roll  `(list tape)`txs
    |=  {t/tape l/(list tape)}
    ?~  l  [:(weld nom t tam) ~]
    [(weld den t) l]
  ::
  ++  tr-meta                                           ::<  metadata
    ::>  builds string that display metadata, including
    ::>  message serial, timestamp, author and audience.
    ::
    ^-  tang
    =.  wen  (sub wen (mod wen (div wen ~s0..0001)))    :: round
    =+  hed=leaf+"{(scow %uv sen)} at {(scow %da wen)}"
    =/  cis
      %+  turn  ~(tap in aud)
      |=  a/circle
      leaf+~(pr-full pr a)
    [%rose [" " ~ ~] [hed >who< [%rose [", " "to " ~] cis] ~]]~
  ::
  ++  tr-body                                           ::<  message content
    ::>  long-form display of message contents, specific
    ::>  to each speech type.
    ::
    |-  ^-  sole-effect
    ?-  -.sep
        $lin
      tan+~[leaf+"{?:(pat.sep "@ " "")}{(trip msg.sep)}"]
      ::
        $url
      url+(crip (apix:en-purl:html url.sep))
      ::
        $exp
      tan+~[rose+[" " ~ ~]^[leaf+"# {(trip exp.sep)}" res.sep]]
      ::
        $ire
      =+  gam=(recall top.sep)
      ?~  gam  $(sep sep.sep)
      :-  %mor
      =-  [- $(sep sep.sep) ~]
      :-  %tan
      ::TODO  in "wrong" order because they get printed in reverse...
      :~  :+  %rose  [" " ~ ~]
          (turn (~(tr-text tr sef u.gam) termwidth) |=(t/tape [%leaf t]))
          [%leaf :(weld "in reply to: " (cite:title aut.u.gam) ": ")]
      ==
      ::
        $fat
      [%mor $(sep sep.sep) tan+(tr-tach tac.sep) ~]
      ::
        $inv
      :-  %tan
      :_  ~
      :-  %leaf
      %+  weld
        ?:  inv.sep
          "you have been invited to "
        "you have been banished from "
      ~(cr-phat cr cir.sep)
      ::
        $app
      tan+~[rose+[": " ~ ~]^~[leaf+"[{(trip app.sep)}]" leaf+(trip msg.sep)]]
    ==
  ::
  ++  tr-tach                                           ::<  attachment
    ::>  renders an attachment.
    ::
    |=  a/attache
    ^-  tang
    ?-  -.a
      $name  (welp $(a tac.a) leaf+"={(trip nom.a)}" ~)
      $tank  +.a
      $text  (turn (flop +.a) |=(b/cord leaf+(trip b)))
    ==
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
      |-
      :-  '_'
      ?.  ?=({$' ' *} t.txt)
        t.txt
      $(txt t.txt)
    ?~  t.txt  "…"
    [i.txt $(txt t.txt)]
  ::
  ++  tr-text                                           ::<  compact contents
    ::>  renders just the most important data of the
    ::>  message. if possible, these stay within a single
    ::>  line.
    ::
    |=  wyd/@ud
    ^-  (list tape)
    ?-  -.sep
        $fat
      %+  weld  $(sep sep.sep)
      ^-  (list tape)
      ?+  -.tac.sep  ["attached: ..." ~]
        $name  ["attached: {(trip nom.tac.sep)}" ~]
      ==
      ::
        $exp
      ::TODO  print truncated res on its own line.
      :_  ~
      (tr-chow wyd '#' ' ' (trip exp.sep))
      ::
        $ire
      $(sep sep.sep)
      ::
        $url
      :_  ~
      =+  ful=(apix:en-purl:html url.sep)
      =.  wyd  (sub wyd 2)  ::  account for prefix.
      ::  if the full url fits, just render it.
      ?:  (gte wyd (lent ful))  ['/' ' ' ful]
      ::  if it doesn't, prefix with _ and render just (the tail of) the domain.
      :+  '/'  '_'
      =+  hok=r.p.p.url.sep
      ~!  hok
      =-  (swag [a=(sub (max wyd (lent -)) wyd) b=wyd] -)
      ^-  tape
      =<  ?:  ?=($& -.hok)
            (reel p.hok .)
          +:(scow %if p.hok)
      |=  {a/knot b/tape}
      ?~  b  (trip a)
      (welp b '.' (trip a))
      ::
        $lin
      ::  glyph prefix
      =/  pef
        ?:  pat.sep  " "
        %~  ar-pref  ar
          ?:  =(who our.bol)  aud
          (~(del in aud) [%& who (main who)])
        ==
      =.  wyd  (sub wyd (lent pef))
      =/  txt  (tuba (trip msg.sep))
      |-  ^-  (list tape)
      ?~  txt  ~
      =/  end
        ?:  (lte (lent txt) wyd)  (lent txt)
        =+  ace=(find " " (flop (scag +(wyd) `(list @c)`txt)))
        ?~  ace  wyd
        (sub wyd u.ace)
      :-  (weld pef (tufa (scag end `(list @c)`txt)))
      $(txt (slag +(end) `(list @c)`txt), pef (reap (lent pef) ' '))
      ::
        $inv
      :_  ~
      %+  tr-chow  wyd
      %+  weld
        ?:  inv.sep
          " invited you to "
        " banished you from "
      ~(cr-phat cr cir.sep)
      ::
        $app
      :_  ~
      (tr-chow wyd "[{(trip app.sep)}]: {(trip msg.sep)}")
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
  ^-  (quip move _+>)
  ?.  (team:title src.bol our.bol)
    ~&  [%peer-talk-reader-stranger src.bol]
    [~ +>]
  ?.  ?=({$sole *} pax)
    ~&  [%peer-talk-reader-strange pax]
    [~ +>]
  ta-done:ta-console:ta
::
++  diff-talk-prize                                     ::<  accept query answer
  ::>
  ::
  |=  {way/wire piz/prize}
  ^-  (quip move _+>)
  ta-done:(ta-take:ta piz)
::
++  diff-talk-rumor                                     ::<  accept query change
  ::>
  ::
  |=  {way/wire rum/rumor}
  ^-  (quip move _+>)
  ta-done:(ta-hear:ta rum)
::
++  poke-sole-action                                    ::<  accept console
  ::>  incoming sole action. process it.
  ::
  |=  act/sole-action
  ta-done:(ta-sole:ta act)
::
++  coup-reader-action                                                ::<  accept n/ack
  ::>
  ::
  |=  {wir/wire fal/(unit tang)}
  ^-  (quip move _+>)
  ?~  fal  [~ +>]
  %-  (slog leaf+"action failed: " u.fal)
  [~ +>]
--
