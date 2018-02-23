::                                                      ::  ::
::::  /app/talk/hoon                                    ::  ::
  ::                                                    ::  ::
::
::TODO  maybe keep track of received grams per circle, too?
::
::TODO  [type query] => [press tab to cycle search results, newest-first]
::      => [escape to clear]
::
::  This client implementation makes use of the %inbox
::  for all its subscriptions and messaging. All
::  rumors received are exclusively about the %inbox,
::  since that's the only thing the client ever
::  subscribes to.
::
/-    hall, sole                                        ::  structures
/+    hall, sole                                        ::  libraries
/=    seed  /~  !>(.)
::
::::
  ::
=,  hall
=,  sole
=>  :>  #
    :>  #  %arch
    :>  #
    :>    data structures
    ::
    |%
    ++  state                                           :>  application state
      $:  ::  messaging state                           ::
          grams/(list telegram)                         :<  all history
          known/(map serial @ud)                        :<  messages heard
          last/@ud                                      :<  last heard
          count/@ud                                     :<  (lent grams)
          sources/(set circle)                          :<  our subscriptions
          ::  circle details                            ::
          remotes/(map circle group)                    :<  remote presences
          mirrors/(map circle config)                   :<  remote configs
          ::  ui state                                  ::
          nicks/(map ship nick)                         :<  human identities
          bound/(map audience char)                     :<  bound circle glyphs
          binds/(jug char audience)                     :<  circle glyph lookup
          cli/shell                                     :<  interaction state
      ==                                                ::
    ++  shell                                           :>  console session
      $:  id/bone                                       :<  identifier
          latest/@ud                                    :<  latest shown msg num
          say/sole-share                                :<  console state
          active/audience                               :<  active targets
          settings/(set term)                           :<  frontend settings
          width/@ud                                     :<  display width
          timez/(pair ? @ud)                            :<  timezone adjustment
      ==                                                ::
    ++  move  (pair bone card)                          :<  all actions
    ++  lime                                            :>  diff fruit
      $%  {$sole-effect sole-effect}                    ::
      ==                                                ::
    ++  pear                                            :>  poke fruit
      $%  {$hall-command command}                       ::
          {$hall-action action}                         ::
      ==                                                ::
    ++  card                                            :>  general card
      $%  {$diff lime}                                  ::
          {$poke wire dock pear}                        ::
          {$peer wire dock path}                        ::
          {$pull wire dock $~}                          ::
      ==                                                ::
    ++  work                                            :>  interface action
      $%  ::  circle management                         ::
          {$join (map circle range)}                    :<  subscribe to
          {$leave audience}                             :<  unsubscribe from
          {$create security name cord}                  :<  create circle
          {$delete name (unit cord)}                    :<  delete circle
          {$depict name cord}                           :<  change description
          {$filter name ? ?}                            :<  change message rules
          {$invite name (set ship)}                     :<  give permission
          {$banish name (set ship)}                     :<  deny permission
          {$source name (map circle range)}             :<  add source
          {$unsource name (map circle range)}           :<  remove source
          ::  personal metadata                         ::
          {$attend audience (unit presence)}            :<  set our presence
          {$name audience human}                        :<  set our name
          ::  messaging                                 ::
          {$say (list speech)}                          :<  send message
          {$eval cord hoon}                             :<  send #-message
          {$target p/audience q/(unit work)}            :<  set active targets
          {$reply $@(@ud {@u @ud}) (list speech)}       :<  reply to
          ::  displaying info                           ::
          {$number $@(@ud {@u @ud})}                    :<  relative/absolute
          {$who audience}                               :<  presence
          {$what (unit $@(char audience))}              :<  show bound glyph
          {$circles $~}                                 :<  show our circles
          {$sources circle}                             :<  show active sources
          ::  ui settings                               ::
          {$bind char (unit audience)}                  :<  bind glyph
          {$unbind char (unit audience)}                :<  unbind glyph
          {$nick (unit ship) (unit cord)}               :<  un/set/show nick
          {$set term}                                   :<  enable setting
          {$unset term}                                 :<  disable setting
          {$width @ud}                                  :<  change display width
          {$timez ? @ud}                                :<  adjust shown times
          ::  miscellaneous                             ::
          {$show circle}                                :<  show membership
          {$hide circle}                                :<  hide membership
          {$help $~}                                    :<  print usage info
      ==                                                ::
    ++  glyphs  `wall`~[">=+-" "}),." "\"'`^" "$%&@"]   :<  circle char pool '
    --
::
:>  #
:>  #  %work
:>  #
:>    functional cores and arms.
::
|_  {bol/bowl:gall $0 state}
::
:>  #  %transition
:>    prep transition
+|
::
++  prep
  :>  adapts state
  ::
  =>  |%
      ++  states
        $%({$0 s/state})
      --
  =|  mos/(list move)
  |=  old/(unit states)
  ^-  (quip move _..prep)
  ?~  old
    ta-done:ta-init:ta
  ?-  -.u.old
      $0
    [mos ..prep(+<+ u.old)]
  ==
::
:>  #
:>  #  %utility
:>  #
:>    small utility functions.
+|
::
++  self
  (true-self our.bol)
::
++  server
  :>  our hall instance
  ^-  dock
  [self %hall]
::
++  inbox
  :>    client's circle name
  :>
  :>  produces the name of the circle used by this
  :>  client for all its operations
  ^-  name
  %inbox
::
++  incir
  :>    client's circle
  :>
  :>  ++inbox, except a full circle.
  ^-  circle
  [self inbox]
::
++  renum
  :>  find the grams list index for gram with serial.
  |=  ser/serial
  ^-  (unit @ud)
  =+  num=(~(get by known) ser)
  ?~  num  ~
  `(sub count +(u.num))
::
++  recall
  :>  find a known gram with serial {ser}.
  |=  ser/serial
  ^-  (unit telegram)
  =+  num=(renum ser)
  ?~  num  ~
  `(snag u.num grams)
::
++  bound-from-binds
  :>    bound from binds
  :>
  :>  using a mapping of character to audiences, create
  :>  a mapping of audience to character.
  ::
  |=  bin/_binds
  ^+  bound
  %-  ~(gas by *(map audience char))
  =-  (zing -)
  %+  turn  ~(tap by bin)
  |=  {a/char b/(set audience)}
  (turn ~(tap by b) |=(c/audience [c a]))
::
++  glyph
  :>  finds a new glyph for assignment.
  ::
  |=  idx/@
  =<  cha
  %+  reel  glyphs
  |=  {all/tape ole/{cha/char num/@}}
  =+  new=(snag (mod idx (lent all)) all)
  =+  num=~(wyt in (~(get ju binds) new))
  ?~  cha.ole  [new num]
  ?:  (lth num.ole num)
    ole
  [new num]
::
++  peer-client
  :>  ui state peer move
  ^-  move
  :*  ost.bol
      %peer
      /server/client
      server
      /client
  ==
::
++  peer-inbox
  ^-  move
  :*  ost.bol
      %peer
      /server/inbox
      server
    ::
      %+  welp  /circle/[inbox]/grams/config/group
      ?.  =(0 count)
        [(scot %ud last) ~]
      =+  history-days=~d5
      [(scot %da (sub now.bol history-days)) ~]
  ==
::
:>  #
:>  #  %engines
:>  #
:>    main cores.
+|
::
++  ta
  :>    per transaction
  :>
  :>  for every transaction/event (poke, peer etc.)
  :>  talk receives, the ++ta transaction core is
  :>  called.
  :>  in processing transactions, ++ta may modify app
  :>  state, or create moves. these moves get produced
  :>  upon finalizing the core's with with ++ta-done.
  :>  when making changes to the shell, the ++sh core is
  :>  used.
  ::
  |_  :>  moves:  moves created by core operations.
      ::
      moves/(list move)
  ::
  :>  #  %resolve
  +|
  ::
  ++  ta-done
    :>    resolve core
    :>
    :>  produces the moves stored in ++ta's moves.
    :>  %sole-effect moves get squashed into a %mor.
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
      ?~  t.q.yop  `i.q.yop                             ::  single sole-effect
      `[%mor (flop q.yop)]                              ::  more sole-effects
    ::  produce moves or sole-effects and moves.
    ?~  foc  moz
    ?~  id.cli  ~&(%client-no-sole moz)
    [[id.cli %diff %sole-effect u.foc] moz]
  ::
  :>  #
  :>  #  %emitters
  :>  #
  :>    arms that create outward changes.
  +|
  ::
  ++  ta-emil
    :>    emit move list
    :>
    :>  adds multiple moves to the core's list.
    :>  flops to emulate ++ta-emit.
    ::
    |=  mol/(list move)
    %_(+> moves (welp (flop mol) moves))
  ::
  ++  ta-emit
    :>  adds a move to the core's list.
    ::
    |=  mov/move
    %_(+> moves [mov moves])
  ::
  :>  #
  :>  #  %interaction-events
  :>  #
  :>    arms that apply events we received.
  +|
  ::
  ++  ta-init
    :>  subscribes to our hall.
    ::
    %-  ta-emil
    ^-  (list move)
    ~[peer-client peer-inbox]
  ::
  ++  ta-take
    :>  accept prize
    ::
    |=  piz/prize
    ^+  +>
    ?+  -.piz  +>
        $client
      %=  +>
        binds   gys.piz
        bound   (bound-from-binds gys.piz)
        nicks   nis.piz
      ==
    ::
        $circle
      %.  nes.piz
      %=  ta-unpack
        sources   (~(run in src.loc.cos.piz) head)
        mirrors   (~(put by rem.cos.piz) incir loc.cos.piz)
        remotes   (~(put by rem.pes.piz) incir loc.pes.piz)
      ==
    ==
  ::
  ++  ta-hear
    :>  apply change
    ::
    |=  rum/rumor
    ^+  +>
    ?+  -.rum  +>
        $client
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
  ++  ta-change-circle
    :>  apply circle change
    ::
    |=  rum/rumor-story
    ^+  +>
    ?+  -.rum
        ~&([%unexpected-circle-rumor -.rum] +>)
    ::
        $gram
      (ta-open nev.rum)
    ::
        $config
      =+  cur=(fall (~(get by mirrors) cir.rum) *config)
      =.  +>.$
        =<  sh-done
        %-  ~(sh-show-config sh cli)
        [cir.rum cur dif.rum]
      =?  +>.$
          ?&  ?=($source -.dif.rum)
              add.dif.rum
              =(cir.rum incir)
          ==
        =*  cir  cir.src.dif.rum
        =+  ren=~(cr-phat cr cir)
        =+  gyf=(~(get by bound) [cir ~ ~])
        =<  sh-done
        =/  sho
          ::  only present if we're here indefinitely.
          =*  ran  ran.src.dif.rum
          ?.  |(?=($~ ran) ?=($~ tal.u.ran))
            ~(. sh cli)
          %-  ~(sh-act sh cli)
          [%notify [cir ~ ~] `%hear]
        ?^  gyf
          (sh-note:sho "has glyph {[u.gyf ~]} for {ren}")
        ::  we use the rendered circle name to determine
        ::  the glyph for higher glyph consistency when
        ::  federating.
        =+  cha=(glyph (mug ren))
        (sh-work:sho %bind cha `[cir ~ ~])
      %=  +>.$
          sources
        ?.  &(?=($source -.dif.rum) =(cir.rum incir))
          sources
        %.  cir.src.dif.rum
        ?:  add.dif.rum
          ~(put in sources)
        ~(del in sources)
      ::
          mirrors
        ?:  ?=($remove -.dif.rum)  (~(del by mirrors) cir.rum)
        %+  ~(put by mirrors)  cir.rum
        (change-config cur dif.rum)
      ==
    ::
        $status
      =+  rem=(fall (~(get by remotes) cir.rum) *group)
      =+  cur=(fall (~(get by rem) who.rum) *status)
      =.  +>.$
        =<  sh-done
        %-  ~(sh-show-status sh cli)
        [cir.rum who.rum cur dif.rum]
      %=  +>.$
          remotes
        %+  ~(put by remotes)  cir.rum
        ?:  ?=($remove -.dif.rum)  (~(del by rem) who.rum)
        %+  ~(put by rem)  who.rum
        (change-status cur dif.rum)
      ==
    ==
  ::
  ++  ta-change-glyph
    :>  applies new set of glyph bindings.
    ::
    |=  {bin/? gyf/char aud/audience}
    ^+  +>
    =+  nek=(change-glyphs binds bin gyf aud)
    ?:  =(nek binds)  +>.$                              ::  no change
    =.  binds  nek
    =.  bound  (bound-from-binds nek)
    sh-done:~(sh-prod sh cli)
  ::
  :>  #
  :>  #  %messages
  :>  #
  :>    storing and updating messages.
  +|
  ::
  ++  ta-unpack
    :>    open envelopes
    :>
    :>  the client currently doesn't care about nums.
    ::
    |=  nes/(list envelope)
    ^+  +>
    ?~  nes  +>
    $(nes t.nes, +> (ta-open i.nes))
  ::
  ++  ta-open
    :>  learn message from an envelope.
    ::
    |=  nev/envelope
    ^+  +>
    =?  last  (gth num.nev last)  num.nev
    (ta-learn gam.nev)
  ::
  ++  ta-learn
    :>    save/update message
    :>
    :>  store an incoming telegram, updating if it
    :>  already exists.
    ::
    |=  gam/telegram
    ^+  +>
    =+  old=(renum uid.gam)
    ?~  old
      (ta-append gam)                                ::  add
    (ta-revise u.old gam)                            ::  modify
  ::
  ++  ta-append
    :>  store a new telegram.
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
  ++  ta-revise
    :>  modify a telegram we know.
    ::
    |=  {num/@ud gam/telegram}
    =+  old=(snag num grams)
    ?:  =(gam old)  +>.$                                ::  no change
    =.  grams
      %+  welp
      (scag num grams)
      [gam (slag +(num) grams)]
    ?:  =(sep.gam sep.old)  +>.$                        ::  no worthy change
    =<  sh-done
    (~(sh-gram sh cli) gam)
  ::
  :>  #
  :>  #  %console
  :>  #
  :>    arms for shell functionality.
  +|
  ::
  ++  ta-console
    :>  initialize the shell of this client.
    ::
    ^+  .
    =/  she/shell
      %*(. *shell id ost.bol, active (sy incir ~), width 80)
    sh-done:~(sh-prod sh she)
  ::
  ++  ta-sole
    :>  apply sole input
    ::
    |=  act/sole-action
    ^+  +>
    ?.  =(id.cli ost.bol)
      ~&(%strange-sole !!)
    sh-done:(~(sh-sole sh cli) act)
  ::
  ++  sh
    :>    per console
    :>
    :>  shell core, responsible for handling user input
    :>  and the related actions, and outputting changes
    :>  to the cli.
    ::
    |_  $:  :>  she: console state.
            ::
            she/shell
        ==
    ::
    :>  #  %resolve
    +|
    ::
    ++  sh-done
      :>  stores changes to the cli.
      ::
      ^+  +>
      +>(cli she)
    ::
    :>  #
    :>  #  %emitters
    :>  #
    :>    arms that create outward changes.
    +|
    ::
    ++  sh-fact
      :>  adds a console effect to ++ta's moves.
      ::
      |=  fec/sole-effect
      ^+  +>
      +>(moves [[id.she %diff %sole-effect fec] moves])
    ::
    ++  sh-act
      :>  adds an action to ++ta's moves.
      ::
      |=  act/action
      ^+  +>
      %=  +>
          moves
        :_  moves
        :*  ost.bol
            %poke
            /client/action
            server
            [%hall-action act]
        ==
      ==
    ::
    :>  #
    :>  #  %cli-interaction
    :>  #
    :>    processing user input as it happens.
    +|
    ::
    ++  sh-sole
      :>  applies sole action.
      ::
      |=  act/sole-action
      ^+  +>
      ?-  -.act
        $det  (sh-edit +.act)
        $clr  ..sh-sole :: (sh-pact ~) :: XX clear to PM-to-self?
        $ret  sh-obey
      ==
    ::
    ++  sh-edit
      :>    apply sole edit
      :>
      :>  called when typing into the cli prompt.
      :>  applies the change and does sanitizing.
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
    ++  sh-read
      :>    command parser
      :>
      :>  parses the command line buffer. produces work
      :>  items which can be executed by ++sh-work.
      ::
      =<  work
      :>  #  %parsers
      :>    various parsers for command line input.
      |%
      ++  expr
        :>  [cord hoon]
        |=  tub/nail  %.  tub
        %+  stag  (crip q.tub)
        wide:(vang & [&1:% &2:% (scot %da now.bol) |3:%])
      ::
      ++  dare
        :>  @dr
        %+  sear
          |=  a/coin
          ?.  ?=({$$ $dr @} a)  ~
          (some `@dr`+>.a)
        nuck:so
      ::
      ++  ship  ;~(pfix sig fed:ag)                     :<  ship
      ++  shiz                                          :<  ship set
        %+  cook
          |=(a/(list ^ship) (~(gas in *(set ^ship)) a))
        (most ;~(plug com (star ace)) ship)
      ::
      ++  cire                                          :<  local circle
        ;~(pfix cen urs:ab)
      ::
      ++  circ                                          :<  circle
        ;~  pose
          (cold incir col)
          ;~(pfix cen (stag self urs:ab))
          ;~(pfix fas (stag (sein:title self) urs:ab))
        ::
          %+  cook
            |=  {a/@p b/(unit term)}
            [a ?^(b u.b %inbox)]
          ;~  plug
            ship
            (punt ;~(pfix fas urs:ab))
          ==
        ==
      ::
      ++  circles-flat                                  :<  collapse mixed list
        |=  a/(list (each circle (set circle)))
        ^-  (set circle)
        ?~  a  ~
        ?-  -.i.a
          $&  (~(put in $(a t.a)) p.i.a)
          $|  (~(uni in $(a t.a)) p.i.a)
        ==
      ::
      ++  cirs                                          :<  non-empty circles
        %+  cook  circles-flat
        %+  most  ;~(plug com (star ace))
        (^pick circ (sear sh-glyf glyph))
      ::
      ++  drat
        :>  @da or @dr
        :>
        :>  pas: whether @dr's are in the past or not.
        |=  pas/?
        =-  ;~(pfix sig (sear - crub:so))
        |=  a/^dime
        ^-  (unit @da)
        ?+  p.a  ~
          $da   `q.a
          $dr   :-  ~
                %.  [now.bol q.a]
                ?:(pas sub add)
        ==
      ::
      ++  pont                                          :<  point for range
        :>  hed: whether this is the head or tail point.
        |=  hed/?
        ;~  pose
          (cold [%da now.bol] (jest 'now'))
          (stag %da (drat hed))
          (stag %ud dem:ag)
        ==
      ::
      ++  rang                                          :<  subscription range
        =+  ;~  pose
              (cook some ;~(pfix fas (pont |)))
              (easy ~)
            ==
        ;~  pose
          (cook some ;~(plug ;~(pfix fas (pont &)) -))
          (easy ~)
        ==
      ::
      ++  sorz                                          :<  non-empty sources
        %+  cook  ~(gas by *(map circle range))
        (most ;~(plug com (star ace)) ;~(plug circ rang))
      ::
      ++  pick                                          :<  message reference
        ;~(pose nump (cook lent (star sem)))
      ::
      ++  nump                                          :<  number reference
        ;~  pose
          ;~(pfix hep dem:ag)
          ;~  plug
            (cook lent (plus (just '0')))
            ;~(pose dem:ag (easy 0))
          ==
          (stag 0 dem:ag)
        ==
      ::
      ++  pore                                          :<  security
        (perk %channel %village %journal %mailbox ~)
      ::
      ++  lobe                                          :<  y/n loob
        ;~  pose
          (cold %& ;~(pose (jest 'y') (jest '&') (just 'true')))
          (cold %| ;~(pose (jest 'n') (jest '|') (just 'false')))
        ==
      ::
      ++  message                                       :<  exp, lin or url msg
        ;~  pose
          ;~(plug (cold %eval hax) expr)
          (stag %say speeches)
        ==
      ::
      ++  speeches                                      :<  lin or url msgs
        %+  most  (jest '•')
        ;~  pose
          (stag %url aurf:de-purl:html)
          :(stag %lin & ;~(pfix pat text))
          :(stag %lin | ;~(less sem hax text))
        ==
      ::
      ++  text                                          :<  msg without break
        %+  cook  crip
        (plus ;~(less (jest '•') next))
      ::
      ++  nick  (cook crip (plus next))                 :<  nickname
      ++  glyph  (mask "/\\\{(<!?{(zing glyphs)}")      :<  circle postfix
      ++  setting                                       :<  setting flag
        %-  perk  :~
          %nicks
          %quiet
          %notify
          %showtime
        ==
      ++  work                                          :<  full input
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
            ;~(pfix ace cire)
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
          ;~((glue ace) (perk %unsource ~) cire sorz)
          ::TODO  why do these nest-fail when doing perk with multiple?
        ::
        ::  personal metadata
        ::
          ;~  (glue ace)
            (perk %attend ~)
            cirs
            ;~  pose
              (cold ~ sig)
              (cook some (perk %gone %idle %hear %talk ~))
            ==
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
          ;~  plug
            (perk %what ~)
            ;~  pose
              ;~(pfix ace (cook some ;~(pose glyph cirs)))
              (easy ~)
            ==
          ==
        ::
          ;~(plug (perk %circles ~) (easy ~))
        ::
          ;~((glue ace) (perk %sources ~) circ)
        ::
          ;~((glue ace) (perk %show ~) circ)
        ::
          ;~((glue ace) (perk %hide ~) circ)
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
          ;~(plug (cold %width (jest 'set width ')) dem:ag)
        ::
          ;~  plug
            (cold %timez (jest 'set timezone '))
          ::
            ;~  pose
              (cold %| (just '-'))
              (cold %& (just '+'))
            ==
          ::
            %+  sear
              |=  a/@ud
              ^-  (unit @ud)
              ?:  &((gte a 0) (lte a 14))
              `a  ~
            dem:ag
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
    ++  sh-sane
      :>    sanitize input
      :>
      :>  parses cli prompt input using ++sh-read and
      :>  sanitizes when invalid.
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
    ++  sh-slug
      :>  corrects invalid prompt input.
      ::
      |=  {lit/(list sole-edit) err/(unit @u)}
      ^+  +>
      ?~  lit  +>
      =^  lic  say.she
          (~(transmit sole say.she) `sole-edit`?~(t.lit i.lit [%mor lit]))
      (sh-fact [%mor [%det lic] ?~(err ~ [%err u.err]~)])
    ::
    ++  sh-obey
      :>    apply result
      :>
      :>  called upon hitting return in the prompt. if
      :>  input is invalid, ++sh-slug is called.
      :>  otherwise, the appropriate work is done and
      :>  the entered command (if any) gets displayed
      :>  to the user.
      ::
      =+  fix=(sh-sane [%nop ~] buf.say.she)
      ?^  lit.fix
        (sh-slug fix)
      =+  jub=(rust (tufa buf.say.she) sh-read)
      ?~  jub  (sh-fact %bel ~)
      %.  u.jub
      =<  sh-work
      =+  buf=buf.say.she
      =?  ..sh-obey  &(?=({$';' *} buf) !?=($reply -.u.jub))
        (sh-note (tufa `(list @)`buf))
      =^  cal  say.she  (~(transmit sole say.she) [%set ~])
      %+  sh-fact  %mor
      :~  [%nex ~]
          [%det cal]
      ==
    ::
    :>  #
    :>  #  %user-action
    :>  #
    :>    processing user actions.
    +|
    ::
    ++  sh-work
      :>    do work
      :>
      :>  implements worker arms for different talk
      :>  commands.
      :>  worker arms must produce updated state.
      ::
      |=  job/work
      ^+  +>
      =<  work
      |%
      ::
      :>  #
      :>  #  %helpers
      :>  #
      +|
      ::
      ++  work
        :>  call correct worker
        ?-  -.job
          ::  circle management
          $join    (join +.job)
          $leave   (leave +.job)
          $create  (create +.job)
          $delete  (delete +.job)
          $depict  (depict +.job)
          $filter  (filter +.job)
          $invite  (permit & +.job)
          $banish  (permit | +.job)
          $source  (source & +.job)
          $unsource  (source | +.job)
          ::  personal metadata
          $attend  (attend +.job)
          $name    (set-name +.job)
          ::  messaging
          $say     (say +.job)
          $eval    (eval +.job)
          $target  (target +.job)
          $reply   (reply +.job)
          ::  displaying info
          $number  (number +.job)
          $who     (who +.job)
          $what    (what +.job)
          $circles  circles
          $sources  (list-sources +.job)
          ::  ui settings
          $bind    (bind +.job)
          $unbind  (unbind +.job)
          $nick    (nick +.job)
          $set     (wo-set +.job)
          $unset   (unset +.job)
          $width   (width +.job)
          $timez   (timez +.job)
          ::  miscelaneous
          $show    (public & +.job)
          $hide    (public | +.job)
          $help    help
        ==
      ::
      ++  activate
        :>  prints message details.
        ::
        |=  gam/telegram
        ^+  ..sh-work
        =+  tay=~(. tr settings.she gam)
        =.  ..sh-work  (sh-fact tr-fact:tay)
        sh-prod(active.she aud.gam)
      ::
      ++  deli
        :>  gets absolute message number from relative.
        ::
        |=  {max/@ud nul/@u fin/@ud}
        ^-  @ud
        =+  dog=|-(?:(=(0 fin) 1 (mul 10 $(fin (div fin 10)))))
        =.  dog  (mul dog (pow 10 nul))
        =-  ?:((lte - max) - (sub - dog))
        (add fin (sub max (mod max dog)))
      ::
      ++  set-glyph
        :>    new glyph binding
        :>
        :>  applies glyph binding to our state and sends
        :>  an action.
        ::
        |=  {cha/char aud/audience}
        =:  bound  (~(put by bound) aud cha)
            binds  (~(put ju binds) cha aud)
        ==
        sh-prod:(sh-act %glyph cha aud &)
      ::
      ++  unset-glyph
        :>    remote old glyph binding
        :>
        :>  removes either {aud} or all bindings on a
        :>  glyph and sends an action.
        ::
        |=  {cha/char aud/(unit audience)}
        =/  ole/(set audience)
          ?^  aud  [u.aud ~ ~]
          (~(get ju binds) cha)
        =.  ..sh-work  (sh-act %glyph cha (fall aud ~) |)
        |-  ^+  ..sh-work
        ?~  ole  ..sh-work
        =.  ..sh-work  $(ole l.ole)
        =.  ..sh-work  $(ole r.ole)
        %=  ..sh-work
          bound  (~(del by bound) n.ole)
          binds  (~(del ju binds) cha n.ole)
        ==
      ::
      ++  reverse-nicks
        :>  finds all ships whose handle matches {nym}.
        ::
        |=  nym/^nick
        ^-  (list ship)
        %+  murn  ~(tap by nicks)
        |=  {p/ship q/^nick}
        ?.  =(q nym)  ~
        [~ u=p]
      ::
      ++  hoon-head
        :>    eval data
        :>
        :>  makes a vase of environment data to evaluate
        :>  against (for #-messages).
        ::
        ^-  vase
        !>  ^-  {our/@p now/@da eny/@uvI}
        [self now.bol (shas %eny eny.bol)]
      ::
      :>  #
      :>  #  %circle-management
      :>  #
      +|
      ::
      ++  join
        :>    %join
        :>
        :>  change local mailbox config to include
        :>  subscriptions to {pas}.
        ::
        |=  pos/(map circle range)
        ^+  ..sh-work
        =+  pas=~(key by pos)
        =.  ..sh-work
          sh-prod(active.she pas)
        ::  default to a day of backlog
        =.  pos
          %-  ~(run by pos)
          |=  r/range
          ?~(r `[da+(sub now.bol ~d1) ~] r)
        (sh-act %source inbox & pos)
      ::
      ++  leave
        :>    %leave
        :>
        :>  change local mailbox config to exclude
        :>  subscriptions to {pas}.
        ::
        |=  pas/(set circle)
        ^+  ..sh-work
        ::  remove *all* sources relating to {pas}.
        =/  pos
          %-  ~(gas in *(set ^source))
          %-  zing
          =/  sos
            =-  ~(tap in src:-)
            (fall (~(get by mirrors) incir) *config)
          %+  turn  ~(tap in pas)
          |=  c/circle
          %+  skim  sos
          |=(s/^source =(cir.s c))
        =.  ..sh-work
          (sh-act %source inbox | pos)
        (sh-act %notify pas ~)
      ::
      ++  create
        :>    %create
        :>
        :>  creates circle {nom} with specified config.
        ::
        |=  {sec/security nom/name txt/cord}
        ^+  ..sh-work
        =.  ..sh-work
          (sh-act %create nom txt sec)
        (join [[[self nom] ~] ~ ~])
      ::
      ++  delete
        :>    %delete
        :>
        :>  deletes our circle {nom}, after optionally
        :>  sending a last announce message {say}.
        ::
        |=  {nom/name say/(unit cord)}
        ^+  ..sh-work
        (sh-act %delete nom say)
      ::
      ++  depict
        :>    %depict
        :>
        :>  changes the description of {nom} to {txt}.
        ::
        |=  {nom/name txt/cord}
        ^+  ..sh-work
        (sh-act %depict nom txt)
      ::
      ++  permit
        :>    %invite / %banish
        :>
        :>  invites or banishes {sis} to/from our
        :>  circle {nom}.
        ::
        |=  {inv/? nom/name sis/(set ship)}
        ^+  ..sh-work
        =.  ..sh-work  (sh-act %permit nom inv sis)
        =-  (sh-act %phrase - [%inv inv [self nom]]~)
        %-  ~(rep in sis)
        |=  {s/ship a/audience}
        (~(put in a) [s %inbox])
      ::
      ++  filter
        |=  {nom/name cus/? utf/?}
        ^+  ..sh-work
        (sh-act %filter nom cus utf)
      ::
      ++  source
        :>    %source
        :>
        :>  adds {pas} to {nom}'s src.
        ::
        |=  {sub/? nom/name pos/(map circle range)}
        ^+  ..sh-work
        (sh-act %source nom sub pos)
      ::
      :>  #
      :>  #  %personal-metadata
      :>  #
      +|
      ::
      ++  attend
        :>  sets our presence to {pec} for {aud}.
        ::
        |=  {aud/audience pec/(unit presence)}
        ^+  ..sh-work
        (sh-act %notify aud pec)
      ::
      ++  set-name
        :>  sets our name to {man} for {aud}.
        ::
        |=  {aud/audience man/human}
        ^+  ..sh-work
        (sh-act %naming aud man)
      ::
      :>  #
      :>  #  %messaging
      :>  #
      +|
      ::
      ++  say
        :>  sends message.
        ::
        |=  sep/(list speech)
        ^+  ..sh-work
        (sh-act %phrase active.she sep)
      ::
      ++  eval
        :>    run
        :>
        :>  executes {exe} and sends both its code and
        :>  result.
        ::
        |=  {txt/cord exe/hoon}
        =>  |.([(sell (slap (slop hoon-head seed) exe))]~)
        =+  tan=p:(mule .)
        (say [%exp txt tan] ~)
      ::
      ++  target
        :>    %target
        :>
        :>  sets messaging target, then execute {woe}.
        ::
        |=  {aud/audience woe/(unit ^work)}
        ^+  ..sh-work
        =.  ..sh-pact  (sh-pact aud)
        ?~(woe ..sh-work work(job u.woe))
      ::
      ++  reply
        :>    %reply
        :>
        :>  send a reply to the selected message.
        ::
        |=  {num/$@(@ud {p/@u q/@ud}) sep/(list speech)}
        ^+  ..sh-work
        ::  =- (say (turn ... [%ire - s])) nest-fails on the - ???
        ::TODO  what's friendlier, reply-to-null or error?
        =/  ser/serial
          ?@  num
            ?:  (gte num count)  0v0
            uid:(snag num grams)
          ?:  (gth q.num count)  0v0
          ?:  =(count 0)  0v0
          =+  msg=(deli (dec count) num)
          uid:(snag (sub count +(msg)) grams)
        (say (turn sep |=(s/speech [%ire ser s])))
      ::
      :>  #
      :>  #  %displaying-info
      :>  #
      +|
      ::
      ++  who
        :>    %who
        :>
        :>  prints presence lists for {cis} or all.
        ::
        |=  cis/(set circle)  ^+  ..sh-work
        =<  (sh-fact %mor (murn (sort ~(tap by remotes) aor) .))
        |=  {cir/circle gop/group}  ^-  (unit sole-effect)
        ?.  |(=(~ cis) (~(has in cis) cir))  ~
        ?:  =(%mailbox sec.con:(fall (~(get by mirrors) cir) *config))  ~
        ?.  (~(has in sources) cir)  ~
        =-  `[%tan rose+[", " `~]^- leaf+~(cr-full cr cir) ~]
        =<  (murn (sort ~(tap by gop) aor) .)
        |=  {a/ship b/presence c/human}  ^-  (unit tank)
        =?  c  =(han.c `(scot %p a))  [~ tru.c]
        ?-  b
          $gone  ~
          $idle  `leaf+:(weld "idle " (scow %p a) " " (trip (fall han.c '')))
          $hear  `leaf+:(weld "hear " (scow %p a) " " (trip (fall han.c '')))
          $talk  `leaf+:(weld "talk " (scow %p a) " " (trip (fall han.c '')))
        ==
      ::
      ++  what
        :>    %what
        :>
        :>  prints binding details. goes both ways.
        ::
        |=  qur/(unit $@(char audience))
        ^+  ..sh-work
        ?^  qur
          ?^  u.qur
            =+  cha=(~(get by bound) u.qur)
            (sh-fact %txt ?~(cha "none" [u.cha]~))
          =+  pan=~(tap in (~(get ju binds) u.qur))
          ?:  =(~ pan)  (sh-fact %txt "~")
          =<  (sh-fact %mor (turn pan .))
          |=(a/audience [%txt ~(ar-phat ar a)])
        %+  sh-fact  %mor
        %-  ~(rep by binds)
        |=  $:  {gyf/char aus/(set audience)}
                lis/(list sole-effect)
            ==
        %+  weld  lis
        ^-  (list sole-effect)
        %-  ~(rep in aus)
        |=  {a/audience l/(list sole-effect)}
        %+  weld  l
        ^-  (list sole-effect)
        [%txt [gyf ' ' ~(ar-phat ar a)]]~
      ::
      ++  number
        :>    %number
        :>
        :>  finds selected message, expand it.
        ::
        |=  num/$@(@ud {p/@u q/@ud})
        ^+  ..sh-work
        |-
        ?@  num
          ?:  (gte num count)
            (sh-lame "{(scow %s (new:si | +(num)))}: no such telegram")
          =.  ..sh-fact  (sh-fact %txt "? {(scow %s (new:si | +(num)))}")
          (activate (snag num grams))
        ?.  (gte q.num count)
          ?:  =(count 0)
            (sh-lame "0: no messages")
          =+  msg=(deli (dec count) num)
          =.  ..sh-fact  (sh-fact %txt "? {(scow %ud msg)}")
          (activate (snag (sub count +(msg)) grams))
        (sh-lame "…{(reap p.num '0')}{(scow %ud q.num)}: no such telegram")
      ::
      ++  circles
        :>    %circles
        :>
        :>  list all local circles.
        ::
        ^+  ..sh-work
        =/  piz
          =-  .^(prize %gx -)
          %+  weld  /(scot %p our.bol)/hall/(scot %da now.bol)
          /circles/(scot %p our.bol)/hall-prize
        ?>  ?=($circles -.piz)
        %+  sh-fact  %mor
        %+  turn  (sort ~(tap in cis.piz) lth)
        |=  a/name  [%txt "%{(trip a)}"]
      ::
      ++  list-sources
        :>    %sources
        :>
        :>  display the active sources for our circle.
        ::
        |=  cir/circle
        ^+  ..sh-work
        %+  sh-fact  %mor
        %+  turn
          ::  make sure to exclude {nom} itself.
          =-  ~(tap in (~(del in src:-) [cir ~]))
          (fall (~(get by mirrors) cir) *config)
        |=  s/^source
        ^-  sole-effect
        :-  %txt
        %+  weld  ~(cr-phat cr cir.s)
        %+  roll  (range-to-path ran.s)
        |=  {a/@ta b/tape}
        :(weld b "/" (trip a))
      ::
      :>  #
      :>  #  %ui-settings
      :>  #
      +|
      ::
      ++  bind
        :>    %bind
        :>
        :>  binds targets {aud} to the glyph {cha}.
        ::
        |=  {cha/char aud/(unit audience)}
        ^+  ..sh-work
        ?~  aud  $(aud `active.she)
        =+  ole=(~(get by bound) u.aud)
        ?:  =(ole [~ cha])  ..sh-work
        %.  "bound {<cha>} {<u.aud>}"
        sh-note:sh-prod:(set-glyph cha u.aud)
      ::
      ++  unbind
        :>    %unbind
        :>
        :>  unbinds targets {aud} to glyph {cha}.
        ::
        |=  {cha/char aud/(unit audience)}
        ^+  ..sh-work
        ?.  ?|  &(?=(^ aud) (~(has by bound) u.aud))
                &(?=($~ aud) (~(has by binds) cha))
            ==
          ..sh-work
        %.  "unbound {<cha>}"
        sh-note:sh-prod:(unset-glyph cha aud)
      ::
      ++  nick
        :>    %nick
        :>
        :>  either shows, sets or unsets nicknames
        :>  depending on arguments.
        ::
        |=  {her/(unit ship) nym/(unit ^nick)}
        ^+  ..sh-work
        ::  no arguments, show all
        ?:  ?=({$~ $~} +<)
          %+  sh-fact  %mor
          %+  turn  ~(tap by nicks)
          |=  {p/ship q/^nick}
          :-  %txt
          "{<p>}: {<q>}"
        ::  show her nick
        ?~  nym
          ?>  ?=(^ her)
          =+  asc=(~(get by nicks) u.her)
          %+  sh-fact  %txt
          ?~  asc  "{<u.her>} unbound"
          "{<u.her>}: {<u.asc>}"
        ::  show nick ship
        ?~  her
          %+  sh-fact  %mor
          %+  turn  (reverse-nicks u.nym)
          |=  p/ship
          [%txt "{<p>}: {<u.nym>}"]
        %.  [%nick u.her (fall nym '')]
        %=  sh-act
            nicks
          ?~  u.nym
            ::  unset nickname
            (~(del by nicks) u.her)
          ::  set nickname
          (~(put by nicks) u.her u.nym)
        ==
      ::
      ++  wo-set
        :>    %set
        :>
        :>  enables ui setting flag.
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
      ++  unset
        :>    %unset
        :>
        :>  disables ui setting flag.
        ::
        |=  neg/term
        ^+  ..sh-work
        %=  ..sh-work
          settings.she  (~(del in settings.she) neg)
        ==
      ::
      ++  width
        :>    ;set width
        :>
        :>  change the display width in cli.
        ::
        |=  wid/@ud
        ^+  ..sh-work
        ..sh-work(width.she (max 30 wid))
      ::
      ++  timez
        :>    ;set timezone
        :>
        :>  adjust the displayed timestamp.
        ::
        |=  tim/(pair ? @ud)
        ^+  ..sh-work
        ..sh-work(timez.she tim)
      ::
      :>  #
      :>  #  %miscellaneous
      :>  #
      +|
      ::
      ++  public
        :>    show/hide membership
        :>
        :>  adds or removes the circle from the public
        :>  membership list.
        ::
        |=  {add/? cir/circle}
        (sh-act %public add cir)
      ::
      ++  help
        :>    %help
        :>
        :>  prints help message
        ::
        (sh-fact %txt "see http://urbit.org/docs/using/messaging/")
      --
    ::
    ++  sh-pact
      :>    update active aud
      :>
      :>  change currently selected audience to {aud}
      :>  and update the prompt.
      ::
      |=  aud/audience
      ^+  +>
      ::  ensure we can see what we send.
      =+  act=(sh-pare aud)
      ?:  =(active.she act)  +>.$
      sh-prod(active.she act)
    ::
    ++  sh-pare
      :>    adjust target list
      :>
      :>  if the audience {aud} does not contain a
      :>  circle we're subscribed to, add our mailbox
      :>  to the audience (so that we can see our own
      :>  message).
      ::
      |=  aud/audience
      ?:  (sh-pear aud)  aud
      (~(put in aud) incir)
    ::
    ++  sh-pear
      :>    hearback
      :>
      :>  produces true if any circle is included in
      :>  our subscriptions, meaning, we hear messages
      :>  sent to {aud}.
      ::
      |=  aud/audience
      ?~  aud  |
      ?|  (~(has in sources) `circle`n.aud)
          $(aud l.aud)
          $(aud r.aud)
      ==
    ::
    ++  sh-glyf
      :>    decode glyph
      :>
      :>  finds the circle(s) that match a glyph.
      ::
      |=  cha/char  ^-  (unit audience)
      =+  lax=(~(get ju binds) cha)
      ::  no circle.
      ?:  =(~ lax)  ~
      ::  single circle.
      ?:  ?=({* $~ $~} lax)  `n.lax
      ::  in case of multiple audiences, pick the most recently active one.
      |-  ^-  (unit audience)
      ?~  grams  ~
      ::  get first circle from a telegram's audience.
      =+  pan=(silt ~(tap in aud.i.grams))
      ?:  (~(has in lax) pan)  `pan
      $(grams t.grams)
    ::
    :>  #
    :>  #  %differs
    :>  #
    :>    arms that calculate differences between datasets.
    +|
    ::
    ++  sh-group-diff
      :>    group diff parts
      :>
      :>  calculates the difference between two presence
      :>  lists, producing lists of removed, added and
      :>  changed presences.
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
    ++  sh-rempe-diff
      :>    remotes diff
      :>
      :>  calculates the difference between two remote
      :>  presence maps, producing a list of removed,
      :>  added and changed presences maps.
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
    ++  sh-remco-diff
      :>    config diff parts
      :>
      :>  calculates the difference between two config
      :>  maps, producing lists of removed, added and
      :>  changed configs.
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
    ++  sh-set-diff
      :>    set diff
      :>
      :>  calculates the difference between two sets,
      :>  procuding lists of removed and added items.
      ::
      |*  {one/(set *) two/(set *)}
      :-  ^=  old  ~(tap in (~(dif in one) two))
          ^=  new  ~(tap in (~(dif in two) one))
    ::
    :>  #
    :>  #  %printers
    :>  #
    :>    arms for printing data to the cli.
    +|
    ::
    ++  sh-lame
      :>    send error
      :>
      :>  just puts some text into the cli as-is.
      ::
      |=  txt/tape
      (sh-fact [%txt txt])
    ::
    ++  sh-note
      :>    shell message
      :>
      :>  left-pads {txt} with heps and prints it.
      ::
      |=  txt/tape
      ^+  +>
      =+  lis=(simple-wrap txt (sub width.she 16))
      %+  sh-fact  %mor
      =+  ?:((gth (lent lis) 0) (snag 0 lis) "")
      :-  txt+(runt [14 '-'] '|' ' ' -)
      %+  turn  (slag 1 lis)
      |=(a/tape txt+(runt [14 ' '] '|' ' ' a))
    ::
    ++  sh-prod
      :>    show prompt
      :>
      :>  makes and stores a move to modify the cli
      :>  prompt to display the current audience.
      ::
      ^+  .
      %+  sh-fact  %pro
      :+  &  %talk-line
      ^-  tape
      =/  rew/(pair (pair cord cord) audience)
          [['[' ']'] active.she]
      =+  cha=(~(get by bound) q.rew)
      ?^  cha  ~[u.cha ' ']
      =+  por=~(ar-prom ar q.rew)
      (weld `tape`[p.p.rew por] `tape`[q.p.rew ' ' ~])
    ::
    ++  sh-rend
      :>  prints a telegram as rendered by ++tr-rend.
      ::
      |=  gam/telegram
      ^+  +>
      =+  lis=~(tr-rend tr settings.she gam)
      ?~  lis  +>.$
      %+  sh-fact  %mor
      %+  turn  `(list tape)`lis
      =+  nom=(scag 7 (cite:title self))
      |=  t/tape
      ?.  ?&  (~(has in settings.she) %notify)
              ?=(^ (find nom (slag 15 t)))
          ==
        [%txt t]
      [%mor [%txt t] [%bel ~] ~]
    ::
    ++  sh-numb
      :>  prints a message number, left-padded by heps.
      ::
      |=  num/@ud
      ^+  +>
      =+  bun=(scow %ud num)
      %+  sh-fact  %txt
      (runt [(sub 13 (lent bun)) '-'] "[{bun}]")
    ::
    ++  sh-cure
      :>  renders a security kind.
      ::
      |=  a/security
      ^-  tape
      (scow %tas a)
    ::
    ++  sh-scis
      :>    render status
      :>
      :>  gets the presence of {saz} as a tape.
      ::
      |=  sat/status
      ^-  tape
      ['%' (trip pec.sat)]
    ::
    ++  sh-show-status
      :>  prints presence changes to the cli.
      ::
      |=  {cir/circle who/ship cur/status dif/diff-status}
      ^+  +>
      ?:  (~(has in settings.she) %quiet)  +>
      %-  sh-note
      %+  weld
        (weld ~(cr-phat cr cir) ": ")
      ?-  -.dif
          $full
        "hey {(scow %p who)} {(scow %tas pec.sat.dif)}"
      ::
          $presence
        "see {(scow %p who)} {(scow %tas pec.dif)}"
      ::
          $human
        %+  weld  "nom {(scow %p who)}"
        ?:  ?=($true -.dif.dif)  ~
        =-  " '{(trip (fall han.man.cur ''))}' -> '{-}'"
        %-  trip
        =-  (fall - '')
        ?-  -.dif.dif
          $full     han.man.dif.dif
          $handle   han.dif.dif
        ==
      ::
          $remove
        "bye {(scow %p who)}"
      ==
    ::
    ++  sh-show-config
      :>  prints config changes to the cli.
      ::
      |=  {cir/circle cur/config dif/diff-config}
      ^+  +>
      ?:  (~(has in settings.she) %quiet)  +>
      ?:  ?=($full -.dif)
        =.  +>
          (sh-note (weld "new " (~(cr-show cr cir) ~)))
        =.  +>  $(dif [%caption cap.cof.dif])
        $(dif [%filter fit.cof.dif])
      ?:  ?=($remove -.dif)
        (sh-note (weld "rip " (~(cr-show cr cir) ~)))
      ?:  ?=($usage -.dif)  +>
      %-  sh-note
      %+  weld
        (weld ~(cr-phat cr cir) ": ")
      ?-  -.dif
          $source
        %+  weld  ?:(add.dif "onn " "off ")
        ~(cr-full cr cir.src.dif)
      ::
          $caption
        "cap: {(trip cap.dif)}"
      ::
          $filter
        ;:  weld
          "fit: caps:"
          ?:(cas.fit.dif "Y" "n")
          " unic:"
          ?:(utf.fit.dif "✔" "n")
        ==
      ::
          $secure
        "sec {(trip sec.con.cur)} -> {(trip sec.dif)}"
      ::
          $permit
        %+  weld
          =?  add.dif
              ?=(?($channel $mailbox) sec.con.cur)
            !add.dif
          ?:(add.dif "inv " "ban ")
        ^-  tape
        %-  ~(rep in sis.dif)
        |=  {s/ship t/tape}
        =?  t  ?=(^ t)  (weld t ", ")
        (weld t (cite:title s))
      ==
    ::
    ++  sh-gram
      :>    show telegram
      :>
      :>  prints the telegram. every fifth message,
      :>  print the message number also.
      ::
      |=  gam/telegram
      ^+  +>
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
    ++  sh-grams
      :>  prints multiple telegrams.
      ::
      |=  gaz/(list telegram)
      ^+  +>
      ?~  gaz  +>
      $(gaz t.gaz, +> (sh-gram i.gaz))
    --
  --
::
:>  #
:>  #  %renderers
:>  #
:>    rendering cores.
+|
::
++  cr
  :>    circle renderer
  :>
  :>  used in both circle and ship rendering.
  ::
  |_  :>  one: the circle.
      ::
      one/circle
  ::
  ++  cr-beat
    :>    {one} more relevant?
    :>
    :>  returns true if one is better to show, false
    :>  otherwise. prioritizes: our > main > size.
    ::
    |=  two/circle
    ^-  ?
    ::  the circle that's ours is better.
    ?:  =(self hos.one)
      ?.  =(self hos.two)  &
      ?<  =(nom.one nom.two)
      ::  if both circles are ours, the main story is better.
      ?:  =(%inbox nom.one)  &
      ?:  =(%inbox nom.two)  |
      ::  if neither are, pick the "larger" one.
      (lth nom.one nom.two)
    ::  if one isn't ours but two is, two is better.
    ?:  =(self hos.two)  |
    ?:  =(hos.one hos.two)
      ::  if they're from the same ship, pick the "larger" one.
      (lth nom.one nom.two)
    ::  if they're from different ships, neither ours, pick hierarchically.
    (lth (xeb hos.one) (xeb hos.two))
  ::
  ++  cr-best
    :>  returns the most relevant circle.
    ::
    |=  two/circle
    ?:((cr-beat two) one two)
  ::
  ++  cr-curt
    :>    prints a ship name in 14 characters.
    :>
    :>  left-pads with spaces. {mup} signifies
    :>  "are there other targets besides this one?"
    ::
    |=  mup/?
    ^-  tape
    =+  raw=(cite:title hos.one)
    (runt [(sub 14 (lent raw)) ' '] raw)
  ::
  ++  cr-nick
    :>    get nick for ship, or shortname if no nick.
    :>
    :>  left-pads with spaces.
    ::
    |=  aud/audience
    ^-  tape
    =/  nic/(unit cord)
      ?:  (~(has by nicks) hos.one)
        (~(get by nicks) hos.one)
      %-  ~(rep in aud)
      |=  {cir/circle han/(unit cord)}
      ?^  han  han
      =+  gop=(~(get by remotes) cir)
      ?~  gop  ~
      han.man:(fall (~(get by u.gop) hos.one) *status)
    ?~  nic  (cr-curt |)
    =+  raw=(scag 14 (trip u.nic))
    =+  len=(sub 14 (lent raw))
    (weld (reap len ' ') raw)
  ::
  ::  todo: figure out why enabling the doccord causes a nest fail, even when
  ::  attached to the arm instead of the product.
  ::
  ++  cr-phat                                           ::<  render accurately
    ::>  prints a circle fully, but still taking
    ::>  "shortcuts" where possible:
    ::>  ":" for local mailbox, "~ship" for foreign
    ::>  mailbox, "%channel" for local circle,
    ::>  "/channel" for parent circle.
    ::
    ^-  tape
    ?:  =(hos.one self)
      ?:  =(nom.one inbox)
        ":"
      ['%' (trip nom.one)]
    =+  wun=(cite:title hos.one)
    ?:  =(nom.one %inbox)
      wun
    ?:  =(hos.one (sein:title self))
      ['/' (trip nom.one)]
    :(welp wun "/" (trip nom.one))
  ::
  ++  cr-full  (cr-show ~)                              :<  render full width
  ::
  ++  cr-show
    :>  renders a circle as text.
    :>
    :>  moy:  multiple circles in audience?
    |=  moy/(unit ?)
    ^-  tape
    ::  render circle (as glyph if we can).
    ?~  moy
      =+  cha=(~(get by bound) one ~ ~)
      =-  ?~(cha - "{u.cha ~} {-}")
      ~(cr-phat cr one)
    (~(cr-curt cr one) u.moy)
  --
::
++  ar
  :>    audience renderer
  :>
  :>  used for representing audiences (sets of circles)
  :>  as tapes.
  ::
  |_  :>  aud: members of the audience.
      ::
      aud/audience
  ::
  ++  ar-best
    :>  find the most relevant circle in the set.
    ::
    ^-  (unit circle)
    ?~  aud  ~
    :-  ~
    |-  ^-  circle
    =+  lef=`(unit circle)`ar-best(aud l.aud)
    =+  rit=`(unit circle)`ar-best(aud r.aud)
    =?  n.aud  ?=(^ lef)  (~(cr-best cr n.aud) u.lef)
    =?  n.aud  ?=(^ rit)  (~(cr-best cr n.aud) u.rit)
    n.aud
  ::
  ++  ar-deaf
    :>  remove ourselves from the audience.
    ::
    ^+  .
    .(aud (~(del in aud) `circle`incir))
  ::
  ++  ar-maud
    :>    multiple audience
    :>
    :>  checks if there's multiple circles in the
    :>  audience via pattern matching.
    ::
    ^-  ?
    =.  .  ar-deaf
    !?=($@($~ {* $~ $~}) aud)
  ::
  ++  ar-phat
    :>  render all circles, no glyphs.
    ::
    ^-  tape
    %-  ~(rep in aud)
    |=  {c/circle t/tape}
    =?  t  ?=(^ t)
      (weld t ", ")
    (weld t ~(cr-phat cr c))
  ::
  ++  ar-prom
    :>  render all circles, ordered by relevance.
    ::
    ^-  tape
    =.  .  ar-deaf
    =/  all
      %+  sort  `(list circle)`~(tap in aud)
      |=  {a/circle b/circle}
      (~(cr-beat cr a) b)
    =+  fir=&
    |-  ^-  tape
    ?~  all  ~
    ;:  welp
      ?:(fir "" " ")
      (~(cr-show cr i.all) ~)
      $(all t.all, fir |)
    ==
  ::
  ++  ar-whom
    :>  render sender as the most relevant circle.
    ::
    (~(cr-show cr (need ar-best)) ~ ar-maud)
  ::
  ++  ar-dire
    :>  returns true if circle is a mailbox of ours.
    ::
    |=  cir/circle  ^-  ?
    ?&  =(hos.cir self)
        =+  sot=(~(get by mirrors) cir)
        &(?=(^ sot) ?=($mailbox sec.con.u.sot))
    ==
  ::
  ++  ar-glyf
    ::  todo: another place where doccords break things.
    ::
    ::>    audience glyph
    ::>
    ::>  get the glyph that corresponds to the audience.
    ::>  for mailbox messages and complex audiences, use
    ::>  reserved "glyphs".
    ::
    ^-  tape
    =+  cha=(~(get by bound) aud)
    ?^  cha  ~[u.cha]
    ?.  (lien ~(tap by aud) ar-dire)
      "*"
    ?:  ?=({^ $~ $~} aud)
      ":"
    ";"
  --
::
++  tr
  :>    telegram renderer
  :>
  :>  responsible for converting telegrams and
  :>  everything relating to them to text to be
  :>  displayed in the cli.
  ::
  |_  $:  :>  sef: settings flags.
          :>  who: author.
          :>  sen: unique identifier.
          :>  aud: audience.
          :>  wen: timestamp.
          :>  sep: message contents.
          ::
          sef/(set term)
          who/ship
          sen/serial
          aud/audience
          wen/@da
          sep/speech
      ==
  ::
  ++  tr-fact
    :>    activate effect
    :>
    :>  produces sole-effect for printing message
    :>  details.
    ::
    ^-  sole-effect
    ~[%mor [%tan tr-meta] tr-body]
  ::
  ++  tr-rend
    :>    renders a telegram
    :>
    :>  the first line will contain the author and
    :>  optional timestamp.
    ::
    ^-  (list tape)
    =/  wyd
      %+  sub  width.cli                                ::  termwidth,
      %+  add  14                                       ::  minus author,
      ?:((~(has in sef) %showtime) 10 0)                ::  minus timestamp.
    =+  txs=(tr-text wyd)
    ?~  txs  ~
    ::  render the author.
    =/  nom/tape
      ?:  (~(has in sef) %nicks)
        (~(cr-nick cr [who %inbox]) aud)
      (~(cr-curt cr [who %inbox]) |)
    ::  regular indent.
    =/  den/tape
      (reap (lent nom) ' ')
    ::  timestamp, if desired.
    =/  tam/tape
      ?.  (~(has in sef) %showtime)  ""
      =.  wen
        %.  [wen (mul q.timez.cli ~h1)]
        ?:(p.timez.cli add sub)
      =+  dat=(yore wen)
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
  ++  tr-meta
    :>    metadata
    :>
    :>  builds string that display metadata, including
    :>  message serial, timestamp, author and audience.
    ::
    ^-  tang
    =.  wen  (sub wen (mod wen (div wen ~s0..0001)))    :: round
    =+  hed=leaf+"{(scow %uv sen)} at {(scow %da wen)}"
    =/  cis
      %+  turn  ~(tap in aud)
      |=  a/circle
      leaf+~(cr-full cr a)
    [%rose [" " ~ ~] [hed >who< [%rose [", " "to " ~] cis] ~]]~
  ::
  ++  tr-body
    :>    message content
    :>
    :>  long-form display of message contents, specific
    :>  to each speech type.
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
      mor+~[txt+"# {(trip exp.sep)}" tan+res.sep]
    ::
        $ire
      =+  num=(~(get by known) top.sep)
      ?~  num  $(sep sep.sep)
      =+  gam=(snag (sub count +(u.num)) grams)
      =-  mor+[tan+- $(sep sep.sep) ~]
      %-  flop  %+  weld
        :_  ~  :-  %leaf
        %+  weld  "in reply to: {(cite:title aut.gam)}: "
        "[{(scow %ud u.num)}]"
      %+  turn  (~(tr-text tr sef gam) width.cli)
      |=(t/tape [%leaf t])
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
      [%mor tan+~[leaf+"[{(trip app.sep)}]: "] $(sep sep.sep) ~]
    ==
  ::
  ++  tr-tach
    :>  renders an attachment.
    ::
    |=  a/attache
    ^-  tang
    ?-  -.a
      $name  (welp $(a tac.a) leaf+"= {(trip nom.a)}" ~)
      $tank  +.a
      $text  (turn (flop +.a) |=(b/cord leaf+(trip b)))
    ==
  ::
  ++  tr-chow
    :>    truncate
    :>
    :>  truncates the {txt} to be of max {len}
    :>  characters. if it does truncate, indicates it
    :>  did so by appending _ or ….
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
  ++  tr-text
    :>    compact contents
    :>
    :>  renders just the most important data of the
    :>  message. if possible, these stay within a single
    :>  line.
    :>
    :>  pre:  replace/append line prefix
    ::TODO  this should probably be redone someday.
    =|  pre/(unit (pair ? tape))
    |=  wyd/@ud
    ^-  (list tape)
    ?-  -.sep
        $fat
      %+  weld  $(sep sep.sep)
      ^-  (list tape)
      ?+  -.tac.sep  [" attached: ..." ~]
        $name  [(scag wyd " attached: {(trip nom.tac.sep)}") ~]
      ==
    ::
        $exp
      :-  (tr-chow wyd '#' ' ' (trip exp.sep))
      ?~  res.sep  ~
      =-  [' ' (tr-chow (dec wyd) ' ' -)]~
      ~(ram re (snag 0 `(list tank)`res.sep))
    ::
        $ire
      $(sep sep.sep, pre `[| "^ "])
    ::
        $url
      :_  ~
      =+  ful=(apix:en-purl:html url.sep)
      =+  pef=q:(fall pre [p=| q=""])
      ::  clean up prefix if needed.
      =?  pef  =((scag 1 (flop pef)) " ")
        (scag (dec (lent pef)) pef)
      =.  pef  (weld "/" pef)
      =.  wyd  (sub wyd +((lent pef)))  ::  account for prefix.
      ::  if the full url fits, just render it.
      ?:  (gte wyd (lent ful))  :(weld pef " " ful)
      ::  if it doesn't, prefix with _ and render just (the tail of) the domain.
      %+  weld  (weld pef "_")
      =+  hok=r.p.p.url.sep
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
      =/  pef/tape
        ?:  &(?=(^ pre) p.u.pre)  q.u.pre
        ?:  pat.sep  " "
        =-  (weld - q:(fall pre [p=| q=" "]))
        %~  ar-glyf  ar
          ?:  =(who self)  aud
          (~(del in aud) [who %inbox])
        ==
      =/  lis/(list tape)
        %+  simple-wrap
          `tape``(list @)`(tuba (trip msg.sep))
        (sub wyd (min (div wyd 2) (lent pef)))
      =+  lef=(lent pef)
      =+  ?:((gth (lent lis) 0) (snag 0 lis) "")
      :-  (weld pef -)
      %+  turn  (slag 1 lis)
      |=(a/tape (runt [lef ' '] a))
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
      $(sep sep.sep, pre `[& "[{(trip app.sep)}]: "])
    ==
  --
::
:>  #
:>  #  %events
:>  #
+|
::
++  quit-server-client
  |=  wir/wire
  ^-  (quip move _+>)
  [[peer-client]~ +>]
::
++  quit-server-inbox
  |=  wir/wire
  ^-  (quip move _+>)
  [[peer-inbox]~ +>]
::
++  peer
  :>  incoming subscription on pax.
  ::
  |=  pax/path
  ^-  (quip move _+>)
  ?.  =(src.bol our.bol)
    ~!  [%peer-talk-stranger src.bol]
    !!
  ?.  ?=({$sole *} pax)
    ~!  [%peer-talk-strange pax]
    !!
  ta-done:ta-console:ta
::
++  diff-hall-prize
  :>  accept query answer
  ::
  |=  {way/wire piz/prize}
  ^-  (quip move _+>)
  ta-done:(ta-take:ta piz)
::
++  diff-hall-rumor
  :>  accept query change
  ::
  |=  {way/wire rum/rumor}
  ^-  (quip move _+>)
  ta-done:(ta-hear:ta rum)
::
++  poke-sole-action
  :>  incoming sole action. process it.
  ::
  |=  act/sole-action
  ta-done:(ta-sole:ta act)
::
::TODO  for debug purposes. remove eventually.
::  users beware, here be dragons.
++  poke-noun
  |=  a/@t
  ^-  (quip move _+>)
  ?:  =(a 'check')
    ~&  'verifying message reference integrity...'
    =-  ~&(- [~ +>.$])
    ~&  [%count--lent count (lent grams)]
    =+  %-  ~(rep by known)
      |=  {{u/serial a/@ud} k/@ud m/@ud}
      :-  ?:((gth a k) a k)
      ?:  =(u uid:(snag (sub count +(a)) grams))  m  +(m)
    :-  %check-talk
    [known=k mismatch=m]
  ?:  =(a 'rebuild')
    ~&  'rebuilding message references...'
    =+  %+  reel  grams
      |=  {t/telegram c/@ud k/(map serial @ud)}
      [+(c) (~(put by k) uid.t c)]
    [~ +>.$(count c, known k)]
  ?:  =(a 'reconnect')
    ~&  'disconnecting and reconnecting to hall...'
    :_  +>
    :~  [ost.bol %pull /server/client server ~]
        [ost.bol %pull /server/inbox server ~]
        peer-client
        peer-inbox
    ==
  ?:  =(a 'reset')
    ~&  'full reset incoming, hold on to your cli...'
    :_  +>(grams ~, known ~, count 0, last 0)
    :~  [ost.bol %pull /server/client server ~]
        [ost.bol %pull /server/inbox server ~]
        peer-client
        peer-inbox
    ==
  ::  this deletes a message from your backlog, and may
  ::  make talk throw stack traces.
  ::  **aka don't run this!**
  ?:  =(a 'screw')
    ~&  'screwing things up...'
    :-  ~
    +>(grams (oust [0 1] grams))
  [~ +>]
::
++  coup-client-action
  :>  accept n/ack
  ::
  |=  {wir/wire fal/(unit tang)}
  ^-  (quip move _+>)
  ?~  fal  [~ +>]
  %-  (slog leaf+"action failed: " u.fal)
  [~ +>]
--
