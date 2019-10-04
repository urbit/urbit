::  chat-cli: cli chat client using chat-store and friends
::
::    pulls all known messages into a single stream.
::    type ;help for usage instructions.
::
::    note that while the chat-store only cares about paths,
::    we mostly deal with [ship path] (aka target) here.
::    when sending messages (through the chat hook),
::    we concat the ship onto the head of the path,
::    and trust it to take care of the rest.
::
::NOTE  the code is a mess. heavily wip!
::
/-  sole-sur=sole, *chat-store, *chat-view, *chat-hook
/+  sole-lib=sole
/=  seed  /~  !>(.)
::
|%
+$  state
  $:  grams=(list mail)
      known=(set [target serial])
      count=@ud                                     ::  (lent grams)
      ::  ui state                                  ::
      ::TODO  nicks from contacts
      bound=(map target char)                       ::  bound circle glyphs
      binds=(jug char target)                       ::  circle glyph lookup
      audience=(set target)                         ::  active targets
      settings=(set term)                           ::  frontend flags
      width=@ud                                     ::  display width
      timez=(pair ? @ud)                            ::  timezone adjustment
      cli=[=bone state=sole-share:sole-sur]         ::  console id & state
  ==
::
+$  mail  [source=target envelope]
+$  target  [=ship =path]
::
+$  glyph  char
++  glyphs  "!@#$%^&()-=_+[]\{};'\\:\",.<>?"
::
+$  command
  $%  [%target (set target)]                     ::  set messaging target
      [%say (list letter)]                          ::  send message
      [%eval cord hoon]                             ::  send #-message
    ::
      [%create path =(unit glyph)]                  ::  create chat
      [%join target =(unit glyph)]                  ::  join target
      [%leave target]                               ::  nuke target
    ::
      [%bind glyph target]                          ::  bind glyph
      [%unbind glyph]                               ::  unbind glyph
      [%what (unit $@(char target))]                ::  glyph lookup
    ::
      [%settings ~]                                 ::  show active settings
      [%set term]                                   ::  set settings flag
      [%unset term]                                 ::  unset settings flag
      [%width @ud]                                  ::  adjust display width
      [%timezone ? @ud]                             ::  adjust time printing
    ::
      [%select $@(back=@ud [zeros=@u num=@ud])]     ::  rel/abs msg selection
      [%chats ~]                                    ::  list available chats
      [%help ~]                                     ::  print usage info
  ==                                                ::
::
+$  move  [bone card]
+$  card
  $%  [%diff %sole-effect sole-effect:sole-sur]
      [%poke wire dock out-action]
      [%peer wire dock path]
  ==
::
+$  out-action
  $%  [%chat-action chat-action]
      [%chat-view-action chat-view-action]
      [%chat-hook-action chat-hook-action]
  ==
--
::
|_  [=bowl:gall state]
::
++  prep
  |=  old=(unit state)
  ~&  %chat-cli-prep
  ?^  old
    :_  this(+<+ u.old)
    [ost.bowl %peer /chat-store [our-self %chat-store] /all]~
  =.  audience  [[our-self /inbox] ~ ~]
  =.  settings  (sy %showtime %notify ~)
  =.  width  80
  :_  this
  ::TODO  %peer /all
  ~
::
++  this  .
::
++  true-self
  |=  who=ship
  ^-  ship
  ?.  ?=(%earl (clan:title who))  who
  ::TODO  but they're moons... isn't ^sein sufficient?
  (sein:title our.bowl now.bowl who)
::
++  our-self  (true-self our.bowl)
::
++  target-to-path
  |=  target
  path
  ::TODO
  :: [(scot %p ship) path]
::
++  path-to-target
  |=  =path
  ^-  target
  ?.  ?=([@ @ *] path)
    ::TODO  but then doing target-to-path won't get us the same path...
    [our-self path]
  =+  who=(slaw %p i.path)
  ?~  who  [our-self path]
  [u.who path]
::
++  diff-chat-initial
  |=  [=wire =inbox]
  ^-  (quip move _this)
  =|  moves=(list move)
  |-  ^-  (quip move _this)
  ?~  inbox  [~ this]
  =*  path  p.n.inbox
  =*  mailbox  q.n.inbox
  =/  =target  (path-to-target path)
  =^  mon  this  (read-envelopes target envelopes.mailbox)
  =^  mol  this  $(inbox l.inbox)
  =^  mor  this  $(inbox r.inbox)
  [:(weld mon mol mor) this]
::
++  read-envelopes
  |=  [=target envs=(list envelope)]
  ^-  (quip move _this)
  ?~  envs  [~ this]
  =^  moi  this  (read-envelope target i.envs)
  =^  mot  this  $(envs t.envs)
  [(weld moi mot) this]
::
++  diff-chat-update
  |=  [=wire upd=chat-update]
  ^-  (quip move _this)
  ?+  -.upd  [~ this]
    %create   (notice-create (path-to-target path.upd))
    %delete   [[(show-delete:sh (path-to-target path.upd)) ~] this]
    %message  (read-envelope (path-to-target path.upd) envelope.upd)
  ==
::
++  notice-create
  |=  =target
  ^-  (quip move _this)
  =^  moz  this
    ?:  (~(has by bound) target)
      [~ this]
    (bind-default-glyph target)
  [[(show-create:sh target) moz] this]
::
++  bind-default-glyph
  |=  =target
  ^-  (quip move _this)
  =-  (bind-glyph - target)
  ::TODO  try not to double-bind
  =-  (snag - glyphs)
  (mod (mug target) (lent glyphs))
::
++  bind-glyph
  |=  [=glyph =target]
  ^-  (quip move _this)
  ::TODO  should send these to settings store eventually
  ::TODO  disallow double-binding glyphs?
  =.  bound  (~(put by bound) target glyph)
  =.  binds  (~(put ju binds) glyph target)
  [(show-glyph:sh glyph `target) this]
::
++  unbind-glyph
  |=  =glyph  ::TODO  do we really not want this optionally per-audience?
  ^-  (quip move _this)
  =/  ole=(set target)
    (~(get ju binds) glyph)
  =.  binds  (~(del by binds) glyph)
  =.  bound
    |-
    ?~  ole  bound
    =.  bound  $(ole l.ole)
    =.  bound  $(ole r.ole)
    (~(del by bound) n.ole)
  [(show-glyph:sh glyph ~) this]
::
++  read-envelope
  |=  [=target =envelope]
  ^-  (quip move _this)
  ?:  (~(has in known) [target uid.envelope])
    ::NOTE  we no-op only because edits aren't possible
    [~ this]
  :-  (print-envelope:sh target envelope)
  %_  this
    known  (~(put in known) [target uid.envelope])
    grams  [[target envelope] grams]
    count  +(count)
  ==
::
++  peer
  |=  =path
  ^-  (quip move _this)
  ?.  (team:title our-self src.bowl)
    ~|  [%peer-talk-stranger src.bowl]
    !!
  ?.  ?=([%sole *] path)
    ~|  [%peer-talk-strange path]
    !!
  =.  bone.cli  ost.bowl
  ::  display a fresh prompt
  :-  [prompt:sh ~]
  ::  start with fresh sole state
  this(state.cli *sole-share:sole-sur)
::
++  poke-sole-action
  |=  act=sole-action:sole-sur
  ^-  (quip move _this)
  ?.  =(bone.cli ost.bowl)
    ~|(%strange-sole !!)
  (sole:sh act)
::
::TODO  maybe separate +shin and +shout
++  sh
  |%
  ++  effect
    ::  console effect move
    ::
    |=  fec=sole-effect:sole-sur
    ^-  move
    [bone.cli %diff %sole-effect fec]
  ::
  ++  print
    ::  just puts some text into the cli as-is.
    ::
    |=  txt=tape
    (effect %txt txt)
  ::
  ++  note
    ::    shell message
    ::
    ::  left-pads {txt} with heps and prints it.
    ::
    |=  txt=tape
    ^-  move
    =+  lis=(simple-wrap txt (sub width 16))
    %+  effect  %mor
    =+  ?:((gth (lent lis) 0) (snag 0 lis) "")
    :-  txt+(runt [14 '-'] '|' ' ' -)
    %+  turn  (slag 1 lis)
    |=(a=tape txt+(runt [14 ' '] '|' ' ' a))
  ::
  ++  prompt
    ::    show prompt
    ::
    ::  makes and stores a move to modify the cli
    ::  prompt to display the current audience.
    ::
    ::TODO  take arg?
    ^-  move
    %+  effect  %pro
    :+  &  %talk-line
    ^-  tape
    =-  ?:  =(1 (lent -))  "{-} "
        "[{-}] "
    :: %-  zing
    :: %+  join  " "
    :: ^-  (list tape)
    :: %+  turn  ~(tap in audience)
    :: |=  =target
    :: ^-  tape
    :: =+  gyf=(~(get by bound) target)
    :: ?^  gyf  ~[u.gyf]
    ~(ar-prom ar audience)
  ::
  ++  sole
    ::  applies sole action.
    ::
    |=  act=sole-action:sole-sur
    ^-  (quip move _this)
    ?-  -.act
      $det  (edit +.act)
      $clr  [~ this] :: (sh-pact ~) ::TODO  clear to PM-to-self?
      $ret  obey
    ==
  ::
  ++  edit
    ::    apply sole edit
    ::
    ::  called when typing into the cli prompt.
    ::  applies the change and does sanitizing.
    ::
    |=  cal=sole-change:sole-sur
    ^-  (quip move _this)
    =^  inv  state.cli  (~(transceive sole-lib state.cli) cal)
    =+  fix=(sanity inv buf.state.cli)
    ?~  lit.fix
      [~ this]
    ::  just capital correction
    ?~  err.fix
      (slug fix)
    ::  allow interior edits and deletes
    ?.  &(?=($del -.inv) =(+(p.inv) (lent buf.state.cli)))
      [~ this]
    (slug fix)
  ::
  ++  read
    ::    command parser
    ::
    ::  parses the command line buffer. produces work
    ::  items which can be executed by ++sh-work.
    ::
    =<  work
    ::  #  %parsers
    ::    various parsers for command line input.
    |%
    ++  expr
      ::  [cord hoon]
      |=  tub/nail  %.  tub
      %+  stag  (crip q.tub)
      wide:(vang & [&1:% &2:% (scot %da now.bowl) |3:%])
    ::
    ++  dare
      ::  @dr
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
    ++  path
      ;~(pfix net (most net urs:ab))
    ::
    ++  tarl                                          ::  local target
      (stag our-self path)
    ::
    ++  tarp                                          ::  sponsor target
      ;~(pfix ket (stag (sein:title our.bowl now.bowl our-self) path))
    ::
    ++  targ                                          ::  target
      ;~  pose
        tarl
        tarp
        ;~(plug ship path)
        (sear glyf glyph)
      ==
    ::
    ++  tars                                          ::  non-empty circles
      %+  cook  ~(gas in *(set target))
      %+  most  ;~(plug com (star ace))
      ;~(pose targ (sear glyf glyph))
    ::
    ++  drat
      ::  @da or @dr
      ::
      ::  pas: whether @dr's are in the past or not.
      |=  pas/?
      =-  ;~(pfix sig (sear - crub:so))
      |=  a/^dime
      ^-  (unit @da)
      ?+  p.a  ~
        $da   `q.a
        $dr   :-  ~
              %.  [now.bowl q.a]
              ?:(pas sub add)
      ==
    ::
    ++  tarz                                          ::  non-empty sources
      %+  cook  ~(gas in *(set target))
      (most ;~(plug com (star ace)) targ)
    ::
    ++  pick                                          ::  message reference
      ;~(pose nump (cook lent (star mic)))
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
    ++  lobe                                          ::  y/n loob
      ;~  pose
        (cold %& ;~(pose (jest 'y') (jest '&') (just 'true')))
        (cold %| ;~(pose (jest 'n') (jest '|') (just 'false')))
      ==
    ::
    ++  message                                       ::  exp, lin or url msg
      ;~  pose
        ;~(plug (cold %eval hax) expr)
        (stag %say letters)
      ==
    ::
    ++  letters                                       ::  lin or url msgs
      %+  most  (jest '•')
      ;~  pose
        ::TODO  (stag %url aurf:de-purl:html)
        :(stag %text ;~(less mic hax text))
      ==
    ::
    ++  text                                          ::  msg without break
      %+  cook  crip
      (plus ;~(less (jest '•') next))
    ::
    ++  nick  (cook crip (plus next))                 ::  nickname
    ++  glyph  (mask glyphs)                          ::  circle postfix
    ++  setting                                       ::  setting flag
      %-  perk  :~
        %notify
        %showtime
      ==
    ++  tag  |*(a=@tas (cold a (jest a)))  ::TODO  into stdlib
    ++  work                                          ::  full input
      %+  knee  *command  |.  ~+
      =-  ;~(pose ;~(pfix mic -) message)
      ::TODO  refactor the optional trailing args, glue junk
      ;~  pose
        (stag %target tars)
      ::
        ;~((glue ace) (tag %create) ;~(plug path (punt ;~(pfix ace glyph))))
      ::
        ;~((glue ace) (tag %join) ;~(plug targ (punt ;~(pfix ace glyph))))
        ;~((glue ace) (tag %leave) targ)
      ::
        ;~((glue ace) (tag %bind) glyph targ)
        ;~((glue ace) (tag %unbind) glyph)
        ;~(plug (perk %what ~) (punt ;~(pfix ace ;~(pose glyph targ))))
      ::
        ;~(plug (tag %settings) (easy ~))
        ;~((glue ace) (tag %set) setting)
        ;~((glue ace) (tag %unset) setting)
        ;~(plug (cold %width (jest 'set width ')) dem:ag)
        ;~  plug
          (cold %timezone (jest 'set timezone '))
          ;~  pose
            (cold %| (just '-'))
            (cold %& (just '+'))
          ==
          %+  sear
            |=  a=@ud
            ^-  (unit @ud)
            ?:(&((gte a 0) (lte a 14)) `a ~)
          dem:ag
        ==
      ::
        ;~(plug (tag %chats) (easy ~))
        ;~(plug (tag %help) (easy ~))
      ::
        (stag %select pick)
      ==
    --
  ::
  ++  obey
    ::    apply result
    ::
    ::  called upon hitting return in the prompt. if
    ::  input is invalid, ++sh-slug is called.
    ::  otherwise, the appropriate work is done and
    ::  the entered command (if any) gets displayed
    ::  to the user.
    ::
    ^-  (quip move _this)
    =+  buf=buf.state.cli
    =+  fix=(sanity [%nop ~] buf)
    ?^  lit.fix
      (slug fix)
    =+  jub=(rust (tufa buf) read)
    ?~  jub  [[(effect %bel ~) ~] this]
    =^  cal  state.cli  (~(transmit sole-lib state.cli) [%set ~])
    =^  moves  this  (work u.jub)
    :_  this
    %+  weld
      ^-  (list move)
      ::  echo commands into scrollback
      ?.  =(`0 (find ";" buf))  ~
      [(note (tufa `(list @)`buf)) ~]
    :_  moves
    %+  effect  %mor
    :~  [%nex ~]
        [%det cal]
    ==
  ::
  ++  work
    ::    do work
    ::
    ::  implements worker arms for different talk
    ::  commands.
    ::  worker arms must produce updated state.
    ::
    |=  job=command
    ^-  (quip move _this)
    |^  ?-  -.job  ::~|([%unimplemented -.job] !!)
          %target  (set-target +.job)
        ::
          %join    (join +.job)
          %leave   (leave +.job)
          %create  (create +.job)
        ::
          %say     (say +.job)
          %eval    (eval +.job)
        ::
          %bind    (bind-glyph +.job)
          %unbind  (unbind-glyph +.job)
          %what    (lookup-glyph +.job)
        ::
          %settings  show-settings
          %set     (set-setting +.job)
          %unset   (unset-setting +.job)
          %width   (set-width +.job)
          %timezone  (set-timezone +.job)
        ::
          %select  (select +.job)
          %chats   chats
          %help    help
        ==
    ::
    ++  act
      |=  [what=term app=term =out-action]
      ^-  move
      :*  ost.bowl
          %poke
          /cli-command/[what]
          [our-self app]
          out-action
      ==
    ::
    ++  set-target
      |=  tars=(set target)
      ^-  (quip move _this)
      =.  audience  tars
      [[prompt ~] this]
    ::
    ++  create
      ::TODO  configurable security
      |=  [=path gyf=(unit char)]
      ^-  (quip move _this)
      ::TODO  check if already exists
      =/  =target  [our-self path]
      =^  moz  this
        ?.  ?=(^ gyf)  [~ this]
        (bind-glyph u.gyf target)
      =-  [[- moz] this(audience [target ~ ~])]
      %^  act  %do-create  %chat-view
      :-  %chat-view-action
      [%create path %channel ~ ~]
    ::
    ++  join
      |=  [=target gyf=(unit char)]
      ^-  (quip move _this)
      =^  moz  this
        ?.  ?=(^ gyf)  [~ this]
        (bind-glyph u.gyf target)
      =-  [[- moz] this(audience [target ~ ~])]
      %^  act  %do-join  %chat-hook
      :-  %chat-hook-action
      [%add-synced target]
    ::
    ::TODO  but if we leave our own circle, then it disappears for everyone?
    ++  leave
      |=  =target
      =-  [[- ~] this]
      %^  act  %do-leave  %chat-hook
      :-  %chat-hook-action
      [%remove (target-to-path target)]
    ::
    ++  say
      |=  letters=(list letter)
      ^-  (quip move _this)
      =/  =serial  (shaf %msg-uid eny.bowl)
      :_  this(eny.bowl (shax eny.bowl))
      ^-  (list move)
      ::TODO  wait, so, is host irrelevant in target? only for joins?
      %+  turn  ~(tap in audience)
      |=  =target
      %^  act  %out-message  %chat-hook
      :-  %chat-action
      :+  %message  (target-to-path target)
      :*  serial
          *@
          our-self
          now.bowl
          (snag 0 letters)  ::TODO  support multiple
      ==
    ::
    ++  eval
      ::    run
      ::
      ::  executes {exe} and sends both its code and
      ::  result.
      ::
      |=  [txt=cord exe=hoon]
      ::  XX revisit
      ::
      ::    this double-virtualizes and clams to disable .^
      ::
      =;  tan=(list tank)
        (say [%code txt tan] ~)
      ;;  (list tank)
      =<  +>
      %+  mong
        :-  mute
        =-  |.([(sell (slap (slop !>(-) seed) exe))]~)
        ^-  [our=@p now=@da eny=@uvI]
        [our-self now.bowl (shas %eny eny.bowl)]
      |=(^ ~)
    ::
    ++  lookup-glyph
      ::TODO  we probably want a function for the (list tape) -> %mor %txt case
      |=  qur=(unit $@(glyph target))
      ^-  (quip move _this)
      =-  [[- ~] this]
      ?^  qur
        ?^  u.qur
          =+  gyf=(~(get by bound) u.qur)
          (print ?~(gyf "none" [u.gyf]~))
        =+  pan=~(tap in (~(get ju binds) `@t`u.qur))
        ?:  =(~ pan)  (print "~")
        =<  (effect %mor (turn pan .))
        |=(t=target [%txt ~(cr-phat cr t)])
      %+  effect  %mor
      %-  ~(rep by binds)
      |=  $:  [=glyph tars=(set target)]
              lis=(list sole-effect:sole-sur)
          ==
      %+  weld  lis
      ^-  (list sole-effect:sole-sur)
      %-  ~(rep in tars)
      |=  [t=target l=(list sole-effect:sole-sur)]
      %+  weld  l
      ^-  (list sole-effect:sole-sur)
      [%txt glyph ' ' ~(cr-phat cr t)]~
    ::
    ++  show-settings
      ^-  (quip move _this)
      :_  this
      :~  %-  print
          %-  zing
          ^-  (list tape)
          :-  "flags: "
          %+  ^join  ", "
          (turn `(list @t)`~(tap in settings) trip)
        ::
          %-  print
          %+  weld  "timezone: "
          ^-  tape
          :-  ?:(p.timez '+' '-')
          (scow %ud q.timez)
        ::
          (print "width: {(scow %ud width)}")
      ==
    ::
    ++  set-setting
      |=  =term
      ^-  (quip move _this)
      [~ this(settings (~(put in settings) term))]
    ::
    ++  unset-setting
      |=  =term
      ^-  (quip move _this)
      [~ this(settings (~(del in settings) term))]
    ::
    ++  set-width
      |=  w=@ud
      [~ this(width w)]
    ::
    ++  set-timezone
      |=  tz=[? @ud]
      [~ this(timez tz)]
    ::
    ++  select
      ::  finds selected message, expand it.
      ::
      ::TODO  this either needs a different implementation or extensive comments
      |=  num=$@(@ud [p=@u q=@ud])
      ^-  (quip move _this)
      |^  ?@  num
            =+  tum=(scow %s (new:si | +(num)))
            ?:  (gte num count)
              %-  just-print
              "{tum}: no such telegram"
            (activate tum num)
          ?.  (gte q.num count)
            ?:  =(count 0)
              (just-print "0: no messages")
            =+  msg=(deli (dec count) num)
            (activate (scow %ud msg) (sub count +(msg)))
          %-  just-print
          "…{(reap p.num '0')}{(scow %ud q.num)}: no such telegram"
      ::
      ++  just-print
        |=  txt=tape
        [[(print txt) ~] this]
      ::
      ++  deli
        ::  gets absolute message number from relative.
        ::
        |=  [max=@ud nul=@u fin=@ud]
        ^-  @ud
        =+  dog=|-(?:(=(0 fin) 1 (mul 10 $(fin (div fin 10)))))
        =.  dog  (mul dog (pow 10 nul))
        =-  ?:((lte - max) - (sub - dog))
        (add fin (sub max (mod max dog)))
      ::
      ++  activate
        ::  prints message details.
        ::
        |=  [number=tape index=@ud]
        ^-  (quip move _this)
        =+  gam=(snag index grams)
        =+  tay=~(. tr settings gam)
        =.  audience  [source.gam ~ ~]
        :_  this
        ^-  (list move)
        :~  (print ['?' ' ' number])
            (effect tr-fact:tay)
            prompt
        ==
      --
    ::
    ++  chats
      ^-  (quip move _this)
      :_  this
      :_  ~
      %+  effect  %mor
      =/  all
        ::TODO  refactor
        ::TODO  remote scries fail... but moon support?
        .^  (set path)
            %gx
            /(scot %p our-self)/chat-store/(scot %da now.bowl)/keys/noun
        ==
      %+  turn  ~(tap in all)
      %+  cork  path-to-target
      |=  target
      :-  %txt
      (weld (scow %p ship) (spud path))
    ::
    ++  help
      ^-  (quip move _this)
      =-  [[- ~] this]
      (print "see https://urbit.org/docs/using/messaging/")
    --
  ::
  ++  sanity
    ::    check input sanity
    ::
    ::  parses cli prompt input using ++read and
    ::  describes error correction when invalid.
    ::
    |=  [inv=sole-edit:sole-sur buf=(list @c)]
    ^-  [lit=(list sole-edit:sole-sur) err=(unit @u)]
    =+  res=(rose (tufa buf) read)
    ?:  ?=(%& -.res)  [~ ~]
    [[inv]~ `p.res]
  ::
  ++  slug
    ::  apply error correction to prompt input
    ::
    |=  [lit=(list sole-edit:sole-sur) err=(unit @u)]
    ^-  (quip move _this)
    ?~  lit  [~ this]
    =^  lic  state.cli
      %-  ~(transmit sole-lib state.cli)
      ^-  sole-edit:sole-sur
      ?~(t.lit i.lit [%mor lit])
    :_  this
    :_  ~
    %+  effect  %mor
    :-  [%det lic]
    ?~(err ~ [%err u.err]~)
  ::
  ++  glyf
    ::    decode glyph
    ::
    ::  finds the circle(s) that match a glyph.
    ::
    |=  cha=char
    ^-  (unit target)
    =+  lax=(~(get ju binds) cha)
    ::  no circle.
    ?:  =(~ lax)  ~
    ::  single circle.
    ?:  ?=([* ~ ~] lax)  `n.lax
    ::  in case of multiple audiences, pick the most recently active one.
    |-  ^-  (unit target)
    ~&  %multi-bind-support-missing
    ?~  grams  ~
    ~
    ::TODO
    :: =+  pan=(silt ~(tap in aud.i.grams))
    :: ?:  (~(has in lax) pan)  `pan
    :: $(grams t.grams)
  ::
  ++  print-envelope
    |=  [=target =envelope]
    ^-  (list move)
    %+  weld
      ^-  (list move)
      ?.  =(0 (mod count 5))  ~
      :_  ~
      =+  num=(scow %ud count)
      %-  print
      (runt [(sub 13 (lent num)) '-'] "[{num}]")
    ::TODO  %notify logic? or do elsewhere? just check the %text msgs
    =+  lis=~(render tr settings target envelope)
    ?~  lis  ~
    :_  ~
    %+  effect  %mor
    %+  turn  `(list tape)`lis
    =+  nom=(scag 7 (cite:title our-self))
    |=  t=tape
    ?.  ?&  (~(has in settings) %notify)
            ?=(^ (find nom (slag 15 t)))
        ==
      [%txt t]
    [%mor [%txt t] [%bel ~] ~]
  ::
  ++  show-create
    |=  =target
    ^-  move
    (note "new: {~(cr-phat cr target)}")
  ::
  ++  show-delete
    |=  =target
    ^-  move
    (note "del: {~(cr-phat cr target)}")
  ::
  ++  show-glyph
    |=  [=glyph target=(unit target)]
    ^-  (list move)
    =-  [prompt - ~]
    %-  note
    %+  weld  "set: {[glyph ~]} -> "
    ?~  target  "nothing"
    ~(cr-phat cr u.target)
  --
::
::
::TODO  code style
++  ar
  ::    audience renderer
  ::
  ::  used for representing audiences (sets of circles)
  ::  as tapes.
  ::
  |_  ::  aud: members of the audience.
      ::
      aud=(set target)
  ::
  ++  ar-best
    ::  find the most relevant circle in the set.
    ::
    ^-  (unit target)
    ?~  aud  ~
    :-  ~
    |-  ^-  target
    =+  lef=`(unit target)`ar-best(aud l.aud)
    =+  rit=`(unit target)`ar-best(aud r.aud)
    =?  n.aud  ?=(^ lef)  (~(cr-best cr n.aud) u.lef)
    =?  n.aud  ?=(^ rit)  (~(cr-best cr n.aud) u.rit)
    n.aud
  ::
  ++  ar-deaf
    ::  remove ourselves from the audience.
    ::
    ^+  .
    .(aud (~(del in aud) [our-self /inbox]))
  ::
  ++  ar-maud
    ::    multiple audience
    ::
    ::  checks if there's multiple circles in the
    ::  audience via pattern matching.
    ::
    ^-  ?
    =.  .  ar-deaf
    !?=($@(~ {* ~ ~}) aud)
  ::
  ++  ar-phat
    ::  render all circles, no glyphs.
    ::
    ^-  tape
    %-  ~(rep in aud)
    |=  {c/target t/tape}
    =?  t  ?=(^ t)
      (weld t ", ")
    (weld t ~(cr-phat cr c))
  ::
  ++  ar-prom
    ::  render all circles, ordered by relevance.
    ::
    ^-  tape
    =.  .  ar-deaf
    =/  all
      %+  sort  `(list target)`~(tap in aud)
      |=  {a/target b/target}
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
    ::  render sender as the most relevant circle.
    ::
    (~(cr-show cr (need ar-best)) ~ ar-maud)
  --
::
++  cr
  ::    target renderer
  ::
  ::  used in both target and ship rendering.
  ::
  |_  ::  one: the target.
      ::
      one=target
  ::
  ++  cr-beat
    ::    {one} more relevant?
    ::
    ::  returns true if one is better to show, false
    ::  otherwise. prioritizes: our > main > size.
    ::
    |=  two=target
    ^-  ?
    ::  the target that's ours is better.
    ?:  =(our-self ship.one)
      ?.  =(our-self ship.two)  &
      ?<  =(path.one path.two)
      ::  if both targets are ours, the main story is better.
      ?:  =(%inbox path.one)  &
      ?:  =(%inbox path.two)  |
      ::  if neither are, pick the "larger" one.
      (lth (lent path.one) (lent path.two))
    ::  if one isn't ours but two is, two is better.
    ?:  =(our-self ship.two)  |
    ?:  =(ship.one ship.two)
      ::  if they're from the same ship, pick the "larger" one.
      (lth (lent path.one) (lent path.two))
    ::  if they're from different ships, neither ours, pick hierarchically.
    (lth (xeb ship.one) (xeb ship.two))
  ::
  ++  cr-best
    ::  returns the most relevant target.
    ::
    |=  two=target
    ?:((cr-beat two) one two)
  ::
  ++  cr-curt
    ::    prints a ship name in 14 characters.
    ::
    ::  left-pads with spaces. {mup} signifies
    ::  "are there other targets besides this one?"
    ::
    |=  mup=?
    ^-  tape
    =+  raw=(cite:title ship.one)
    (runt [(sub 14 (lent raw)) ' '] raw)
  ::
  ++  cr-nick
    ::    get nick for ship, or shortname if no nick.
    ::
    ::  left-pads with spaces.
    ::
    |=  source=target
    ::TODO  get nick from contacts store?
    (cr-curt |)
  ::
  ++  cr-phat                                           :::  render accurately
    ::  prints a target fully as ~ship/path.
    ::  for local targets, print as /path.
    ::  for targets on our sponsor, ^/path.
    ::
    ^-  tape
    %+  weld
      ?:  =(our-self ship.one)  ~
      ?:  =((sein:title our.bowl now.bowl our-self) ship.one)  "^"
      (scow %p ship.one)
    (spud path.one)
  ::
  ++  cr-full  (cr-show ~)                              ::  render full width
  ::
  ++  cr-show
    ::  renders a target as text.
    ::
    ::  moy:  multiple targets in audience?
    |=  moy=(unit ?)
    ^-  tape
    ::  render target (as glyph if we can).
    ?~  moy
      =+  cha=(~(get by bound) one)
      =-  ?~(cha - "{u.cha ~}")
      ~(cr-phat cr one)
    (~(cr-curt cr one) u.moy)
  ::
  ++  cr-dire
    ::  returns true if circle is a mailbox of ours.
    ::
    |=  cir=target  ^-  ?
    ?&  =(ship.cir our-self)
        ::TODO  permissions check
    ==
  ::
  ++  cr-glyph
    ::    target glyph
    ::
    ::  get the glyph that corresponds to the target.
    ::  for mailboxes and complex audiences, use
    ::  reserved "glyphs".
    ::
    ^-  tape
    =+  gyf=(~(get by bound) one)
    ?^  gyf  ~[u.gyf]
    ?.  (cr-dire one)
      "*"
    ":"
  --
::
++  tr
  ::    telegram renderer
  ::
  ::  responsible for converting telegrams and
  ::  everything relating to them to text to be
  ::  displayed in the cli.
  ::
  |_  $:  settings=(set term)
          source=target
          envelope
      ==
  ::
  ++  tr-fact
    ::    activate effect
    ::
    ::  produces sole-effect for printing message
    ::  details.
    ::
    ^-  sole-effect:sole-sur
    ~[%mor [%tan tr-meta] tr-body]
  ::
  ++  render
    ::    renders a telegram
    ::
    ::  the first line will contain the author and
    ::  optional timestamp.
    ::
    ^-  (list tape)
    =/  wyd
      %+  sub  width                                    ::  termwidth,
      %+  add  14                                       ::  minus author,
      ?:((~(has in settings) %showtime) 10 0)           ::  minus timestamp.
    =+  txs=(tr-text wyd)
    ?~  txs  ~
    ::  render the author.
    =/  nom=tape
      ?:  (~(has in settings) %nicks)
        (~(cr-nick cr [author /inbox]) source)
      (~(cr-curt cr [author /inbox]) |)
    ::  regular indent.
    =/  den=tape
      (reap (lent nom) ' ')
    ::  timestamp, if desired.
    =/  tam=tape
      ?.  (~(has in settings) %showtime)  ""
      =.  when
        %.  [when (mul q.timez ~h1)]
        ?:(p.timez add sub)
      =+  dat=(yore when)
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
    |=  [t=tape l=(list tape)]
    ?~  l  [:(weld nom t tam) ~]
    [(weld den t) l]
  ::
  ++  tr-meta
    ::    metadata
    ::
    ::  builds string that display metadata, including
    ::  message serial, timestamp, author and audience.
    ::
    ^-  tang
    =.  when  (sub when (mod when (div when ~s0..0001)))    :: round
    =+  hed=leaf+"{(scow %uv uid)} at {(scow %da when)}"
    =/  src=tape  ~(cr-phat cr source)
    [%rose [" " ~ ~] [hed >author< [%rose [", " "to " ~] [leaf+src]~] ~]]~
  ::
  ++  tr-body
    ::    message content
    ::
    ::  long-form display of message contents, specific
    ::  to each speech type.
    ::
    |-  ^-  sole-effect:sole-sur
    ?-  -.letter
        %text
      tan+~[leaf+"{(trip text.letter)}"]
    ::
        %url
      url+url.letter
    ::
        %code
      =/  texp=tape  ['>' ' ' (trip expression.letter)]
      :-  %mor
      |-  ^-  (list sole-effect:sole-sur)
      ?:  =("" texp)  [tan+output.letter ~]
      =/  newl  (find "\0a" texp)
      ?~  newl  [txt+texp $(texp "")]
      =+  (trim u.newl texp)
      :-  txt+(scag u.newl texp)
      $(texp [' ' ' ' (slag +(u.newl) texp)])
    ==
  ::
  ++  tr-chow
    ::    truncate
    ::
    ::  truncates the {txt} to be of max {len}
    ::  characters. if it does truncate, indicates it
    ::  did so by appending _ or ….
    ::
    |=  [len=@u txt=tape]
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
    ::    compact contents
    ::
    ::  renders just the most important data of the
    ::  message. if possible, these stay within a single
    ::  line.
    ::
    ::  pre:  replace/append line prefix
    ::TODO  this should probably be redone someday.
    =|  pre=(unit (pair ? tape))
    |=  wyd=@ud
    ^-  (list tape)
    ?-  -.letter
        %code
      =+  texp=(trip expression.letter)
      =+  newline=(find "\0a" texp)
      =?  texp  ?=(^ newline)
        (weld (scag u.newline texp) "  ...")
      :-  (tr-chow wyd '#' ' ' texp)
      ?~  output.letter  ~
      =-  [' ' (tr-chow (dec wyd) ' ' -)]~
      ~(ram re (snag 0 `(list tank)`output.letter))
    ::
        %url
      :_  ~
      =+  ful=(trip url.letter)
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
      ::TODO  need kinda dangerous...
      =+  hok=r.p:(need (de-purl:html url.letter))
      =-  (swag [a=(sub (max wyd (lent -)) wyd) b=wyd] -)
      ^-  tape
      =<  ?:  ?=(%& -.hok)
            (reel p.hok .)
          +:(scow %if p.hok)
      |=  [a=knot b=tape]
      ?~  b  (trip a)
      (welp b '.' (trip a))
    ::
        %text
      ::  glyph prefix
      =/  pef=tape
        ?:  &(?=(^ pre) p.u.pre)  q.u.pre
        =-  (weld - q:(fall pre [p=| q=" "]))
        ~(cr-glyph cr source)
      =/  lis=(list tape)
        %+  simple-wrap
          `tape``(list @)`(tuba (trip text.letter))
        (sub wyd (min (div wyd 2) (lent pef)))
      =+  lef=(lent pef)
      =+  ?:((gth (lent lis) 0) (snag 0 lis) "")
      :-  (weld pef -)
      %+  turn  (slag 1 lis)
      |=(a=tape (runt [lef ' '] a))
    ==
  --
::
++  simple-wrap
  |=  {txt/tape wyd/@ud}
  ^-  (list tape)
  ?~  txt  ~
  =+  ^-  {end/@ud nex/?}
    ?:  (lte (lent txt) wyd)  [(lent txt) &]
    =+  ace=(find " " (flop (scag +(wyd) `tape`txt)))
    ?~  ace  [wyd |]
    [(sub wyd u.ace) &]
  :-  (tufa (scag end `(list @)`txt))
  $(txt (slag ?:(nex +(end) end) `tape`txt))
--