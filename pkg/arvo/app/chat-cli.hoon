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
/-  sole-sur=sole, *chat-store, *chat-view, *chat-hook
/+  sole-lib=sole
/=  seed  /~  !>(.)
::
|%
+$  state
  $:  grams=(list mail)                             ::  all messages
      known=(set [target serial])                   ::  known message lookup
      count=@ud                                     ::  (lent grams)
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
  $%  [%target (set target)]                        ::  set messaging target
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
      [%select $@(rel=@ud [zeros=@u abs=@ud])]      ::  rel/abs msg selection
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
++  this  .
::  +prep: setup & state adapter
::
++  prep
  |=  old=(unit state)
  ~&  %chat-cli-prep
  ?^  old
    [~ this(+<+ u.old)]
  :-  [connect ~]
  %_  this
    audience  [[our-self /] ~ ~]
    settings  (sy %showtime %notify ~)
    width  80
  ==
::  +connect: connect to the chat-store
::
++  connect
  ^-  move
  [ost.bowl %peer /chat-store [our-self %chat-store] /all]
::  +true-self: moons to planets
::
++  true-self
  |=  who=ship
  ^-  ship
  ?.  ?=(%earl (clan:title who))  who
  (sein:title our.bowl now.bowl who)
++  our-self  (true-self our.bowl)
::  +target-to-path: prepend ship to the path
::
++  target-to-path
  |=  target
  path
  ::TODO
  :: [(scot %p ship) path]
::  +path-to-target: deduces a target from a mailbox path
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
::  +poke-noun: debug helpers
::
++  poke-noun
  |=  a=*
  ^-  (quip move _this)
  ?:  ?=(%connect a)
    [[connect ~] this]
  [~ this]
::  +poke-sole-action: handle cli input
::
++  poke-sole-action
  |=  act=sole-action:sole-sur
  ^-  (quip move _this)
  ?.  =(bone.cli ost.bowl)
    ~|(%strange-sole !!)
  (sole:sh-in act)
::  +peer: accept only cli subscriptions from ourselves
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
  :-  [prompt:sh-out ~]
  ::  start with fresh sole state
  this(state.cli *sole-share:sole-sur)
::  +diff-chat-initial: catch up on messages
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
::  +diff-chat-update: get new mailboxes & messages
::
++  diff-chat-update
  |=  [=wire upd=chat-update]
  ^-  (quip move _this)
  ?+  -.upd  [~ this]
    %create   (notice-create (path-to-target path.upd))
    %delete   [[(show-delete:sh-out (path-to-target path.upd)) ~] this]
    %message  (read-envelope (path-to-target path.upd) envelope.upd)
  ==
::
++  read-envelopes
  |=  [=target envs=(list envelope)]
  ^-  (quip move _this)
  ?~  envs  [~ this]
  =^  moi  this  (read-envelope target i.envs)
  =^  mot  this  $(envs t.envs)
  [(weld moi mot) this]
::
++  notice-create
  |=  =target
  ^-  (quip move _this)
  =^  moz  this
    ?:  (~(has by bound) target)
      [~ this]
    (bind-default-glyph target)
  [[(show-create:sh-out target) moz] this]
::  +bind-default-glyph:
::
++  bind-default-glyph
  |=  =target
  ^-  (quip move _this)
  =-  (bind-glyph - target)
  ::TODO  try not to double-bind
  =-  (snag - glyphs)
  (mod (mug target) (lent glyphs))
::  +bind-glyph: add binding for glyph
::
++  bind-glyph
  |=  [=glyph =target]
  ^-  (quip move _this)
  ::TODO  should send these to settings store eventually
  ::TODO  disallow double-binding glyphs?
  =.  bound  (~(put by bound) target glyph)
  =.  binds  (~(put ju binds) glyph target)
  [(show-glyph:sh-out glyph `target) this]
::  +unbind-glyph: remove all binding for glyph
::
++  unbind-glyph
  |=  =glyph  ::TODO  do we really not want this optionally per-target?
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
  [(show-glyph:sh-out glyph ~) this]
::  +decode-glyph: find the target that matches a glyph, if any
::
++  decode-glyph
  |=  =glyph
  ^-  (unit target)
  =+  lax=(~(get ju binds) glyph)
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
::  +read-envelope: add envelope to state and show it to user
::
++  read-envelope
  |=  [=target =envelope]
  ^-  (quip move _this)
  ?:  (~(has in known) [target uid.envelope])
    ::NOTE  we no-op only because edits aren't possible
    [~ this]
  :-  (show-envelope:sh-out target envelope)
  %_  this
    known  (~(put in known) [target uid.envelope])
    grams  [[target envelope] grams]
    count  +(count)
  ==
::
::  +sh-in: handle user input
::
++  sh-in
  ::NOTE  interestingly, adding =,  sh-out breaks compliation
  |%
  ::  +sole: apply sole action
  ::
  ++  sole
    |=  act=sole-action:sole-sur
    ^-  (quip move _this)
    ?-  -.act
      $det  (edit +.act)
      $clr  [~ this]
      $ret  obey
    ==
  ::  +edit: apply sole edit
  ::
  ::    called when typing into the cli prompt.
  ::    applies the change and does sanitizing.
  ::
  ++  edit
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
  ::  +sanity: check input sanity
  ::
  ::    parses cli prompt using +read.
  ::    if invalid, produces error correction description, for use with +slug.
  ::
  ++  sanity
    |=  [inv=sole-edit:sole-sur buf=(list @c)]
    ^-  [lit=(list sole-edit:sole-sur) err=(unit @u)]
    =+  res=(rose (tufa buf) read)
    ?:  ?=(%& -.res)  [~ ~]
    [[inv]~ `p.res]
  ::  +slug: apply error correction to prompt input
  ::
  ++  slug
    |=  [lit=(list sole-edit:sole-sur) err=(unit @u)]
    ^-  (quip move _this)
    ?~  lit  [~ this]
    =^  lic  state.cli
      %-  ~(transmit sole-lib state.cli)
      ^-  sole-edit:sole-sur
      ?~(t.lit i.lit [%mor lit])
    :_  this
    :_  ~
    %+  effect:sh-out  %mor
    :-  [%det lic]
    ?~(err ~ [%err u.err]~)
  ::  +read: command parser
  ::
  ::    parses the command line buffer.
  ::    produces commands which can be executed by +work.
  ::
  ++  read
    |^
      ~!  (scan "" (cmd %create [path ~] [glyph ~]))
      %+  knee  *command  |.  ~+
      =-  ;~(pose ;~(pfix mic -) message)
      ;~  pose
        (stag %target tars)
      ::
        ;~((glue ace) (tag %create) ;~(plug path (punt ;~(pfix ace glyph))))
        ;~((glue ace) (tag %join) ;~(plug targ (punt ;~(pfix ace glyph))))
        ;~((glue ace) (tag %leave) targ)
      ::
        ;~((glue ace) (tag %bind) glyph targ)
        ;~((glue ace) (tag %unbind) glyph)
        ;~(plug (perk %what ~) (punt ;~(pfix ace ;~(pose glyph targ))))
      ::
        ;~(plug (tag %settings) (easy ~))
        ;~((glue ace) (tag %set) flag)
        ;~((glue ace) (tag %unset) flag)
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
        (stag %select nump)
      ==
    ::
    ::TODO
    :: ++  cmd
    ::   |*  [cmd=term req=(list rule) opt=(list rule)]
    ::   |^  ;~  plug
    ::         (tag cmd)
    ::       ::
    ::         ::TODO  this feels slightly too dumb
    ::         ?~  req
    ::           ?~  opt  (easy ~)
    ::           (opt-rules opt)
    ::         ?~  opt  (req-rules req)
    ::         ;~(plug (req-rules req) (opt-rules opt))  ::TODO  rest-loop
    ::       ==
    ::   ++  req-rules
    ::     |*  req=(lest rule)
    ::     =-  ;~(pfix ace -)
    ::     ?~  t.req  i.req
    ::     ;~(plug i.req $(req t.req))
    ::   ++  opt-rules
    ::     |*  opt=(lest rule)
    ::     =-  (punt ;~(pfix ace -))
    ::     ?~  t.opt  ;~(pfix ace i.opt)
    ::     ;~(pfix ace ;~(plug i.opt $(opt t.opt)))
    ::   --
    ::
    ++  tag   |*(a=@tas (cold a (jest a)))  ::TODO  into stdlib
    ++  ship  ;~(pfix sig fed:ag)
    ++  path  ;~(pfix net (most net urs:ab))
    ::  +tarl: local target, as /path
    ::
    ++  tarl  (stag our-self path)
    ::  +tarp: sponsor target, as ^/path
    ::
    ++  tarp
      =-  ;~(pfix ket (stag - path))
      (sein:title our.bowl now.bowl our-self)
    ::  +targ: any target, as tarl, tarp, ~ship/path or glyph
    ::
    ++  targ
      ;~  pose
        tarl
        tarp
        ;~(plug ship path)
        (sear decode-glyph glyph)
      ==
    ::  +tars: set of comma-separated targs
    ::
    ++  tars
      %+  cook  ~(gas in *(set target))
      (most ;~(plug com (star ace)) targ)
    ::  +glyph: shorthand character
    ::
    ++  glyph  (mask glyphs)
    ::  +flag: valid flag
    ::
    ++  flag
      %-  perk  :~
        %notify
        %showtime
      ==
    ::  +nump: message number reference
    ::
    ++  nump
      ;~  pose
        ;~(pfix hep dem:ag)
        ;~  plug
          (cook lent (plus (just '0')))
          ;~(pose dem:ag (easy 0))
        ==
        (stag 0 dem:ag)
        (cook lent (star mic))
      ==
    ::  +message: lin, url, or #exp message
    ::
    ++  message
      ;~  pose
        ;~(plug (cold %eval hax) expr)
        (stag %say letters)
      ==
    ::  +letters: •-separated text or url messages
    ::
    ++  letters
      %+  most  (jest '•')
      ;~  pose
        (stag %url turl)
        :(stag %me ;~(pfix vat text))
        :(stag %text ;~(less mic hax text))
      ==
    ::  +turl: url parser
    ::
    ++  turl
      =-  (sear - text)
      |=  t=cord
      ^-  (unit cord)
      ?~((rush t aurf:de-purl:html) ~ `t)
    ::  +text: text message body
    ::
    ++  text
      %+  cook  crip
      (plus ;~(less (jest '•') next))
    ::  +expr: parse expression into [cord hoon]
    ::
    ++  expr
      |=  tub=nail
      %.  tub
      %+  stag  (crip q.tub)
      wide:(vang & [&1:% &2:% (scot %da now.bowl) |3:%])
    --
  ::  +obey: apply result
  ::
  ::    called upon hitting return in the prompt.
  ::    if input is invalid, +slug is called.
  ::    otherwise, the appropriate work is done and
  ::    the command (if any) gets echoed to the user.
  ::
  ++  obey
    ^-  (quip move _this)
    =+  buf=buf.state.cli
    =+  fix=(sanity [%nop ~] buf)
    ?^  lit.fix
      (slug fix)
    =+  jub=(rust (tufa buf) read)
    ?~  jub  [[(effect:sh-out %bel ~) ~] this]
    =^  cal  state.cli  (~(transmit sole-lib state.cli) [%set ~])
    =^  moves  this  (work u.jub)
    :_  this
    %+  weld
      ^-  (list move)
      ::  echo commands into scrollback
      ?.  =(`0 (find ";" buf))  ~
      [(note:sh-out (tufa `(list @)`buf)) ~]
    :_  moves
    %+  effect:sh-out  %mor
    :~  [%nex ~]
        [%det cal]
    ==
  ::  +work: run user command
  ::
  ++  work
    |=  job=command
    ^-  (quip move _this)
    |^  ?-  -.job
          %target    (set-target +.job)
          %say       (say +.job)
          %eval      (eval +.job)
        ::
          %join      (join +.job)
          %leave     (leave +.job)
          %create    (create +.job)
        ::
          %bind      (bind-glyph +.job)
          %unbind    (unbind-glyph +.job)
          %what      (lookup-glyph +.job)
        ::
          %settings  show-settings
          %set       (set-setting +.job)
          %unset     (unset-setting +.job)
          %width     (set-width +.job)
          %timezone  (set-timezone +.job)
        ::
          %select    (select +.job)
          %chats     chats
          %help      help
        ==
    ::  +act: build action move
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
    ::  +set-target: set audience, update prompt
    ::
    ++  set-target
      |=  tars=(set target)
      ^-  (quip move _this)
      =.  audience  tars
      [[prompt:sh-out ~] this]
    ::  +create: new local mailbox
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
    ::  +join: sync with remote mailbox
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
    ::  +leave: unsync & destroy mailbox
    ::
    ::TODO  but if we leave our own circle, then it disappears for everyone?
    ++  leave
      |=  =target
      =-  [[- ~] this]
      %^  act  %do-leave  %chat-hook
      :-  %chat-hook-action
      [%remove (target-to-path target)]
    ::  +say: send messages
    ::
    ++  say
      |=  letters=(list letter)
      ^-  (quip move _this)
      =/  =serial  (shaf %msg-uid eny.bowl)
      :_  this(eny.bowl (shax eny.bowl))
      ^-  (list move)
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
    ::  +eval: run hoon, send code and result as message
    ::
    ::    this double-virtualizes and clams to disable .^ for security reasons
    ::
    ++  eval
      |=  [txt=cord exe=hoon]
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
    ::  +lookup-glyph: print glyph info for all, glyph or target
    ::
    ++  lookup-glyph
      |=  qur=(unit $@(glyph target))
      ^-  (quip move _this)
      =-  [[- ~] this]
      ?^  qur
        ?^  u.qur
          =+  gyf=(~(get by bound) u.qur)
          (print:sh-out ?~(gyf "none" [u.gyf]~))
        =+  pan=~(tap in (~(get ju binds) `@t`u.qur))
        ?:  =(~ pan)  (print:sh-out "~")
        =<  (effect:sh-out %mor (turn pan .))
        |=(t=target [%txt ~(phat tr t)])
      %-  print-more:sh-out
      %-  ~(rep by binds)
      |=  $:  [=glyph tars=(set target)]
              lis=(list tape)
          ==
      %+  weld  lis
      ^-  (list tape)
      %-  ~(rep in tars)
      |=  [t=target l=(list tape)]
      %+  weld  l
      ^-  (list tape)
      [glyph ' ' ~(phat tr t)]~
    ::  +show-settings: print enabled flags, timezone and width settings
    ::
    ++  show-settings
      ^-  (quip move _this)
      :_  this
      :~  %-  print:sh-out
          %-  zing
          ^-  (list tape)
          :-  "flags: "
          %+  ^join  ", "
          (turn `(list @t)`~(tap in settings) trip)
        ::
          %-  print:sh-out
          %+  weld  "timezone: "
          ^-  tape
          :-  ?:(p.timez '+' '-')
          (scow %ud q.timez)
        ::
          (print:sh-out "width: {(scow %ud width)}")
      ==
    ::  +set-setting: enable settings flag
    ::
    ++  set-setting
      |=  =term
      ^-  (quip move _this)
      [~ this(settings (~(put in settings) term))]
    ::  +unset-setting: disable settings flag
    ::
    ++  unset-setting
      |=  =term
      ^-  (quip move _this)
      [~ this(settings (~(del in settings) term))]
    ::  +set-width: configure cli printing width
    ::
    ++  set-width
      |=  w=@ud
      [~ this(width w)]
    ::  +set-timezone: configure timestamp printing adjustment
    ::
    ++  set-timezone
      |=  tz=[? @ud]
      [~ this(timez tz)]
    ::  +select: expand message from number reference
    ::
    ++  select
      ::NOTE  rel is the nth most recent message,
      ::      abs is the last message whose numbers ends in n
      ::      (with leading zeros used for precision)
      ::
      |=  num=$@(rel=@ud [zeros=@u abs=@ud])
      ^-  (quip move _this)
      |^  ?@  num
            =+  tum=(scow %s (new:si | +(num)))
            ?:  (gte rel.num count)
              %-  just-print
              "{tum}: no such telegram"
            (activate tum rel.num)
          ?.  (gte abs.num count)
            ?:  =(count 0)
              (just-print "0: no messages")
            =+  msg=(index (dec count) num)
            (activate (scow %ud msg) (sub count +(msg)))
          %-  just-print
          "…{(reap zeros.num '0')}{(scow %ud abs.num)}: no such telegram"
      ::  +just-print: full [moves state] output with a single print move
      ::
      ++  just-print
        |=  txt=tape
        [[(print:sh-out txt) ~] this]
      ::  +index: get message index from absolute reference
      ::
      ++  index
        |=  [max=@ud nul=@u fin=@ud]
        ^-  @ud
        =+  dog=|-(?:(=(0 fin) 1 (mul 10 $(fin (div fin 10)))))
        =.  dog  (mul dog (pow 10 nul))
        =-  ?:((lte - max) - (sub - dog))
        (add fin (sub max (mod max dog)))
      ::  +activate: echo message selector and print details
      ::
      ++  activate
        |=  [number=tape index=@ud]
        ^-  (quip move _this)
        =+  gam=(snag index grams)
        =.  audience  [source.gam ~ ~]
        :_  this
        ^-  (list move)
        :~  (print:sh-out ['?' ' ' number])
            (effect:sh-out ~(render-activate mr gam))
            prompt:sh-out
        ==
      --
    ::  +chats: display list of local mailboxes
    ::
    ++  chats
      ^-  (quip move _this)
      :_  this
      :_  ~
      %-  print-more:sh-out
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
      (weld (scow %p ship) (spud path))
    ::  +help: print (link to) usage instructions
    ::
    ++  help
      ^-  (quip move _this)
      =-  [[- ~] this]
      (print:sh-out "see https://urbit.org/docs/using/messaging/")
    --
  --
::
::  +sh-out: output to the cli
::
++  sh-out
  |%
  ::  +effect: console effect move
  ::
  ++  effect
    |=  fec=sole-effect:sole-sur
    ^-  move
    [bone.cli %diff %sole-effect fec]
  ::  +print: puts some text into the cli as-is
  ::
  ++  print
    |=  txt=tape
    ^-  move
    (effect %txt txt)
  ::  +print-more: puts lines of text into the cli
  ::
  ++  print-more
    |=  txs=(list tape)
    ^-  move
    %+  effect  %mor
    (turn txs |=(t=tape [%txt t]))
  ::  +note: prints left-padded ---| txt
  ::
  ++  note
    |=  txt=tape
    ^-  move
    =+  lis=(simple-wrap txt (sub width 16))
    %-  print-more
    =+  ?:((gth (lent lis) 0) (snag 0 lis) "")
    :-  (runt [14 '-'] '|' ' ' -)
    %+  turn  (slag 1 lis)
    |=(a=tape (runt [14 ' '] '|' ' ' a))
  ::  +prompt: update prompt to display current audience
  ::
  ++  prompt
    ^-  move
    %+  effect  %pro
    :+  &  %talk-line
    ^-  tape
    =-  ?:  =(1 (lent -))  "{-} "
        "[{-}] "
    =/  all
      %+  sort  ~(tap in audience)
      |=  [a=target b=target]
      (~(beat tr a) b)
    =+  fir=&
    |-  ^-  tape
    ?~  all  ~
    ;:  welp
      ?:(fir "" " ")
      ~(show tr i.all)
      $(all t.all, fir |)
    ==
  ::  +show-envelope: print incoming message
  ::
  ::    every five messages, prints the message number also.
  ::    if the message mentions the user's (shortened) ship name,
  ::    and the %notify flag is set, emit a bell.
  ::
  ++  show-envelope
    |=  [=target =envelope]
    ^-  (list move)
    %+  weld
      ^-  (list move)
      ?.  =(0 (mod count 5))  ~
      :_  ~
      =+  num=(scow %ud count)
      %-  print
      (runt [(sub 13 (lent num)) '-'] "[{num}]")
    =+  lis=~(render-inline mr target envelope)
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
  ::  +show-create: print mailbox creation notification
  ::
  ++  show-create
    |=  =target
    ^-  move
    (note "new: {~(phat tr target)}")
  ::  +show-delete: print mailbox deletion notification
  ::
  ++  show-delete
    |=  =target
    ^-  move
    (note "del: {~(phat tr target)}")
  ::  +show-glyph: print glyph un/bind notification
  ::
  ++  show-glyph
    |=  [=glyph target=(unit target)]
    ^-  (list move)
    =-  [prompt - ~]
    %-  note
    %+  weld  "set: {[glyph ~]} -> "
    ?~  target  "nothing"
    ~(phat tr u.target)
  --
::
::  +tr: render targets
::
++  tr
  |_  ::  one: the target.
      ::
      one=target
  ::  +beat: true if one is more "relevant" than two
  ::
  ++  beat
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
  ::  +phat: render target fully
  ::
  ::    renders as ~ship/path.
  ::    for local mailboxes, renders just /path.
  ::    for sponsor's mailboxes, renders ^/path.
  ::
  ::NOTE  but, given current implementation, all will be local
  ::
  ++  phat
    ^-  tape
    %+  weld
      ?:  =(our-self ship.one)  ~
      ?:  =((sein:title our.bowl now.bowl our-self) ship.one)  "^"
      (scow %p ship.one)
    (spud path.one)
  ::  +show: render as tape, as glyph if we can
  ::
  ++  show
    ^-  tape
    =+  cha=(~(get by bound) one)
    ?~(cha phat "{u.cha ~}")
  ::  +glyph: tape for glyph of target, defaulting to *
  ::
  ++  glyph
    ^-  tape
    [(~(gut by bound) one '*') ~]
  --
::
::  +mr: render messages
::
++  mr
  |_  $:  source=target
          envelope
      ==
  ::  +activate: produce sole-effect for printing message details
  ::
  ++  render-activate
    ^-  sole-effect:sole-sur
    ~[%mor [%tan meta] body]
  ::  +meta: render message metadata (serial, timestamp, author, target)
  ::
  ++  meta
    ^-  tang
    =.  when  (sub when (mod when (div when ~s0..0001)))    :: round
    =+  hed=leaf+"{(scow %uv uid)} at {(scow %da when)}"
    =/  src=tape  ~(phat tr source)
    [%rose [" " ~ ~] [hed >author< [%rose [", " "to " ~] [leaf+src]~] ~]]~
  ::  +body: long-form render of message contents
  ::
  ++  body
    |-  ^-  sole-effect:sole-sur
    ?-  -.letter
        ?(%text %me)
      =/  pre=tape  ?:(?=(%me -.letter) "@ " "")
      tan+~[leaf+"{pre}{(trip +.letter)}"]
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
  ::  +render-inline: produces lines to display message body in scrollback
  ::
  ++  render-inline
    ^-  (list tape)
    =/  wyd
      ::  termwidth,
      %+  sub  width
      ::  minus autor,
      %+  add  14
      ::  minus timestamp.
      ?:((~(has in settings) %showtime) 10 0)
    =+  txs=(line wyd)
    ?~  txs  ~
    ::  nom: rendered author
    ::  den: regular indent
    ::  tam: timestamp, if desired
    ::
    =/  nom=tape  (nome author)
    =/  den=tape  (reap (lent nom) ' ')
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
  ::  +nome: prints a ship name in 14 characters, left-padding with spaces
  ::
  ++  nome
    |=  =ship
    ^-  tape
    =+  raw=(cite:title ship)
    (runt [(sub 14 (lent raw)) ' '] raw)
  ::  +line: renders most important contents, tries to fit one line
  ::
  ::TODO  this should probably be rewritten someday
  ++  line
    ::  pre:  replace/append line prefix
    ::
    =|  pre=(unit (pair ? tape))
    |=  wyd=@ud
    ^-  (list tape)
    ?-  -.letter
        %code
      =+  texp=(trip expression.letter)
      =+  newline=(find "\0a" texp)
      =?  texp  ?=(^ newline)
        (weld (scag u.newline texp) "  ...")
      :-  (truncate wyd '#' ' ' texp)
      ?~  output.letter  ~
      =-  [' ' (truncate (dec wyd) ' ' -)]~
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
      =+  prl=(rust ful aurf:de-purl:html)
      ?~  prl  (weld (scag (dec wyd) ful) "…")
      =+  hok=r.p.p.u.prl
      =-  (swag [a=(sub (max wyd (lent -)) wyd) b=wyd] -)
      ^-  tape
      =<  ?:  ?=(%& -.hok)
            (reel p.hok .)
          +:(scow %if p.hok)
      |=  [a=knot b=tape]
      ?~  b  (trip a)
      (welp b '.' (trip a))
    ::
        ?(%text %me)
      ::  glyph prefix
      =/  pef=tape
        ?:  &(?=(^ pre) p.u.pre)  q.u.pre
        ?:  ?=(%me -.letter)  " "
        =-  (weld - q:(fall pre [p=| q=" "]))
        ~(glyph tr source)
      =/  lis=(list tape)
        %+  simple-wrap
          `tape``(list @)`(tuba (trip +.letter))
        (sub wyd (min (div wyd 2) (lent pef)))
      =+  lef=(lent pef)
      =+  ?:((gth (lent lis) 0) (snag 0 lis) "")
      :-  (weld pef -)
      %+  turn  (slag 1 lis)
      |=(a=tape (runt [lef ' '] a))
    ==
  ::  +truncate: truncate txt to fit len, indicating truncation with _ or …
  ::
  ++  truncate
    |=  [len=@u txt=tape]
    ^-  tape
    ?:  (gth len (lent txt))  txt
    =.  txt  (scag len txt)
    |-
    ?~  txt  txt
    ?:  =(' ' i.txt)
      |-
      :-  '_'
      ?.  ?=([%' ' *] t.txt)
        t.txt
      $(txt t.txt)
    ?~  t.txt  "…"
    [i.txt $(txt t.txt)]
  --
::
++  simple-wrap
  |=  [txt=tape wid=@ud]
  ^-  (list tape)
  ?~  txt  ~
  =+  ^-  [end=@ud nex=?]
    ?:  (lte (lent txt) wid)  [(lent txt) &]
    =+  ace=(find " " (flop (scag +(wid) `tape`txt)))
    ?~  ace  [wid |]
    [(sub wid u.ace) &]
  :-  (tufa (scag end `(list @)`txt))
  $(txt (slag ?:(nex +(end) end) `tape`txt))
--