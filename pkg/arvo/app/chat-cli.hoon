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
/-  *chat-store, *chat-view, *chat-hook,
    *permission-store, *group-store, *invite-store,
    sole-sur=sole
/+  sole-lib=sole, chat-eval, default-agent, verb
::
|%
+$  state
  $:  grams=(list mail)                             ::  all messages
      known=(set [target serial])                   ::  known message lookup
      count=@ud                                     ::  (lent grams)
      bound=(map target glyph)                      ::  bound circle glyphs
      binds=(jug glyph target)                      ::  circle glyph lookup
      audience=(set target)                         ::  active targets
      settings=(set term)                           ::  frontend flags
      width=@ud                                     ::  display width
      timez=(pair ? @ud)                            ::  timezone adjustment
      cli=state=sole-share:sole-sur                 ::  console state
      eny=@uvJ                                      ::  entropy
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
      [%say letter]                                 ::  send message
      [%eval cord hoon]                             ::  send #-message
    ::
      [%create chat-security path (unit glyph)]     ::  create chat
      [%delete path]                                ::  delete chat
      [%invite ?(%r %w %rw) path (set ship)]        ::  allow
      [%banish ?(%r %w %rw) path (set ship)]        ::  disallow
    ::
      [%join target (unit glyph)]                   ::  join target
      [%leave target]                               ::  nuke target
    ::
      [%bind glyph target]                          ::  bind glyph
      [%unbind glyph (unit target)]                 ::  unbind glyph
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
+$  card  card:agent:gall
--
=|  state
=*  all-state  -
=<
  %+  verb  |
  ^-  agent:gall
  |_  =bowl:gall
  +*  this       .
      talk-core  +>
      tc         ~(. talk-core(eny eny.bowl) bowl)
      def        ~(. (default-agent this %|) bowl)
  ::
  ++  on-init
    ^-  (quip card _this)
    =^  cards  all-state  (prep:tc ~)
    [cards this]
  ::
  ++  on-save
    !>(all-state)
  ::
  ++  on-load
    |=  old-state=vase
    ^-  (quip card _this)
    =/  old  !<(state old-state)
    =^  cards  all-state  (prep:tc `old)
    [cards this]
  ::
  ++  on-poke
    |=  [=mark =vase]
    ^-  (quip card _this)
    =^  cards  all-state
      ?+  mark        (on-poke:def mark vase)
        %noun         (poke-noun:tc mark !<(* vase))
        %sole-action  (poke-sole-action:tc !<(sole-action:sole-sur vase))
      ==
    [cards this]
  ::
  ++  on-watch
    |=  =path
    ^-  (quip card _this)
    =^  cards  all-state  (peer:tc path)
    [cards this]
  ::
  ++  on-leave  on-leave:def
  ++  on-peek   on-peek:def
  ++  on-agent
    |=  [=wire =sign:agent:gall]
    ^-  (quip card _this)
    =^  cards  all-state
      ?-    -.sign
          %poke-ack   [- all-state]:(on-agent:def wire sign)
          %watch-ack  [- all-state]:(on-agent:def wire sign)
          %kick       ~&  %chat-cli-kicked  `all-state
          %fact
        ?+  p.cage.sign  ~|([%chat-cli-bad-sub-mark wire p.cage.sign] !!)
          %chat-update  (diff-chat-update:tc wire !<(chat-update q.cage.sign))
        ==
      ==
    [cards this]
  ::
  ++  on-arvo   on-arvo:def
  ++  on-fail   on-fail:def
  --
::
|_  =bowl:gall
::  +prep: setup & state adapter
::
++  prep
  |=  old=(unit state)
  ^-  (quip card state)
  ?^  old
    [~ u.old]
  =^  cards  all-state
    %_  catch-up
      audience  [[our-self /] ~ ~]
      settings  (sy %showtime %notify ~)
      width  80
    ==
  [[connect cards] all-state]
::  +catch-up: process all chat-store state
::
++  catch-up
  ^-  (quip card state)
  =/  =inbox
    .^  inbox
        %gx
        (scot %p our.bowl)
        %chat-store
        (scot %da now.bowl)
        /all/noun
    ==
  |-  ^-  (quip card state)
  ?~  inbox  [~ all-state]
  =*  path  p.n.inbox
  =*  mailbox  q.n.inbox
  =/  =target  (path-to-target path)
  =^  cards-n  all-state  (read-envelopes target envelopes.mailbox)
  =^  cards-l  all-state  $(inbox l.inbox)
  =^  cards-r  all-state  $(inbox r.inbox)
  [:(weld cards-n cards-l cards-r) all-state]
::  +connect: connect to the chat-store
::
++  connect
  ^-  card
  [%pass /chat-store %agent [our-self %chat-store] %watch /updates]
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
  [(scot %p ship) path]
::  +path-to-target: deduces a target from a mailbox path
::
++  path-to-target
  |=  =path
  ^-  target
  ?.  ?=([@ @ *] path)
    ::TODO  can we safely assert the above?
    ~&  [%path-without-host path]
    [our-self path]
  =+  who=(slaw %p i.path)
  ?~  who  [our-self path]
  [u.who t.path]
::  +poke-noun: debug helpers
::
++  poke-noun
  |=  a=*
  ^-  (quip card state)
  ?:  ?=(%connect a)
    [[connect ~] all-state]
  ?:  ?=(%catch-up a)
    catch-up
  [~ all-state]
::  +poke-sole-action: handle cli input
::
++  poke-sole-action
  ::TODO  use id.act to support multiple separate sessions
  |=  [act=sole-action:sole-sur]
  ^-  (quip card state)
  (sole:sh-in act)
::  +peer: accept only cli subscriptions from ourselves
::
++  peer
  |=  =path
  ^-  (quip card state)
  ?.  (team:title our-self src.bowl)
    ~|  [%peer-talk-stranger src.bowl]
    !!
  ?.  ?=([%sole *] path)
    ~|  [%peer-talk-strange path]
    !!
  ::  display a fresh prompt
  :-  [prompt:sh-out ~]
  ::  start with fresh sole state
  all-state(state.cli *sole-share:sole-sur)
::  +diff-chat-update: get new mailboxes & messages
::
++  diff-chat-update
  |=  [=wire upd=chat-update]
  ^-  (quip card state)
  ?+  -.upd  [~ all-state]
    %create   (notice-create +.upd)
    %delete   [[(show-delete:sh-out (path-to-target path.upd)) ~] all-state]
    %message  (read-envelope (path-to-target path.upd) envelope.upd)
  ==
::
++  read-envelopes
  |=  [=target envs=(list envelope)]
  ^-  (quip card state)
  ?~  envs  [~ all-state]
  =^  cards-i  all-state  (read-envelope target i.envs)
  =^  cards-t  all-state  $(envs t.envs)
  [(weld cards-i cards-t) all-state]
::
++  notice-create
  |=  =target
  ^-  (quip card state)
  =^  cards  all-state
    ?:  (~(has by bound) target)
      [~ all-state]
    (bind-default-glyph target)
  [[(show-create:sh-out target) cards] all-state]
::  +bind-default-glyph: bind to default, or random available
::
++  bind-default-glyph
  |=  =target
  ^-  (quip card state)
  =;  =glyph  (bind-glyph glyph target)
  |^  =/  g=glyph  (choose glyphs)
      ?.  (~(has by binds) g)  g
      =/  available=(list glyph)
        %~  tap  in
        (~(dif in `(set glyph)`(sy glyphs)) ~(key by binds))
      ?~  available  g
      (choose available)
  ++  choose
    |=  =(list glyph)
    =;  i=@ud  (snag i list)
    (mod (mug target) (lent list))
  --
::  +bind-glyph: add binding for glyph
::
++  bind-glyph
  |=  [=glyph =target]
  ^-  (quip card state)
  ::TODO  should send these to settings store eventually
  ::  if the target was already bound to another glyph, un-bind that
  ::
  =?  binds  (~(has by bound) target)
    (~(del ju binds) (~(got by bound) target) target)
  =.  bound  (~(put by bound) target glyph)
  =.  binds  (~(put ju binds) glyph target)
  [(show-glyph:sh-out glyph `target) all-state]
::  +unbind-glyph: remove all binding for glyph
::
++  unbind-glyph
  |=  [=glyph targ=(unit target)]
  ^-  (quip card state)
  ?^  targ
    =.  binds  (~(del ju binds) glyph u.targ)
    =.  bound  (~(del by bound) u.targ)
    [(show-glyph:sh-out glyph ~) all-state]
  =/  ole=(set target)
    (~(get ju binds) glyph)
  =.  binds  (~(del by binds) glyph)
  =.  bound
    |-
    ?~  ole  bound
    =.  bound  $(ole l.ole)
    =.  bound  $(ole r.ole)
    (~(del by bound) n.ole)
  [(show-glyph:sh-out glyph ~) all-state]
::  +decode-glyph: find the target that matches a glyph, if any
::
++  decode-glyph
  |=  =glyph
  ^-  (unit target)
  =+  lax=(~(get ju binds) glyph)
  ::  no circle
  ?:  =(~ lax)  ~
  %-  some
  ::  single circle
  ?:  ?=([* ~ ~] lax)  n.lax
  ::  in case of multiple audiences, pick the most recently active one
  |-  ^-  target
  ?~  grams  -:~(tap in lax)
  =*  source  source.i.grams
  ?:  (~(has in lax) source)
    source
  $(grams t.grams)
::  +read-envelope: add envelope to state and show it to user
::
++  read-envelope
  |=  [=target =envelope]
  ^-  (quip card state)
  ?:  (~(has in known) [target uid.envelope])
    ::NOTE  we no-op only because edits aren't possible
    [~ all-state]
  :-  (show-envelope:sh-out target envelope)
  %_  all-state
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
    ^-  (quip card state)
    ?-  -.dat.act
      %det  (edit +.dat.act)
      %clr  [~ all-state]
      %ret  obey
      %tab  [~ all-state]
    ==
  ::  +edit: apply sole edit
  ::
  ::    called when typing into the cli prompt.
  ::    applies the change and does sanitizing.
  ::
  ++  edit
    |=  cal=sole-change:sole-sur
    ^-  (quip card state)
    =^  inv  state.cli  (~(transceive sole-lib state.cli) cal)
    =+  fix=(sanity inv buf.state.cli)
    ?~  lit.fix
      [~ all-state]
    ::  just capital correction
    ?~  err.fix
      (slug fix)
    ::  allow interior edits and deletes
    ?.  &(?=($del -.inv) =(+(p.inv) (lent buf.state.cli)))
      [~ all-state]
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
    ^-  (quip card state)
    ?~  lit  [~ all-state]
    =^  lic  state.cli
      %-  ~(transmit sole-lib state.cli)
      ^-  sole-edit:sole-sur
      ?~(t.lit i.lit [%mor lit])
    :_  all-state
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
      %+  knee  *command  |.  ~+
      =-  ;~(pose ;~(pfix mic -) message)
      ;~  pose
        (stag %target tars)
      ::
        ;~  (glue ace)
          (tag %create)
          security
          ;~(plug path (punt ;~(pfix ace glyph)))
        ==
        ;~((glue ace) (tag %delete) path)
        ;~((glue ace) (tag %invite) rw path ships)
        ;~((glue ace) (tag %banish) rw path ships)
      ::
        ;~((glue ace) (tag %join) ;~(plug targ (punt ;~(pfix ace glyph))))
        ;~((glue ace) (tag %leave) targ)
      ::
        ;~((glue ace) (tag %bind) glyph targ)
        ;~((glue ace) (tag %unbind) ;~(plug glyph (punt ;~(pfix ace targ))))
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
    ::  +ships: set of comma-separated ships
    ::
    ++  ships
      %+  cook  ~(gas in *(set ^ship))
      (most ;~(plug com (star ace)) ship)
    ::
    ::  +security: security mode
    ::
    ++  security
      (perk %channel %village %journal %mailbox ~)
    ::  +rw: read, write, or read-write
    ::
    ++  rw
      (perk %rw %r %w ~)
    ::
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
    ::  +message: all messages
    ::
    ++  message
      ;~  pose
        ;~(plug (cold %eval hax) expr)
        (stag %say letter)
      ==
    ::  +letter: simple messages
    ::
    ++  letter
      ;~  pose
        (stag %url turl)
        (stag %me ;~(pfix vat text))
        (stag %text ;~(less mic hax text))
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
    ^-  (quip card state)
    =+  buf=buf.state.cli
    =+  fix=(sanity [%nop ~] buf)
    ?^  lit.fix
      (slug fix)
    =+  jub=(rust (tufa buf) read)
    ?~  jub  [[(effect:sh-out %bel ~) ~] all-state]
    =^  cal  state.cli  (~(transmit sole-lib state.cli) [%set ~])
    =^  cards  all-state  (work u.jub)
    :_  all-state
    %+  weld
      ^-  (list card)
      ::  echo commands into scrollback
      ?.  =(`0 (find ";" buf))  ~
      [(note:sh-out (tufa `(list @)`buf)) ~]
    :_  cards
    %+  effect:sh-out  %mor
    :~  [%nex ~]
        [%det cal]
    ==
  ::  +work: run user command
  ::
  ++  work
    |=  job=command
    ^-  (quip card state)
    |^  ?-  -.job
          %target    (set-target +.job)
          %say       (say +.job)
          %eval      (eval +.job)
        ::
          %create    (create +.job)
          %delete    (delete +.job)
          %invite    (change-permission & +.job)
          %banish    (change-permission | +.job)
        ::
          %join      (join +.job)
          %leave     (leave +.job)
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
    ::  +act: build action card
    ::
    ++  act
      |=  [what=term app=term =cage]
      ^-  card
      :*  %pass
          /cli-command/[what]
          %agent
          [our-self app]
          %poke
          cage
      ==
    ::  +invite-card: build invite card
    ::
    ++  invite-card
      |=  [where=path who=ship]
      ^-  card
      :*  %pass
          /cli-command/invite
          %agent
          [who %invite-hook]  ::NOTE  only place chat-cli pokes others
          %poke
          %invite-action
        ::
          !>
          ^-  invite-action
          :^  %invite  /chat
            (shax (jam [our-self where] who))
          ^-  invite
          =;  desc=cord
            [our-self %chat-hook where who desc]
          %-  crip
          %+  weld
            "You have been invited to chat at "
          ~(full tr [our-self where])
      ==
    ::  +set-target: set audience, update prompt
    ::
    ++  set-target
      |=  tars=(set target)
      ^-  (quip card state)
      =.  audience  tars
      [[prompt:sh-out ~] all-state]
    ::  +create: new local mailbox
    ::
    ++  create
      |=  [security=chat-security =path gyf=(unit char)]
      ^-  (quip card state)
      ::TODO  check if already exists
      =/  =target  [our-self path]
      =.  audience  [target ~ ~]
      =^  moz  all-state
        ?.  ?=(^ gyf)  [~ all-state]
        (bind-glyph u.gyf target)
      =-  [[- moz] all-state]
      %^  act  %do-create  %chat-view
      :-  %chat-view-action
      !>
      :^  %create  path  security
      ::  ensure we can read from/write to our own chats
      ::
      :-  ::  read
          ?-  security
            ?(%channel %journal)  ~
            ?(%village %mailbox)  [our-self ~ ~]
          ==
      ::  write
      ?-  security
        ?(%channel %mailbox)  ~
        ?(%village %journal)  [our-self ~ ~]
      ==
    ::  +delete: delete local chats
    ::
    ++  delete
      |=  =path
      ^-  (quip card state)
      =-  [[- ~] all-state]
      %^  act  %do-delete  %chat-view
      :-  %chat-view-action
      !>
      [%delete (target-to-path our-self path)]
    ::  +change-permission: modify permissions on a local chat
    ::
    ++  change-permission
      |=  [allow=? rw=?(%r %w %rw) =path ships=(set ship)]
      ^-  (quip card state)
      :_  all-state
      =;  cards=(list card)
        ?.  allow  cards
        %+  weld  cards
        %+  turn  ~(tap in ships)
        (cury invite-card path)
      %+  murn
        ^-  (list term)
        ?-  rw
          %r   [%read ~]
          %w   [%write ~]
          %rw  [%read %write ~]
        ==
      |=  =term
      ^-  (unit card)
      =.  path
        =-  (snoc `^path`- term)
        [%chat (target-to-path our-self path)]
      ::  whitelist: empty if no matching permission, else true if whitelist
      ::
      =/  whitelist=(unit ?)
        =;  perm=(unit permission)
          ?~(perm ~ `?=(%white kind.u.perm))
        ::TODO  +permission-of-target?
        .^  (unit permission)
            %gx
            (scot %p our-self)
            %permission-store
            (scot %da now.bowl)
            %permission
            (snoc path %noun)
        ==
      ?~  whitelist
        ~&  [%weird-no-permission path]
        ~
      %-  some
      %^  act  %do-permission  %group-store
      :-  %group-action
      !>
      ?:  =(u.whitelist allow)
        [%add ships path]
      [%remove ships path]
    ::  +join: sync with remote mailbox
    ::
    ++  join
      |=  [=target gyf=(unit char)]
      ^-  (quip card state)
      =^  moz  all-state
        ?.  ?=(^ gyf)  [~ all-state]
        (bind-glyph u.gyf target)
      =.  audience  [target ~ ~]
      =;  =card
        [[card prompt:sh-out moz] all-state]
      ::TODO  ideally we'd check permission first. attempting this and failing
      ::      gives ugly %chat-hook-reap
      %^  act  %do-join  %chat-view
      :-  %chat-view-action
      !>
      [%join target]
    ::  +leave: unsync & destroy mailbox
    ::
    ::TODO  allow us to "mute" local chats using this
    ++  leave
      |=  =target
      =-  [[- ~] all-state]
      ?:  =(our-self ship.target)
        %-  print:sh-out
        "can't ;leave local chats, maybe use ;delete instead"
      %^  act  %do-leave  %chat-hook
      :-  %chat-hook-action
      !>
      [%remove (target-to-path target)]
    ::  +say: send messages
    ::
    ++  say
      |=  =letter
      ^-  (quip card state)
      ~!  bowl
      =/  =serial  (shaf %msg-uid eny.bowl)
      :_  all-state(eny (shax eny.bowl))
      ^-  (list card)
      %+  turn  ~(tap in audience)
      |=  =target
      %^  act  %out-message  %chat-hook
      :-  %chat-action
      !>
      :+  %message  (target-to-path target)
      [serial *@ our-self now.bowl letter]
    ::  +eval: run hoon, send code and result as message
    ::
    ::    this double-virtualizes and clams to disable .^ for security reasons
    ::
    ++  eval
      |=  [txt=cord exe=hoon]
      (say %code txt (eval:chat-eval bowl exe))
    ::  +lookup-glyph: print glyph info for all, glyph or target
    ::
    ++  lookup-glyph
      |=  qur=(unit $@(glyph target))
      ^-  (quip card state)
      =-  [[- ~] all-state]
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
      ^-  (quip card state)
      :_  all-state
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
      ^-  (quip card state)
      [~ all-state(settings (~(put in settings) term))]
    ::  +unset-setting: disable settings flag
    ::
    ++  unset-setting
      |=  =term
      ^-  (quip card state)
      [~ all-state(settings (~(del in settings) term))]
    ::  +set-width: configure cli printing width
    ::
    ++  set-width
      |=  w=@ud
      [~ all-state(width w)]
    ::  +set-timezone: configure timestamp printing adjustment
    ::
    ++  set-timezone
      |=  tz=[? @ud]
      [~ all-state(timez tz)]
    ::  +select: expand message from number reference
    ::
    ++  select
      ::NOTE  rel is the nth most recent message,
      ::      abs is the last message whose numbers ends in n
      ::      (with leading zeros used for precision)
      ::
      |=  num=$@(rel=@ud [zeros=@u abs=@ud])
      ^-  (quip card state)
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
      ::  +just-print: full [cards state] output with a single print card
      ::
      ++  just-print
        |=  txt=tape
        [[(print:sh-out txt) ~] all-state]
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
        ^-  (quip card state)
        =+  gam=(snag index grams)
        =.  audience  [source.gam ~ ~]
        :_  all-state
        ^-  (list card)
        :~  (print:sh-out ['?' ' ' number])
            (effect:sh-out ~(render-activate mr gam))
            prompt:sh-out
        ==
      --
    ::  +chats: display list of local mailboxes
    ::
    ++  chats
      ^-  (quip card state)
      :_  all-state
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
      ^-  (quip card state)
      =-  [[- ~] all-state]
      (print:sh-out "see https://urbit.org/using/operations/using-your-ship/#messaging")
    --
  --
::
::  +sh-out: output to the cli
::
++  sh-out
  |%
  ::  +effect: console effect card
  ::
  ++  effect
    |=  fec=sole-effect:sole-sur
    ^-  card
    ::TODO  don't hard-code session id 'drum' here
    [%give %fact `/sole/drum %sole-effect !>(fec)]
  ::  +print: puts some text into the cli as-is
  ::
  ++  print
    |=  txt=tape
    ^-  card
    (effect %txt txt)
  ::  +print-more: puts lines of text into the cli
  ::
  ++  print-more
    |=  txs=(list tape)
    ^-  card
    %+  effect  %mor
    (turn txs |=(t=tape [%txt t]))
  ::  +note: prints left-padded ---| txt
  ::
  ++  note
    |=  txt=tape
    ^-  card
    =+  lis=(simple-wrap txt (sub width 16))
    %-  print-more
    =+  ?:((gth (lent lis) 0) (snag 0 lis) "")
    :-  (runt [14 '-'] '|' ' ' -)
    %+  turn  (slag 1 lis)
    |=(a=tape (runt [14 ' '] '|' ' ' a))
  ::  +prompt: update prompt to display current audience
  ::
  ++  prompt
    ^-  card
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
    ^-  (list card)
    %+  weld
      ^-  (list card)
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
    ^-  card
    (note "new: {~(phat tr target)}")
  ::  +show-delete: print mailbox deletion notification
  ::
  ++  show-delete
    |=  =target
    ^-  card
    (note "del: {~(phat tr target)}")
  ::  +show-glyph: print glyph un/bind notification
  ::
  ++  show-glyph
    |=  [=glyph target=(unit target)]
    ^-  (list card)
    :_  [prompt ~]
    %-  note
    %+  weld  "set: {[glyph ~]} "
    ?~  target  "unbound"
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
  ::  +full: render target fully, always
  ::
  ++  full
    ^-  tape
    (weld (scow %p ship.one) (spud path.one))
  ::  +phat: render target with local shorthand
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
