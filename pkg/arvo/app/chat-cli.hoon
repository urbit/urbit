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
/-  view=chat-view, hook=chat-hook,
    *permission-store, *group-store, *invite-store,
    *rw-security, sole-sur=sole
/+  sole-lib=sole, default-agent, verb, dbug, store=chat-store,
    auto=language-server-complete
::
|%
+$  card  card:agent:gall
::
+$  versioned-state
  $%  state-1
      state-0
  ==
::
+$  state-1
  $:  %1
      grams=(list mail)                             ::  all messages
      known=(set [target serial:store])             ::  known message lookup
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
+$  state-0
  $:  grams=(list [[=ship =path] envelope:store])   ::  all messages
      known=(set [[=ship =path] serial])            ::  known message lookup
      count=@ud                                     ::  (lent grams)
      bound=(map [=ship =path] glyph)               ::  bound circle glyphs
      binds=(jug glyph [=ship =path])               ::  circle glyph lookup
      audience=(set [=ship =path])                  ::  active targets
      settings=(set term)                           ::  frontend flags
      width=@ud                                     ::  display width
      timez=(pair ? @ud)                            ::  timezone adjustment
      cli=state=sole-share:sole-sur                 ::  console state
      eny=@uvJ                                      ::  entropy
  ==
::
+$  mail  [source=target envelope:store]
+$  target  [in-group=? =ship =path]
::
+$  glyph  char
++  glyphs  "!@#$%^&()-=_+[]\{}'\\:\",.<>?"
::
+$  nu-security  ?(%channel %village %village-with-group)
::
+$  command
  $%  [%target (set target)]                        ::  set messaging target
      [%say letter:store]                           ::  send message
      [%eval cord hoon]                             ::  send #-message
    ::
      ::
      ::  create chat
      [%create nu-security path (unit glyph) (unit ?)]
      [%delete path]                                ::  delete chat
      [%invite [? path] (set ship)]                   ::  allow
      [%banish [? path] (set ship)]                   ::  disallow
    ::
      [%join target (unit glyph) (unit ?)]          ::  join target
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
--
=|  state-1
=*  state  -
::
%-  agent:dbug
%+  verb  |
^-  agent:gall
=<
  |_  =bowl:gall
  +*  this       .
      talk-core  +>
      tc         ~(. talk-core(eny eny.bowl) bowl)
      def        ~(. (default-agent this %|) bowl)
  ::
  ++  on-init
    ^-  (quip card _this)
    =^  cards  state  (prep:tc ~)
    [cards this]
  ::
  ++  on-save  !>(state)
  ::
  ++  on-load
    |=  old-state=vase
    ^-  (quip card _this)
    =/  old  !<(versioned-state old-state)
    =^  cards  state  (prep:tc `old)
    [cards this]
  ::
  ++  on-poke
    |=  [=mark =vase]
    ^-  (quip card _this)
    =^  cards  state
      ?+  mark        (on-poke:def mark vase)
        %noun         (poke-noun:tc !<(* vase))
        %sole-action  (poke-sole-action:tc !<(sole-action:sole-sur vase))
      ==
    [cards this]
  ::
  ++  on-watch
    |=  =path
    ^-  (quip card _this)
    =^  cards  state  (peer:tc path)
    [cards this]
  ::
  ++  on-leave  on-leave:def
  ++  on-peek   on-peek:def
  ++  on-agent
    |=  [=wire =sign:agent:gall]
    ^-  (quip card _this)
    =^  cards  state
      ?-    -.sign
        %poke-ack   [- state]:(on-agent:def wire sign)
        %watch-ack  [- state]:(on-agent:def wire sign)
      ::
          %kick
        :_  state
        ?+  wire  ~
          [%chat-store ~]  ~[connect:tc]
          [%invites ~]     ~[connect-invites:tc]
        ==
      ::
          %fact
        ?+  p.cage.sign  ~|([%chat-cli-bad-sub-mark wire p.cage.sign] !!)
          %chat-update    (diff-chat-update:tc wire !<(update:store q.cage.sign))
          %invite-update  (handle-invite-update:tc !<(invite-update q.cage.sign))
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
  |=  old=(unit versioned-state)
  ^-  (quip card _state)
  ?~  old
    =^  cards  state
      %_  catch-up
        audience  [[| our-self /] ~ ~]
        settings  (sy %showtime %notify ~)
        width     80
      ==
    [[connect connect-invites cards] state]
  :-  %+  weld
        ?:  (~(has by wex.bowl) [/invites our-self %invite-store])  ~
        ~[connect-invites]
      ?:  (~(has by wex.bowl) [/chat-store our-self %chat-store])  ~
      ~[connect]
  ::
  ^-  state-1
  ?-  -.u.old
      %1
    =?  width.u.old  =(0 width.u.old)  80
    u.old(bound (~(gas by *(map target glyph)) ~(tap by bound.u.old)))
  ::
      ?(~ ^)
    :-  %1
    %=  u.old
      grams  ~  ::NOTE  this only impacts historic message lookup in chat-cli
    ::
        known
      ^-  (set [target serial])
      %-  ~(run in known.u.old)
      |=  [t=[ship path] s=serial]
      [`target`[| t] s]
    ::
        bound
      ^-  (map target glyph)
      %-  ~(gas by *(map target glyph))
      %+  turn  ~(tap by bound.u.old)
      |=  [t=[ship path] g=glyph]
      [`target`[| t] g]
    ::
        binds
      ^-  (jug glyph target)
      %-  ~(run by binds.u.old)
      |=  s=(set [ship path])
      %-  ~(run in s)
      |=  t=[ship path]
      `target`[| t]
    ::
        audience
      ^-  (set target)
      %-  ~(run in audience.u.old)
      |=  t=[ship path]
      `target`[| t]
    ==
  ==
::  +catch-up: process all chat-store state
::
++  catch-up
  ^-  (quip card _state)
  =/  =inbox:store
    (scry-for inbox:store %chat-store /all)
  |-  ^-  (quip card _state)
  ?~  inbox  [~ state]
  =*  path  p.n.inbox
  =*  mailbox  q.n.inbox
  =/  =target  (path-to-target path)
  =^  cards-n  state  (read-envelopes target (flop envelopes.mailbox))
  =^  cards-l  state  $(inbox l.inbox)
  =^  cards-r  state  $(inbox r.inbox)
  [:(weld cards-n cards-l cards-r) state]
::  +connect: connect to the chat-store
::
++  connect
  ^-  card
  [%pass /chat-store %agent [our-self %chat-store] %watch /updates]
::
++  connect-invites
  ^-  card
  [%pass /invites %agent [our.bowl %invite-store] %watch /invitatory/chat]
::
++  our-self  (name:title our.bowl)
::  +target-to-path: prepend ship to the path
::
++  target-to-path
  |=  target
  %+  weld
    ?:(in-group ~ /~)
  [(scot %p ship) path]
::  +path-to-target: deduces a target from a mailbox path
::
++  path-to-target
  |=  =path
  ^-  target
  =^  in-group  path
    ?.  ?=([%'~' *] path)
      [& path]
    [| t.path]
  :-  in-group
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
  ^-  (quip card _state)
  ?:  ?=(%connect a)
    [[connect ~] state]
  ?:  ?=(%catch-up a)
    catch-up
  [~ state]
::  +poke-sole-action: handle cli input
::
++  poke-sole-action
  ::TODO  use id.act to support multiple separate sessions
  |=  [act=sole-action:sole-sur]
  ^-  (quip card _state)
  (sole:sh-in act)
::  +peer: accept only cli subscriptions from ourselves
::
++  peer
  |=  =path
  ^-  (quip card _state)
  ?.  (team:title our-self src.bowl)
    ~|  [%peer-talk-stranger src.bowl]
    !!
  ?.  ?=([%sole *] path)
    ~|  [%peer-talk-strange path]
    !!
  ::  display a fresh prompt
  :-  [prompt:sh-out ~]
  ::  start with fresh sole state
  state(state.cli *sole-share:sole-sur)
::  +handle-invite-update: get new invites
::
++  handle-invite-update
  |=  upd=invite-update
  ^-  (quip card _state)
  ?+  -.upd  [~ state]
    %invite  [[(show-invite:sh-out invite.upd) ~] state]
  ==
::  +diff-chat-update: get new mailboxes & messages
::
++  diff-chat-update
  |=  [=wire upd=update:store]
  ^-  (quip card _state)
  ?+  -.upd  [~ state]
    %create    (notice-create (path-to-target path.upd))
    %delete    [[(show-delete:sh-out (path-to-target path.upd)) ~] state]
    %message   (read-envelope (path-to-target path.upd) envelope.upd)
    %messages  (read-envelopes (path-to-target path.upd) (flop envelopes.upd))
  ==
::
++  read-envelopes
  |=  [=target envs=(list envelope:store)]
  ^-  (quip card _state)
  ?~  envs  [~ state]
  =^  cards-i  state  (read-envelope target i.envs)
  =^  cards-t  state  $(envs t.envs)
  [(weld cards-i cards-t) state]
::
++  notice-create
  |=  =target
  ^-  (quip card _state)
  =^  cards  state
    ?:  (~(has by bound) target)
      [~ state]
    (bind-default-glyph target)
  [[(show-create:sh-out target) cards] state]
::  +bind-default-glyph: bind to default, or random available
::
++  bind-default-glyph
  |=  =target
  ^-  (quip card _state)
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
  ^-  (quip card _state)
  ::TODO  should send these to settings store eventually
  ::  if the target was already bound to another glyph, un-bind that
  ::
  =?  binds  (~(has by bound) target)
    (~(del ju binds) (~(got by bound) target) target)
  =.  bound  (~(put by bound) target glyph)
  =.  binds  (~(put ju binds) glyph target)
  [(show-glyph:sh-out glyph `target) state]
::  +unbind-glyph: remove all binding for glyph
::
++  unbind-glyph
  |=  [=glyph targ=(unit target)]
  ^-  (quip card _state)
  ?^  targ
    =.  binds  (~(del ju binds) glyph u.targ)
    =.  bound  (~(del by bound) u.targ)
    [(show-glyph:sh-out glyph ~) state]
  =/  ole=(set target)
    (~(get ju binds) glyph)
  =.  binds  (~(del by binds) glyph)
  =.  bound
    |-
    ?~  ole  bound
    =.  bound  $(ole l.ole)
    =.  bound  $(ole r.ole)
    (~(del by bound) n.ole)
  [(show-glyph:sh-out glyph ~) state]
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
  |=  [=target =envelope:store]
  ^-  (quip card _state)
  ?:  (~(has in known) [target uid.envelope])
    ::NOTE  we no-op only because edits aren't possible
    [~ state]
  :-  (show-envelope:sh-out target envelope)
  %_  state
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
    ^-  (quip card _state)
    ?-  -.dat.act
      %det  (edit +.dat.act)
      %clr  [~ state]
      %ret  obey
      %tab  (tab +.dat.act)
    ==
  ::  +tab-list: static list of autocomplete entries
  ++  tab-list
    ^-  (list (option:auto tank))
    :~
      [%join leaf+";join ~ship/chat-name (glyph)"]
      [%leave leaf+";leave ~ship/chat-name"]
      ::
      [%create leaf+";create [type] /chat-name (glyph)"]
      [%delete leaf+";delete /chat-name"]
      [%invite leaf+";invite /chat-name ~ships"]
      [%banish leaf+";banish /chat-name ~ships"]
    ::
      [%bind leaf+";bind [glyph] ~ship/chat-name"]
      [%unbind leaf+";unbind [glyph]"]
      [%what leaf+";what (~ship/chat-name) (glyph)"]
    ::
      [%settings leaf+";settings"]
      [%set leaf+";set key (value)"]
      [%unset leaf+";unset key"]
    ::
      [%chats leaf+";chats"]
      [%help leaf+";help"]
    ==
  ++  tab
    |=  pos=@ud
    ^-  (quip card _state)
    ?:  ?|  =(~ buf.state.cli)
            !=(';' -.buf.state.cli)
        ==
      :_  state
      [(effect:sh-out [%bel ~]) ~]
    ::
    =+  (get-id:auto pos (tufa buf.state.cli))
    =/  needle=term
      (fall id '')
    ?:  &(!=(pos 1) =(0 (met 3 needle)))
      [~ state]  :: autocomplete empty command iff user at start of command
    =/  options=(list (option:auto tank))
      (search-prefix:auto needle tab-list)
    =/  advance=term
      (longest-match:auto options)
    =/  to-send=tape
      (trip (rsh 3 (met 3 needle) advance))
    =/  send-pos
      (add pos (met 3 (fall forward '')))
    =|  moves=(list card)
    =?  moves  ?=(^ options)
      [(tab:sh-out options) moves]
    =|  fxs=(list sole-effect:sole-sur)
    |-  ^-  (quip card _state)
    ?~  to-send
      [(flop moves) state]
    =^  char  state.cli
      (~(transmit sole-lib state.cli) [%ins send-pos `@c`i.to-send])
    %_  $
      moves  [(effect:sh-out %det char) moves]
      send-pos  +(send-pos)
      to-send  t.to-send
    ==
  ::  +edit: apply sole edit
  ::
  ::    called when typing into the cli prompt.
  ::    applies the change and does sanitizing.
  ::
  ++  edit
    |=  cal=sole-change:sole-sur
    ^-  (quip card _state)
    =^  inv  state.cli  (~(transceive sole-lib state.cli) cal)
    =+  fix=(sanity inv buf.state.cli)
    ?~  lit.fix
      [~ state]
    ::  just capital correction
    ?~  err.fix
      (slug fix)
    ::  allow interior edits and deletes
    ?.  &(?=($del -.inv) =(+(p.inv) (lent buf.state.cli)))
      [~ state]
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
    ^-  (quip card _state)
    ?~  lit  [~ state]
    =^  lic  state.cli
      %-  ~(transmit sole-lib state.cli)
      ^-  sole-edit:sole-sur
      ?~(t.lit i.lit [%mor lit])
    :_  state
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
          ;~  plug
            path
            (punt ;~(pfix ace glyph))
            (punt ;~(pfix ace (fuss 'y' 'n')))
          ==
        ==
        ;~((glue ace) (tag %delete) path)
        ;~((glue ace) (tag %invite) tarx ships)
        ;~((glue ace) (tag %banish) tarx ships)
      ::
        ;~  (glue ace)
          (tag %join)
          ;~  plug
            targ
            (punt ;~(pfix ace glyph))
            (punt ;~(pfix ace (fuss 'y' 'n')))
          ==
        ==
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
      ::
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
    ++  path  ;~(pfix net ;~(plug urs:ab (easy ~)))  ::NOTE  short only, tmp
    ::  +mang: un/managed indicator prefix
    ::
    ++  mang
      ;~  pose
        (cold %| (jest '~/'))
        (cold %& (easy ~))
      ==
    ::  +tarl: local target, as /path
    ::
    ++  tarl  (stag our-self path)
    ::  +tarx: local target, maybe managed
    ::
    ++  tarx  ;~(plug mang path)
    ::  +tarp: sponsor target, as ^/path
    ::
    ++  tarp
      =-  ;~(pfix ket (stag - path))
      (sein:title our.bowl now.bowl our-self)
    ::  +targ: any target, as tarl, tarp, ~ship/path or glyph
    ::
    ++  targ
      ;~  pose
        ;~  plug
          mang
        ::
          ;~  pose
            tarl
            tarp
            ;~(plug ship path)
          ==
        ==
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
      (perk %channel %village-with-group %village ~)
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
    ^-  (quip card _state)
    =+  buf=buf.state.cli
    =+  fix=(sanity [%nop ~] buf)
    ?^  lit.fix
      (slug fix)
    =+  jub=(rust (tufa buf) read)
    ?~  jub  [[(effect:sh-out %bel ~) ~] state]
    =^  cal  state.cli  (~(transmit sole-lib state.cli) [%set ~])
    =^  cards  state  (work u.jub)
    :_  state
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
    ^-  (quip card _state)
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
          [our-self %chat-hook where who '']
      ==
    ::  +set-target: set audience, update prompt
    ::
    ++  set-target
      |=  tars=(set target)
      ^-  (quip card _state)
      =.  audience  tars
      [[prompt:sh-out ~] state]
    ::  +create: new local mailbox
    ::
    ++  create
      |=  [security=nu-security =path gyf=(unit char) allow-history=(unit ?)]
      ^-  (quip card _state)
      =/  with-group=?     ?=(%village-with-group security)
      =/  =target          [with-group our-self path]
      =/  real-path=^path  (target-to-path target)
      =/  =rw-security
        ?-  security
          %channel                         %channel
          ?(%village %village-with-group)  %village
        ==
      ?^  (scry-for (unit mailbox:store) %chat-store [%mailbox real-path])
        =-  [[- ~] state]
        %-  print:sh-out
        "{(spud path)} already exists!"
      =.  audience  [target ~ ~]
      =^  moz  state
        ?.  ?=(^ gyf)  [~ state]
        (bind-glyph u.gyf target)
      =-  [[- moz] state]
      %^  act  %do-create  %chat-view
      :-  %chat-view-action
      !>  ^-  action:view
      :*  %create
          (rsh 3 1 (spat path))
          ''
          real-path  ::  chat
          real-path  ::  group
          rw-security
          ~
          (fall allow-history %.y)
      ==
    ::  +delete: delete local chats
    ::
    ++  delete
      |=  =path
      ^-  (quip card _state)
      =-  [[- ~] state]
      %^  act  %do-delete  %chat-view
      :-  %chat-view-action
      !>  ^-  action:view
      [%delete (target-to-path | our-self path)]
    ::  +change-permission: modify permissions on a local chat
    ::
    ++  change-permission
      |=  [allow=? [group=? =path] ships=(set ship)]
      ^-  (quip card _state)
      :_  state
      =/  real-path=^path
        (target-to-path group our-self path)
      =;  permit=(unit card)
        %+  weld  (drop permit)
        ?.  allow  ~
        ^-  (list card)
        %+  murn  ~(tap in ships)
        |=  =ship
        ^-  (unit card)
        ::  if they weren't permitted before, some hook will send an invite.
        ::  but if they already were, we want to send an invite ourselves.
        ::
        ?.  %^  scry-for  ?
              %permission-store
            [%permitted (scot %p ship) real-path]
          ~
        `(invite-card real-path ship)
      ::  whitelist: empty if no matching permission, else true if whitelist
      ::
      =/  whitelist=(unit ?)
        =;  perm=(unit permission)
          ?~(perm ~ `?=(%white kind.u.perm))
        ::TODO  +permission-of-target?
        %^  scry-for  (unit permission)
          %permission-store
        [%permission real-path]
      ?~  whitelist
        ~&  [%weird-no-permission real-path]
        ~
      %-  some
      %^  act  %do-permission  %group-store
      :-  %group-action
      !>  ^-  group-action
      ?:  =(u.whitelist allow)
        [%add ships real-path]
      [%remove ships real-path]
    ::  +join: sync with remote mailbox
    ::
    ++  join
      |=  [=target gyf=(unit char) ask-history=(unit ?)]
      ^-  (quip card _state)
      =^  moz  state
        ?.  ?=(^ gyf)  [~ state]
        (bind-glyph u.gyf target)
      =.  audience  [target ~ ~]
      =;  =card
        [[card prompt:sh-out moz] state]
      ::TODO  ideally we'd check permission first. attempting this and failing
      ::      gives ugly %chat-hook-reap
      %^  act  %do-join  %chat-view
      :-  %chat-view-action
      !>  ^-  action:view
      [%join ship.target (target-to-path target) (fall ask-history %.y)]
    ::  +leave: unsync & destroy mailbox
    ::
    ::TODO  allow us to "mute" local chats using this
    ++  leave
      |=  =target
      =-  [[- ~] state]
      ?:  =(our-self ship.target)
        %-  print:sh-out
        "can't ;leave local chats, maybe use ;delete instead"
      %^  act  %do-leave  %chat-hook
      :-  %chat-hook-action
      !>  ^-  action:hook
      [%remove (target-to-path target)]
    ::  +say: send messages
    ::
    ++  say
      |=  =letter:store
      ^-  (quip card _state)
      ~!  bowl
      =/  =serial  (shaf %msg-uid eny.bowl)
      :_  state(eny (shax eny.bowl))
      ^-  (list card)
      %+  turn  ~(tap in audience)
      |=  =target
      %^  act  %out-message  %chat-hook
      :-  %chat-action
      !>  ^-  action:store
      :+  %message  (target-to-path target)
      [serial *@ our-self now.bowl letter]
    ::  +eval: run hoon, send code and result as message
    ::
    ::    this double-virtualizes and clams to disable .^ for security reasons
    ::
    ++  eval
      |=  [txt=cord exe=hoon]
      (say %code txt (eval:store bowl exe))
    ::  +lookup-glyph: print glyph info for all, glyph or target
    ::
    ++  lookup-glyph
      |=  qur=(unit $@(glyph target))
      ^-  (quip card _state)
      =-  [[- ~] state]
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
      ^-  (quip card _state)
      :_  state
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
      ^-  (quip card _state)
      [~ state(settings (~(put in settings) term))]
    ::  +unset-setting: disable settings flag
    ::
    ++  unset-setting
      |=  =term
      ^-  (quip card _state)
      [~ state(settings (~(del in settings) term))]
    ::  +set-width: configure cli printing width
    ::
    ++  set-width
      |=  w=@ud
      [~ state(width w)]
    ::  +set-timezone: configure timestamp printing adjustment
    ::
    ++  set-timezone
      |=  tz=[? @ud]
      [~ state(timez tz)]
    ::  +select: expand message from number reference
    ::
    ++  select
      ::NOTE  rel is the nth most recent message,
      ::      abs is the last message whose numbers ends in n
      ::      (with leading zeros used for precision)
      ::
      |=  num=$@(rel=@ud [zeros=@u abs=@ud])
      ^-  (quip card _state)
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
        [[(print:sh-out txt) ~] state]
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
        ^-  (quip card _state)
        =+  gam=(snag index grams)
        =.  audience  [source.gam ~ ~]
        :_  state
        ^-  (list card)
        :~  (print:sh-out ['?' ' ' number])
            (effect:sh-out ~(render-activate mr gam))
            prompt:sh-out
        ==
      --
    ::  +chats: display list of local mailboxes
    ::
    ++  chats
      ^-  (quip card _state)
      :_  state
      :_  ~
      %-  print-more:sh-out
      =/  all
        %^  scry-for  (set path)
          %chat-store
        /keys
      %+  turn  ~(tap in all)
      %+  cork  path-to-target
      |=  target
      (weld (scow %p ship) (spud path))
    ::  +help: print (link to) usage instructions
    ::
    ++  help
      ^-  (quip card _state)
      =-  [[- ~] state]
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
    [%give %fact ~[/sole/drum] %sole-effect !>(fec)]
  ::  +tab: print tab-complete list
  ::
  ++  tab
    |=  options=(list [cord tank])
    ^-  card
    (effect %tab options)
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
    |=  [=target =envelope:store]
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
  ::  +show-invite: print incoming invite notification
  ::
  ++  show-invite
    |=  invite
    ^-  card
    %-  note
    %+  weld  "invited to: "
    ~(phat tr (path-to-target path))
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
    ;:  weld
      ?:(in-group.one "" "~/")
      (scow %p ship.one)
      (spud path.one)
    ==
  ::  +phat: render target with local shorthand
  ::
  ::    renders as ~ship/path.
  ::    for local mailboxes, renders just /path.
  ::    for sponsor's mailboxes, renders ^/path.
  ::
  ++  phat
    ^-  tape
    %+  weld
      ?:(in-group.one "" "~/")
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
          envelope:store
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
          ~|  [%weird-text `@`+.letter]
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
  =/  [end=@ud nex=?]
    ?:  (lte (lent txt) wid)  [(lent txt) &]
    =+  ace=(find " " (flop (scag +(wid) `tape`txt)))
    ?~  ace  [wid |]
    [(sub wid u.ace) &]
  :-  (tufa (scag end `(list @)`txt))
  $(txt (slag ?:(nex +(end) end) `tape`txt))
::
::NOTE  anything that uses this breaks moons support, because moons don't sync
::      full app state rn
++  scry-for
  |*  [=mold app=term =path]
  .^  mold
    %gx
    (scot %p our.bowl)
    app
    (scot %da now.bowl)
    (snoc `^path`path %noun)
  ==
--
