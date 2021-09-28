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
/-  *resource, post, store=chat-store
/+  shoe, default-agent, verb, dbug, graph=graph-store, libgraph=graph
::
|%
+$  card  card:shoe
::
+$  versioned-state
  $%  state-3
      state-2
      state-1
      state-0
  ==
::
+$  state-3
  $:  %3
      ::TODO  support multiple sessions
      sessions=(map sole-id session)                ::  sole sessions
      bound=(map resource glyph)                    ::  bound resource glyphs
      binds=(jug glyph resource)                    ::  resource glyph lookup
      settings=(set term)                           ::  frontend flags
      width=@ud                                     ::  display width
      timez=(pair ? @ud)                            ::  timezone adjustment
  ==
::
+$  sole-id  @ta
+$  session
  $:  viewing=(set resource)                        ::  connected graphs
      history=(list uid:post)                       ::  scrollback pointers
      count=@ud                                     ::  (lent history)
      audience=target                               ::  active target
  ==
::
::TODO  remove for breach
+$  target-2  [in-group=? =ship =path]
+$  mail  [source=target-2 envelope:store]
+$  state-2
  $:  %2
      grams=(list mail)                             ::  all messages
      known=(set [target-2 serial:store])           ::  known message lookup
      count=@ud                                     ::  (lent grams)
      bound=(map target-2 glyph)                    ::  bound circle glyphs
      binds=(jug glyph target-2)                    ::  circle glyph lookup
      audience=(set target-2)                       ::  active targets
      settings=(set term)                           ::  frontend flags
      width=@ud                                     ::  display width
      timez=(pair ? @ud)                            ::  timezone adjustment
  ==
::
+$  state-1
  $:  %1
      grams=(list mail)                             ::  all messages
      known=(set [target-2 serial:store])           ::  known message lookup
      count=@ud                                     ::  (lent grams)
      bound=(map target-2 glyph)                    ::  bound circle glyphs
      binds=(jug glyph target-2)                    ::  circle glyph lookup
      audience=(set target-2)                       ::  active targets
      settings=(set term)                           ::  frontend flags
      width=@ud                                     ::  display width
      timez=(pair ? @ud)                            ::  timezone adjustment
      cli=state=sole-share:shoe                     ::  console state
      eny=@uvJ                                      ::  entropy
  ==
::
+$  state-0
  $:  grams=(list [[=ship =path] envelope:store])   ::  all messages
      known=(set [[=ship =path] serial:store])      ::  known message lookup
      count=@ud                                     ::  (lent grams)
      bound=(map [=ship =path] glyph)               ::  bound circle glyphs
      binds=(jug glyph [=ship =path])               ::  circle glyph lookup
      audience=(set [=ship =path])                  ::  active targets
      settings=(set term)                           ::  frontend flags
      width=@ud                                     ::  display width
      timez=(pair ? @ud)                            ::  timezone adjustment
      cli=state=sole-share:shoe                     ::  console state
      eny=@uvJ                                      ::  entropy
  ==
::
+$  target  resource
::
+$  glyph  char
++  glyphs  "!@#$%^&()-=_+[]\{}'\\:\",.<>?"
::
+$  command
  $%  [%target target]                              ::  set messaging target
      [%say content:post]                           ::  send message
      [%eval cord hoon]                             ::  send #-message
    ::                                              ::
      [%view $?(~ target)]                          ::  notice chat
      [%flee target]                                ::  ignore chat
    ::                                              ::
      [%bind glyph target]                          ::  bind glyph
      [%unbind glyph (unit target)]                 ::  unbind glyph
      [%what (unit $@(char target))]                ::  glyph lookup
    ::                                              ::
      [%settings ~]                                 ::  show active settings
      [%set term]                                   ::  set settings flag
      [%unset term]                                 ::  unset settings flag
      [%width @ud]                                  ::  adjust display width
      [%timezone ? @ud]                             ::  adjust time printing
    ::                                              ::
      [%select $@(rel=@ud [zeros=@u abs=@ud])]      ::  rel/abs msg selection
      [%chats ~]                                    ::  list available chats
      [%help ~]                                     ::  print usage info
  ==                                                ::
::
--
=|  state-3
=*  state  -
::
%-  agent:dbug
%+  verb  |
%-  (agent:shoe command)
^-  (shoe:shoe command)
=<
  |_  =bowl:gall
  +*  this       .
      talk-core  +>
      tc         ~(. talk-core bowl)
      def        ~(. (default-agent this %|) bowl)
      des        ~(. (default:shoe this command) bowl)
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
      ==
    [cards this]
  ::
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
          [%graph-store ~]  ~[connect:tc]
        ==
      ::
          %fact
        ?+  p.cage.sign  ~|([dap.bowl %bad-sub-mark wire p.cage.sign] !!)
            %graph-update-3
          %-  on-graph-update:tc
          !<(update:graph q.cage.sign)
        ==
      ==
    [cards this]
  ::
  ++  on-watch  on-watch:def
  ++  on-leave  on-leave:def
  ++  on-peek   on-peek:def
  ++  on-arvo   on-arvo:def
  ++  on-fail   on-fail:def
  ::
  ++  command-parser
    |=  =sole-id
    parser:(make:sh:tc sole-id)
  ::
  ++  tab-list
    |=  =sole-id
    tab-list:sh:tc
  ::
  ++  on-command
    |=  [=sole-id =command]
    =^  cards  state
      (work:(make:sh:tc sole-id) command)
    [cards this]
  ::
  ++  on-connect
    |=  =sole-id
    ^-  (quip card _this)
    [[prompt:(make:sh-out:tc sole-id)]~ this]
  ::
  ++  can-connect     can-connect:des
  ++  on-disconnect   on-disconnect:des
  --
::
|_  =bowl:gall
+*  libgraph  ~(. ^libgraph bowl)
::  +prep: setup & state adapter
::
++  prep
  |=  old=(unit versioned-state)
  ^-  (quip card _state)
  ?~  old
    [~[connect] state(width 80)]
  ::
  =?  u.old  ?=(?(~ ^) -.u.old)
    ^-  state-1
    :-  %1
    %=  u.old
      grams  ~  ::NOTE  this only impacts historic message lookup in chat-cli
    ::
        known
      ^-  (set [target-2 serial:store])
      %-  ~(run in known.u.old)
      |=  [t=[ship path] s=serial:store]
      [`target-2`[| t] s]
    ::
        bound
      ^-  (map target-2 glyph)
      %-  ~(gas by *(map target-2 glyph))
      %+  turn  ~(tap by bound.u.old)
      |=  [t=[ship path] g=glyph]
      [`target-2`[| t] g]
    ::
        binds
      ^-  (jug glyph target-2)
      %-  ~(run by binds.u.old)
      |=  s=(set [ship path])
      %-  ~(run in s)
      |=  t=[ship path]
      `target-2`[| t]
    ::
        audience
      ^-  (set target-2)
      %-  ~(run in audience.u.old)
      |=  t=[ship path]
      `target-2`[| t]
    ==
  ::
  =?  u.old  ?=(%1 -.u.old)
    ^-  state-2
    =,  u.old
    :*  %2
      grams  known  count
      bound  binds  audience
      settings  width  timez
    ==
  ::
  =^  cards  u.old
    ?.  ?=(%2 -.u.old)  [~ u.old]
    :-  :~  [%pass /chat-store %agent [our-self %chat-store] %leave ~]
            [%pass /invites %agent [our.bowl %invite-store] %leave ~]
        ==
    ^-  state-3
    :-  %3
    :*  %+  ~(put in *(map sole-id session))
          (cat 3 'drum_' (scot %p our.bowl))
        :*  ~  ~  0
          ::
            ?~  audience.u.old  *target
            [ship ?~(path %$ i.path)]:n.audience.u.old
        ==
      ::
        %-  ~(gas by *(map resource glyph))
        %+  turn  ~(tap in bound.u.old)
        |=  [t=target-2 g=glyph]
        [[ship.t ?~(path.t %$ i.path.t)] g]
      ::
        ^-  (jug glyph resource)
        %-  ~(run by binds.u.old)
        |=  s=(set target-2)
        %-  ~(run in s)
        |=  t=target-2
        [ship.t ?~(path.t %$ i.path.t)]
      ::
        settings.u.old
        width.u.old
        timez.u.old
    ==
  ::
  ?>  ?=(%3 -.u.old)
  :_  u.old
  %+  welp
    cards
  ?:  %-  ~(has by wex.bowl)
      [/graph-store our-self %graph-store]
    ~
  ~[connect]
::  +connect: connect to the graph-store
::
++  connect
  ^-  card
  [%pass /graph-store %agent [our-self %graph-store] %watch /updates]
::
::TODO  better moon support. (name:title our.bowl)
++  our-self  our.bowl
::
++  get-session
  |=  =sole-id
  ^-  session
  (~(gut by sessions) sole-id %*(. *session audience [our-self %$]))
::  +tor: term ordering for targets
::
++  tor
  |=  [[* a=term] [* b=term]]
  (aor a b)
::  +ior: index ordering for nodes
::
++  ior
  |=  [[a=index:post *] [b=index:post *]]
  (aor a b)
::  +safe-get-graph: virtualized +get-graph
::
++  safe-get-graph
  |=  =resource
  ^-  (unit update:graph)
  =/  res=(each update:graph tang)
    ::TODO  doesn't actually contain the crash?
    %-  mule  |.
    (get-graph:libgraph resource)
  ?-  -.res
    %&  `p.res
    %|  ~
  ==
::  +is-chat-graph: check whether graph contains chat-style data
::
++  is-chat-graph
  |=  =resource
  ^-  ?
  =/  update=(unit update:graph)
    (safe-get-graph resource)
  ?~  update  |
  ?>  ?=(%add-graph -.q.u.update)
  =(`%graph-validator-chat mark.q.u.update)
::  +poke-noun: debug helpers
::
++  poke-noun
  |=  a=*
  ^-  (quip card _state)
  ?:  ?=(%connect a)
    [[connect ~] state]
  [~ state]
::  +handle-graph-update: get new mailboxes & messages
::
++  on-graph-update
  |=  upd=update:graph
  ^-  (quip card _state)
  ?.  ?=(?(%remove-graph %add-nodes) -.q.upd)
    [~ state]
  =/  sez=(list [=sole-id =session])
    ~(tap by sessions)
  =|  cards=(list card)
  |-
  ?~  sez  [cards state]
  =^  caz  session.i.sez
    ?-  -.q.upd
      %remove-graph  (~(notice-remove se i.sez) +.q.upd)
    ::
        %add-nodes
      ?.  (~(has in viewing.session.i.sez) resource.q.upd)
        [~ session.i.sez]
      %+  ~(read-posts se i.sez)
        resource.q.upd
      (sort ~(tap by nodes.q.upd) ior)
    ==
  =.  sessions  (~(put by sessions) i.sez)
  $(sez t.sez, cards (weld cards caz))
::  +se: session event handling
::
++  se
  |_  [=sole-id =session]
  +*  sh-out  ~(. ^sh-out sole-id session)
  ::
  ++  read-posts
    |=  [=target nodes=(list [=index:post =node:graph])]
    ^-  (quip card _session)
    =^  cards  nodes
      ^-  (quip card _nodes)
      =+  count=(lent nodes)
      ?.  (gth count 10)  [~ nodes]
      :_  (swag [(sub count 10) 10] nodes)
      [(print:sh-out "skipping {(scow %ud (sub count 10))} messages...")]~
    |-
    ?~  nodes  [cards session]
    =^  caz  session
      (read-post target [index post.node]:i.nodes)
    $(cards (weld cards caz), nodes t.nodes)
  ::
  ::  +read-post: add envelope to state and show it to user
  ::
  ++  read-post
    |=  [=target =index:post =maybe-post:graph]
    ^-  (quip card _session)
    ?-    -.maybe-post
        %|  [~ session]
        %&
      :-  (show-post:sh-out target p.maybe-post)
      %_  session
        history  [[target index] history.session]
        count    +(count.session)
      ==
    ==
  ::
  ++  notice-remove
    |=  =target
    ^-  (quip card _session)
    ?.  (~(has in viewing.session) target)
      [~ session]
    :-  [(show-delete:sh-out target) ~]
    session(viewing (~(del in viewing.session) target))
  --
::
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
  |=  [=session =glyph]
  ^-  (unit target)
  =+  lax=(~(get ju binds) glyph)
  ::  no target
  ?:  =(~ lax)  ~
  %-  some
  ::  single target
  ?:  ?=([* ~ ~] lax)  n.lax
  ::  in case of multiple matches, pick one we're viewing
  =.  lax  (~(uni in lax) viewing.session)
  ?:  ?=([* ~ ~] lax)  n.lax
  ::  in case of multiple audiences, pick the most recently active one
  |-  ^-  target
  ?~  history.session  -:~(tap in lax)
  =*  resource  resource.i.history.session
  ?:  (~(has in lax) resource)
    resource
  $(history.session t.history.session)
::
::  +sh: shoe handling
::
++  sh
  |_  [=sole-id session]
  +*  session  +<+
      sh-out   ~(. ^sh-out sole-id session)
      put-ses  state(sessions (~(put by sessions) sole-id session))
  ::
  ++  make
    |=  =^sole-id
    %_  ..make
      sole-id  sole-id
      +<+      (get-session sole-id)
    ==
  ::  +read: command parser
  ::
  ::    parses the command line buffer.
  ::    produces commands which can be executed by +work.
  ::
  ++  parser
    |^
      %+  stag  |
      %+  knee  *command  |.  ~+
      =-  ;~(pose ;~(pfix mic -) message)
      ;~  pose
        (stag %target targ)
      ::
        ;~((glue ace) (tag %view) targ)
        ;~((glue ace) (tag %flee) targ)
        ;~(plug (tag %view) (easy ~))
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
    ++  group  ;~((glue fas) ship sym)
    ++  tag   |*(a=@tas (cold a (jest a)))  ::TODO  into stdlib
    ++  ship  ;~(pfix sig fed:ag)
    ++  name  ;~(pfix fas urs:ab)
    ::  +tarl: local target, as /path
    ::
    ++  tarl  (stag our-self name)
    ::  +targ: any target, as tarl, tarp, ~ship/path or glyph
    ::
    ++  targ
      ;~  pose
        tarl
        ;~(plug ship name)
        (sear (cury decode-glyph session) glyph)
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
        (stag %say content)
      ==
    ::  +content: simple messages
    ::TODO  mentions
    ::
    ++  content
      ;~  pose
        (stag %url turl)
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
      (plus next)
    ::  +expr: parse expression into [cord hoon]
    ::
    ++  expr
      |=  tub=nail
      %.  tub
      %+  stag  (crip q.tub)
      wide:(vang & [&1:% &2:% (scot %da now.bowl) |3:%])
    --
  ::  +tab-list: command descriptions
  ::
  ++  tab-list
    ^-  (list [@t tank])
    :~
      [';view' leaf+";view ~ship/chat-name (glyph)"]
      [';flee' leaf+";flee ~ship/chat-name"]
    ::
      [';bind' leaf+";bind [glyph] ~ship/chat-name"]
      [';unbind' leaf+";unbind [glyph]"]
      [';what' leaf+";what (~ship/chat-name) (glyph)"]
    ::
      [';settings' leaf+";settings"]
      [';set' leaf+";set key (value)"]
      [';unset' leaf+";unset key"]
    ::
      [';chats' leaf+";chats"]
      [';help' leaf+";help"]
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
          %view      (view +.job)
          %flee      (flee +.job)
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
    ::  +set-target: set audience, update prompt
    ::
    ++  set-target
      |=  =target
      ^-  (quip card _state)
      =.  audience  target
      [[prompt:sh-out ~] put-ses]
    ::  +view: start printing messages from a resource
    ::
    ++  view
      |=  target=$?(~ target)
      ^-  (quip card _state)
      ::  without argument, print all we're viewing
      ::
      ?~  target
        [[(show-chats:sh-out ~(tap in viewing))]~ state]
      ::  only view existing chat-type graphs
      ::
      ?.  (is-chat-graph target)
        [[(note:sh-out "no such chat")]~ put-ses]
      =.  audience  target
      =.  viewing   (~(put in viewing) target)
      =^  cards  state
        ?:  (~(has by bound) target)
          [~ state]
        (bind-default-glyph target)
      [[prompt:sh-out cards] put-ses]
    ::  +flee: stop printing messages from a resource
    ::
    ++  flee
      |=  =target
      ^-  (quip card _state)
      =.  viewing  (~(del in viewing) target)
      [~ put-ses]
    ::  +say: send messages
    ::
    ++  say
      |=  msg=content:post
      ^-  (quip card _state)
      =/  =serial:store  (shaf %msg-uid eny.bowl)
      :_  state
      :_  ~
      ::TODO  move creation into lib?
      %^  act  %out-message
        %graph-push-hook
      :-  %graph-update-3
      !>  ^-  update:graph
      :-  now.bowl
      :+  %add-nodes  audience
      %-  ~(put by *(map index:post node:graph))
      :-  ~[now.bowl]
      :_  *internal-graph:graph
      ^-  maybe-post:graph
      [%& `post:post`[our-self ~[now.bowl] now.bowl [msg]~ ~ ~]]
    ::  +eval: run hoon, send code and result as message
    ::
    ::    this double-virtualizes and clams to disable .^ for security reasons
    ::
    ++  eval
      |=  [txt=cord exe=hoon]
      ~&  %eval-tmp-disabled
      [~ state]
      ::TODO  why -find.eval??
      :: (say %code txt (eval:store bowl exe))
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
          %+  join  ", "
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
      [~ state(width (max 40 w))]
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
        ::NOTE  graph store allows node deletion, so can this crash?
        =/  =uid:post    (snag index history)
        =/  =node:graph  (got-node:libgraph uid)
        =.  audience     resource.uid
        ?:  ?=(%| -.post.node)
          [~ state]
        :_  put-ses
        ^-  (list card)
        :~  (print:sh-out ['?' ' ' number])
            (effect:sh-out ~(render-activate mr resource.uid p.post.node))
            prompt:sh-out
        ==
      --
    ::  +chats: display list of joined chats
    ::
    ++  chats
      ^-  (quip card _state)
      :_  state
      :_  ~
      %-  show-chats:sh-out
      (skim ~(tap in get-keys:libgraph) is-chat-graph)
    ::  +help: print (link to) usage instructions
    ::
    ++  help
      ^-  (quip card _state)
      :_  state
      =-  (turn - print:sh-out)
      :~  ";view ~host/chat to print messages for a chat you've already joined."
          ";flee ~host/chat to stop printing messages for a chat."
          "For more details:"
          "https://urbit.org/using/operations/using-your-ship/#messaging"
      ==
    --
  --
::
::  +sh-out: ouput to session
::
++  sh-out
  |_  [=sole-id session]
  ++  make
    |=  =^sole-id
    %_  ..make
      sole-id  sole-id
      +<+      (get-session sole-id)
    ==
  ::  +effex: emit shoe effect card
  ::
  ++  effex
    |=  effect=shoe-effect:shoe
    ^-  card
    [%shoe ~[sole-id] effect]
  ::  +effect: emit console effect card
  ::
  ++  effect
    |=  effect=sole-effect:shoe
    ^-  card
    (effex %sole effect)
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
    =+  ~(show tr audience)
    ?:(=(1 (lent -)) "{-} " "[{-}] ")
  ::  +show-post: print incoming message
  ::
  ::    every five messages, prints the message number also.
  ::    if the message mentions the user's (shortened) ship name,
  ::    and the %notify flag is set, emit a bell.
  ::
  ++  show-post
    |=  [=target =post:post]
    ^-  (list card)
    %+  weld
      ^-  (list card)
      ?.  =(0 (mod count 5))  ~
      :_  ~
      =+  num=(scow %ud count)
      %-  print
      (runt [(sub 13 (lent num)) '-'] "[{num}]")
    ^-  (list card)
    :-  (effex ~(render-inline mr target post))
    =;  mentioned=?
      ?.  mentioned  ~
      [(effect %bel ~)]~
    %+  lien  contents.post
    (cury test %mention our.bowl)
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
  ::  +show-chats: print list of targets
  ::
  ++  show-chats
    |=  chats=(list target)
    ^-  card
    %-  print-more
    %+  turn  (sort chats tor)
    |=  resource
    "{(nome:mr entity)}/{(trip name)}"
  --
::
::  +tr: render targets (resource identifiers)
::
++  tr
  |_  tr=target
  ::  +full: render target fully, always (as ~ship/path)
  ::
  ++  full
    ^-  tape
    "{(scow %p entity.tr)}/{(trip name.tr)}"
  ::  +phat: render target with local shorthand
  ::
  ::    renders as ~ship/path.
  ::    for local mailboxes, renders just /path.
  ::
  ++  phat
    ^-  tape
    %+  weld
      ?:  =(our-self entity.tr)  ~
      (scow %p entity.tr)
    "/{(trip name.tr)}"
  ::  +show: render as tape, as glyph if we can
  ::
  ++  show
    ^-  tape
    =+  cha=(~(get by bound) tr)
    ?~(cha phat [u.cha ~])
  ::  +glyph: tape for glyph of target, defaulting to *
  ::
  ++  glyph
    ^-  tape
    [(~(gut by bound) tr '*') ~]
  --
::
::  +mr: render messages
::
++  mr
  |_  $:  source=target
          post:post
      ==
  +*  showtime  (~(has in settings) %showtime)
      notify    (~(has in settings) %notify)
  ::
  ++  content-width
    ::  termwidth, minus author, timestamp, and padding
    %+  sub  width
    %+  add  15
    ?:(showtime 11 0)
  ::
  ++  render-inline
    ^-  shoe-effect:shoe
    :+  %row
      :-  15
      ?.  showtime
        ~[(sub width 16)]
      ~[(sub width 26) 9]
    :+  t+(crip (weld (nome author) ~(glyph tr source)))
      t+(crip line)
    ?.  showtime  ~
    :_  ~
    :-  %t
    =.  time-sent
      %-  ?:(p.timez add sub)
      [time-sent (mul q.timez ~h1)]
    =+  dat=(yore time-sent)
    =*  t   (d-co:co 2)
    =,  t.dat
    %-  crip
    :(weld "~" (t h) "." (t m) "." (t s))
  ::
  ++  line
    ^-  tape
    %-  zing
    %+  join  "\0a"
    %-  turn
    :_  |=(ls=(list tape) `tape`(zing (join " " ls)))
    %+  roll  contents
    |=  [=content:post out=(list (list tape))]
    ?-  -.content
      %text       (append-inline out (trip text.content))
      %mention    (append-inline out (scow %p ship.content))
      %reference  (append-inline out "^")
    ::
        %code
      %+  snoc  out
      ^-  (list tape)
      :-  (trip expression.content)
      ?:  =(~ output.content)  ~
      :-  "\0a"
      ~(ram re (snag 0 output.content))^~
    ::
        %url
      %+  append-inline  out
      =+  wyd=content-width
      =+  ful=(trip url.content)
      ::  if the full url fits, just render it.
      ?:  (gte wyd (lent ful))  ful
      ::  if it doesn't, prefix with _ and truncate domain with ellipses
      =.  wyd  (sub wyd 2)
      :-  '_'
      =-  (weld - "_")
      =+  prl=(rust ful aurf:de-purl:html)
      ?~  prl  (scag wyd ful)
      =+  hok=r.p.p.u.prl
      =;  domain=tape
        %+  swag
          [(sub (max wyd (lent domain)) wyd) wyd]
        domain
      ?.  ?=(%& -.hok)
        +:(scow %if p.hok)
      %+  reel  p.hok
      |=  [a=knot b=tape]
      ?~  b  (trip a)
      (welp b '.' (trip a))
    ==
  ::
  ++  append-newline
    |=  [content=(list (list tape)) newline=tape]
    ^-  (list (list tape))
    (snoc content ~[newline])
  ::
  ++  append-inline
    |=  [content=(list (list tape)) inline=tape]
    ^-  (list (list tape))
    ?:  =(~ content)
      ~[~[inline]]
    =/  last
      (dec (lent content))
    =/  old=(list tape)
      (snag last content)
    =/  new=(list tape)
      (snoc old inline)
    (snap content last new)

  ::  +activate: produce sole-effect for printing message details
  ::
  ++  render-activate
    ^-  sole-effect:shoe
    ~[%mor [%tan meta] body]
  ::  +meta: render message metadata (serial, timestamp, author, target)
  ::
  ++  meta
    ^-  tang
    =+  hed=leaf+"{(scow %uv (fall hash 0))} at {(scow %da time-sent)}"
    =/  src=tape  ~(phat tr source)
    [%rose [" " ~ ~] [hed >author< [%rose [", " "to " ~] [leaf+src]~] ~]]~
  ::  +body: long-form render of message contents
  ::
  ++  body
    |-  ^-  sole-effect:shoe
    :-  %mor
    %+  turn  contents
    |=  =content:post
    ^-  sole-effect:shoe
    ?-  -.content
      %text       txt+(trip text.content)
      %url        url+url.content
    ::
        %reference
      ?-  -.reference.content
          %graph
        txt+"[reference to msg in {~(phat tr resource.uid.reference.content)}]"
      ::
          %group
        txt+"[reference to msg in {~(phat tr group.reference.content)}]"
      ::
          %app
        =,  reference.content
        txt+"[reference to app: {(scow %p ship)}/{(trip desk)}{(spud path)}]"
      ==
    ::
        %mention
      ?.  =(ship.content our-self)  txt+(scow %p ship.content)
      :-  %mor
      :-  klr+[[`%br ~ ~]^(scow %p ship.content)]~  ::TODO  inline
      ?.(notify ~ [%bel ~]~)
    ::
        %code
      :-  %txt
      %+  weld  (trip expression.content)
      ?:  =(~ output.content)  ~
      :-  '\0a'
      ~(ram re (snag 0 output.content))
    ==
  ::  +nome: prints a ship name in 14 characters, left-padding with spaces
  ::
  ++  nome
    |=  =ship
    ^-  tape
    =+  raw=(cite:title ship)
    (runt [(sub 14 (lent raw)) ' '] raw)
  --
::
++  simple-wrap
  |=  [txt=tape wid=@ud]
  ^-  (list tape)
  ?~  txt  ~
  =/  [end=@ud nex=?]
    =+  ret=(find "\0a" (scag +(wid) `tape`txt))
    ?^  ret  [u.ret &]
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
