/-  *permission-store, *invite-store, hook=chat-push-hook, old=chat-hook
/+  default-agent, verb, dbug, store=chat-store, metadata, *userspace
|%
+$  card  card:agent:gall
+$  versioned-state
  $%  [%0 state-0]
  ==
+$  state-0
  $:  providers=(jug rid ship)
      allow-history=(map rid ?)
  ==
--
::
=|  [%0 state-0]
=*  state  -
::
%-  agent:dbug
%+  verb  |
::
|^
^-  agent:gall
|_  =bowl:gall
+*  this  .
    def  ~(. (default-agent this %|) bowl)
::
++  on-init
  ^-  (quip card _this)
  :_  this
  :-  [%pass /permissions %agent [our.bowl %permission-store] %watch /updates]
  ?.  .^(? %gu /(scot %p our.bowl)/chat-hook/(scot %da now.bowl))
    ~
  =/  contents
    .^  contents:old
        %gx
        (scot %p our.bowl)
        %chat-hook
        (scot %da now.bowl)
        %contents
        %noun
        ~
    ==
  %+  murn  ~(tap by synced.contents)
  |=  [=path =ship]
  ^-  (unit card)
  ?.  =(our.bowl ship)
    ~
  =/  allow-history  (fall (~(get by allow-history.contents) path) |)
  =/  =action:hook  [%add path allow-history]
  ~&  [%migrating-local-chat path allow-history=allow-history]
  =/  vas  !>(action)
  `[%pass / %agent [our.bowl %chat-push-hook] %poke %chat-push-hook-action vas]
::
++  on-save  !>(state)
++  on-load
  |=  =vase
  =/  old  !<(versioned-state vase)
  ?-  -.old
    %0  [~ this(state old)]
  ==
::
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  |^
  =^  cards  state
    ?+  mark  (on-poke:def mark vase)
        %json  (chat-action (action:dejs:store !<(json vase)))
        %chat-action  (chat-action !<(action:store vase))
        %chat-push-hook-action  (chat-push-hook-action !<(action:hook vase))
    ==
  [cards this]
  ::
  ++  chat-action
    |=  act=action:store
    ^-  (quip card _state)
    ?>  ?=(%message -.act)
    :_  state
    =/  =rid  (path-to-rid path.act)
    ?.  (~(has by providers) rid)  ~
    ::  If this is the push-hook of a proxy, prohibit writing
    ?>  =(our.bowl ship.rid)
    ?.  (is-permitted bowl src.bowl path.act)  ~
    =*  letter  letter.envelope.act
    =?  letter
        ?&  (team:title our.bowl src.bowl)
            ?=(%code -.letter)
            ?=(~ output.letter)
        ==
      =/  =hoon  (ream expression.letter)
      letter(output (eval:store bowl hoon))
    =:  author.envelope.act  src.bowl
        when.envelope.act  now.bowl
    ==
    [%pass / %agent [our.bowl %chat-store] %poke %chat-action !>(act)]~
  ::
  ++  chat-push-hook-action
    |=  act=action:hook
    ^-  (quip card _state)
    ?.  (team:title our.bowl src.bowl)
      [~ state]
    ?-  -.act
        %add
      =/  =rid  (path-to-rid path.act)
      =/  chat-path  [%mailbox path.act]
      =/  chat-wire  [%store path.act]
      ?:  (~(has by providers) rid)  [~ state]
      =:  providers  (~(put ju providers) rid our.bowl)
          allow-history  (~(put by allow-history) rid allow-history.act)
      ==
      :_  state
      [%pass chat-wire %agent [our.bowl %chat-store] %watch chat-path]~
    ::
        %set-proxies
      =/  =rid  (path-to-rid path.act)
      ?>  (~(has by providers) rid)
      ?>  (~(all in proxies.act) |=(=ship (is-permitted bowl ship path.act)))
      =.  providers  (~(put by providers) rid (~(put in proxies.act) our.bowl))
      [~ state]
    ::
        %remove
      =/  =rid  (path-to-rid path.act)
      ?.  (~(has by providers) rid)  [~ state]
      =.  providers  (~(del by providers) rid)
      =/  backlog-paths=(list path)
        %+  murn  ~(tap by sup.bowl)
        |=  [duct [@p =path]]
        ^-  (unit ^path)
        ?:  ?=([%backlog *] path)
          `path
        ~
      :_  state
      :~  [%give %kick [%mailbox path.act]~ ~]
          :: XX what happens if backlog-paths is empty?
          [%give %kick backlog-paths ~]
          [%pass [%mailbox path.act] %agent [our.bowl %chat-store] %leave ~]
      ==
    ==
  --
::
++  on-watch
  |=  =path
  ^-  (quip card _this)
  |^
  ?+  path          (on-watch:def path)
      [%backlog *]  [(backlog t.path) this]
      [%mailbox *]  [(mailbox t.path) this]
  ==
  ::
  ++  provider-for
    |=  [=ship =rid]
    ^-  ^ship
    =/  candidates  ~(tap in (~(got by providers) rid))
    =/  idx  (mod ship (lent `(list @p)`candidates))
    (snag idx candidates)
  ::
  ++  redirect
    |=  [=rid provider=ship]
    ^-  (list card)
    =/  chat  (rid-to-path rid)
    ::  XX can it be guaranteed that the fact be sent before the kick?
    :~  [%give %fact ~ %chat-push-hook-update !>([%redirect provider chat])]
        ::  XX what happens if I use ~ here instead of [path]~, or don't
        ::  specify the ship?
        [%give %kick ~[path] `src.bowl]
    ==
  ::
  ++  backlog
    |=  =^path
    ^-  (list card)
    ?>  ?=(^ path)
    =/  last  (dec (lent path))
    =/  backlog-latest=(unit @ud)  (rush (snag last `(list @ta)`path) dem:ag)
    =/  pas  `^^path`(oust [last 1] `(list @ta)`path)
    =/  =rid  (path-to-rid pas)
    ?>  (~(has by providers) rid)
    ?>  (is-permitted bowl src.bowl pas)
    ::  Check whether we should proxy
    ?.  ?|  (~(has ju providers) rid src.bowl)
            =(our.bowl (provider-for src.bowl rid))
        ==
      ~&  [%redirect src.bowl %to (provider-for src.bowl rid)]
      (redirect rid (provider-for src.bowl rid))
    ::  Provide directly
    =/  envs  envelopes:(need (chat-scry:store bowl pas))
    =/  length  (lent envs)
    =/  latest
      ?~  backlog-latest  length
      ?:  (gth u.backlog-latest length)  length
      (sub length u.backlog-latest)
    =.  envs  (scag latest envs)
    =/  =vase  !>([%messages pas 0 latest envs])
    %-  zing
    :~  [%give %fact ~ %chat-update !>([%create pas])]~
        ?.  ?&(?=(^ backlog-latest) (~(has by allow-history) rid))  ~
        [%give %fact ~ %chat-update vase]~
        [%give %kick [%backlog path]~ `src.bowl]~
    ==
  ::
  ++  mailbox
    |=  =^path
    ^-  (list card)
    =/  =rid  (path-to-rid path)
    ?>  (~(has by providers) rid)
    ?>  (is-permitted bowl src.bowl path)
    ::  Check whether we should proxy
    ?.  ?|  (~(has ju providers) rid src.bowl)
            =(our.bowl (provider-for src.bowl rid))
        ==
      ~&  [%redirect src.bowl %to (provider-for src.bowl rid)]
      (redirect rid (provider-for src.bowl rid))
    ::  Provide directly
    =/  box  (chat-scry:store bowl path)
    ?~  box  !!
    [%give %fact ~ %chat-update !>([%create path])]~
  --
::
++  on-agent
  |=  [=wire =sign:agent:gall]
  ^-  (quip card _this)
  |^
  ?+  -.sign                  (on-agent:def wire sign)
      %kick  ?+  wire         !!
          [%permissions ~]    kick-permissions
          [%store @ *]        (kick-store wire)
      ==
      %watch-ack              (watch-ack wire p.sign)
      %fact  ?+  p.cage.sign  (on-agent:def wire sign)
          %chat-update        (fact-chat-update !<(update:store q.cage.sign))
          %permission-update  (fact-perm-update !<(permission-update q.cage.sign))
  ==  ==
  ::
  ++  kick-permissions
    :_  this
    [%pass /permissions %agent [our.bowl %permission-store] %watch /updates]~
  ::
  ++  kick-store
    |=  wire=(lest @ta)
    ~&  store-kick+wire
    ?.  (~(has by providers) (path-to-rid t.wire))  [~ this]
    ~&  %chat-store-resubscribe
    =/  mailbox=(unit mailbox:store)
      (chat-scry:store bowl t.wire)
    :_  this
    [%pass wire %agent [our.bowl %chat-store] %watch [%mailbox t.wire]]~
  ::
  ++  watch-ack
    |=  [=^wire tang=(unit tang)]
    ^-  (quip card _this)
    ?~  tang  [~ this]
    ?.  ?=([%store *] wire)  [~ this]
    =/  pok  [%poke %chat-push-hook-action !>([%remove t.wire])]
    :_  this
    [%pass / %agent [our.bowl %chat-push-hook] pok]~
  ::
  ++  fact-chat-update
    |=  =update:store
    ^-  (quip card _this)
    ?>  (team:title our.bowl src.bowl)
    ?+  -.update     [~ this]
        %delete
      =/  =rid  (path-to-rid path.update)
      ?.  (~(has by providers) rid)  [~ this]
      =.  providers  (~(del by providers) rid)
      :_  this
      [%pass [%mailbox path.update] %agent [our.bowl %chat-store] %leave ~]~
    ::
        %message
      :_  this
      [%give %fact [%mailbox path.update]~ %chat-update !>(update)]~
    ::
        %messages
      :_  this
      [%give %fact [%mailbox path.update]~ %chat-update !>(update)]~
    ==
  ::
  ++  fact-perm-update
    |=  fact=permission-update
    ^-  (quip card _this)
    |^
    :_  this
    ?+  -.fact   ~
        %add     (handle-permissions %add path.fact who.fact)
        %remove  (handle-permissions %remove path.fact who.fact)
    ==
    ++  handle-permissions
      |=  [kind=?(%add %remove) =path who=(set ship)]
      ^-  (list card)
      %-  zing
      %+  turn
        (~(app-paths-from-group metadata bowl) %chat path)
      |=  chat=^path
      ^-  (list card)
      ?.  (~(has by providers) (path-to-rid chat))  ~
      %-  zing
      %+  turn  ~(tap in who)
      |=  =ship
      ?:  (is-permitted bowl ship chat)
        ::  XX If they are permitted, under what cicumstances will it be
        ::  remove?
        ?:  ?|(=(kind %remove) =(ship our.bowl) (is-managed path))  ~
        ::  if ship has just been added to the permitted group,
        ::  send them an invite
        ~[(send-invite chat ship)]
      ::  if ship is not permitted, kick their subscription
      [%give %kick [%mailbox chat]~ `ship]~
    ::
    ++  send-invite
      |=  [=path =ship]
      ^-  card
      =/  =invite  [our.bowl %chat-push-hook path ship '']
      =/  act=invite-action  [%invite /chat (shaf %msg-uid eny.bowl) invite]
      [%pass / %agent [our.bowl %invite-hook] %poke %invite-action !>(act)]
    ::
    ++  is-managed
      |=  =path
      ^-  ?
      ?>  ?=(^ path)
      !=(i.path '~')
    --
  --
::
++  on-leave  on-leave:def
++  on-peek   on-peek:def
++  on-arvo   on-arvo:def
++  on-fail   on-fail:def
--
++  is-permitted
  |=  [=bowl:gall who=ship chat=path]
  ^-  ?
  %+  lien  (~(groups-from-resource metadata bowl) [%chat chat])
  |=  group=path
  .^  ?
    %gx
    (scot %p our.bowl)
    %permission-store
    (scot %da now.bowl)
    (snoc `path`[%permitted (scot %p who) group] %noun)
  ==
--
