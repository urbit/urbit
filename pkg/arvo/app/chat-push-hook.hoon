/-  *permission-store, *invite-store, hook=chat-push-hook
/+  default-agent, verb, dbug, store=chat-store, metadata, *userspace
|%
+$  card  card:agent:gall
+$  versioned-state
  $%  [%0 state-0]
  ==
+$  state-0
  $:  sharing=(set rid)
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
  [%pass /permissions %agent [our.bowl %permission-store] %watch /updates]~
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
    ?.  (~(has in sharing) (path-to-rid path.act))  ~
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
      ?:  (~(has in sharing) rid)  [~ state]
      =:  sharing  (~(put in sharing) rid)
          allow-history  (~(put by allow-history) rid allow-history.act)
      ==
      :_  state
      [%pass chat-wire %agent [our.bowl %chat-store] %watch chat-path]~
    ::
        %remove
      =/  =rid  (path-to-rid path.act)
      ?.  (~(has in sharing) rid)
        ~&  [dap.bowl %already-not-sharing rid]
        [~ state]
      =.  sharing  (~(del in sharing) rid)
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
  ++  backlog
    |=  =^path
    ^-  (list card)
    ?>  ?=(^ path)
    =/  last  (dec (lent path))
    =/  backlog-latest=(unit @ud)  (rush (snag last `(list @ta)`path) dem:ag)
    =/  pas  `^^path`(oust [last 1] `(list @ta)`path)
    =/  =rid  (path-to-rid pas)
    ?>  (~(has in sharing) rid)
    ?>  (is-permitted bowl src.bowl pas)
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
    ?>  (~(has in sharing) (path-to-rid path))
    ?>  (is-permitted bowl src.bowl path)
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
    ?.  (~(has in sharing) (path-to-rid t.wire))  [~ this]
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
      ?.  (~(has in sharing) rid)  [~ this]
      =.  sharing  (~(del in sharing) rid)
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
      ?.  (~(has in sharing) (path-to-rid chat))  ~
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
