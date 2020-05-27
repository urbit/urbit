/-  *permission-store, *invite-store, hook=chat-push-hook
/+  default-agent, verb, dbug, store=chat-store, lib=chat-hooks
|%
+$  card  card:agent:gall
+$  versioned-state
  $%  [%0 state-0]
  ==
+$  state-0
  $:  sharing=(set path)
      allow-history=(map path ?)
  ==
--
::
=|  [%0 state-0]
=*  state  -
::
%-  agent:dbug
%+  verb  |
::
^-  agent:gall
|_  bol=bowl:gall
+*  this  .
    def  ~(. (default-agent this %|) bol)
::
++  on-init
  ^-  (quip card _this)
  :_  this
  [%pass /permissions %agent [our.bol %permission-store] %watch /updates]~
::
++  on-save  !>(state)
++  on-load
  |=  vax=vase
  =/  old  !<(versioned-state vax)
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
    ::  XX why do we use ?> here but ?. ~ below?
    ?>  ?=(%message -.act)
    :_  state
    ?.  (~(has in sharing) path.act)  ~
    ?.  (is-permitted:lib bol src.bol path.act)  ~
    ::  XX do we still need this part?
    =*  letter  letter.envelope.act
    =?  letter
        ?&  (team:title our.bol src.bol)
            ?=(%code -.letter)
            ?=(~ output.letter)
        ==
      =/  =hoon  (ream expression.letter)
      letter(output (eval:store bol hoon))
    =:  author.envelope.act  src.bol
        when.envelope.act  now.bol
    ==
    [%pass / %agent [our.bol %chat-store] %poke %chat-action !>(act)]~
   
  ::
  ++  chat-push-hook-action
    |=  act=action:hook
    ^-  (quip card _state)
    ?-  -.act
        %add
      ?>  (team:title our.bol src.bol)
      =/  chat-path  [%mailbox path.act]
      =/  chat-wire  [%store path.act]
      ?:  (~(has in sharing) path.act)  [~ state]
      =:  sharing  (~(put in sharing) path.act)
          allow-history  (~(put by allow-history) path.act allow-history.act)
      ==
      :_  state
      [%pass chat-wire %agent [our.bol %chat-store] %watch chat-path]~
    ::
        %remove
      ?.  !(team:title our.bol src.bol)
        [~ state]
      ?.  (~(has in sharing) path.act)
        ~&  [dap.bol %already-not-sharing path.act]
        [~ state]
      =.  sharing  (~(del in sharing) path.act)
      :_  state
      :~  [%give %kick [%mailbox path.act]~ ~]
          [%pass [%mailbox path.act] %agent [our.bol %chat-store] %leave ~]
          ::  XX is it worth kicking backlog subscriptions here just to be
          ::  sure?
      ==
    ==
  --
::
++  on-watch
  |=  pax=path
  ^-  (quip card _this)
  |^
  ?+  pax           (on-watch:def pax)
      [%backlog *]  [(backlog t.pax) this]
      [%mailbox *]  [(mailbox t.pax) this]
  ==
  ::
  ++  backlog
    |=  pax=path
    ^-  (list card)
    ?>  ?=(^ pax)
    =/  last  (dec (lent pax))
    =/  backlog-latest=(unit @ud)  (rush (snag last `(list @ta)`pax) dem:ag)
    =/  pas  `path`(oust [last 1] `(list @ta)`pax)
    ?>  ?=([* ^] pas)
    ?>  (~(has in sharing) pas)
    ?>  (is-permitted:lib bol src.bol pas)
    =/  envs  envelopes:(need (chat-scry:lib bol pas))
    =/  length  (lent envs)
    =/  latest
      ?~  backlog-latest  length
      ?:  (gth u.backlog-latest length)  length
      (sub length u.backlog-latest)
    =.  envs  (scag latest envs)
    =/  =vase  !>([%messages pas 0 latest envs])
    %-  zing
    :~  [%give %fact ~ %chat-update !>([%create pas])]~
        ?.  ?&(?=(^ backlog-latest) (~(has by allow-history) pas))  ~
        [%give %fact ~ %chat-update vase]~
        [%give %kick [%backlog pax]~ `src.bol]~
    ==
  ::
  ++  mailbox
    |=  pax=path
    ^-  (list card)
    ?>  ?=(^ pax)
    ?>  (~(has in sharing) pax)
    ?>  (is-permitted:lib bol src.bol pax)
    =/  box  (chat-scry:lib bol pax)
    ?~  box  !!
    [%give %fact ~ %chat-update !>([%create pax])]~
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
    [%pass /permissions %agent [our.bol %permission-store] %watch /updates]~
  ::
  ++  kick-store
    |=  wire=(lest @ta)
    ~&  store-kick+wire
    ?.  (~(has in sharing) t.wire)  [~ this]
    ~&  %chat-store-resubscribe
    =/  mailbox=(unit mailbox:store)
      (chat-scry:lib bol t.wire)
    :_  this
    [%pass wire %agent [our.bol %chat-store] %watch [%mailbox t.wire]]~
  ::
  ++  watch-ack
    |=  [=^wire tang=(unit tang)]
    ^-  (quip card _this)
    ?~  tang  [~ this]
    ?.  ?=([%store *] wire)  [~ this]
    =/  pok  [%poke %chat-push-hook-action !>([%remove t.wire])]
    :_  this
    [%pass / %agent [our.bol %chat-push-hook] pok]~
  ::
  ++  fact-chat-update
    |=  =update:store
    ^-  (quip card _this)
    ?>  (team:title our.bol src.bol)  ::  XX or do I need to use ?.  [~ this]
    ?+  -.update     [~ this]
        %delete
      ?.  (~(has in sharing) path.update)  [~ this]
      =.  sharing  (~(del in sharing) path.update)
      :_  this
      [%pass [%mailbox path.update] %agent [our.bol %chat-store] %leave ~]~
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
        %add     (handle-permissions [%add path.fact who.fact])
        %remove  (handle-permissions [%remove path.fact who.fact])
    ==
    ++  handle-permissions
      |=  [kind=?(%add %remove) pax=path who=(set ship)]
      ^-  (list card)
      %-  zing
      %+  turn
        (chats-of-group:lib bol pax)
      |=  chat=path
      ^-  (list card)
      ?.  (~(has in sharing) chat)  ~
      %-  zing
      %+  turn  ~(tap in who)
      |=  =ship
      ?:  (is-permitted:lib bol ship chat)
        ?:  ?|(=(kind %remove) =(ship our.bol) (is-managed pax))  ~
        ::  if ship has just been added to the permitted group,
        ::  send them an invite
        ~[(send-invite chat ship)]
      ::  if ship is not permitted, kick their subscription
      [%give %kick [%mailbox chat]~ `ship]~
    ::
    ++  send-invite
      |=  [=path =ship]
      ^-  card
      =/  =invite  [our.bol %chat-hook path ship '']
      =/  act=invite-action  [%invite /chat (shaf %msg-uid eny.bol) invite]
      [%pass / %agent [our.bol %invite-hook] %poke %invite-action !>(act)]
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
