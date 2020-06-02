/-  *invite-store, hook=chat-pull-hook
/+  default-agent, verb, dbug, store=chat-store, *userspace
|%
+$  card  card:agent:gall
+$  versioned-state
  $%  [%0 state-0]
  ==
+$  state-0
  $:  tracking=(map rid ship)
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
|_  =bowl:gall
+*  this  .
    def  ~(. (default-agent this %|) bowl)
::
++  on-init
  ^-  (quip card _this)
  :_  this
  =/  inv  [our.bowl %invite-store]
  :~  [%pass / %agent inv %poke %invite-action !>([%create /chat])]
      [%pass /invites %agent inv %watch /invitatory/chat]
  ==
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
  ?.  ?=(%chat-pull-hook-action mark)
    (on-poke:def mark vase)
  ?>  (team:title our.bowl src.bowl)
  =/  act  !<(action:hook vase)
  ?-  -.act
      %add
    =/  =rid  (path-to-rid path.act)
    ?:  (~(has by tracking) rid)  [~ this]
    =.  tracking  (~(put by tracking) rid ship.act)
    ?.  ask-history.act
      =/  chat-path  [%mailbox path.act]
      :_  this
      [%pass chat-path %agent [ship.act %chat-push-hook] %watch chat-path]~
    =/  mailbox=(unit mailbox:store)  (chat-scry:store bowl path.act)
    =/  backlog=path
      :-  %backlog
      %+  weld  path.act
      ?~(mailbox /0 /(scot %ud (lent envelopes.u.mailbox)))
    :_  this
    :~  [%pass backlog %agent [ship.act %chat-push-hook] %watch backlog]
        :*  %give
            %fact
            ~[/tracking]
            %chat-pull-hook-update
            !>([%tracking tracking])
    ==  ==
  ::
      %remove
    ^-  (quip card _this)
    |^
    =/  =rid  (path-to-rid path.act)
    =/  ship  (~(get by tracking) rid)
    ?~  ship
      ~&  [dap.bowl %unknown-host-cannot-leave path.act]
      [~ this]
    ?:  &(!=(u.ship src.bowl) !(team:title our.bowl src.bowl))
      [~ this]
    =.  tracking  (~(del by tracking) rid)
    :_  this
    :*  [%pass [%mailbox path.act] %agent [u.ship %chat-push-hook] %leave ~]
        :*  %give
            %fact
            ~[/tracking]
            %chat-pull-hook-update
            !>([%tracking tracking])
        ==
        (pull-backlog-subscriptions u.ship path.act)
    ==
    ::
    ++  pull-backlog-subscriptions
      |=  [target=ship chat=path]
      ^-  (list card)
      %+  murn  ~(tap by wex.bowl)
      |=  [[=wire =ship =term] [acked=? =path]]
      ^-  (unit card)
      ?.  ?&  =(ship target)
              ?=([%backlog *] wire)
              =(`1 (find chat wire))
          ==
        ~
      `[%pass wire %agent [ship %chat-push-hook] %leave ~]
    --
  ==
::
++  on-watch
  |=  =path
  ^-  (quip card _this)
  ?.  =(/tracking path)  (on-watch:def path)
  ?>  (team:title our.bowl src.bowl)
  :_  this
  [%give %fact ~ %chat-pull-hook-update !>([%tracking tracking])]~
::
++  on-agent
  |=  [=wire =sign:agent:gall]
  ^-  (quip card _this)
  ?+  -.sign  (on-agent:def wire sign)
      %kick
    ?+  wire  !!
        [%mailbox @ *]
      ~&  mailbox-kick+wire
      =/  =rid  (path-to-rid t.wire)
      ?.  (~(has by tracking) rid)  [~ this]
      ~&  %chat-pull-hook-resubscribe
      =/  =ship  (~(got by tracking) rid)
      =/  mailbox=(unit mailbox:store)  (chat-scry:store bowl t.wire)
      =/  chat-history
        %+  welp  backlog+t.wire
        ?~(mailbox /0 /(scot %ud (lent envelopes.u.mailbox)))
      :_  this
      [%pass chat-history %agent [ship %chat-push-hook] %watch chat-history]~
    ::
        [%backlog @ @ *]
      =/  chat=path  (oust [(dec (lent t.wire)) 1] `(list @ta)`t.wire)
      =/  =rid  (path-to-rid chat)
      ?.  (~(has by tracking) rid)  [~ this]
      =/  =path  ?~((chat-scry:store bowl chat) wire [%mailbox chat])
      :_  this
      [%pass path %agent [ship.rid %chat-push-hook] %watch path]~
    ==
  ::
      %watch-ack
    =/  tnk  p.sign
    ?~  tnk  [~ this]
    ?.  ?=([%backlog @ @ @ *] wire)  [~ this]
    =/  chat=path  (oust [(dec (lent t.wire)) 1] `(list @ta)`t.wire)
    :_  this
    %.  :~  :*  %pass
                /
                %agent
                [our.bowl %chat-view]
                %poke
                %chat-view-action
                !>([%delete chat])
        ==  ==
    %-  slog
    :*  leaf+"chat-pull-hook failed subscribe on {(spud chat)}"
        leaf+"stack trace:"
        u.tnk
    ==
  ::
      %fact
    |^
    ?+  p.cage.sign  (on-agent:def wire sign)
        %chat-update    (fact-chat-update !<(update:store q.cage.sign))
        %invite-update  (fact-invite-update !<(invite-update q.cage.sign))
    ==
    ::
    ++  fact-chat-update
      |=  =update:store
      ^-  (quip card _this)
      |^
      ?+  -.update   [~ this]
          %create
        =/  =rid  (path-to-rid path.update)
        :_  this
        =/  ship  (~(get by tracking) rid)
        ?~  ship  ~
        ?.  =(src.bowl u.ship)  ~
        [(chat-poke [%create path.update])]~
      ::
          %delete
        =/  =rid  (path-to-rid path.update)
        =/  ship  (~(get by tracking) rid)
        ?~  ship  [~ this]
        ?.  =(u.ship src.bowl)  [~ this]
        =.  tracking  (~(del by tracking) rid)
        :_  this
        :~  (chat-poke [%delete path.update])
            :*  %pass
                [%mailbox path.update]
                %agent
                [src.bowl %chat-push-hook]
                %leave
                ~
            ==
            :*  %give
                %fact
                ~[/tracking]
                %chat-pull-hook-update
                !>([%tracking tracking])
        ==  ==
      ::
          %message
        =/  =rid  (path-to-rid path.update)
        :_  this
        =/  ship  (~(get by tracking) rid)
        ?~  ship  ~
        ?.  =(src.bowl u.ship)  ~
        [(chat-poke [%message path.update envelope.update])]~
      ::
          %messages
        :_  this
        =/  =rid  (path-to-rid path.update)
        =/  ship  (~(get by tracking) rid)
        ?~  ship  ~
        ?.  =(src.bowl u.ship)  ~
        [(chat-poke [%messages path.update envelopes.update])]~
      ==
      ::
      ++  chat-poke
        |=  act=action:store
        ^-  card
        [%pass / %agent [our.bowl %chat-store] %poke %chat-action !>(act)]
      --
    ::
    ++  fact-invite-update
      |=  fact=invite-update
      ^-  (quip card _this)
      :_  this
      ?+  -.fact  ~
          %accepted
        =/  ask-history  ?~((chat-scry:store bowl path.invite.fact) %.y %.n)
        =*  ship       ship.invite.fact
        =*  app-path  path.invite.fact
        :~  :*  %pass
                /
                %agent
                [our.bowl %chat-view]
                %poke
                %chat-view-action
                !>([%join ship app-path ask-history])
        ==  ==
      ==
    --
  ==
::
++  on-leave  on-leave:def
++  on-peek   on-peek:def
++  on-arvo   on-arvo:def
++  on-fail   on-fail:def
--
