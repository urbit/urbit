/-  *invite-store, hook=chat-pull-hook
/+  default-agent, verb, dbug, store=chat-store, lib=chat-hooks
|%
+$  card  card:agent:gall
+$  versioned-state
  $%  [%0 state-0]
  ==
+$  state-0
  $:  tracking=(map path ship)
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
  =/  inv  [our.bol %invite-store]
  :~  [%pass / %agent inv %poke %invite-action !>([%create /chat])]
      [%pass /invites %agent inv %watch /invitatory/chat]
  ==
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
  ?.  ?=(%chat-pull-hook-action mark)
    (on-poke:def mark vase)
  =/  act  !<(action:hook vase)
  ?-  -.act
      %add
    ?>  (team:title our.bol src.bol)
    ?:  (~(has by tracking) path.act)  [~ this]
    =.  tracking  (~(put by tracking) path.act ship.act)
    ?.  ask-history.act
      =/  chat-path  [%mailbox path.act]
      :_  this
      [%pass chat-path %agent [ship.act %chat-push-hook] %watch chat-path]~
    =/  mailbox=(unit mailbox:store)  (chat-scry:lib bol path.act)
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
    ::  XX chat-hook had a path for reading the ship out of the path if not
    ::  present in the tracking map. Why is this helpful or necessary?
    ::  If it's not in the map, what's the effect of removing it?
    ::  Canceling subscriptions that shouldn't exist?
    ^-  (quip card _this)
    |^
    =/  ship  (~(get by tracking) path.act)
    ?~  ship
      ~&  [dap.bol %unknown-host-cannot-leave path.act]
      [~ this]
    ?:  &(!=(u.ship src.bol) !(team:title our.bol src.bol))
      [~ this]
    =.  tracking  (~(del by tracking) path.act)
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
      %+  murn  ~(tap by wex.bol)
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
  |=  pax=path
  ^-  (quip card _this)
  ?.  =(/tracking pax)  (on-watch:def pax)
  ?>  (team:title our.bol src.bol)
  :_  this
  [%give %fact ~ %chat-pull-hook-update !>([%tracking tracking])]~
::
++  on-agent
  |=  [=wire =sign:agent:gall]
  ^-  (quip card _this)
  |^
  ?+  -.sign  (on-agent:def wire sign)
      %kick
    ?+  wire  !!
        [%mailbox @ *]
      ~&  mailbox-kick+wire
      ?.  (~(has by tracking) t.wire)  [~ this]
      ~&  %chat-pull-hook-resubscribe
      =/  =ship  (~(got by tracking) t.wire)
      =/  mailbox=(unit mailbox:store)  (chat-scry:lib bol t.wire)
      =/  chat-history
        %+  welp  backlog+t.wire
        ?~(mailbox /0 /(scot %ud (lent envelopes.u.mailbox)))
      :_  this
      [%pass chat-history %agent [ship %chat-push-hook] %watch chat-history]~
    ::
        [%backlog @ @ *]
      =/  chat=path  (oust [(dec (lent t.wire)) 1] `(list @ta)`t.wire)
      ?.  (~(has by tracking) chat)  [~ this]
      =/  =ship
        ?:  =('~' i.t.wire)
          (slav %p i.t.t.wire)
        (slav %p i.t.wire)
      =/  =path  ?~((chat-scry:lib bol chat) wire [%mailbox chat])
      :_  this
      [%pass path %agent [ship %chat-push-hook] %watch path]~
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
                [our.bol %chat-view]
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
        :_  this
        ?>  ?=([* ^] path.update)
        =/  shp  (~(get by tracking) path.update)
        ?~  shp  ~
        ?.  =(src.bol u.shp)  ~
        [(chat-poke [%create path.update])]~
      ::
          %delete
        ?>  ?=([* ^] path.update)
        =/  shp  (~(get by tracking) path.update)
        ?~  shp  [~ this]
        ?.  =(u.shp src.bol)  [~ this]
        =.  tracking  (~(del by tracking) path.update)
        :_  this
        :~  (chat-poke [%delete path.update])
            :*  %pass
                [%mailbox path.update]
                %agent
                [src.bol %chat-push-hook]
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
        :_  this
        ?>  ?=([* ^] path.update)
        =/  shp  (~(get by tracking) path.update)
        ?~  shp  ~
        ?.  =(src.bol u.shp)  ~
        [(chat-poke [%message path.update envelope.update])]~
      ::
          %messages
        :_  this
        ?>  ?=([* ^] path.update)
        =/  shp  (~(get by tracking) path.update)
        ?~  shp  ~
        ?.  =(src.bol u.shp)  ~
        [(chat-poke [%messages path.update envelopes.update])]~
      ==
      ::
      ++  chat-poke
        |=  act=action:store
        ^-  card
        [%pass / %agent [our.bol %chat-store] %poke %chat-action !>(act)]
      --
    ++  fact-invite-update
      |=  fact=invite-update
      ^-  (quip card _this)
      :_  this
      ?+  -.fact  ~
          %accepted
        =/  ask-history  ?~((chat-scry:lib bol path.invite.fact) %.y %.n)
        =*  shp       ship.invite.fact
        =*  app-path  path.invite.fact
        :~  :*  %pass
                /
                %agent
                [our.bol %chat-view]
                %poke
                %chat-view-action
                !>([%join shp app-path ask-history])
        ==  ==
      ==
    --
  ==
  ::
  --
::
++  on-leave  on-leave:def
++  on-peek   on-peek:def
++  on-arvo   on-arvo:def
++  on-fail   on-fail:def
--
