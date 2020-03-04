::  link-listen-hook: get your friends' bookmarks
::
::    on-init, subscribes to all groups on this ship. for every ship in a group,
::    we subscribe to their link's local-pages and annotations
::    at the group path (through link-proxy-hook),
::    and forwards all entries into our link as submissions and comments.
::
::    if a subscription to a group member fails, we assume it's because their
::    group definition hasn't been updated to include us yet.
::    we retry with exponential backoff, maxing out at one hour timeouts.
::
/-  *link, group-store
/+  default-agent, verb, dbug
::
|%
+$  state-0
  $:  %0
      retry-timers=(map target @dr)
  ==
::
+$  what-target  ?(%local-pages %annotations)
+$  target
  $:  what=what-target
      who=ship
      where=path
  ==
++  wire-to-target
  |=  =wire
  ^-  target
  ?>  ?=([what-target @ ^] wire)
  [i.wire (slav %p i.t.wire) t.t.wire]
++  target-to-wire
  |=  target
  ^-  wire
  [what (scot %p who) where]
::
+$  card  card:agent:gall
--
::
=|  state-0
=*  state  -
::
%-  agent:dbug
%+  verb  |
^-  agent:gall
=<
  |_  =bowl:gall
  +*  this  .
      do    ~(. +> bowl)
      def   ~(. (default-agent this %|) bowl)
  ::
  ++  on-init
    ^-  (quip card _this)
    :_  this
    [watch-groups:do]~
  ::
  ++  on-save  !>(state)
  ++  on-load
    |=  old=vase
    ^-  (quip card _this)
    [~ this(state !<(state-0 old))]
  ::
  ++  on-agent
    |=  [=wire =sign:agent:gall]
    ^-  (quip card _this)
    ?:  ?=([%groups ~] wire)
      =^  cards  state
        (take-groups-sign:do sign)
      [cards this]
    ?:  ?=([%links ?(%local-pages %annotations) @ ^] wire)
      =^  cards  state
        (take-link-sign:do (wire-to-target t.wire) sign)
      [cards this]
    ?:  ?=([%forward ^] wire)
      =^  cards  state
        (take-forward-sign:do t.wire sign)
      [cards this]
    ?:  ?=([%prod *] wire)
      ~|  [%weird-sign -.sign]
      ?>  ?=(%poke-ack -.sign)
      ?~  p.sign  [~ this]
      %-  (slog [leaf+"failed to prod" u.p.sign])
      [~ this]
    ~|  [dap.bowl %weird-wire wire]
    !!
  ::
  ++  on-poke
    |=  [=mark =vase]
    ?.  ?=(%link-listen-poke mark)
      (on-poke:def mark vase)
    =/  =path  !<(path vase)
    :_  this
    %+  weld
      (take-retry:do %local-pages src.bowl path)
    (take-retry:do %annotations src.bowl path)
  ::
  ++  on-arvo
    |=  [=wire =sign-arvo]
    ^-  (quip card _this)
    ?+  sign-arvo  (on-arvo:def wire sign-arvo)
        [%g %done *]
      ?~  error.sign-arvo  [~ this]
      =/  =tank  leaf+"{(trip dap.bowl)}'s message went wrong!"
      %-  (slog tank tang.u.error.sign-arvo)
      [~ this]
    ::
        [%b %wake *]
      ?>  ?=([%retry @ @ ^] wire)
      ?^  error.sign-arvo
        =/  =tank  leaf+"wake on {(spud wire)} went wrong!"
        %-  (slog tank u.error.sign-arvo)
        [~ this]
      :_  this
      (take-retry:do (wire-to-target t.wire))
    ==
  ::
  ++  on-peek   on-peek:def
  ++  on-watch  on-watch:def
  ++  on-leave  on-leave:def
  ++  on-fail   on-fail:def
  --
::
::
|_  =bowl:gall
::
::  groups subscription
::
++  watch-groups
  ^-  card
  [%pass /groups %agent [our.bowl %group-store] %watch /all]
::
++  take-groups-sign
  |=  =sign:agent:gall
  ^-  (quip card _state)
  ?-  -.sign
    %poke-ack   ~|([dap.bowl %unexpected-poke-ack /groups] !!)
    %kick       [[watch-groups]~ state]
  ::
      %watch-ack
    ?~  p.sign  [~ state]
    =/  =tank
      :-  %leaf
      "{(trip dap.bowl)} failed subscribe to groups. very wrong!"
    %-  (slog tank u.p.sign)
    [~ state]
  ::
      %fact
    =*  mark  p.cage.sign
    =*  vase  q.cage.sign
    ?+  mark  ~|([dap.bowl %unexpected-mark mark] !!)
      %group-initial  (handle-group-initial !<(groups:group-store vase))
      %group-update   (handle-group-update !<(group-update:group-store vase))
    ==
  ==
::
++  handle-group-initial
  |=  =groups:group-store
  ^-  (quip card _state)
  =|  cards=(list card)
  =/  groups=(list [=path =group:group-store])
    ~(tap by groups)
  |-
  ?~  groups  [cards state]
  =^  caz  state
    %-  handle-group-update
    [%add [group path]:i.groups]
  $(cards (weld cards caz), groups t.groups)
::
++  handle-group-update
  |=  upd=group-update:group-store
  ^-  (quip card _state)
  :_  state
  ?+  -.upd  ~
      ?(%path %add %remove)
    =/  whos=(list ship)  ~(tap in members.upd)
    |-  ^-  (list card)
    ?~  whos  ~
    ::  no need to subscribe to ourselves
    ::
    ?:  =(our.bowl i.whos)
      $(whos t.whos)
    ?:  ?=(%remove -.upd)
      %+  weld
        $(whos t.whos)
      (end-link-subscriptions i.whos pax.upd)
    :^    (start-link-subscription %local-pages i.whos pax.upd)
        (start-link-subscription %annotations i.whos pax.upd)
      (prod-other-listener i.whos pax.upd)
    $(whos t.whos)
  ==
::
::  link subscriptions
::
++  start-link-subscription
  |=  =target
  ^-  card
  :*  %pass
      [%links (target-to-wire target)]
      %agent
      [who.target %link-proxy-hook]
      %watch
      ?-  what.target
        %local-pages  [what where]:target
        %annotations  [what %$ where]:target
      ==
  ==
::
++  end-link-subscriptions
  |=  [who=ship where=path]
  ^-  (list card)
  |^  ~[(end %local-pages) (end %annotations)]
  ::
  ++  end
    |=  what=what-target
    :*  %pass
        [%links (target-to-wire what who where)]
        %agent
        [who %link-proxy-hook]
        %leave
        ~
    ==
  --
::
++  prod-other-listener
  |=  [who=ship where=path]
  ^-  card
  :*  %pass
      [%prod (scot %p who) where]
      %agent
      [who %link-listen-hook]
      %poke
      %link-listen-poke
      !>(where)
  ==
::
++  take-link-sign
  |=  [=target =sign:agent:gall]
  ^-  (quip card _state)
  ?-  -.sign
    %poke-ack   ~|([dap.bowl %unexpected-poke-ack /links target] !!)
    %kick       [[(start-link-subscription target)]~ state]
  ::
      %watch-ack
    ?~  p.sign
      =.  retry-timers  (~(del by retry-timers) target)
      [~ state]
    ::  our subscription request got rejected,
    ::  most likely because our group definition is out of sync with theirs.
    ::  set timer for retry.
    ::
    (start-retry target)
  ::
      %fact
    =*  mark  p.cage.sign
    =*  vase  q.cage.sign
    ?+  mark  ~|([dap.bowl %unexpected-mark mark] !!)
        %link-initial
      %-  handle-link-initial
      [who.target where.target !<(initial vase)]
    ::
        %link-update
      %-  handle-link-update
      [who.target where.target !<(update vase)]
    ==
  ==
::
++  start-retry
  |=  =target
  ^-  (quip card _state)
  =/  timer=@dr
    %+  min  ~h1
    %+  mul  2
    (~(gut by retry-timers) target ~s15)
  =.  retry-timers
    (~(put by retry-timers) target timer)
  :_  state
  :_  ~
  :*  %pass
      [%retry (target-to-wire target)]
      [%arvo %b %wait (add now.bowl timer)]
  ==
::
++  take-retry
  |=  =target
  ^-  (list card)
  ::  relevant: whether :who is still in group :where
  ::
  =;  relevant=?
    ?.  relevant  ~
    [(start-link-subscription target)]~
  ?:  %-  ~(has by wex.bowl)
      [[%links (target-to-wire target)] who.target %link-proxy-hook]
    |
  %.  who.target
  %~  has  in
  =-  (fall - *group:group-store)
  .^  (unit group:group-store)
    %gx
    (scot %p our.bowl)
    %group-store
    (scot %da now.bowl)
    (snoc where.target %noun)
  ==
::
++  do-link-action
  |=  [=wire =action]
  ^-  card
  :*  %pass
      wire
      %agent
      [our.bowl %link-store]
      %poke
      %link-action
      !>(action)
  ==
::
++  handle-link-initial
  |=  [who=ship where=path =initial]
  ^-  (quip card _state)
  ?>  =(src.bowl who)
  ?+  -.initial  ~|([dap.bowl %unexpected-initial -.initial] !!)
      %local-pages
    =/  =pages  (~(got by pages.initial) where)
    (handle-link-update who where [%local-pages where pages])
  ::
      %annotations
    =/  urls=(list [=url =notes])
      ~(tap by (~(got by notes.initial) where))
    =|  cards=(list card)
    |-  ^-  (quip card _state)
    ?~  urls  [cards state]
    =^  caz  state
      ^-  (quip card _state)
      =,  i.urls
      (handle-link-update who where [%annotations where url notes])
    $(urls t.urls, cards (weld cards caz))
  ==
::
++  handle-link-update
  |=  [who=ship where=path =update]
  ^-  (quip card _state)
  ?>  =(src.bowl who)
  :_  state
  ?+  -.update  ~|([dap.bowl %unexpected-update -.update] !!)
      %local-pages
    %+  turn  pages.update
    |=  =page
    %+  do-link-action
      [%forward %local-page (scot %p who) where]
    [%hear where who page]
  ::
      %annotations
    %+  turn  notes.update
    |=  =note
    ^-  card
    %+  do-link-action
      [%forward %annotation (scot %p who) where]
    [%read where url.update who note]
  ==
::
++  take-forward-sign
  |=  [=wire =sign:agent:gall]
  ^-  (quip card _state)
  ~|  [%unexpected-sign on=[%forward wire] -.sign]
  ?>  ?=(%poke-ack -.sign)
  ?~  p.sign  [~ state]
  =/  =tank
    :-  %leaf
    ;:  weld
      (trip dap.bowl)
      " failed to save submission from "
      (spud wire)
    ==
  %-  (slog tank u.p.sign)
  [~ state]
--
