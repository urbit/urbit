::  hark-chat-hook: notifications for chat-store [landscape]
::
/-  store=hark-store, post, group-store, metadata-store, hook=hark-chat-hook
/+  resource, metadata, default-agent, dbug, chat-store, grpl=group
::
~%  %hark-chat-hook-top  ..is  ~
|%
+$  card  card:agent:gall
+$  versioned-state
  $%  state-0
  ==
::
+$  state-0
  $:  %0
      watching=(set path)
      mentions=_&
  ==
::
--
::
=|  state-0
=*  state  -
::
=>
  |_  =bowl:gall
  ::
  ++  give
    |=  [paths=(list path) =update:hook]
    ^-  (list card)
    [%give %fact paths hark-chat-hook-update+!>(update)]~
  ::
  ++  watch-chat
    ^-  card
    [%pass /chat %agent [our.bowl %chat-store] %watch /all]
  --
%-  agent:dbug
^-  agent:gall
~%  %hark-chat-hook-agent  ..card  ~
|_  =bowl:gall
+*  this  .
    ha    ~(. +> bowl)
    def   ~(. (default-agent this %|) bowl)
    met   ~(. metadata bowl)
    grp   ~(. grpl bowl)
::
++  on-init
  :_  this
  ~[watch-chat:ha]
::
++  on-save  !>(state)
++  on-load
  |=  old=vase
  ^-  (quip card _this)
  :_  this(state !<(state-0 old))
  ?:  (~(has by wex.bowl) [/chat our.bowl %chat-store])
    ~
  ~[watch-chat:ha]
::
++  on-watch  
  |=  =path
  ^-  (quip card _this)
  =^  cards  state
    ?+    path  (on-watch:def path)
      ::
        [%updates ~]  
      :_  state
      %+  give:ha  ~
      :*  %initial
          watching
      ==
    ==
  [cards this]
::
++  on-poke
  ~/  %hark-chat-hook-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  |^
  ?>  (team:title our.bowl src.bowl)
  =^  cards  state
    ?+  mark           (on-poke:def mark vase)
        %hark-chat-hook-action
      (hark-chat-hook-action !<(action:hook vase))
    ==
  [cards this]
  ::
  ++  hark-chat-hook-action
    |=  =action:hook
    ^-  (quip card _state)
    |^
    :-  (give:ha ~[/updates] action)
    ?-  -.action
      %listen  (listen +.action)
      %ignore  (ignore +.action)
      %set-mentions  (set-mentions +.action)
    ==
    ++  listen
      |=  chat=path
      ^+  state
      state(watching (~(put in watching) chat))
    ::
    ++  ignore
      |=  chat=path
      ^+  state
      state(watching (~(del in watching) chat))
    ::
    ++  set-mentions
      |=  ment=?
      ^+  state
      state(mentions ment)
    --
  --
::
++  on-agent
  ~/  %hark-chat-hook-agent
  |=  [=wire =sign:agent:gall]
  ^-  (quip card _this)
  |^
  ?+  -.sign  (on-agent:def wire sign)
      %kick
    :_  this
    ?.  ?=([%chat ~] wire)
      ~
    ~[watch-chat:ha]
  ::
      %fact
    ?.  ?=(%chat-update p.cage.sign)
      (on-agent:def wire sign)
    =^  cards  state
      (chat-update !<(update:chat-store q.cage.sign))
    [cards this]
  ==
  ::
  ++  chat-update
    |=  =update:chat-store
    ^-  (quip card _state)
    ?+   -.update   `state
      %initial  (process-initial +.update)
      %create   (process-new +.update)
      ::
        %message
      :_  state
      (process-envelope path.update envelope.update)
      ::
        %messages  
      :_  state
      %-  zing
      (turn envelopes.update (cury process-envelope path.update))
    ==
  ++  process-initial
    |=  =inbox:chat-store
    ^-  (quip card _state)
    =/  keys=(list path)
      ~(tap in ~(key by inbox))
    =|  cards=(list card)
    |-  
    ?~  keys
      [cards state]
    =*  path  i.keys
    =^  cs  state
      (process-new path)
    $(cards (weld cards cs), keys t.keys)
  ::
  ++  process-new
    |=  chat=path
    ^-  (quip card _state)
    =/  groups=(list path)
      (groups-from-resource:met %chat chat)
    ?~  groups
      `state
    ?:  (is-managed-path:grp i.groups)
      `state
    `state(watching (~(put in watching) chat))
  :: 
  ++  is-mention
    |=  =envelope:chat-store
    ?.  ?=(%text -.letter.envelope)  %.n
    ?&  mentions
        ?=  ^ 
        (find (scow %p our.bowl) (trip text.letter.envelope))
    ==
  ::
  ++  is-notification
    |=  [=path =envelope:chat-store]
    ?&  (~(has in watching) path)
        !=(author.envelope our.bowl)
    ==  
  ::
  ++  process-envelope
    |=  [=path =envelope:chat-store]
    ^-  (list card)
    =/  mention=?
      (is-mention envelope)
    ?.  ?|(mention (is-notification path envelope))
      ~
    =/  =index:store
      [%chat path mention]
    =/  =contents:store
      [%chat ~[envelope]]
    ~[(poke-store %add index when.envelope %.n contents)]
  ::
  ++  poke-store
    |=  =action:store
    ^-  card
    =-  [%pass /store %agent [our.bowl %hark-store] %poke -]
    hark-action+!>(action)
  --
::
++  on-peek  on-peek:def
::
++  on-leave  on-leave:def
++  on-arvo  on-arvo:def
++  on-fail   on-fail:def
--
