::  chat-hook:
::  previously responsible for replication of chat data between ships
::
/-  *permission-store, *invite-store, *metadata-store,
    *permission-hook, *group-store, *permission-group-hook,  ::TMP  for upgrade
    hook=chat-hook,
    view=chat-view
/+  default-agent, verb, dbug, store=chat-store
~%  %chat-hook-top  ..is  ~
|%
+$  card  card:agent:gall
::
+$  versioned-state
  $%  state-0
      state-1
  ==
::
+$  state-1
  $:  %1
      loaded-cards=(list card)
      state-base
  ==
+$  state-0  [%0 state-base]
+$  state-base
  $:  =synced:hook
      invite-created=_|
      allow-history=(map path ?)
  ==
--
=|  state-1
=*  state  -
::
%-  agent:dbug
%+  verb  |
^-  agent:gall
|_  =bowl:gall
+*  this       .
    def        ~(. (default-agent this %|) bowl)
::
++  on-init   on-init:def
++  on-save   !>(state)
++  on-load
  |=  old-vase=vase
  ^-  (quip card _this)
  =/  old  !<(versioned-state old-vase)
  :_  ?.  ?=(%1 -.old)
        ~&  'very old chat-hook state; cannot migrate your chats'
        this
      this(state old)
  :*  :*  %pass
          /
          %agent
          [our.bowl %hood]
          %poke
          %drum-start
          !>([%home %chat-push-hook])
      ==
  ::
      :*  %pass
          /
          %agent
          [our.bowl %hood]
          %poke
          %drum-start
          !>([%home %chat-pull-hook])
      ==
  ::
      %+  turn  ~(tap by wex.bowl)
      |=  [[=wire =ship =term] [acked=? =path]]
      [%pass wire %agent [ship term] %leave ~]
  ==
::
++  on-peek
  |=  =path
  ^-  (unit (unit cage))
  |^
  ?+  path              (on-peek:def path)
      [%x %contents ~]  ``noun+!>(contents)
  ==
  ++  contents
    ^-  contents:hook
    [synced allow-history]
  --
::
++  on-poke   on-poke:def
++  on-watch  on-watch:def
++  on-agent  on-agent:def
++  on-leave  on-leave:def
++  on-arvo   on-arvo:def
++  on-fail   on-fail:def
--
