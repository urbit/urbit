::  hark: notifications [landscape]
::
/-  *resource, store=hark, post
/+  default-agent, dbug
=*  resource  resource:post
::
~%  %hark-top  ..is  ~
|%
+$  card  card:agent:gall
+$  versioned-state
  $%  state-0
  ==
::
+$  state-0
  $:  %0
      unreads=(map group=resource (map =app=resource unread-mop:store))
  ==
::
++  orm  ((ordered-map atom unread:store) gth)
--
::
=|  state-0
=*  state  -
::
%-  agent:dbug
^-  agent:gall
~%  %hark-agent  ..card  ~
|_  =bowl:gall
+*  this  .
    def   ~(. (default-agent this %|) bowl)
::
++  on-init  [~ this]
++  on-save  !>(state)
++  on-load
  |=  old=vase
  ^-  (quip card _this)
  [~ this(state !<(state-0 old))]
::
++  on-watch
  ~/  %hark-watch
  |=  =path
  ^-  (quip card _this)
  |^
  ?>  (team:title our.bowl src.bowl)
  =/  cards=(list card)
    ?+  path           (on-watch:def path)
        [%updates ~]
      %-  give
      :-  %keys
      %-  ~(run by unreads)
      |=  res=(map app=resource unread-mop:store)
      ~(key by res)
    ==
  [cards this]
  ::
  ++  give
    |=  =update-0:store
    ^-  (list card)
    [%give %fact ~ [%hark-update !>([%0 update-0])]]~
  --
::
++  on-poke
  ~/  %hark-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  |^
  ?>  (team:title our.bowl src.bowl)
  =^  cards  state
    ?+  mark           (on-poke:def mark vase)
        %hark-action   (hark-action !<(action:store vase))
    ==
  [cards this]
  ::
  ++  hark-action
    |=  =action:store
    ^-  (quip card _state)
    |^
    ?-  -.action
      %listen  (listen +.action)
      %ignore  (ignore +.action)
      %read    (read +.action)
    ==
    ::
    ++  listen
      |=  =app=resource
      ^-  (quip card _state)
      ::  set up subscriptions and create entry in maps
      [~ state]
    ::
    ++  ignore
      |=  =app=resource
      ^-  (quip card _state)
      ::  remove subscriptions and delete entry in maps
      [~ state]
    ::
    ++  read
      |=  =read-type:store
      ^-  (quip card _state)
      ?-  -.read-type
          %group
        ::  group-resource.read-type
        [~ state]
          %app
        ::  app-resource.read-type
        [~ state]
          %app-at-index
        ::  app-resource.read-type
        ::  index.read-type
        [~ state]
      ==
    ::
    ++  give
      |=  [paths=(list path) update=update-0:store]
      ^-  (list card)
      [%give %fact paths [%hark-update !>([%0 update])]]~
    --
  --
::
++  on-peek
  ~/  %hark-peek
  |=  =path
  ^-  (unit (unit cage))
  !!
::
++  on-arvo
  ~/  %hark-arvo
  |=  [=wire =sign-arvo]
  ^-  (quip card _this)
  !!
::
++  on-agent  on-agent:def  ::  TODO resubscribe on kick
++  on-leave  on-leave:def
++  on-fail   on-fail:def
--
