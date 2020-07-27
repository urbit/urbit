::  group-hook: allow syncing group data from foreign paths to local paths
::
/-  *group, hook=group-hook, *invite-store
/+  default-agent, verb, dbug, store=group-store, grpl=group, pull-hook, push-hook, resource
~%  %group-hook-top  ..is  ~
|%
+$  card  card:agent:gall
::
++  versioned-state
  $%  state-zero
      state-one
  ==
::
::
+$  state-zero
  $:  %0
      synced=(map path ship)
  ==
::
+$  state-one
  $:  %1
      ~
  ==
::
--
::
=|  state-one
=*  state  -
::
%-  agent:dbug
%+  verb  |
^-  agent:gall
|_  =bowl:gall
+*  this        .
    group-core  +>
    gc          ~(. group-core bowl)
    def         ~(. (default-agent this %|) bowl)
::
++  on-init  on-init:def
  ::  ^-  (quip card _this)
  ::  :_  this
  ::  ~[watch-store:gc]
++  on-save  !>(state)
++  on-load
  |=  =vase
  ^-  (quip card _this)
  =/  old  !<(versioned-state vase)
  ?-  -.old
      %1  [~ this(state old)]
      %0
    :_  this(state *state-one)
    |^
    %+  murn
      ~(tap by synced.old)
    |=  [=path host=ship]
    ^-  (unit card)
    ?>  ?=([@ @ *] path)
    :: ignore duplicate publish groups
    ?:  =(4 (lent path))
      ~&  "ignoring: {<path>}"
      ~ 
    =/  pax=^path
      ?:  =('~' i.path)
        t.path
      path
    =/  rid=resource
      ?>  ?=([@ @ *] pax)
      =/  ship
        (slav %p i.pax)
      [ship i.t.pax]
    ?:  =(our.bowl host)
      `(add-push rid)
    `(add-pull rid host)
    ::
    ++  poke-our
      |=  [app=term =cage]
      ^-  card
      [%pass / %agent [our.bowl app] %poke cage]
    ++  add-pull
      |=  [rid=resource host=ship]
      ^-  card
      %+  poke-our
        %group-pull-hook
      :-  %pull-hook-action
      !>  ^-  action:pull-hook
      [%add host rid]
    ::
    ++  add-push
      |=  rid=resource
      ^-  card
      %+  poke-our
        %group-push-hook
      :-  %push-hook-action
      !>  ^-  action:push-hook
      [%add rid]
    --

  ==

::
++  on-poke  on-poke:def
::
++  on-agent  on-agent:def
::
++  on-watch  on-watch:def
::
++  on-leave  on-leave:def

++  on-peek   on-peek:def
++  on-arvo   on-arvo:def
++  on-fail   on-fail:def
--
