::  metadata-hook [landscape]:
::
::  allow syncing foreign metadata
::
::  watch paths:
::  /group/%group-path                      all updates related to this group
::
/-  *metadata-store, *metadata-hook
/+  default-agent, dbug, verb, grpl=group, *migrate, resource
~%  %metadata-hook-top  ..part  ~
|%
+$  card  card:agent:gall
+$  versioned-state
  $%  state-zero
      state-one
      state-two
  ==
::
+$  state-zero
  $:  %0
      synced=(map path ship)
  ==
+$   state-one
  $:  %1
      synced=(map path ship)
  ==
+$   state-two
  [%2 ~]
--
=|  state-two
=*  state  -
%-  agent:dbug
%+  verb  |
^-  agent:gall
|_  =bowl:gall
+*  this  .
    def   ~(. (default-agent this %|) bowl)
++  on-init   on-init:def
++  on-save   !>(state)
++  on-load
  |=  =vase
  =/  m-old=(unit versioned-state)
     (mole |.(!<(versioned-state vase)))
  ?~  m-old  `this
  =*  old  u.m-old
  |^
  ?:  ?=(%2 -.old)
    `this
  :_  this
  %+  murn
    ~(tap by synced.old)
  |=  [group=path =ship]
  %+  bind
    (de-path-soft:resource group)
  |=  rid=resource
  ?:  =(our.bowl ship)
    (push-metadata rid)
  (pull-metadata rid ship)
  ::
  ++  poke-our
    |=  [app=term =cage]
    ^-  card
    [%pass / %agent [our.bowl app] %poke cage]
  ::
  ++  push-metadata
    |=  rid=resource
    ^-  card
    (poke-our %metadata-push-hook push-hook-action+!>([%add rid]))
  ::
  ++  pull-metadata
    |=  [rid=resource =ship]
    ^-  card
    (poke-our %metadata-pull-hook pull-hook-action+!>([%add ship rid]))
  --
::
++  on-poke   on-poke:def
++  on-watch  on-watch:def
++  on-peek   on-peek:def
++  on-leave  on-leave:def
++  on-agent  on-agent:def
++  on-arvo   on-arvo:def
++  on-fail   on-fail:def
--
