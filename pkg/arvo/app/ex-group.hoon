::  app/ex-group.hoon
/-  *groups, *sync-hook
|%
+$  move  [bone card]
+$  card
  $%  [%poke wire dock poke]
  ==
::
+$  poke
  $%  [%group-action group-action]
      [%sync-hook-action sync-hook-action]
  ==
+$  state
  $%  [%0 state-zero]
  ==
::
+$  state-zero
  $:  groups=(map path group)
  ==
--
::
|_  [bol=bowl:gall state]
::
++  this  .
::
++  prep
  |=  old=(unit state)
  ^-  (quip move _this)
  :_
  ?~  old
    this
  this(+<+ u.old)
  ?:  =(our.bol ~zod)
    =/  grp=group  (silt [~bus]~)
    :~  (group-poke [%bundle /ex])
        (group-poke [%add grp /ex])
        (sync-hook-poke %group-sync [%add-owned /ex])
    ==
  :~  (sync-hook-poke %group-sync [%add-synced ~zod /ex])
  ==
::
++  poke-noun
  |=  cmd=cord
  ^-  (quip move _this)
  =/  grp-nec=group  (silt [~nec]~)
  =/  grp-bus=group  (silt [~bus]~)
  ?:  =('add-ex-nec' cmd)
    :_  this
    :~  (group-poke [%add grp-nec /ex])
        (sync-hook-poke %group-sync [%add-owned /ex])
    ==
  ?:  =('sync-zod-ex' cmd)
     :_  this
     :~  (sync-hook-poke %group-sync [%add-synced ~zod /ex])
     ==
  ?:  =('rm-bus-ex' cmd)
    =/  grp=group  (silt [~bus]~)
    :_  this
    :~  (group-poke [%remove grp /ex])
    ==
  :_  this
  :~  (group-poke [%unbundle /ex])
  ==
::
++  group-poke
  |=  act=group-action
  ^-  move
  [ost.bol %poke /ex [our.bol %groups] [%group-action act]]
::
++  sync-hook-poke
  |=  [app=@tas act=sync-hook-action]
  ^-  move
  [ost.bol %poke /ex [our.bol app] [%sync-hook-action act]]
::
--

