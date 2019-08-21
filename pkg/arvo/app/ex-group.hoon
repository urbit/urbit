::  app/ex-group.hoon
/-  *groups, *group-sync
|%
+$  move  [bone card]
+$  card
  $%  [%poke wire dock poke]
  ==
::
+$  poke
  $%  [%group-action group-action]
      [%group-sync-action group-sync-action]

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
        (group-sync-poke [%add-owned /ex])
    ==
  :~  (group-sync-poke [%add-synced ~zod /ex])
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
        (group-sync-poke [%add-owned /ex])
    ==
  ?:  =('sync-zod-ex' cmd)
     :_  this
     :~  (group-sync-poke [%add-synced ~zod /ex])
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
++  group-sync-poke
  |=  act=group-sync-action
  ^-  move
  [ost.bol %poke /ex [our.bol %group-sync] [%group-sync-action act]]
::
--

