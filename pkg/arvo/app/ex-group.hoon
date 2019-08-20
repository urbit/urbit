::  app/ex-group.hoon
/-  *groups
|%
+$  move  [bone card]
+$  card
  $%  [%poke wire dock [%group-action group-action]]
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
  ?~  old
    [~ this]
  [~ this(+<+ u.old)]
::
++  poke-noun
  |=  cmd=cord
  ^-  (quip move _this)
  ?:  =('make-ex-bus' cmd)
    =/  grp=group  (silt [~bus]~)
    :_  this
    :~  (group-poke [%bundle /ex])
        (group-poke [%add grp /ex])
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
--

