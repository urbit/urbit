::  app/ex-chat.hoon
/-  *groups
|%
+$  move  [bone card]
::
+$  card
  $%  [%quit ~]
      [%poke wire dock poke]
      [%peer wire dock path]
  ==
::
+$  state
  $%  [%0 state-zero]
  ==
::
+$  chat  (list [=ship =cord])
::
+$  state-zero
  $:  inbox=(map path chat)
  ==
::
+$  poke
  $%  [%group-action group-action]
      [%noun group-sync-action]
  ==
::
+$  chat-action
  $%  [%create =path =group]
      [%invite =path =group]
      [%join =ship =path]
      [%leave =ship =path]
      [%message =ship =cord]
  ==
::
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
  |=  act=chat-action
  ?-  -.act
      %create
  ::
      %invite
  ::
      %join
  ::
      %leave
  ::
      %message
  ==
::
--

