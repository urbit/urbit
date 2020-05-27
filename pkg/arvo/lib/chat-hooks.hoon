/-  *permission-store, *group-store, *metadata-store, *chat-store
^?
|%
++  chat-scry
  |=  [bol=bowl:gall pax=path]
  ^-  (unit mailbox)
  %:  scry
      (unit mailbox)
      bol
      %chat-store
      [%mailbox pax]
  ==
::  NOTE this assumes permission paths match group paths
::
++  is-permitted
  |=  [bol=bowl:gall who=ship chat=path]
  ^-  ?
  %+  lien  (groups-of-chat bol chat)
  |=  =group-path
  %:  scry
      ?
      bol
      %permission-store
      [%permitted (scot %p who) group-path]
  ==
::
++  chats-of-group
  |=  [bol=bowl:gall =group-path]
  ^-  (list path)
  ::  if metadata-store isn't running yet, we're still in the upgrade ota phase.
  ::  we can't get chats from the metadata-store, but can make assumptions
  ::  about group path shape, and the chat that would match it.
  ::TODO  remove me at some point.
  ::
  ?.  .^(? %gu (scot %p our.bol) %metadata-store (scot %da now.bol) ~)  ~
  %+  murn
    ^-  (list resource)
    =;  resources
      %~  tap  in
      %+  ~(gut by resources)
        group-path
      *(set resource)
    .^  (jug path resource)
      %gy
      (scot %p our.bol)
      %metadata-store
      (scot %da now.bol)
      /group-indices
    ==
  |=  resource
  ^-  (unit path)
  ?.  =(%chat app-name)  ~
  `app-path
::
++  groups-of-chat
  |=  [bol=bowl:gall chat=path]
  ^-  (list group-path)
  ::  if metadata-store isn't running yet, we're still in the upgrade ota phase.
  ::  we can't get groups from the metadata-store, but can make assumptions
  ::  about chat path shape, and the chat that would match it.
  ::  TODO  remove me at some point.
  ::
  ?.  .^(? %gu (scot %p our.bol) %metadata-store (scot %da now.bol) ~)  ~
  =;  resources
    %~  tap  in
    %+  ~(gut by resources)
      [%chat chat]
    *(set group-path)
  .^  (jug resource group-path)
    %gy
    (scot %p our.bol)
    %metadata-store
    (scot %da now.bol)
    /resource-indices
  ==
::
++  scry
  |*  [=mold bol=bowl:gall app=term =path]
  .^  mold
    %gx
    (scot %p our.bol)
    app
    (scot %da now.bol)
    (snoc `^path`path %noun)
  == 
--
