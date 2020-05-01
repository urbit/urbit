::  metadata: helpers for getting data from the metadata-store
::
/-  *metadata-store, *metadata-hook
::
|_  [=bowl:gall =app-name]
++  card  card:agent:gall
++  is-running
  ^-  ?
  .^(? %gu (scot %p our.bowl) %metadata-store (scot %da now.bowl) ~)
++  app-paths-from-group
  |=  =group-path
  ^-  (list app-path)
  ?.  is-running  ~
  %+  murn
    %~  tap  in
    =-  (~(gut by -) group-path ~)
    .^  (jug ^group-path resource)
      %gy
      (scot %p our.bowl)
      %metadata-store
      (scot %da now.bowl)
      /group-indices
    ==
  |=  =resource
  ^-  (unit app-path)
  ?.  =(app-name.resource app-name)  ~
  `app-path.resource
::
++  resources-from-app
   %~  tap  in
   %.  app-name
   %~  get  ju
   .^  (jug app-name:md-store [group-path:md-store app-path:md-store])
     %gy
     (scot %p our.bowl)
     %metadata-store
     (scot %da now.bowl)
     /app-indices
   ==
::
++  groups-from-resource
  |=  =app-path
  ^-  (list group-path)
  =;  resources
    %~  tap  in
    %+  ~(gut by resources)
      [app-name app-path]
    *(set group-path)
  .^  (jug resource group-path)
    %gy
    (scot %p our.bowl)
    %metadata-store
    (scot %da now.bowl)
    /resource-indices
  ==
::
++  check-resource-permissions
  |=  [=ship =resource]
  ^-  ?
  %+  lien  (groups-from-resource resource)
  |=  =group-path
  .^  ?
    %gx
    (scot %p our.bowl)
    %permission-store
    (scot %da now.bowl)
    %permitted
    (scot %p ship)
    (snoc group-path %noun)
  ==
++  get-metadata
  |=  [=group-path =app-path]
  ?.  is-running  ~
  .^  (unit metadata)
    %gx
    (scot %p our.bowl)
    %metadata-store
    (scot %da now.bowl)
    %metadata
    (scot %t (spat group-path))
    app-name
    (scot %t (spat app-path))
  ==

++  poke
  |=  [=wire =^app-name =cage]
  ^-  card
  [%pass wire %agent [our.bowl app-name] %poke cage]
::
++  poke-store
  |=  [=wire =action]
  ^-  card
  %^  poke
      wire
    %metadata-store
  :-  %metadata-action
  !>(action)
::
++  poke-hook
  |=  [=wire =metadata-hook-action]
  ^-  card
  %^  poke
      wire
    %metadata-hook
  :-  %metadata-hook-action
  !>(metadata-hook-action)
::
++  add
  |=  [=group-path =app-path =metadata]
  ^-  card
  %+  poke-store
    ;:  weld
      /metadata/store/add
      group-path
      app-path
    ==
  [%add group-path [app-name app-path] metadata]
::
++  add-owned
  |=  =path
  ^-  card
  %+  poke-hook
    (weld /metadata/hook/add-owned path)
  [%add-owned path]
::
++  add-synced
  |=  [=ship =path]
  ^-  card
  %+  poke-hook
    (weld /metadata/hook/add-synced/[(scot %p ship)] path)
  [%add-synced ship path]
::
++  remove-store
  |=  [=app-path =group-path]
  ^-  card
  %+  poke-store
    :(weld /metadata/store/remove group-path app-path)
  [%remove group-path app-name app-path]
::
++  remove-hook
  |=  =group-path
  ^-  card
  %+  poke-hook
    (weld /metadata/hook/remove group-path)
  [%remove group-path]
++  remove
  |=  [=app-path =group-path]
  ^-  (list card)
  :~  (remove-hook group-path)
      (remove-store app-path group-path)
  ==
::
++  create
  |=  [=group-path =app-path =metadata]
  ^-  (list card)
  :~  (add group-path app-path metadata)
      (add-owned group-path)
  ==

--
