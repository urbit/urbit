::  metadata: helpers for getting data from the metadata-store
::
/-  *metadata-store
::
|_  =bowl:gall
++  app-paths-from-group
  |=  [=app-name =group-path]
  ^-  (list app-path)
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
++  groups-from-resource
  |=  =resource
  ^-  (list group-path)
  =;  resources
    %~  tap  in
    %+  ~(gut by resources)
      resource
    *(set group-path)
  .^  (jug ^resource group-path)
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
--