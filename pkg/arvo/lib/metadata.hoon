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
    .^  (jug ^group-path md-resource)
      %gy
      (scot %p our.bowl)
      %metadata-store
      (scot %da now.bowl)
      /group-indices
    ==
  |=  =md-resource
  ^-  (unit app-path)
  ?.  =(app-name.md-resource app-name)  ~
  `app-path.md-resource
::
++  groups-from-resource
  |=  =md-resource
  ^-  (list group-path)
  =;  resources
    %~  tap  in
    %+  ~(gut by resources)
      md-resource
    *(set group-path)
  .^  (jug ^md-resource group-path)
    %gy
    (scot %p our.bowl)
    %metadata-store
    (scot %da now.bowl)
    /resource-indices
  ==
::
++  check-resource-permissions
  |=  [=ship =md-resource]
  ^-  ?
  %+  lien  (groups-from-resource md-resource)
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
