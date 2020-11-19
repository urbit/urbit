::  metadata: helpers for getting data from the metadata-store
::
/-  *metadata-store
/+  res=resource
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
++  peek-metadata
  |=  [app-name=term =group=resource:res =app=resource:res]
  ^-  (unit metadata)
  =/  group-cord=cord  (scot %t (spat (en-path:res group-resource)))
  =/  app-cord=cord    (scot %t (spat (en-path:res app-resource)))
  =/  our=cord  (scot %p our.bowl)
  =/  now=cord  (scot %da now.bowl)
  .^  (unit metadata)
    %gx  (scot %p our.bowl)  %metadata-store  (scot %da now.bowl)
    %metadata  group-cord  app-name  app-cord  /noun
  ==
::
++  group-from-app-resource
  |=  [app=term =app=resource:res]
  ^-  (unit resource:res)
  =/  app-path  (en-path:res app-resource)
  =/  group-paths  (groups-from-resource app app-path)
  ?~  group-paths
    ~
  `(de-path:res i.group-paths)
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
