::  metadata: helpers for getting data from the metadata-store
::
/-  *metadata-store
/+  res=resource
::
|_  =bowl:gall
++  app-paths-from-group
  |=  [=app-name group=resource]
  ^-  (list resource)
  %+  murn
    %~  tap  in
    =-  (~(gut by -) group ~)
    .^  (jug resource md-resource)
      %gy
      (scot %p our.bowl)
      %metadata-store
      (scot %da now.bowl)
      /group-indices
    ==
  |=  =md-resource
  ^-  (unit resource)
  ?.  =(app-name.md-resource app-name)  ~
  `resource.md-resource
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
  |=  =md-resource
  ^-  (unit resource:res)
  =/  groups  (groups-from-resource md-resource)
  ?~  groups  ~
  `i.groups
::
++  groups-from-resource
  |=  =md-resource
  ^-  (list resource)
  =;  resources
    %~  tap  in
    %+  ~(gut by resources)
      md-resource
    *(set resource)
  .^  (jug ^md-resource resource)
    %gy
    (scot %p our.bowl)
    %metadata-store
    (scot %da now.bowl)
    /resource-indices
  ==
--
