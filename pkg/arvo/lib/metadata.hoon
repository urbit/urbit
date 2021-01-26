::  metadata: helpers for getting data from the metadata-store
::
/-  *metadata-store
/+  resource
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
++  app-metadata-for-group
  |=  [group=resource =app-name]
  =/  =associations
    (metadata-for-group group)
  %-  ~(gas by *^associations)
  %+  skim  ~(tap by associations)
  |=([=md-resource association] =(app-name app-name.md-resource))
::
++  metadata-for-group
  |=  group=resource
  .^  associations
    %gx  (scot %p our.bowl)  %metadata-store  (scot %da now.bowl)
    %group  (snoc (en-path:resource group) %noun)
  ==
::
++  md-resources-from-group
  |=  group=resource
  =-  (~(get ju -) group)
  .^  (jug resource md-resource)
    %gy
    (scot %p our.bowl)
    %metadata-store
    (scot %da now.bowl)
    /group-indices
  ==
::
++  peek-association
  |=  [app-name=term rid=resource]
  .^  (unit association)
    %gx  (scot %p our.bowl)  %metadata-store  (scot %da now.bowl)
    %metadata  app-name  (snoc (en-path:resource rid) %noun)
  ==
::
++  peek-metadata
  |=  =md-resource
  %+  bind  (peek-association md-resource)
  |=(association metadata)
::
++  peek-group
  |=  =md-resource
  ^-  (unit resource)
  %+  bind  (peek-association md-resource)
  |=(association group)
--
