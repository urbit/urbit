/-  *resource
/+  store=graph-store
|_  =bowl:gall
++  scry-for
  |*  [=mold =path]
  .^  mold
    %gx
    (scot %p our.bowl)
    %graph-store
    (scot %da now.bowl)
    (snoc `^path`path %noun)
  ==
::
++  get-graph
  |=  res=resource
  ^-  update:store
  %+  scry-for  update:store
  /graph/(scot %p entity.res)/[name.res]
::
++  get-update-log
  |=  rid=resource 
  ^-  update-log:store
  %+  scry-for  update-log:store
  /update-log/(scot %p entity.rid)/[name.rid]
::
++  peek-update-log
  |=  res=resource
  ^-  (unit time)
  (scry-for (unit time) /peek-update-log/(scot %p entity.res)/[name.res])
::
++  get-update-log-subset
  |=  [res=resource start=@da]
  ^-  update-log:store
  %+  scry-for  update-log:store
  /update-log-subset/(scot %p entity.res)/[name.res]/(scot %da start)/'~'
--
