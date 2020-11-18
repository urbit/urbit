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
++  got-node
  |=  [res=resource =index:store]
  ^-  node:store
  =+  %+  scry-for  ,=update:store
      %+  weld
        /node/(scot %p entity.res)/[name.res]
      (turn index (cury scot %ud))
  ?>  ?=(%0 -.update)
  ?>  ?=(%add-nodes -.q.update)
  ?>  ?=(^ nodes.q.update)
  q.n.nodes.q.update
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
::
++  get-keys
  ^-  resources
  =+  %+  scry-for  ,=update:store
      /keys
  ?>  ?=(%0 -.update)
  ?>  ?=(%keys -.q.update)
  resources.q.update
--
