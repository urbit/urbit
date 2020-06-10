/-  *post
|%
++  mop
  |*  [key=mold value=mold]
  |=  ord=$-([key key] ?)
  |=  a=*
  =/  b  ;;((tree [key=key val=value]) a)
  ?>  (check-balance:((ordered-map key value) ord) b)
  b
::
+$  graphs       (map resource graph)
+$  tag-queries  (jug term resource)
+$  action-logs  (map resource action-log)
+$  network
  $:  =graphs
      =tag-queries
      =action-logs
  ==
::
+$  action-log  ((mop time action) lth)
::
+$  graph  ((mop atom node) lth)
+$  internal-graph
  $~  [%empty ~]
  $%  [%graph p=graph]
      [%empty ~]
  ==
::
+$  node   [=post children=internal-graph]
::
+$  action
  $%  [%0 action-0]
  ==
::
+$  action-0
  $%  [%add-graph =resource =graph]
      [%remove-graph =resource]
    ::
      [%add-nodes =resource nodes=(map index node)]
      [%remove-nodes =resource indices=(set index)]
    ::
      [%add-signatures =uid =signatures]
      [%remove-signatures =uid =signatures]
    ::
      [%add-tag =term =resource]
      [%remove-tag =term =resource]
  ==
::
+$  update
  $%  [%0 update-0]
  ==
::
+$  update-0
  $%  [%keys =resources]
      action-0
  ==
--
