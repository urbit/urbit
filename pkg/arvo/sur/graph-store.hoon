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
+$  graph        ((mop atom node) gth)
+$  node         [=post children=internal-graph]
+$  graphs       (map resource graph)
+$  tag-queries  (jug term resource)
+$  action-log   ((mop time update) lth)
+$  action-logs  (map resource action-log)
::
+$  internal-graph
  $~  [%empty ~]
  $%  [%graph p=graph]
      [%empty ~]
  ==
::
+$  network
  $:  =graphs
      =tag-queries
      =action-logs
      archive=graphs
  ==
::
+$  update
  $%  [%0 update-0]
  ==
::
+$  update-0
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
    ::
      [%archive-graph =resource]
      [%unarchive-graph =resource]
    ::
    ::  NOTE: cannot be sent as pokes
    ::
      [%keys =resources]
      [%tags tags=(set term)]
      [%tag-queries =tag-queries]
  ==
--
