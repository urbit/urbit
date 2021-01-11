/-  *post
|%
+$  graph         ((mop atom node) gth)
+$  marked-graph  [p=graph q=(unit mark)]
::
+$  node          [=post children=internal-graph]
+$  graphs        (map resource marked-graph)
::
+$  tag-queries   (jug term resource)
::
+$  update-log    ((mop time logged-update) gth)
+$  update-logs   (map resource update-log)
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
      =update-logs
      archive=graphs
      validators=(set mark)
  ==
::
+$  update
  $%  [%0 p=time q=update-0]
  ==
::
+$  logged-update
  $%  [%0 p=time q=logged-update-0]
  ==
::
+$  logged-update-0
  $%  [%add-nodes =resource nodes=(map index node)]
      [%remove-nodes =resource indices=(set index)]
      [%add-signatures =uid =signatures]
      [%remove-signatures =uid =signatures]
  ==
::
+$  update-0
  $%  logged-update-0
      [%add-graph =resource =graph mark=(unit mark) overwrite=?]
      [%remove-graph =resource]
    ::
      [%add-tag =term =resource]
      [%remove-tag =term =resource]
    ::
      [%archive-graph =resource]
      [%unarchive-graph =resource]
      [%run-updates =resource =update-log]
    ::
    ::  NOTE: cannot be sent as pokes
    ::
      [%keys =resources]
      [%tags tags=(set term)]
      [%tag-queries =tag-queries]
  ==
--
