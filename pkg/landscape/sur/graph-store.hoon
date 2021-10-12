/-  *post
|%
+$  graph         ((mop atom node) gth)
+$  marked-graph  [p=graph q=(unit mark)]
::
+$  maybe-post    (each post hash)
+$  node          [post=maybe-post children=internal-graph]
+$  graphs        (map resource marked-graph)
::
+$  tag-queries   (jug term uid)
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
      ~
  ==
::
+$  update  [p=time q=action]
::
+$  logged-update  [p=time q=logged-action]
  
::
+$  logged-action
  $%  [%add-graph =resource =graph mark=(unit mark) overwrite=?]
      [%add-nodes =resource nodes=(map index node)]
      [%remove-posts =resource indices=(set index)]
      [%add-signatures =uid =signatures]
      [%remove-signatures =uid =signatures]
  ==
::
+$  action
  $%  logged-action
      [%remove-graph =resource]
    ::
      [%add-tag =term =uid]
      [%remove-tag =term =uid]
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
::
+$  permissions  
  [admin=permission-level writer=permission-level reader=permission-level]
::
::  $permission-level:  levels of permissions in increasing order
::  
::    %no: May not add/remove node
::    %self: May only nodes beneath nodes that were added by
::      the same pilot, may remove nodes that the pilot 'owns'
::    %yes: May add a node or remove node
+$  permission-level
  ?(%no %self %yes)
::
::  %graph-store types version 2
::
++  two
  =<  [. post-one]
  =,  post-one
  |%
  +$  maybe-post    (each post hash)
  ++  orm      ((on atom node) gth)
  ++  orm-log  ((on time logged-update) gth)
  ::
  +$  graph         ((mop atom node) gth)
  +$  marked-graph  [p=graph q=(unit mark)]
  ::
  +$  node          [post=maybe-post children=internal-graph]
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
  +$  update  [p=time q=action]
  ::
  +$  logged-update  [p=time q=logged-action]
  ::
  +$  logged-action
    $%  [%add-graph =resource =graph mark=(unit mark) overwrite=?]
        [%add-nodes =resource nodes=(map index node)]
        [%remove-nodes =resource indices=(set index)]
        [%add-signatures =uid =signatures]
        [%remove-signatures =uid =signatures]
    ==
  ::
  +$  action
    $%  logged-action
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
::
::  %graph-store types version 1
::
++  one 
  =<  [. post-one]
  =,  post-one
  |%
  ++  orm      ((on atom node) gth)
  ++  orm-log  ((on time logged-update) gth)
  ::
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
  +$  update  [p=time q=action]
  ::
  +$  logged-update  [p=time q=logged-action]
  ::
  +$  logged-action
    $%  [%add-graph =resource =graph mark=(unit mark) overwrite=?]
        [%add-nodes =resource nodes=(map index node)]
        [%remove-nodes =resource indices=(set index)]
        [%add-signatures =uid =signatures]
        [%remove-signatures =uid =signatures]
    ==
  ::
  +$  action
    $%  logged-action
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
::
::  %graph-store types version 0
::
++  zero
  =<  [. post-zero]
  =,  post-zero
  |%
  ++  orm      ((ordered-map atom node) gth)
  ++  orm-log  ((ordered-map time logged-update) gth)
  ::
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
    $%  [%add-graph =resource =graph mark=(unit mark) overwrite=?]
        [%add-nodes =resource nodes=(map index node)]
        [%remove-nodes =resource indices=(set index)]
        [%add-signatures =uid =signatures]
        [%remove-signatures =uid =signatures]
    ==
  ::
  +$  update-0
    $%  logged-update-0
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
--
