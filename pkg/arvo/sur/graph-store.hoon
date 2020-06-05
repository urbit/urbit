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
+$  network
  $:  graphs=(map resource graph)
      tags=(set term)
      tag-queries=(map term resources)
  ==
::
+$  graph  ((mop atom node) lth)
+$  internal-graph
  $~  [%empty ~]
  $%  ::
      ::  a graph and timestamp of when it was last modified
      [%graph p=graph q=time]
      [%empty ~]
      [%empty-at-time p=time]
  ==
::
+$  node   [=post children=internal-graph]
+$  action
  $%  [%add-graph =resource =graph]
      [%remove-graph =resource]
    ::
      [%add-nodes nodes=(map resource (map index node))]
      [%remove-nodes nodes=(jug resource index)]
    ::
      [%add-signatures =uid =signatures]
      [%remove-signatures =uid =signatures]
    ::
      [%add-tag =term =resources]
      [%remove-tag =term =resources]
  ==
::
+$  update
  $%  [%keys =resources]
      [%initial =network]
      action
  ==
--
