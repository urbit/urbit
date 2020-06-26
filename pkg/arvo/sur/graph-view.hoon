/-  *graph-store, *post
|%
+$  query-type
  $%  [%all ~]
      [%keys ~]
      [%tags ~]
      [%tag-queries ~]
      [%graph =resource]
      [%graph-subset =resource start=(unit atom) end=(unit atom)]
      [%node =resource =index]
      [%post =resource =index]
      [%node-children =resource =index]
      [%node-children-subset =resource start=(unit atom) end=(unit atom) =index]
  ==
::
+$  action
  $%  [%fetch connection=@ type=query-type]
  ==
::
+$  update
  $%  [%0 update-0]
  ==
::
+$  update-0
  $%  [%graph-subset =resource start=(unit atom) end=(unit atom) =graph]
      [%node =resource =index =node]
      [%post =resource =index =post]
      [%node-children =resource =index =graph]
      $:  %node-children-subset
          =resource
          start=(unit atom)
          end=(unit atom)
          =index
          =graph
      ==
  ==
--
