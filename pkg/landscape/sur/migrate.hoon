/-  met=metadata-store, gra=graph-store
/-  *group
|%
+$  flag  (pair ship term)
++  graph
  |%
  +$  import
    [writers=(set ship) =association:met =update-log:gra =graph:gra]
  +$  imports  (map flag import)
  --
::
++  groups
  |%
  +$  import   [=association:met chans=(map flag =association:met) roles=(set flag) =group]
  +$  imports  (map flag import)
  --
++  club
  |%
  +$  import   [ships=(set ship) =association:met =graph:gra]
  +$  imports  (map flag import)
  --
--
    
