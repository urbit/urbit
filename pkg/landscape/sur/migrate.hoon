/-  met=metadata-store, gra=graph-store
/-  *group
|%
+$  flag  (pair ship term)
++  chat
  |%
  +$  import
    [writers=(set ship) =association:met =update-log:gra =graph:gra]
  +$  imports  (map flag import)
  --
++  groups
  |%
  +$  import   [=association:met =group]
  +$  imports  (map flag import)
  --
--
    
