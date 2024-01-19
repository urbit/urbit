|%
+$  docs
  (trel meta node kids)
+$  meta
  $:  title=@t
      description=@t
  ==
+$  node
  $:  =poke
      =deps
      =scry
  ==
+$  deps  $~(~ (map term dep))
+$  rel-path  path  :: TODO fix
+$  spec  [=path hash=@uvH]
+$  dep   [=meta def=(unit path) =docs]
+$  poke  [sur=@t desc=@t]
+$  scry  ~
+$  dita  (each iota aura)
+$  pish  (list dita)
+$  kids
  $~  ~
  (map pish docs)
--
