|%
+$  tiles          (map term tile)
+$  tile-ordering  (list term)
::
+$  tile
  $:  type=tile-type
      is-shown=?
  ==
::
+$  tile-type
  $%  [%basic title=cord icon-url=cord linked-url=cord]
      [%custom ~]
  ==
::
+$  action
  $%  [%add name=term =tile]
      [%remove name=term]
      [%change-order =tile-ordering]
      [%change-first-time first-time=?]
      [%change-is-shown name=term is-shown=?]
  ==
::
+$  update
  $%  [%initial =tiles =tile-ordering first-time=?]
      [%keys keys=(set term)]
      action
  ==
--
