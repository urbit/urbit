|%
+$  tile  [name=@tas subscribe=path]
::
+$  tile-data  (map @tas [jon=json url=@t])
::
+$  action
  $%  [%add name=@tas subscribe=path url=@t]
      [%remove name=@tas subscribe=path]
  ==
--
