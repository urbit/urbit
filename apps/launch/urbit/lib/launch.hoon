::
|%
::
+$  move  [bone card]
::
+$  card
  $%  [%http-response =http-event:http]
      [%connect wire binding:http-server term]
      [%peer wire dock path]
      [%diff %json json]
  ==
::
+$  tile  [name=@tas subscribe=path]
::
+$  tile-data  (map @tas [jon=json url=@t])
::
+$  action  [name=@tas subscribe=path url=@t]
::
+$  state
  $%  [%0 tiles=(set tile) data=tile-data path-to-tile=(map path @tas)]
  ==
::
--
::
