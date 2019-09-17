|%
+$  kind  ?(%black %white)
::
+$  permission
  $:  =kind
      who=(set ship)
  ==
::
+$  permission-map  (map path permission)
::
+$  permission-update
  $%  [%create =path =permission]
      [%delete =path]
      [%add =path who=(set ship)]
      [%remove =path who=(set ship)]
  ==
::
+$  permission-action
  $%  permission-update
      [%allow =path who=(set ship)]
      [%deny =path who=(set ship)]
  ==
::
--

