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
  $%  [%create =path =permission]              ::  create perm at path
      [%delete =path]                          ::  delete perm at path
      [%add =path who=(set ship)]              ::  add ships to perm path
      [%remove =path who=(set ship)]           ::  remove ships from perm path
  ==
::
+$  permission-action
  $%  permission-update
      [%allow =path who=(set ship)]            ::  if %black, remove
                                               ::  if %white, add
      [%deny =path who=(set ship)]             ::  if %black, add
                                               ::  if %white, remove
  ==
::
--

