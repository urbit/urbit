|%
+$  permission
  $:  kind=?(%black %white)
      who=(set ship)
  ==
::
+$  permission-map  (map path permission)
::
+$  affiliation-map  (map ship (set path))
::
+$  permission-diff
  $%  [%create =permission]
      [%delete ~]
      [%add who=(set ship)]
      [%remove who=(set ship)]
  ==
::
::TODO  since we only operate on one permission at once, these can be =path
::      but we want to keep an affordance for showing current state...
+$  affiliation-diff
  $%  [%add where=(set path)]
      [%remove where=(set path)]
  ==
::
+$  permission-action
  $:  =path
    ::
      $=  what
      $%  permission-diff
          [%allow who=(set ship)]
          [%deny who=(set ship)]
      ==
  ==
::
--

