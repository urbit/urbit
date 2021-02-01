/-  *resource
^?
|%
::
+$  action
  [%join =resource =ship]
::
+$  progress
  ?(%start %added final)
::
+$  final
  ?(%no-perms %strange %done)
::
+$  update
  $%  [%initial initial=(map resource progress)]
      [%progress =resource =progress]
  ==
--
