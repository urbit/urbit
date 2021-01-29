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
  $%  [%initial =resources]
      [%progress =resource =progress]
  ==
--
