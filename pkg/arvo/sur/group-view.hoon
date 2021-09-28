/-  *resource, *group
^?
|%
::
+$  request
  $:  hidden=?
      started=time
      =ship
      =progress
  ==
::
+$  action
  $%  ::  host side
      [%create name=term =policy title=@t description=@t]
      [%remove =resource]
      ::  client side
      [%join =resource =ship]
      [%leave =resource]
      ::
      [%invite =resource ships=(set ship) description=@t]
      ::  pending ops
      [%hide =resource]
  ==

::
+$  progress
  ?(%start %added final)
::
+$  final
  ?(%no-perms %strange %done)
::
+$  update
  $%  [%initial initial=(map resource request)]
      [%started =resource =request]
      [%progress =resource =progress]
      [%hide =resource]
  ==
--
