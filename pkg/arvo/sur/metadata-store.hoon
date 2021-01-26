/-  *resource
^?
|%
::
+$  group-path    path
+$  app-name      term
+$  app-path      path
+$  md-resource   [=app-name =resource]
+$  association   [group=resource =metadata]
+$  associations  (map md-resource association)
+$  group-preview
  $:  group=resource
      channels=associations
      members=@ud
      channel-count=@ud
      =metadata
  ==
::
+$  color  @ux
+$  url    @t
::
::  $permissions: tag identifying variation in permissions
::
::    This will be passed to the graph-permissions mark
::    conversion to allow for custom permissions.
::
::    %reader-comments: Allow readers to comment, regardless
::      of whether they can write.
::    %$: No variation
::
+$  permissions  ?(%reader-comments %$)
+$  metadata
  $:  title=cord
      description=cord
      =color
      date-created=time
      creator=ship
      module=term
      picture=url
      preview=_|
      =permissions
  ==
::
+$  metadata-action
  $%  [%add group=resource resource=md-resource =metadata]
      [%remove group=resource resource=md-resource]
      [%initial-group group=resource =associations]
  ==
::
+$  metadata-hook-update
   $%  [%req-preview group=resource]
       [%preview group-preview]
   ==
::
+$  metadata-update
  $%  metadata-action
      [%associations =associations]
      [%updated-metadata group=resource resource=md-resource before=metadata =metadata]
  ==
--
