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
::
+$  color  @ux
+$  metadata
  $:  title=cord
      description=cord
      =color
      date-created=time
      creator=ship
      module=term
  ==
::
+$  metadata-action
  $%  [%add group=resource resource=md-resource =metadata]
      [%remove group=resource resource=md-resource]
      [%update group=resource resource=md-resource =metadata]
      [%initial-group group=resource resources=(map md-resource metadata)]
  ==
::
+$  metadata-update
  $%  metadata-action
      [%associations =associations]
      [%updated-metadata group=resource resource=md-resource before=metadata =metadata]
  ==
--
