/-  *resource
|%
::
+$  group-path    path
+$  app-name      term
+$  app-path      path
+$  md-resource   [=app-name =resource]
+$  associations  (map [group=resource md-resource] metadata)
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
  ==
::
+$  metadata-update
  $%  metadata-action
      [%associations =associations]
      [%update-metadata group=resource resource=md-resource =metadata]
  ==
--
