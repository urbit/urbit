|%
+$  group-path    path
+$  app-name      term
+$  app-path      path
+$  md-resource   [=app-name =app-path]
+$  associations  (map [group-path md-resource] metadata)
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
  $%  [%add =group-path resource=md-resource =metadata]
      [%remove =group-path resource=md-resource]
  ==
::
+$  metadata-update
  $%  metadata-action
      [%associations =associations]
      [%update-metadata =group-path resource=md-resource =metadata]
  ==
--
