|%
+$  group-path    path
+$  app-name      @tas
+$  app-path      path
+$  md-resource   [=app-name =app-path]
+$  associations  (map [group-path md-resource] metadata)
::
+$  metadata
  $:  title=@t
      description=@t
      color=@ux
      date-created=@da
      creator=@p
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
