|%
+$  group-path    path
+$  app-name      @tas
+$  app-path      path
+$  resource      [=app-name =app-path]
+$  associations  (map [group-path resource] metadata)
::
+$  metadata
  $:  title=@t
      description=@t
      color=@ux
      date-created=@da
      creator=@p
  ==
::
+$  action
  $%  [%add =group-path =resource =metadata]
      [%remove =group-path =resource]
  ==
::
+$  update
  $%  action
      [%associations =associations]
      [%update-metadata =group-path =resource =metadata]
  ==
--
