|%
+$  group-path  path
+$  app-name  @tas
+$  app-path  path
+$  associated  (map [group-path app-name app-path] metadata)
::
+$  metadata
  $:  title=@t
      description=@t
      color=@ux
  ==
::
+$  metadata-action
  $%  [%add =group-path =app-name =app-path =metadata]
      [%remove =group-path =app-name =app-path]
  ==
::
+$  metadata-update
  $%  metadata-action
      [%app-indices app-indices=associated]
      [%update-metadata =group-path =app-name =app-path =metadata]
  ==
--
