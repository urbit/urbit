|%
+$  group-path  path
+$  app-name  @tas
+$  app-path  path
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
+$  metadata-update  metadata-action
--
