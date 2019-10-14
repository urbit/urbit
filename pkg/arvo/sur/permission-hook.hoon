|%
+$  permission-hook-action
  $%  ::  %add-owned: make a permission set accessible to foreign ships
      ::  who are allowed by the permission set at the access path.
      ::
      [%add-owned owned=path access=path]
      ::  %add-synced: mirror a foreign permission set to our permission-store
      ::
      [%add-synced =ship =path]
      ::  %remove: stop mirroring a foreign permission set or allowing a local
      ::  permission set to be mirrored
      ::
      [%remove =path]
  ==
--
