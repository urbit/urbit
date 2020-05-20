|%
+$  contact-hook-action
  $%  ::  %add-owned: make a contacts list accessible to foreign ships
      ::  who are members of that list
      ::
      [%add-owned =path]
      ::  %add-synced: mirror a foreign contacts list to our contact-store
      ::
      [%add-synced =ship =path]
      ::  %remove: stop mirroring a foreign contacts list or stop allowing
      ::  a local contacts list to be mirrored
      ::
      [%remove =path]
  ==
::
+$  synced  (map path ship)
+$  contact-hook-update  [%initial =synced]
--
