/-  *rw-security
^?
|%
+$  synced  (map path ship)
+$  action
  $%  ::  %add-owned: make a chatroom accessible to foreign ships
      ::
      [%add-owned =path allow-history=?]
      ::  %add-synced: mirror a foreign chatroom to our chat-store
      ::
      [%add-synced =ship =path ask-history=?]
      ::  %remove: stop mirroring a foreign chatroom or allowing a local
      ::  chatroom to be mirrored
      ::
      [%remove =path]
  ==
::
+$  update  [%initial =synced]
--
