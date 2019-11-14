/-  *rw-security
|%
+$  chat-hook-action
  $%  ::  %add-owned: make a chatroom accessible to foreign ships
      ::  specified by the rw-security model
      ::
      [%add-owned =path security=rw-security allow-history=?]
      ::  %add-synced: mirror a foreign chatroom to our chat-store
      ::
      [%add-synced =ship =path ask-history=?]
      ::  %remove: stop mirroring a foreign chatroom or allowing a local
      ::  chatroom to be mirrored
      ::
      [%remove =path]
  ==
--
