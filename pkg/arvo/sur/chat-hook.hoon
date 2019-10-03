|%
+$  chat-security
  $?  $channel                                          ::  blacklist
      $village                                          ::  whitelist
      $journal                                          ::  pub r, whitelist w
      $mailbox                                          ::  our r, blacklist w
  ==
::
+$  chat-hook-action
  $%  ::  %add-owned: make a chatroom accessible to foreign ships
      ::  specified by the chat-security model
      ::
      [%add-owned =path security=chat-security]
      ::  %add-synced: mirror a foreign chatroom to our chat-store
      ::
      [%add-synced =ship =path]
      ::  %remove: stop mirroring a foreign chatroom or allowing a local
      ::  chatroom to be mirrored
      ::
      [%remove =path]
  ==
--

