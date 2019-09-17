|%
+$  chat-security
  $?  $channel                                          ::  blacklist
      $village                                          ::  whitelist
      $journal                                          ::  pub r, whitelist w
      $mailbox                                          ::  our r, blacklist w
  ==
::
+$  chat-hook-action
  $%  [%add-owned =path security=chat-security]
      [%add-synced =ship =path]
      [%remove =path]
  ==
--

