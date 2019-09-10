|%
+$  inbox-security
  $?  $channel                                          ::  blacklist
      $village                                          ::  whitelist
      $journal                                          ::  pub r, whitelist w
      $mailbox                                          ::  our r, blacklist w
  ==
::
+$  permission-hook-action
  $%  [%add-owned =path security=inbox-security]
      [%remove-owned =path]
      [%add-synced =ship =path]
      [%remove-synced =ship =path]
  ==
--

