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
      [%add-synced =ship =path]
      [%remove =path]
  ==
--

