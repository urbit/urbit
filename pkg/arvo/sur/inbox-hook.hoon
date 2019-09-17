|%
+$  inbox-security
  $?  $channel                                          ::  blacklist
      $village                                          ::  whitelist
      $journal                                          ::  pub r, whitelist w
      $mailbox                                          ::  our r, blacklist w
  ==
::
+$  inbox-hook-action
  $%  [%add-owned =path security=inbox-security]
      [%add-synced =ship =path]
      [%remove =path]
  ==
--

