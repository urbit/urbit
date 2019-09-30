|%
+$  chat-security
  $?  $channel                                          ::  blacklist
      $village                                          ::  whitelist
      $journal                                          ::  pub r, whitelist w
      $mailbox                                          ::  our r, blacklist w
  ==
::
+$  chat-view-action
  $%  [%create =path security=chat-security read=(set ship) write=(set ship)]
      [%delete =path]
  ==
--
