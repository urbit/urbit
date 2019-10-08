|%
+$  chat-security
  $?  $channel                                          ::  black r, black w
      $village                                          ::  white r, white w
      $journal                                          ::  black r, white w
      $mailbox                                          ::  white r, black w
  ==
::
+$  chat-view-action
  $%  [%create =path security=chat-security read=(set ship) write=(set ship)]
      [%delete =path]
  ==
--
