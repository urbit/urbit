/+  *userspace
^?
|%
+$  action
  $%  ::  %add: subscribe to a foreign chatroom, mirroring its contents into
      ::  our store
      ::
      [%add =ship =rid ask-history=?]
      ::  %remove: stop subscribing to a foreign chatroom
      ::
      [%remove =rid]
  ==
::
+$  update
  $%  ::  %tracking: the foreign chats we are subscribed to are...
      ::
      [%tracking tracking=(map rid ship)]
  ==
--
