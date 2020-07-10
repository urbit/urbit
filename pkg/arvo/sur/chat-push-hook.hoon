/+  *userspace
^?
|%
+$  action
  $%  ::  %add: make a local chatroom accessible to foreign ships
      ::
      [%add =rid allow-history=?]
      ::  %set-proxies: record the set of proxies for a local chatroom.
      ::  When a ship subscribes, she may instead be redirected to
      ::  subscibe to a proxy.
      ::
      [%set-proxies =rid proxies=(set ship)]
      ::  %remove: stop allowing a local chatroom to be mirrored
      ::
      [%remove =rid]
  ==
+$  update
  $%  ::  %redirect: the subscriber should not attempt to subscribe
      ::  to the resource on this ship, but should instead submit
      ::  a subscription request for the same resource on the
      ::  given other ship
      ::
      [%redirect proxy=ship =rid]
  ==
--
