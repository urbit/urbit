/-  resource, graph-store
|%
+$  provider-action
  $%  [%add service=term notify=@t binding=@t auth-token=@t =whitelist]
      [%remove service=term]
      [%client-join service=term address=@t]
      [%client-leave service=term]
  ==
::
+$  client-action
  $%  [%connect-provider who=@p service=term address=@t]
      [%remove-provider who=@p service=term]
  ==
::
+$  notification
  $:  =resource:resource
      =index:graph-store
  ==
::
+$  whitelist
  $:  public=?
      kids=?
      users=(set ship)
      groups=(set resource:resource)
  ==
::
+$  update
  $%  [%notification =notification]
  ==
--
