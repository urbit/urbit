|%
+$  credentials
  $:  endpoint=@t
      access-key-id=@t
      secret-access-key=@t
  ==
::
+$  action
  $%  [%set-endpoint endpoint=@t]
      [%set-access-key-id access-key-id=@t]
      [%set-secret-access-key secret-access-key=@t]
  ==
::
+$  update
  $%  [%credentials =credentials]
      action
  ==
--
