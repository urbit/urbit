|%
+$  credentials
  $:  api-key=@t
  ==
::
+$  action
  $%  [%set-api-key api-key=@t]
  ==
::
+$  update
  $%  [%credentials =credentials]
      action
  ==
--
