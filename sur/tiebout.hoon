/-  hall
=,  eyre
::
::
|%
+$  notification
  $:  token=@t
      topic=@t
      payload=(map @t json)
  ==
::
+$  action
  $%  [%token tok=@t]               ::  set device token
      [%king kng=@p]                ::  set ship to route notifications through
      [%baseurl bur=@t]             ::  set url to send notifications to
      [%notify not=notification]    ::  send notification
      [%add-circle nom=name:hall]   ::  send notifications for this circle
      [%del-circle nom=name:hall]   ::  stop sending for this circle
  ==
::
--
