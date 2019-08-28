|%
+$  action
  $%  [%new-session pax=(unit path)]
      [%delete-session id=@u]
      [%switch-session id=@u]
      [%set-path pax=path]
  ==
+$  poke  [app=@tas mark=@tas dat=json]
+$  dom
  $~  [%text '']
  $%  [%text bod=@t]
      [%button bod=dom act=poke]
  ==
--
