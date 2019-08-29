|%
+$  action
  $%  [%new-session pax=(unit path)]
      [%delete-session id=@u]
      [%switch-session id=@u]
      [%set-path pax=path]
  ==
+$  poke  [app=@tas mark=@tas dat=json]
+$  form-dom
  $^  [hed=form-dom tal=form-dom]
  $%  [%text-input name=@tas]
      [%submit bod=dom]
  ==
+$  dom
  $~  [%text '']
  $^  [hed=dom tal=dom]
  $%  [%text bod=@t]
      [%button bod=dom act=poke]
      [%form app=@tas mark=@tas bod=form-dom]
  ==
--
