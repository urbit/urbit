|%
+$  action
  $%  [%new-session pax=(unit path)]
      [%delete-session id=@u]
      [%switch-session id=@u]
      [%set-path pax=path]
  ==
+$  poke  [app=@tas mark=@tas dat=json]
+$  peer  [app=@tas pax=path]
+$  dom
  $~  [%empty ~]
  $%
  ::  basic elements
  ::
      [%empty ~]
      [%text bod=@t]
      [%button bod=dom act=poke]
  ::  form elements
  ::
      [%form app=@tas mark=@tas dat=(map @tas @t) bod=dom]
      [%text-input name=@tas]
      [%submit bod=dom]
  ::  layout elements
  ::
      [%size w=@u h=@u bod=dom]
      [%padding t=@u b=@u l=@u r=@u bod=dom]
      [%horizontal bod=(list dom)]
      [%vertical bod=(list dom)]
  ::  custom react
  ::
      [%include js=@t]
      [%component name=@t sub=(unit peer)]
  ==
--
