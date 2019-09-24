|%
+$  action
  $%  [%new-view nom=@tas dep=dependencies]
      [%change-deps nom=@tas dep=dependencies]
      [%switch-view nom=@tas]
      [%delete-view nom=@tas]
  ==
::
+$  dependencies
  $:  clay=(list [beam care:clay])
      gall=(list [app=@tas sub=path])
      raw=(unit json)
      ren=renderer
  ==
::
+$  renderer  @tas
::
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
