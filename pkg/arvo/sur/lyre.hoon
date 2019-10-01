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
::
+$  peer  [app=@tas pax=path]
::
+$  dom
  $~  [%empty ~]
  $%
  ::  basic elements
  ::
      [%empty ~]
      [%text sty=(list typography space) bod=@t]
      [%button bod=dom act=poke]
      [%image dat=@t]
  ::  form elements
  ::
      [%form app=@tas mark=@tas dat=(map @tas @t) bod=dom]
      [%text-input sty=(list typography) name=@tas]
      [%submit bod=dom]
  ::  layout elements
  ::
::      [%size w=@u h=@u bod=dom]
      [%padding t=@u b=@u l=@u r=@u bod=dom]
      [%horizontal bod=(list dom)]
      [%vertical bod=(list dom)]
      [%list sty=(list ?(flex layout typography bg-color border space)) bod=(list dom)]
      [%box sty=(list ?(flex layout typography bg-color border space)) bod=dom]
  ::  custom react
  ::
      [%include js=@t]
      [%component name=@t sub=(unit peer)]
  ==
::
::  styling
::
+$  size
  $%  [%per @u]
      [%pix @u]
  ==
::
++  color  @t
::
+$  typography
  $%  [%color color]
      [%font-family @t]
      [%font-size @u]
      [%font-weight @u]
  ==
::
+$  bg-color  [%bg-color color]
::
+$  layout
  $%  [%width size]
      [%height size]
  ==
::
+$  flex
  $%  [%axis ?(%row %col)]
      [%basis size]
      [%grow @u]
      [%shrink @u]
  ==
::
+$  border
  $%  [%border ?]
      [%ba ?]
      [%by ?]
      [%bx ?]
      [%bt ?]
      [%bb ?]
      [%bl ?]
      [%br ?]
  ==
::
+$  space
  $%  [%m @u]
      [%my @u]
      [%mx @u]
      [%mt @u]
      [%mb @u]
      [%ml @u]
      [%mr @u]
  ==
::
--
