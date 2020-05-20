/-  *identity
|%
+$  rolodex    (map path contacts)
+$  contacts    (map ship contact)
+$  avatar
  $%  [%octt content-type=@t octs=[p=@ud q=@t]]
      [%url url=@t]
  ==
::
+$  contact
  $:  nickname=@t
      email=@t
      phone=@t
      website=@t
      notes=@t
      color=@ux
      avatar=(unit avatar)
  ==
::
+$  edit-field
  $%  [%nickname nickname=@t]
      [%email email=@t]
      [%phone phone=@t]
      [%website website=@t]
      [%notes notes=@t]
      [%color color=@ux]
      [%avatar avatar=(unit avatar)]
  ==
::
+$  contact-action
  $%  [%create =path]
      [%delete =path]
      [%add =path =ship =contact]
      [%remove =path =ship]
      [%edit =path =ship =edit-field]
  ==
::
+$  contact-update
  $%  [%rolodex =rolodex]
      [%contacts =path =contacts]
      contact-action
  ==
--
