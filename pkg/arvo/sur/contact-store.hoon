/-  *identity
|%
+$  rolodex-0  (map path contacts-0)
+$  rolodex    (map path contacts)
::
+$  contacts-0  (map ship contact-0)
+$  contacts    (map ship contact)
::
+$  avatar-0  [content-type=@t octs=[p=@ud q=@t]]
+$  avatar
  $%  [%octt [file-extension=@t octs=[p=@ud q=@t]]]
      [%url url=@t]
  ==
::
+$  contact-0
  $:  nickname=@t
      email=@t
      phone=@t
      website=@t
      notes=@t
      color=@ux
      avatar=(unit avatar-0)
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
