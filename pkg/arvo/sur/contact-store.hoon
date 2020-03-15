/-  *identity
|%
+$  rolodex-old  (map path contacts-old)
::
+$  contacts-old  (map ship contact-old)
::
+$  contact-old
  $:  nickname=@t
      email=@t
      phone=@t
      website=@t
      notes=@t
      color=@ux
      avatar=(unit avatar)
  ==
+$  rolodex  (map path contacts)
::
+$  contacts  (map ship contact)
::
+$  avatar  [content-type=@t octs=[p=@ud q=@t]]
::
+$  contact
  $:  nickname=@t
      email=@t
      phone=@t
      website=@t
      notes=@t
      foreground-color=$~(0xff.ffff @ux)
      background-color=@ux
      avatar=(unit avatar)
  ==
::
+$  edit-field
  $%  [%nickname nickname=@t]
      [%email email=@t]
      [%phone phone=@t]
      [%website website=@t]
      [%notes notes=@t]
      [%foreground-color foreground-color=@ux]
      [%background-color background-color=@ux]
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
