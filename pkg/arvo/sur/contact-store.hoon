|%
+$  rolodex  (map ship contact)
+$  contact
  $:  nickname=@t
      email=@t
      phone=@t
      website=@t
      color=@ux
      avatar=(unit @t)
  ==
::
+$  edit-field
  $%  [%nickname nickname=@t]
      [%email email=@t]
      [%phone phone=@t]
      [%website website=@t]
      [%color color=@ux]
      [%avatar avatar=(unit @t)]
  ==
::
+$  update
  $%  [%initial =rolodex]
      [%add =ship =contact]
      [%remove =ship]
      [%edit =ship =edit-field]
  ==
--
