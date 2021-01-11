/-  *resource
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
+$  beings
  $%  [%ships ships=(set ship)]
      [%group =resource]
  ==
::
+$  update
  $%  [%initial =rolodex]
      [%add =ship =contact]
      [%remove =ship]
      [%edit =ship =edit-field]
      [%allow =beings]
      [%disallow =beings]
  ==
--
