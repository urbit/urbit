/-  *resource
|%
+$  rolodex  (map ship contact)
+$  contact
  $:  nickname=@t
      bio=@t
      status=@t
      color=@ux
      avatar=(unit @t)
      cover=(unit @t)
      last-updated=@da
  ==
::
+$  edit-field
  $%  [%nickname nickname=@t]
      [%bio bio=@t]
      [%status status=@t]
      [%color color=@ux]
      [%avatar avatar=(unit @t)]
      [%cover cover=(unit @t)]
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
