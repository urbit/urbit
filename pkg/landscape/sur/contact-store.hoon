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
      groups=(set resource)
      last-updated=@da
  ==
::
+$  edit-field
  $%  [%nickname nickname=@t]
      [%bio bio=@t]
      [%status status=@t]
      [%color color=@ux]
      [%avatar avatar=(unit @t)]
      [%add-group =resource]
      [%remove-group =resource]
      [%cover cover=(unit @t)]
  ==
::
+$  beings
  $%  [%ships ships=(set ship)]
      [%group =resource]
  ==
::
+$  update
  $%  [%initial =rolodex is-public=?]
      [%add =ship =contact]
      [%remove =ship]
      [%edit =ship =edit-field timestamp=@da]
      [%allow =beings]
      [%disallow =beings]
      [%set-public public=?]
  ==
--
