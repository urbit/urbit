::  link: social bookmarking
::
::    link operates on the core structure of "pages", which are URLs saved at a
::    specific time with a specific title.
::    submissions, then, are pages received from a specific ship.
::
|%
::  primitives
::
+$  url   @t
+$  site  @t  ::  domain, host, etc.
::  +page: a saved URL with timestamp and custom title
::
+$  page
  $:  title=@t
      =url
      =time
  ==
::  +submission: a page saved by a ship
::
+$  submission
  $:  =ship
      page
  ==
::  +note: a comment on some url
::
+$  note
  $:  =time
      udon=@t
  ==
::  +comment: a comment by a ship on some url
::
+$  comment
  $:  =ship
      note
  ==
::  lists, reverse chronological / newest first
::
+$  pages        (list page)
+$  submissions  (list submission)
+$  notes        (list note)
+$  comments     (list comment)
::
::  state builder
::
++  per-path-url
  |$  [value]
  (map path (map url value))
--
