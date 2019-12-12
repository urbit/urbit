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
::  lists, reverse chronological / newest first
::
+$  pages        (list page)
+$  submissions  (list submission)
::
::  +action: local actions
::
+$  action
  $%  [%add =path title=@t =url]
      [%hear =path from=ship =page]  ::TODO  just =submission?
  ==
::  +update: local updates
::
::NOTE  we include paths explicitly to support the "subscribed to all" case
::
+$  update
  $%  [%local-pages =path =pages]
      [%submissions =path =submissions]
  ==
--
