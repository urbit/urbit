::  link-store: store specific types
::
/-  *link
^?
|%
::
::  +action: local actions
::
+$  action
  $%  ::    user actions
      ::
      ::  %save: save page to path on our ship
      ::
      [%save =path title=@t =url]
      ::  %note: save a note for a url
      ::
      [%note =path =url udon=@t]
      ::  %seen: mark item as read (~ for all in path)
      ::
      [%seen =path url=(unit url)]
      ::    hook actions
      ::
      ::  %hear: hear about page at path on other ship
      ::
      [%hear =path submission]
      ::  %read: hear about note on url from ship
      ::
      [%read =path =url comment]
  ==
::
::  +initial: local result
::
+$  initial
  $%  [%local-pages pages=(map path pages)]
      [%submissions submissions=(map path submissions)]
      [%annotations notes=(per-path-url notes)]
      [%discussions comments=(per-path-url comments)]
  ==
::  +update: local updates
::
::NOTE  we include paths/urls to support the "subscribed to all" case
::
+$  update
  $%  [%local-pages =path =pages]
      [%submissions =path =submissions]
      [%annotations =path =url =notes]
      [%discussions =path =url =comments]
      [%observation =path urls=(set url)]
  ==
--
