/-  *resource, graph-store, post, group-store, metadata-store
^? 
|%
::
+$  index
  $%  [%graph group=resource graph=resource module=@t description=@t]
      [%group group=resource description=@t]
  ==
::
+$  group-contents
  $~  [%add-members *resource ~]
  $%  $>(?(%add-members %remove-members) update:group-store)
      metadata-action:metadata-store
  ==
::
+$  notification
  [date=@da read=? =contents]
::
+$  contents
  $%  [%graph =(list post:post)]
      [%group =(list group-contents)]
  ==
::
+$  timebox
  (map index notification)
::
+$  notifications
  ((mop @da timebox) lth)
::
+$  action
  $%  [%add =index =notification]
      [%archive time=@da index]
      [%read time=@da index]
      [%unread time=@da index]
      [%set-dnd dnd=?]
      [%seen ~]
  ==
::
++  indexed-notification
  [index notification]
::  
+$  update
  $%  action
      [%more =(list update)]
      [%timebox time=@da archived=? =(list [index notification])]
      [%count count=@ud]
  ==
--
