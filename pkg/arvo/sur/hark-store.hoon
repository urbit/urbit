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
  $%  [%graph =(list content:post)]
      [%group =(list group-contents)]
  ==
::
+$  timebox
  (map index notification)
::
+$  notifications
  ((mop atom timebox) lth)
::
+$  action
  $%  [%add =index =notification]
      [%archive time=@da index]
      [%read time=@da index]
      [%unread time=@da index]
      [%seen ~]
  ==
::  
+$  update
  $%  action
  ==
--
