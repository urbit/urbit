/-  *resource, graph-store, post, group-store, metadata-store, chat-store
^? 
|%
+$  index
  $%  [%graph group=resource graph=resource module=@t description=@t]
      [%group group=resource description=@t]
      [%chat chat=path mention=?]
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
      [%chat =(list envelope:chat-store)]
  ==
::
+$  timebox
  (map index notification)
::
+$  notifications
  ((mop @da timebox) gth)
::
+$  action
  $%  [%add =index =notification]
      [%archive time=@da index]
      [%read time=@da index]
      [%read-index index]
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
      [%more more=(list update)]
      [%added time=@da =index =notification]
      [%timebox time=@da archived=? =(list [index notification])]
      [%count count=@ud]
      [%unreads unreads=(list [index @ud])]
  ==
--
