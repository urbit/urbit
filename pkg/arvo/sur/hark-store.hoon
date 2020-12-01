/-  chat-store, graph-store, post, *resource, group-store, metadata-store
^? 
|%
++  state-zero
  |% 
  +$  state
    $:  %0
        notifications=notifications
        archive=notifications
        last-seen=@da
        dnd=_|
    ==
  ::
  +$  notifications
    ((mop @da timebox) gth)
  ::
  +$  timebox
    (map index notification)
  ::
  +$  index
    $%  [%graph group=resource graph=resource module=@t description=@t]
        [%group group=resource description=@t]
        [%chat chat=path mention=?]
    ==
  --
::
+$  index
  $%  $:  %graph
          group=resource
          graph=resource
          module=@t
          description=@t
          =index:graph-store
      ==
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
  $%  [%add-note =index =notification]
      [%archive time=@da index]
    ::
      [%unread-count =index =time]
      [%read-count =index]
    ::
      [%unread-each =index ref=index:graph-store time=@da]
      [%read-each index ref=index:graph-store]
    ::
      [%read-note time=@da index]
      [%unread-note time=@da index]
    ::
      [%read-all ~]
      [%set-dnd dnd=?]
      [%seen ~]
  ==
::
+$  indexed-notification
  [index notification]
::
+$  index-stats
  [notifications=@ud =unreads last-seen=@da]
::
+$  unreads
  $%  [%count num=@ud]
      [%each indices=(set index:graph-store)]
  ==
::  
+$  update
  $%  action
      [%more more=(list update)]
      [%added time=@da =index =notification]
      [%read-index =index]
      [%read time=@da =index]
      [%timebox time=@da archived=? =(list [index notification])]
      [%count count=@ud]
      [%unreads unreads=(list [index index-stats])]
  ==
--
