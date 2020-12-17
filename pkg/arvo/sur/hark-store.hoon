/-  chat-store, graph-store, post, *resource, group-store, metadata-store
^? 
|%
++  state-zero
  |% 
  +$  state
    $:  %0
        notifications=notifications
        archive=notifications
        current-timebox=@da
        dnd=_|
    ==
  ++  orm
    ((ordered-map @da timebox) gth)
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
  ::
  +$  contents
    $%  [%graph =(list post:post)]
        [%group =(list group-contents)]
        [%chat =(list envelope:chat-store)]
    ==
  ::
  +$  notification
    [date=@da read=? =contents]
  --
::
++  state-one
  |%
  +$  state
    $:  %1
        unreads-each=(jug index index:graph-store)
        unreads-count=(map index @ud)
        last-seen=(map index @da)
        =notifications
        archive=notifications
        current-timebox=@da
        dnd=_|
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
  ((mop @da timebox) gth)
::
+$  action
  $%  [%add-note =index =notification]
      [%archive time=@da index]
    ::
      [%unread-count =stats-index =time]
      [%read-count =stats-index]
    ::
    ::
      [%unread-each =stats-index ref=index:graph-store time=@da]
      [%read-each =stats-index ref=index:graph-store]
    ::
      [%read-note time=@da index]
      [%unread-note time=@da index]
    ::
      [%seen-index time=@da =stats-index]
      [%remove-graph =resource]
    ::
      [%read-all ~]
      [%set-dnd dnd=?]
      [%seen ~]
  ==
::
++  stats-index
  $%  [%graph graph=resource =index:graph-store]
      [%group group=resource]
  ==
::
+$  indexed-notification
  [index notification]
::
+$  stats
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
      [%timebox time=@da archived=? =(list [index notification])]
      [%count count=@ud]
      [%clear =stats-index]
      [%unreads unreads=(list [stats-index stats])]
  ==
--
