/-  chat-store, graph-store, post, *resource, group-store, metadata-store
^? 
|%
::
+$  old-state
  [?(%0 %1 %2 %3 %4 %5 %6) *]
::
+$  state-7
  [%7 base-state]
::
+$  versioned-state
  $%  old-state
      state-7
  ==
+$  unread-stats
  [indices=(set index:graph-store) last=@da]
::
+$  base-state
  $:  unreads-each=(jug stats-index index:graph-store)
      unreads-count=(map stats-index @ud)
      timeboxes=(map stats-index @da)
      unread-notes=timebox
      last-seen=(map stats-index @da)
      =notifications
      archive=notifications
      current-timebox=@da
      dnd=_|
  ==
::
+$  index
  $%  $:  %graph
          graph=resource
          mark=(unit mark)
          description=@t
          =index:graph-store
      ==
      [%group group=resource description=@t]
  ==
::
+$  group-contents
  $~  [%add-members *resource ~]
  $>(?(%add-members %remove-members) update:group-store)
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
    ::  if .time is ~, then archiving unread notification
    ::  else, archiving read notification
      [%archive time=(unit @da) =index]
    ::
      [%unread-count =stats-index =time]
      [%read-count =stats-index]
    ::
      [%unread-each =stats-index ref=index:graph-store time=@da]
      [%read-each =stats-index ref=index:graph-store]
    ::
      [%read-note =index]
    ::
      [%seen-index time=@da =stats-index]
    ::
      [%read-graph =resource]
      [%read-group =resource]
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
  [=unreads last-seen=@da]
::
+$  unreads
  $%  [%count num=@ud]
      [%each indices=(set index:graph-store)]
  ==
::  
+$  update
  $%  action
      [%more more=(list update)]
      [%added =index =notification]
      [%note-read =time =index]
      [%timebox time=(unit @da) =(list [index notification])]
      [%count count=@ud]
      [%clear =stats-index]
      [%unreads unreads=(list [stats-index stats])]
  ==
::  historical
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
    $%  [%graph graph=resource module=@t description=@t]
        [%group group=resource description=@t]
        [%chat chat=path mention=?]
    ==
  ::
  +$  group-contents
    $~  [%add-members *resource ~]
    $%  [%add *]
        [%remove *]  :: old metadata actions
        $>(?(%add-members %remove-members) update:group-store)
    ==
  ::
  +$  contents
    $%  [%graph =(list post:post-zero:post)]
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
        =notifications:state-two
        archive=notifications:state-two
        current-timebox=@da
        dnd=_|
    ==
  --
++  state-two
  =<  state
  |% 
  +$  state
    $:  unreads-each=(jug stats-index index:graph-store)
        unreads-count=(map stats-index @ud)
        last-seen=(map stats-index @da)
        =notifications
        archive=notifications
        current-timebox=@da
        dnd=_|
    ==
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
  ++  orm
    ((ordered-map @da timebox) gth)
  ::
  +$  notification
    [date=@da read=? =contents]
  ::
  +$  contents
    $%  [%graph =(list post:post-zero:post)]
        [%group =(list group-contents)]
    ==
  ::
  +$  group-contents
    group-contents:state-zero
  ::
  +$  timebox
    (map index notification)
  ::
  +$  notifications
    ((mop @da timebox) gth)
  ::
  --
::
++  state-three
  =<  state
  |% 
  +$  state
    $:  unreads-each=(jug stats-index index:graph-store)
        unreads-count=(map stats-index @ud)
        last-seen=(map stats-index @da)
        =notifications
        archive=notifications
        current-timebox=@da
        dnd=_|
    ==
  ::
  ++  orm
    ((ordered-map @da timebox) gth)
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
  +$  notification
    [date=@da read=? =contents]
  ::
  +$  contents
    $%  [%graph =(list post:post-zero:post)]
        [%group =(list group-contents)]
    ==
  ::
  +$  timebox
    (map index notification)
  ::
  +$  notifications
    ((mop @da timebox) gth)
  ::
  --
::
++  state-four
  =<  base-state
  |%
  ++  orm
    ((ordered-map @da timebox) gth)
  ::
  +$  base-state
    $:  unreads-each=(jug stats-index index:graph-store)
        unreads-count=(map stats-index @ud)
        last-seen=(map stats-index @da)
        =notifications 
        archive=notifications
        current-timebox=@da
        dnd=_|
    ==
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
    $>(?(%add-members %remove-members) update:group-store)
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
      ::
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
    [notifications=(set [time index]) =unreads last-seen=@da]
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
--
