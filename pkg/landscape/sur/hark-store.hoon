^? 
::
::  %hark-store: Notification, unreads store
::
::    Timeboxing & binning: 
::
::    Unread notifications accumulate in $unreads. They are grouped by
::    their $bin. A notification may become read by either:
::    a) being read by a %read-count or %read-each or %read-note
::    b) being read by a %seen
::
::    If a) then we insert the corresponding bin into $reads at the
::    current timestamp
::    If b) then we empty $unreads and move all bins to $reads at the
::    current timestamp
::
::    Unread tracking:
::    Unread tracking has two 'modes' which may be used concurrently,
::    if necessary.
::    
::    count:
::      This stores the unreads as a simple atom, describing the number
::      of unread items. May be increased with %unread-count and
::      set to zero with %read-count. Ideal for high-frequency linear
::      datastructures, e.g. chat
::    each:
::      This stores the unreads as a set of paths, describing the set of
::      unread items. Unreads may be added to the set with %unread-each
::      and removed with %read-each. Ideal for non-linear, low-frequency
::      datastructures, e.g. blogs
::
|%
::  $place: A location, under which landscape stores stats
::    
::  .desk must match q.byk.bowl
::  Examples:
::    A chat:
::  [%landscape /~dopzod/urbit-help]
::    A note in a notebook:
::  [%landscape /~darrux-landes/feature-requests/12374893234232]
::    A group:
::  [%hark-group-hook /~bitbet-bolbel/urbit-community]
::    Comments on a link
::  [%landscape /~dabben-larbet/urbit-in-the-news/17014118450499614194868/2]
::
+$  place  [=desk =path]
::
::  $bin: Identifier for grouping notifications
::
::  Examples 
::   A mention in a chat:
::  [/mention %landscape /~dopzod/urbit-help]
::   New messages in a chat
::  [/message %landscape /~dopzod/urbit-help]
::    A new comment in a notebook:
::  [/comment %landscape /~darrux-landes/feature-requests/12374893234232/2]
::
+$  bin  [=path =place]
::  $content: Notification content
+$  content
  $%  [%ship =ship]
      [%text =cord]
  ==
::
::  $body: A notification body
::
+$  body
  $:  title=(list content)
      content=(list content)
      =time
      binned=path
      link=path
  ==
::
+$  notification
  [date=@da =bin body=(list body)]
::  $timebox: Read notifications from a particular time
+$  timebox
  (map bin notification)
::  $reads: Read notifications, ordered by time
+$  reads
  ((mop @da timebox) gth)
::  +unreads: Unread notifications
+$  unreads
  (map bin notification)
::
+$  action
  $%  ::  hook actions
      ::
      ::  %add-note: add a notification
      [%add-note =bin =body]
      ::  %del-place: Underlying resource disappeared, remove all
      ::  associated notifications
      [%del-place =place]
      ::  %unread-count: Change unread count by .count
      [%unread-count =place inc=? count=@ud]
      ::  %unread-each: Add .path to list of unreads for .place
      [%unread-each =place =path]
      ::  %seen-index: Update last-updated for .place to now.bowl
      [%seen-index =place time=(unit time)] 
      ::  store actions
      ::
      ::  %archive: archive single notification
      ::  if .time is ~, then archiving unread notification
      ::  else, archiving read notification
      [%archive time=(unit @da) =bin]
      ::  %read-count: set unread count to zero
      [%read-count =place]
      ::  %read-each: remove path from unreads for .place
      [%read-each =place =path]
      ::  %read-note: Read note at .bin
      [%read-note =bin]
      ::  %archive-all: Archive all notifications
      [%archive-all ~]
      ::  %read-all: Read all all notifications
      [%read-all ~]
      ::  %seen: User opened notifications, reset timeboxing logic.
      [%seen ~]
    :: 
    ::  XX: previously in hark-store, now deprecated
    ::  the hooks responsible for creating notifications may offer pokes
    ::  similar to this
    ::  [%read-graph =resource]
    ::  [%read-group =resource]
    ::  [%remove-graph =resource]
    ::
  ==
::  .stats: Statistics for a .place
::
+$  stats
  $:  count=@ud
      each=(set path)
      last=@da
      timebox=(unit @da)
  ==
::  
+$  update
  $%  action
      :: %more: more updates
      [%more more=(list update)]
      :: %note-read: note has been read with timestamp
      [%note-read =time =bin]
      [%added =notification]
      :: %timebox: description of timebox. 
      ::
      :: If time is ~, this is the unread timebox
      [%timebox time=(unit @da) =(list [bin notification])]
      :: %place-stats: description of .stats for a .place
      [%place-stats =place =stats]
      :: %place-stats: stats for all .places
      [%all-stats places=(map place stats)]
  ==
--

