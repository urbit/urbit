/-  *safe-applet
::
::  A thread on a message board
::
::  The thread applet owns all the messages
::
|%
::
:::::::::::::::::::: EVENT TYPES
::
::  the parent-event of thread is a board issued id number
::
+$  parent-event
  [id=@ud now=@da =community-signature]
::  the child-event of the toplevel is ~. The toplevel has no 
::
+$  child-event
  ~
::  the user-event of the toplevel is a signed post
::
+$  user-event
  $%  [%new-post =post]
      [%delete-post to-delete=@ud]
  ==
::  the server-event of a thread is the additional metadata
::
+$  server-event
  $:  ::  the post-id is assigned server side
      ::
      id=@ud
      ::  the date assigned on the server side (never trust the client)
      ::
      date=@da
      ::  whether this is this poster's first post in this thread
      ::
      new-poster=?
  ==
::  +return-event: passed back to our parent
::
+$  return-event
  $%  ::  accepted: the id number was consumed
      ::
      [%accepted id=@u]
      ::  ignored: the id number was consumed, and still accept the message.
      ::
      [%ignored id=@u]
      ::  reject the entire event
      ::
      [%reject ~]
  ==
::
:::::::::::::::::::: OTHER DATA
::
::  the snapshot of a thread is all its posts plus metadata
::
+$  snapshot
  $:  ::  reverse list of all %new-post events
      ::
      posts=(list [user-event server-event])
      ::  number of unique posters
      ::
      poster-count=@ud
  ==
::
+$  private-state
  $:  ::  mapping between post ids and community identity that posted message.
      ::
      ::    Note that this isn't based on the node-local signature, but is
      ::    based on the toplevel community signature, since "banning" from an
      ::    inner node works by sending messages all the way up to %auth to
      ::    instruct it to stop accepting messages from that community tag.
      ::
      identities=(map @ud @udpoint)
      ::  a set of all community tags that have posted in this thread.
      ::
      ::    Used to calculate the :poster-count in the public state.
      ::
      posted=(set @udpoint)
  ==
::
+$  on-process-response
  $%  [%log =server-event =return-event]
      [%return =return-event]
  ==
::
+$  post
  $:  subject=@t
      text=@t
  ==
::
:::::::::::::::::::: FLOW CONTROL
::
++  on-process-event
  |=  [=parent-event =user-event =snapshot =private-state]
  ^-  [on-process-response _private-state]
  ::
  =/  post-id  id.parent-event
  =/  community-tag  tag.community-signature.parent-event
  ::
  ?-    -.user-event
      %new-post
    =/  new-poster=?  !(~(has in posted.private-state) community-tag)
    ::
    :-  [%log [post-id now.parent-event new-poster] [%accepted post-id]]
    %_    private-state
        identities
      (~(put by identities.private-state) post-id community-tag)
    ::
        posted
      (~(put in posted.private-state) community-tag)
    ==
  ::
      %delete-post
    ::  does the post we're trying to delete actually exist?
    ::
    ?~  point-id=(~(get by identities.private-state) to-delete.user-event)
      ~&  [%rejecting-no-such-post to-delete.user-event]
      [[%return [%reject ~]] private-state]
    ::  make sure the sender is either the original poster or a moderator
    ::
    ?.  ?|  is-moderator.community-signature.parent-event
            =(community-tag u.point-id)
        ==
      ~&  [%rejecting-mismatch community-tag u.point-id]
      [[%return [%reject ~]] private-state]
    ::
    :-  [%log [to-delete.user-event now.parent-event %.n] [%ignored post-id]]
    %_    private-state
        identities
      (~(del by identities.private-state) to-delete.user-event)
    ==
  ==
::  applies an event or fails
::
++  apply-event-to-snapshot
  |=  [user-event=(unit user-event) private=server-event =snapshot]
  ^-  _snapshot
  ::  All events in a thread have user-event
  ?~  user-event
    snapshot
  ::
  ?-    -.u.user-event
      %new-post
    %_  snapshot
        posts
      ^+  posts.snapshot
      (weld posts.snapshot ^+(posts.snapshot [[u.user-event private] ~]))
    ::
        poster-count
      ?:(new-poster.private +(poster-count.snapshot) poster-count.snapshot)
    ==
  ::
      %delete-post
    ::  filter the post with that id out of the snapshot state
    ::
    %_    snapshot
        posts
      %+  skip  posts.snapshot
      |=  [* private=server-event]
      =(to-delete.u.user-event id.private)
    ==
  ==
--
