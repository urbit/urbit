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
  [id=@ud =community-signature]
::  the child-event of the toplevel is ~. The toplevel has no 
::
+$  child-event
  ~
::  the user-event of the toplevel is a signed post
::
+$  user-event
  $:  [%new-post =post]
::      [%delete-post id=@ud]
  ==
::  the private-event of a thread is the additional metadata
::
+$  private-event
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
  ==
::
:::::::::::::::::::: OTHER DATA
::
::  the snapshot of a thread is all its posts plus metadata
::
+$  snapshot
  $:  ::  reverse list of all %new-post events
      ::
      posts=(list [user-event private-event])
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
  $%  [%log =private-event =return-event]
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
  ::  TODO: Need to put a valid time on this.
  ::
  =/  new-poster=?  (~(has in posted.private-state) community-tag)
  ::
  :-  [%log [post-id ~2019.5.5 new-poster] [%accepted post-id]]
  %_    private-state
      identities
    (~(put by identities.private-state) post-id community-tag)
  ::
      posted
    (~(put in posted.private-state) community-tag)
  ==
::  applies an event or fails
::
++  apply-event-to-snapshot
  |=  [=user-event private=private-event =snapshot]
  ^-  _snapshot
  ::
  %_  snapshot
      posts
    [[user-event private] posts.snapshot]
  ::
      poster-count
    ?:(new-poster.private +(poster-count.snapshot) poster-count.snapshot)
  ==
--
