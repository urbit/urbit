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
  [id=@ud =processed-signature]
::  the child-event of the toplevel is ~. The toplevel has no 
::
+$  child-event
  ~
::  the user-event of the toplevel is a signed post
::
+$  user-event
  $:  [%new-post =post]
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
  ==
::  +return-event: passed back to our parent
::
+$  return-event
  $%  [%accepted id=@u]
      [%ignored id=@u]
  ==
::
:::::::::::::::::::: OTHER DATA
::
::  the snapshot of a thread is all its posts plus metadata
::
+$  snapshot
  $:  posts=(list [user-event private-event])
      ::
      ::posters=@
  ==
::
+$  private-state
  ~
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
  =/  id  id.parent-event
  [[%log [id ~2019.5.5] [%accepted id]] private-state]
::  applies an event or fails
::
++  apply-event-to-snapshot
  |=  [=user-event private=private-event =snapshot]
  ^-  _snapshot
  ::
  snapshot(posts [[user-event private] posts.snapshot])
--
