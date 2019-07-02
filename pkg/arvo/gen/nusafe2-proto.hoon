/-  ring
::
::  To post to /board/1234, you must post:
::
::  [outer-sig [/board/1234 [signature %post blah]]]
::
::  In turn, the processing goes like this:
::
::  - Checks outer-signature.
::
::  - Calls (on-route-for-child:toplevel outer-sig ~ /), which can kill the
::  processing or can send a piece of data downwards
::
::  - Calls (on-route-for-child:board outer-sig ~ /board), which can kill or send
::  a piece of data downwards. In this case, a new post number is allocated and
::  is sent downwards between nodes...
::
::  - Calls (on-incoming-event:post outer-sig [~ postid=523] /board/1234
::  signature [%post blah]), which can cancel the event or can add its own
::  private data. In this case, it adds the postid allocated by its parent and
::  then 

|%
::  TODO: REAL TYPES
::
++  ring-sig  @
::
+$  post
  $:  =ring-sig
  ::
      subject=@t
      text=@t
  ==
::
+$  node-control
  $%  ::  creates a new node and re-dispatch the event to it
      ::
      ::    The create call will cause a return event of its own.
      ::
      [%create id=@t child-event=vase]
      ::  handles this event ourselves, first emitting the combination of the
      ::  user event and the private-event to the log, and then returning return-event.
      ::
      [%log private-event=vase]
      ::  reject this event, aborting processing
      ::
      [%reject ~]
      ::  TODO: %redirect?
  ==
::
++  node-type-board
  |%
  ++  parent-event
    ring-sig
  ::  the board passes the newly allocated id to the thread
  ::
  +$  child-event
    id=@ud
  ::  the user-event of the board is a new post request
  ::
  +$  user-event
    $%  [%new-post =post]
::        [%delete-post =post]
    ==
  ::
  +$  private-event
    ~
  ::
  +$  private-state
    $:  next-postid=_1
        other=@
    ==
  ::
  +$  snapshot
    ~
  ::
  +$  child-returned
    ::  return-event:node-type-thread
    $%  [%accepted id=@u]
        [%ignored id=@u]
    ==
  ::
  +$  return-event
    $%  [%accept ~]
        [%reject ~]
    ==
  ::
  +$  node-control
    $:  [%create id=@t =child-event]
    ==
  ::  +on-route: called when we must route a message to our children
  ::
  ::    In the +on-route phase, we take the input parent-event and calculate
  ::    what the child-event we give to the next path segment is, with the
  ::    ability to halt the processing of this event by returning ~.
  ::
  ++  on-route
    |=  [=path =parent-event =private-state]
    ^-  (unit child-event)
    ::  we could block the route if we wanted here!
    ::
    `next-postid.private-state
  ::  +on-process-event: called when we give the passed in user-event to its
  ::  target node.
  ::
  ::    In the +on-process-event phase, we take the parent-event and the
  ::    user-event that the user passed to, and create a list of effects. We
  ::    don't actually modify the private state yet, since the return value can
  ::    cause a rollback.
  ::
  ++  on-process-event
    |=  [=parent-event =user-event =private-state]
    ^-  node-control
    ::
    ?-    -.user-event
        %new-post
      =/  id  next-postid.private-state
      ::
      [%create (scot %ud id) id]
    ==
  ::  +apply-event-to-snapshot: called to (re)play the event log
  ::
  ::    In the +apply-event-to-snapshot phase, we take a %log event generated
  ::    from +on-process-event and modify the snapshot. Behind the scenes, the
  ::    event log is synced to clients, who call this to update their own
  ::    snapshots of the current state.
  ::
  ++  apply-event-to-snapshot
    |=  [=user-event =private-event =snapshot]
    ^-  _snapshot
    ~&  %todo
    snapshot
  ::  +on-return-event: called in response to a normal handle to commit state
  ::
  ::    When +on-process-event creates a %log event, the effects of the %log
  ::    are performed, and then we produce the return value here. We either
  ::    create a return-event or cause a state rollback by returning ~. We also
  ::    return a list of threads to archive, which deletes them on the server
  ::    and archives them on the client.
  ::
  ++  on-return
    |=  [=user-event =private-event =private-state]
    ^-  [return-event archive-list=(list @t) _private-state]
    ::  In the normal case, we just accept what's occured.
    ::
    [[%accept ~] ~ private-state(next-postid +(next-postid.private-state))]
  ::  +on-child-returned: called when a child returns a value to us.
  ::
  ::    If we dispatched to a child through +on-route or through a %create call
  ::    in +on-process-event, we need to take that return value and translate
  ::    it into a return value to our own parent, committing any state along
  ::    the way.
  ::
  ++  on-child-returned
    |=  [=child-returned =private-state]
    ^-  [(unit return-event) archive-list=(list @t) _private-state]
    ?-    -.child-returned
        %accepted
      ?>  =(id.child-returned next-postid.private-state)
      [`[%accept ~] ~ private-state(next-postid +(next-postid.private-state))]
    ::
        %ignored
      [~ ~ private-state]
    ==
  --
::
::  Inside a node, you have
::
++  node-type-thread
  |%
  ::
  :::::::::::::::::::: EVENT TYPES
  ::
  ::  the parent-event of thread is a board issued id number
  ::
  ++  parent-event
    child-event:node-type-board
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
  +$  node-control
    $%  [%log =private-event]
        [%return =return-event]
    ==
  ::
  :::::::::::::::::::: FLOW CONTROL
  ::
  ++  on-process-event
    |=  [=parent-event =user-event =private-state]
    ^-  node-control
    ::
    [%log id.parent-event ~2019.5.5]
  ::  applies an event or fails
  ::
  ++  apply-event-to-snapshot
    |=  [=user-event private=private-event =snapshot]
    ^-  _snapshot
    ::
    snapshot(posts [[user-event private] posts.snapshot])
  ::
  ::  
  ++  on-return
    |=  [=user-event =private-event =private-state]
    ^-  [return-event archive-list=(list @t) _private-state]
    ::  In the normal case, we just accept what's occured.
    ::
    [[%accepted id.private-event] ~ private-state]
  ::
  ::  we don't have children so we don't implement +on-child-returned
  --
::  currently a hack. to make this work really generically, we'll need to make
::  things sorta vase based where we connect types pulled out of the vases
::  instead of an each of the two types.
::
++  node-state
  $~  [*vase 1 ~ *vase *vase ~]
  $:  app-vase=vase
      next-event-id=@ud
      event-log=(list [id=@ud user-event=vase private-event=vase])
      snapshot=vase
      private-state=vase
      children=(map @t node-state)
  ==
::  +sump: like arvo sump, translates vases into cards between applets
::
++  sump
  |=  wec/vase
  ^-  node-control
  ::
  =.  wec  (sped wec)
  ~&  [%wec wec]
  =/  tag  (slot 2 wec)
  ?+    q.tag  ~&  [%bad-tag q.tag]  !!
      %create
    =/  id  (slot 6 wec)
    =/  event  (slot 7 wec)
    [%create ;;(@t q.id) event]
  ::
      %log
    =/  event  (slot 3 wec)
    [%log event]
  ::
      %reject
    [%reject ~]
  ==
::
++  said                                            ::  vase to (list move)
  |=  vud/vase
  |-  ^-  (list node-control)
  ?:  =(~ q.vud)  ~
  ~&  [%vud vud]
  =/  hed  (slot 2 vud)
  =/  tal  (slot 3 vud)
  [(sump hed) $(vud tal)]
::
::  +node-executor: applies a message to a node in a route
::
::    Returns a list of return messages (ignored at the toplevel) and the
::    modified node state. The flow of the node-executor is to dispatch to
::    hierarchical set of nodes. Let's say we have the following node tree:
::
::      /
::      /board
::      /board/123
::      /board/456
::
::    The way to think of a message is as a series of function calls. We call
::    +on-route in /, which generates the parent event for +on-route in /board,
::    which generates the parent event in /board/123. You then call /board/123
::    with the user's request. You then route the return value from /board/123
::    to /board, from /board to /, and then use the assumed [%accept
::    ~]/[%reject ~] return call from the toplevel node.
::
::    TODO: (list vase) to (unit vase)?
::
::    TODO: Create events and archive events need to go in the log.
::
++  node-executor
  |=  [parent-event=vase route=path message=vase state=node-state]
  ^-  [(unit vase) _state]
  ::  If we still have remaining path elements, dispatch on them.
  ::
  ?^  route
    ::
    ~&  [%keys (turn ~(tap by children.state) head)]
    ::
    ?~  sub-node=(~(get by children.state) i.route)
      ~&  [%four-oh-four i.route]
      [~ state]
    ::
    =/  on-route=vase  (slap app-vase.state [%limb %on-route])
    =/  args  :(slop !>(route) parent-event private-state.state)
    =/  raw-result  (slam on-route args)
    ::  raw-result is a (unit *), where we abort processing if we get a sig
    ::  back
    ::
    ?:  =(~ q.raw-result)
      ~&  [%node-canceled-event ~]
      [~ state]
    ::
    =/  child-event=vase  (slot 3 raw-result)
    ::
    =^  return-values  u.sub-node
      (node-executor child-event t.route message u.sub-node)
    ::
    ::  what we want is to mandate a single return value instead of an
    ::  arbitrary list. this really requires that the types above line up
    ::  differently.
    ::
    ::  =/  on-return-event=vase  (slap app-vase.state [%limb %on-return-event])
    ::  =.  args  :(slop 
    ::

    ~&  [%ret return-values]
    [~ state]
  ::  we've reached the node we're trying to talk to.
  ::
  =/  on-process-event=vase  (slap app-vase.state [%limb %on-process-event])
  =/  args  :(slop parent-event message private-state.state)
  =/  raw-result  (slam on-process-event args)
  ::
  =/  event=node-control  (sump raw-result)
  ::
  ~&  [%event event]
  ::
  ?-    -.event
      %create
    ::
    ::  If we've created a new item, and call the return-event, we have a
    ::  return value from the previous +on-process-event above, and a return
    ::  event from the +on-process-event we're about to call on the new
    ::  thread-node. We can only have one. +node-control is thus wrong.

    ::  we create a new thread and then give it the default types.
    ::
    =/  thread=node-state
      :*  !>(node-type-thread)
          1
          ~
          !>(*snapshot:node-type-thread)
          !>(*private-state:node-type-thread)
          ~
      ==
    ::
    =^  return-event  thread  (node-executor child-event.event / message thread)
    ::
    ~&  [%create-return return-event]
    ~&  [%saving id.event]
    [~ state(children (~(put by children.state) id.event thread))]
  ::
      %log
    ::  Calculate the return value and short circuit if 
    ::
    =/  on-return=vase  (slap app-vase.state [%limb %on-return])
    =/  args  :(slop message private-event.event private-state.state)
    =/  raw-result  (slam on-return args)
    ::
    ?:  =(~ q.raw-result)


    ::  when we receive a %log event, we commit this to the event log
    ::
    =/  apply-event-to-snapshot=vase
      (slap app-vase.state [%limb %apply-event-to-snapshot])
    =/  args  :(slop message private-event.event snapshot.state)
    =.  snapshot.state  (slam apply-event-to-snapshot args)

    ::
    [~ state]
  ::
      %reject
    ::  in the rejection state, immediately abort.
    [~ state]
  ==
--
:-  %say
|=  $:  {now/@da eny/@uvJ bec/beak}
        ~
        ~
    ==
:-  %noun
::
::  from outside, we receive [first-parent /board/123 user-event]

::  (on-route:/ ring-signature)
::  (on-route:/board <result above>)
::  (on-route:/board/123 <result above> user-event)

::
::  the event flow is:
::
::  - parent-event is either received from outside or the direct parent node.
::    - first-parent is sent to (on-route / first-parent)
::    - then (on-route /board <return value above>)
::    - then (on-process-event /board/thread <return value above> user-event)
::
::  This works for the thread (sorta) but where does spawning behaviour come in?
::

=/  board=node-state  [!>(node-type-board) 0 ~ !>(*snapshot:node-type-board) !>(*private-state:node-type-board) ~]

=^  returns  board  (node-executor !>(0) / !>([%new-post [0 'subject' 'text']]) board)

::  TODO: This no longer crashes, but we need to first implement the return
::  events above to increment the postid, and then we have to finish routing
::  back to the post.
::
=^  ret2  board  (node-executor !>(0) /1 !>([%new-post [0 'reply' 'text reply']]) board)

~&  [%ret2 ret2]
0
