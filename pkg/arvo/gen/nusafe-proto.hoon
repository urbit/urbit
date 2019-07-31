/-  ring, *safe-applet, common=safe-common, client=safe-client, server=safe-server
/+  ring, *safe-signatures, *safe-client, *safe-common, *safe-server
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
::  A processed signature is an attestation by a parent node to a child node
::  that a valid signature was made.
::
++  processed-signature
  $%  [%ship ship=@]
      [%ring =path tag=(unit @udpoint)]
  ==
::
++  process-signature
  |=  [=path =full-signature]
  ^-  processed-signature
  ::
  ?-  -.full-signature
    %ship  [%ship ship.full-signature]
    %ring  [%ring path y.raw.ring-signature.full-signature]
  ==
::  the authenticating toplevel node
::
++  node-type-auth
  |%
  ++  parent-event
    input:toplevel-interface
  ::
  +$  child-event
    processed-signature
  ::  user events which target the toplevel node are all about doing membership checks
  ::
  +$  user-event
    $%  [%invite ship=@p]
        [%create name=@t app-type=@t =signature-type]
    ==
  ::
  +$  private-event
    ~
  ::
  +$  private-state
    ~
  ::
  +$  snapshot
    $:  ::  this node keeps track of the
        ::
        banned-tags=(map @udpoint @da)
    ==
  ::
  +$  child-returned
    $%  [%accept ~]
        [%reject ~]
    ==
  ::
  +$  return-event
    output:toplevel-interface
  ::
  +$  on-process-response
    $%  [%log =private-event =return-event]
        [%create @t @t =signature-type =child-event]
        [%return =return-event]
    ==
  ::  +on-route: everything routes through the toplevel node. this is what does 
  ::
  ::    TODO: child-event or return-event, to force routing to be able to act
  ::    and terminate processing?
  ::
  ++  on-route
    |=  [=path =parent-event =private-state]
    ^-  (unit child-event)
    ::
    ?-    -.parent-event
        %ship
      ~&  [%todo-verify-signature-for-ship ship.parent-event]
      `[%ship ship.parent-event]
    ::
        %ring
      ::  we are the toplevel, the only path we accept is /
      ::
      ?.  =(/ path)
        ~
      ::
      =/  ships=(set @p)
        %-  sy
        %+  turn
          ~(tap by participants.ring-signature.parent-event)
        head
      ::
      ~&  [%todo-verify-ring-signature-for ships]
      ::
      `[%ring / y.raw.ring-signature.parent-event]
    ==
  ::
  ++  on-process-event
    |=  [=parent-event =user-event =private-state]
    ^-  [on-process-response _private-state]
    ::  todo: since we're the toplevel node, we also need to perform auth here.
    ::
    ?-    -.user-event
        %invite
      [[%return [%accept-and-invite-member ship.user-event]] private-state]
    ::
        %create
      ~&  [%inside-create user-event]
      :_  private-state
      :*  %create
          name.user-event
          app-type.user-event
          %unlinked
          :: TODO: We must get our current node name and weld our new name to the end.
          ::
          (process-signature / parent-event)
      ==
    ==
  ::
  ++  apply-event-to-snapshot
    |=  [=user-event =private-event =snapshot]
    ^-  _snapshot
    ?-    -.user-event
    ::
        %invite
      snapshot
    ::
        %create
      snapshot
    ==
  ::
  ++  on-child-return
    |=  [=child-returned =private-state]
    ^-  [return-event _private-state]
    [child-returned private-state]
  --
::  the board node: toplevel auth owns nodes
::
++  node-type-board
  |%
  ++  parent-event
    processed-signature
  ::  the board passes the newly allocated id to the thread
  ::
  +$  child-event
    [id=@ud =processed-signature]
  ::  the user-event of the board is a new post request
  ::
  +$  user-event
    $%  [%create name=@t type=@t =signature-type]
        [%new-post =post]
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
  +$  on-process-response
    $%  [%create id=@t type=@t =signature-type =child-event]
        [%return =return-event]
    ==
  ::
  +$  post
    $:  subject=@t
        text=@t
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
    `[next-postid.private-state parent-event]
  ::  +on-process-event: called when we give the passed in user-event to its
  ::  target node.
  ::
  ::    In the +on-process-event phase, we take the parent-event and the
  ::    user-event that the user passed to, and create a list of effects along
  ::    with modifications to private state that the user never sees.
  ::
  ++  on-process-event
    |=  [=parent-event =user-event =private-state]
    ^-  [on-process-response _private-state]
    ::
    ?-    -.user-event
        %new-post
      =/  id  next-postid.private-state
      ::  new threads inherit the board configuration
      ::
      :_  private-state
      :*  %create
          (scot %ud id)
          %thread
          %inherit
          [next-postid.private-state parent-event]
      ==
    ::
        %create
      [[%return [%accept ~]] private-state]
    ==
  ::  +apply-event-to-snapshot: called to replay the event log
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
  ::  +on-child-return: called on parent nodes when +on-process-event creates a %return-event
  ::
  ::    In the +on-return-event phase, we take a %return event generated by a
  ::    child node and modify the state with it. This is how 
  ::
  ++  on-child-return
    |=  [=child-returned =private-state]
    ^-  [return-event _private-state]
    ?-    -.child-returned
        %accepted
      ?>  =(id.child-returned next-postid.private-state)
      [[%accept ~] private-state(next-postid +(next-postid.private-state))]
    ::
        %ignored
      [[%accept ~] private-state]
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
    |=  [=parent-event =user-event =private-state]
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
::
++  app-map
  ^-  (map @t vase)
  (my [[%auth !>(node-type-auth)] [%board !>(node-type-board)] [%thread !>(node-type-thread)] ~])
::
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


::  toplevel container node
::
=/  toplevel
  %-  instantiate-node
    :*  app-map
        /
        %auth
        %community
        :*  ~
            (sy [~littel-ponnys ~rovnys-ricfer ~palfun-foslup ~rapfyr-diglyt ~])
            'our town'
            ~zod
    ==  ==
::

::
::  initializes the 'our town' community
::
~&  %server---------1
=^  ret1  toplevel
  (apply-to-server app-map [[%ship ~zod 5] [%ship ~zod 5] / [%invite ~ponnys-podfer]] toplevel)
~&  [%changes ret1]
::  'our town' should have a 'shitposting' board
::
~&  %server---------2
=^  ret2  toplevel
  (apply-to-server app-map [[%ship ~zod 5] [%ship ~zod 5] / [%create 'shitposting' %board %unlinked]] toplevel)
~&  [%changes ret2]
::  time to start shitposting!
::
~&  %server---------3
=^  ret3  toplevel
  (apply-to-server app-map [[%ship ~zod 5] [%ship ~zod 5] /shitposting [%new-post 'subject' 'text']] toplevel)
~&  [%changes ret3]

~&  %client---------1
::  The client wants to post to /shitposting/1, and to do so, it needs the
::  information about /, /shitposting, and /shitposting/1.
::
=|  client-state=node:client
::
=/  root-state=(unit peer-diff:common)  (get-snapshot-as-peer-diff / toplevel)
=.  client-state  (apply-to-client app-map [/ (need root-state)] client-state)
::
=/  board-state=(unit peer-diff:common)  (get-snapshot-as-peer-diff /shitposting toplevel)
=.  client-state  (apply-to-client app-map [/shitposting (need board-state)] client-state)
::
=/  snapshot-state=(unit peer-diff:common)  (get-snapshot-as-peer-diff /shitposting/1 toplevel)
=.  client-state  (apply-to-client app-map [/shitposting/1 (need snapshot-state)] client-state)

~&  [%server-state toplevel]
~&  [%client-state client-state]

::  continue shitposting in the current thread!
::
~&  %server---------4
=^  ret4  toplevel
  (apply-to-server app-map [[%ship ~zod 5] [%ship ~zod 5] /shitposting/1 [%new-post 'reply' 'text reply']] toplevel)
~&  [%changes ret4]

?>  ?=(^ ret4)
=.  client-state  (apply-to-client app-map [/shitposting/1 peer-diff.i.ret4] client-state)


~&  %client---------2

::  Here's a signed request from the client to be sent into the server.
::
~&  [%signed-request (sign-user-event ~zod now eny /shitposting/1 [%new-post 'another' 'more'] client-state app-map)]

::  zero
::
0


:: The system verifies that the signatures are valid before dispatch, but the
:: signatures are made available so that the system can perform its own
:: additional checks.
