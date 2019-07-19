/-  ring
/+  ring
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
::  The toplevel signature is what comes in on the auth app.
::
++  full-signature
  $%  [%ship ship=@p sig=@]
      [%ring =ring-signature:ring]
  ==
::  A processed signature is an attestation by a parent node to a child node
::  that a valid signature was made.
::
++  processed-signature
  $%  [%ship ship=@]
      [%ring tag=(unit @udpoint)]
  ==
::
++  process-signature
  |=  =full-signature
  ^-  processed-signature
  ::
  ?-  -.full-signature
    %ship  [%ship ship.full-signature]
    %ring  [%ring y.raw.ring-signature.full-signature]
  ==
::  what sort of signature this node wants
::
::    We don't want to give applets the ability to build arbitrary signatures,
::    since they could leak information that way. Instead, we make the sort of
::    signature a node wants
::
+$  signature-type
  $?  ::  sign publicly with your ship identity
      ::
      %ship
      ::  sign with an ulinked ring-signature.
      ::
      %unlinked
      ::  sign with a linked ring-signature with scope of the community.
      ::
      %community
      ::  sign with a linked ring-signature with scope of the current path.
      ::
      %self
      ::  walk upwards recursively to find a strategy
      ::
      %inherit
  ==
::
+$  post
  $:  sig=full-signature
  ::
      subject=@t
      text=@t
  ==
::
+$  on-process-response
  $%  ::  emits an event to this node's event log with a corresponding piece of data
      ::
      [%log private-event=vase return-event=vase]
      ::  creates a new node and re-dispatch the event to it
      ::
      [%create sub-id=@t type=@t =signature-type child-event=vase]
      ::  returns a value upwards
      ::
      [%return return-event=vase]
  ==
::  the authenticating toplevel node
::
++  node-type-auth
  |%
  ++  parent-event
    full-signature
  ::
  +$  child-event
    processed-signature
  ::  user events which target the toplevel node are all about doing membership checks
  ::
  +$  user-event
    $%  [%init community-name=@t host-ship=@p initial-members=(set @p)]
        [%add-member ship=@p]
        [%remove-member ship=@p]
    ::
        [%create name=@t type=@t =signature-type]
    ==
  ::
  +$  private-event
    ~
  ::
  +$  private-state
    ::  todo: don't leave all mods as a having the same power this is how
    ::  communities get destroyed
    $:  initialized=_|
    ==
  ::
  +$  snapshot
    $:  ::  community-name: unique name for this host-ship
        ::
        community-name=@t
        ::  host-ship: the ship hosting this community
        ::
        host-ship=@p
        ::  invited: the set of ships which can post to this
        ::
        invited=(set @p)
    ==
  ::
  +$  child-returned
    $%  [%accept ~]
        [%reject ~]
    ==
  ::
  +$  return-event
    $%  [%accept ~]
        [%reject ~]
        [%add-member ship=@p]
        [%remove-member ship=@p]
    ==
  ::
  +$  on-process-response
    $%  [%log =private-event =return-event]
        [%create @t @t =signature-type =child-event]
    ==
  ::  +on-route: everything routes through the toplevel node. this is what does 
  ::
  ++  on-route
    |=  [=path =parent-event =private-state]
    ^-  (unit child-event)
    ::
    ~&  [%todo-add-auth-check-for parent-event]
    ::
    `(process-signature parent-event)
  ::
  ++  on-process-event
    |=  [=parent-event =user-event =private-state]
    ^-  [on-process-response _private-state]
    ::  todo: since we're the toplevel node, we also need to perform auth here.
    ::
    ?-    -.user-event
        %init
      ::  we can only call the init function when we aren't initialized
      ::
      ?>  ?=(%.n initialized.private-state)
      [[%log ~ [%accept ~]] private-state(initialized %.y)]
    ::
        %add-member
      [[%log ~ [%accept ~]] private-state]
    ::
        %remove-member
      [[%log ~ [%accept ~]] private-state]
    ::
        %create
      ~&  [%inside-create user-event]
      :_  private-state
      :*  %create
          name.user-event
          type.user-event
          %unlinked
          (process-signature parent-event)
      ==
    ==
  ::
  ++  apply-event-to-snapshot
    |=  [=user-event =private-event =snapshot]
    ^-  _snapshot
    ?-    -.user-event
        %init
      %_    snapshot
        community-name  community-name.user-event
        host-ship       host-ship.user-event
        invited         initial-members.user-event
      ==
    ::
        %add-member
      snapshot(invited (~(put in invited.snapshot) ship.user-event))
    ::
        %remove-member
      snapshot(invited (~(del in invited.snapshot) ship.user-event))
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
++  event-log-item
  $%  ::  a special init which must be the first item in the / node.
      ::
      [%toplevel-init initial-invited=(set @p) community-name=@t]
      ::  invites a person into the community. only valid on the / node.
      ::
      [%toplevel-invite ship=@p]
  ::
      ::
      ::  the first item in any normal event log; sets signature information,
      ::  but calls no node code
      [%init type=@t =signature-type]
      ::
      ::  when sending across the wire, just send the value in the vase, not
      ::  the type. the other side knows what app its for and at least for now,
      ::  the remote will call the mold.
      [%log user-event=vase private-event=vase]
      ::
      ::  creates a new child node under this
      [%create sub-id=@t type=@t =signature-type]
  ==
::
++  app-map
  ^-  (map @t vase)
  (my [[%auth !>(node-type-auth)] [%board !>(node-type-board)] [%thread !>(node-type-thread)] ~])
::  the server state
::
++  server
  =<  community
  |%
  ++  community
    $:  invited=(set @p)
        root=node
    ==
  ::
  ++  node
    $~  [%auth 1 ~ *vase *vase ~]
    $:  app-type=@t
        next-event-id=@ud
        event-log=(list [id=@ud =event-log-item])
        snapshot=vase
        private-state=vase
        children=(map @t node)
    ==
  --

::  the client state
::
::  ++  client
::    =<  community
::    |%
::    ++  community
::      $:  invited=(set @p)
::          root=node
::      ==
::    ::  the theoretical "top" node which sits outside the system. This data is
::    ::  only snaps
::    ::
::    ++  directory
::      $:  invited=(set @p)
::          root=node
::      ==
::    ::
::    ++  node
::      $~  [%board ~ *vase ~]
::      $:  app-type=@t
::          partial-event-log=(list [id=@ud item=event-item])
::          top-snapshot=(unit top-state)
::          snapshot=vase
::          children=(map @t node)
::      ==
::    ::  state created from event log items that
::    ::
::    ++  top-state
::      $:  invited=(set @p)
::          community-name=@t
::      ==
::    ::  on the client, we may only have events 1,2,3 and then a snapshot of 10
::    ::  and then events 11,12,13.
::    ::
::    ++  event-item
::      $:  [%snapshot =vase]
::          [%event =event-log-item]
::      ==
::    --

::  +sump: like arvo sump, translates vases into cards between applets
::
++  sump
  |=  wec/vase
  ^-  on-process-response
  ::
  =.  wec  (sped wec)
  =/  tag  (slot 2 wec)
  ?+    q.tag  !!
      %log
    =/  private-event  (slot 6 wec)
    =/  return-event  (slot 7 wec)
    [%log private-event return-event]
  ::
      %create
    =/  id  (slot 6 wec)
    =/  node-type  (slot 14 wec)
    =/  sig-type  (slot 30 wec)
    =/  child-event  (slot 31 wec)
    [%create ;;(@t q.id) ;;(@t q.node-type) ;;(signature-type q.sig-type) child-event]
  ::
      %return
    =/  event  (slot 3 wec)
    [%return event]
  ==
::  all hail joe for this
::
++  bunt-a-vase
  |=  v=vase
  ^-  vase
  (slap v [%kttr [%like [[%& 1] ~] ~]])
::
++  instantiate-node
  |=  type=term
  ^-  node:server
  ::
  =/  new-item-vase=vase       (~(got by app-map) type)
  ::
  =/  snapshot-type=vase       (slap new-item-vase [%limb %snapshot])
  =/  private-state-type=vase  (slap new-item-vase [%limb %private-state])
  ::
  :*  type
      1
      ~
      (bunt-a-vase snapshot-type)
      (bunt-a-vase private-state-type)
      ~
  ==
::
::
++  process-child-returned
  |=  [app-vase=vase child-returned=vase state=node:server]
  ^-  [vase _state]
  ::
  =/  on-child-return=vase  (slap app-vase [%limb %on-child-return])
  =/  args=vase  :(slop child-returned private-state.state)
  ::
  =/  raw-result  (slam on-child-return args)
  ::
  =/  return-event=vase  (slot 2 raw-result)
  =.  private-state.state  (slot 3 raw-result)
  ::
  [return-event state]
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
::    TODO: vase to (unit vase)? Right now !>(~) lets me make progress but
::    is wrong in the error handling case
::
::    TODO: Archive events need to go in the log.
::
::
++  node-executor
  |=  $:  parent-event=vase
          route=path
          full-path=path
          message=vase
          state=node:server
      ==
  ^-  [vase _state]
  ::
  ~&  [%full-path full-path]
  ::
  =/  app-vase=vase  (~(got by app-map) app-type.state)
  ::  If we still have remaining path elements, dispatch on them.
  ::
  ?^  route
    ::
    ~&  [%keys (turn ~(tap by children.state) head)]
    ::
    ?~  sub-node=(~(get by children.state) i.route)
      ~&  [%four-oh-four i.route]
      [!>(~) state]
    ::
    =/  on-route=vase  (slap app-vase [%limb %on-route])
    =/  args  :(slop !>(route) parent-event private-state.state)
    =/  raw-result  (slam on-route args)
    ::  raw-result is a (unit *), where we abort processing if we get a sig
    ::  back
    ::
    ?:  =(~ q.raw-result)
      ~&  [%node-canceled-event ~]
      [!>(~) state]
    ::
    =/  child-event=vase  (slot 3 raw-result)
    ::
    =^  return-value  u.sub-node
      (node-executor child-event t.route full-path message u.sub-node)
    ::
    =.  children.state  (~(put by children.state) i.route u.sub-node)
    ::
    (process-child-returned app-vase return-value state)
  ::  we've reached the node we're trying to talk to.
  ::
  =/  on-process-event=vase  (slap app-vase [%limb %on-process-event])
  =/  args  :(slop parent-event message private-state.state)
  =/  raw-result  (slam on-process-event args)
  ::
  =/  response=on-process-response  (sump (slot 2 raw-result))
  =.  private-state.state  (slot 3 raw-result)
  ::
  ?-    -.response
      %log
    ::  when we receive a %log event, we commit this to the event log
    ::
    =/  apply-event-to-snapshot=vase
      (slap app-vase [%limb %apply-event-to-snapshot])
    =/  args  :(slop message private-event.response snapshot.state)
    =.  snapshot.state  (slam apply-event-to-snapshot args)
    ::
    ~&  [%log user-event=message private-event=private-event.response]
    =.  event-log.state
      [[next-event-id.state [%log message private-event.response]] event-log.state]
    =.  next-event-id.state  +(next-event-id.state)
    ::
    ~&  [%new-snapshot full-path snapshot.state]
    ::
    [return-event.response state]
  ::
      %create
    ::
    =/  created=node:server  (instantiate-node type.response)
    ::  write the creation information into the event log so that when
    ::  replayed, we get the configuration.
    ::
    =.  event-log.created
      [[0 [%init type.response signature-type.response]] ~]
    ::
    =^  return  created
      (node-executor child-event.response / (weld full-path [sub-id.response ~]) message created)
    ::
    ~&  [%created type=type.response sub-id=sub-id.response return=return]
    =.  children.state  (~(put by children.state) sub-id.response created)
    =.  event-log.state
      :_  event-log.state
      [next-event-id.state [%create sub-id.response type.response signature-type.response]]
    =.  next-event-id.state  +(next-event-id.state)
    ::
    (process-child-returned app-vase return state)
  ::
      %return
    ::  when we receive a %return value, we pass the value up to the callers
    ::
    [return-event.response state]
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

::  =/  community-server
::    %-  instantiate-community  :*
::      name='our twon'
::      host=~zod
::      members=(sy [~littel-ponnys ~rovnys-ricfer ~palfun-foslup ~rapfyr-diglyt ~])
::      toplevel=%auth
::    ==



::  toplevel container node
::
=/  toplevel  (instantiate-node %auth)
::  initializes the 'our town' community
::
~&  %phase---------1
=^  ret1  toplevel
  (node-executor !>([%ship ~zod 5]) / / !>([%init 'our town' ~zod (sy [~littel-ponnys ~])]) toplevel)
~&  [%ret ret1]
::  'our town' should have a 'shitposting' board
::
~&  %phase---------2
=^  ret2  toplevel
  (node-executor !>([%ship ~zod 5]) / / !>([%create 'shitposting' %board %unlinked]) toplevel)
~&  [%ret2 ret2]
~&  [%toplevel toplevel]
::  time to start shitposting!
::
~&  %phase---------3
=^  ret3  toplevel
  (node-executor !>([%ship ~zod 5]) /shitposting /shitposting !>([%new-post [[%ship ~zod 5] 'subject' 'text']]) toplevel)
~&  [%ret3 ret3]
::  continue shitposting in the current thread!
::
~&  %phase---------4
=^  ret4  toplevel
  (node-executor !>([%ship ~zod 5]) /shitposting/1 /shitposting/1 !>([%new-post [[%ship ~zod 5] 'reply' 'text reply']]) toplevel)
~&  [%ret4 ret4]
::
~&  [%final-sate toplevel]


::  zero
::
0

:: So we now need to have a way to make messages to send to the 
::
::  =/  message-package
::    (message-builder /shitposting/1 !>([%new-post 'reply' 'text reply']) local-toplevel)

::  how messages are signed has to be part of the node at build time. 
::
::  [%create id=5 type=%board sig-type=[%ring ?(%anon %parent %self)]]
::
::  then we can take the 
::

::  if the membership pool is inside the applets, we have to trust the applets
::  to maintain it. This implies:
::
::  - Our system state is [invited=(set @p) root-node], where the invited sit outside
::
::  - To initialize this, we'll need a separate toplevel.





::  ::
::  ::  go to the top and then [path user-event] -> signature-request

::  ::
::  ::  in this whole prototype, i've put the signature inside user-event under the
::  ::  idea that letting this be configurable is correct. but we don't want to let
::  ::  the applets touch a users' key material; all signatures should be of the
::  ::  form of a signature request.
::  ::
::  ::  we need to produce two signatures: the toplevel one that the host sees and
::  ::  the user-event one which goes into the event log. for the toplevel one, we
::  ::  need to work on the
::  ::
::  ::  if we give the toplevel node the ability to maintain the member list, what
::  ::  mischief could a malicious applet get up to? For now, we're going to just
::  ::  let that be app state, but that should seriously be put into the event log
::  ::  management in v2.
::  ::
::  ++  user-signature-request
::    $%  [%make-ring tag=(unit path)]
::    ==
::  ::
::  ++  toplevel-signature-request
::    $%  [%make-ring ships=(set @p) tag=(unit path)]
::        [%make-id ship=@p]
::    ==
::  ::
::  ++  produce-signature
::    |=  [=user-signature-request =snapshot]
::    ^-  toplevel-signature-request
::    ::
::    ?-  user-signature-request
::      %make-ring  [%make-ring ships.snapshot tag.user-signature-request]
::      %make-id    [%make-id ship.snapshot]
::    ==

::  ::  Maybe I'm overthinking this. Should 

::  ++  sign-user-event
::    |=  [=user-event =path]


:: The system verifies that the signatures are valid before dispatch, but the
:: signatures are made available so that the system can perform its own
:: additional checks.



:: Do I need an init on each node? How else do they learn the pieces of state?
