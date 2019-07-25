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
::  Your community can do whatever it wants, as long as it obeys this toplevel interface
::
++  toplevel-interface
  |%
  ::  the input to your node; this is your parent-event
  ::
  ++  input
    full-signature
  ::  the output of your node; this is your return-event
  ::
  ++  output
    $%  ::  accepts the message and release all the effects and state changes
        ::  it produced.
        ::
        [%accept ~]
        ::  rejects this message and ignores all the effects and reverts all
        ::  state changes.
        ::
        [%reject ~]
        ::  a special output that can only occur on the toplevel node; this
        ::  accepts the incoming new invitiees.
        ::
        [%accept-and-invite-member ship=@p]
        ::
        ::  TODO: Should there be an uninvite here? If there should be, how do
        ::  you avoid abusive @p posting? If not, how do you fix threats to
        ::  eject yourself in response to blackmail?
    ==
  --
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
        [%create name=@t type=@t =signature-type]
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
          type.user-event
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
++  app-map
  ^-  (map @t vase)
  (my [[%auth !>(node-type-auth)] [%board !>(node-type-board)] [%thread !>(node-type-thread)] ~])
::  common data structures between the server and the client
::
++  common
  |%
  ::  every root node keeps metadata about the entire server, which is used for signatures
  ::
  ++  top-state
    $:  invited=(set @p)
        community-name=@t
        original-host=@p
    ==
  ::  +snapshot representation for being on a ship
  ::
  ++  snapshot
    $~  [%auth ~ %community *vase ~]
    $:  ::  which of the built-in app types we represent
        ::
        app-type=@t
        ::  the top-state; community identification information only on /
        ::
        top-state=(unit top-state)
        ::  what sort of signature should be sent to this node
        ::
        =signature-type
        ::  the public per-applet snapshot data
        ::
        snapshot=vase
        ::  the live iterable children are tracked as a set inside the snapshot
        ::
        children=(set @t)
    ==
  ::  all events that could exist in an event log
  ::
  ++  event-log-item
    $%  ::  a special init which must be the first item in the / node.
        ::
        $:  %toplevel-init
            initial-invited=(set @p)
            community-name=@t
            original-host=@p
            type=@t
            =signature-type
        ==
        ::  invites a person into the community. only valid on the / node.
        ::
        [%toplevel-invite ship=@p]
    ::
        ::
        ::  the first item in any normal event log; sets signature information,
        ::  but calls no node code
        [%init route=path type=@t =signature-type]
        ::
        ::  when sending across the wire, just send the value in the vase, not
        ::  the type. the other side knows what app its for and at least for now,
        ::  the remote will call the mold.
        ::
        $:  %log
            ::  msg-signature=full-signature
            ::  route=path
            user-event=vase
            private-event=vase
        ==
        ::
        ::  creates a new child node under this
        [%create sub-id=@t type=@t =signature-type]
    ==
  ::  representation of a snapshot on the wire; contains no vases
  ::
  ++  transport-snapshot
    $:  ::  which of the built-in app types we represent
        ::
        app-type=@t
        ::  the top-state; community identification information only on /
        ::
        top-state=(unit top-state)
        ::  what sort of signature should be sent to this node
        ::
        =signature-type
        ::  a snapshot reconstitutable with app-type into a real vase
        ::
        raw-snapshot=*
        ::  the live iterable children are tracked as a set inside the snapshot
        ::
        children=(set @t)
    ==
  ::  representation of an event-log-item on the wire; contains no vases
  ::
  ++  transport-event-log-item
    $%  ::  a special init which must be the first item in the / node.
        ::
        $:  %toplevel-init
            initial-invited=(set @p)
            community-name=@t
            original-host=@p
            type=@t
            =signature-type
        ==
        ::  invites a person into the community. only valid on the / node.
        ::
        [%toplevel-invite ship=@p]
    ::
        ::
        ::  the first item in any normal event log; sets signature information,
        ::  but calls no node code
        [%init route=path type=@t =signature-type]
        ::
        ::  when sending across the wire, just send the value in the vase, not
        ::  the type. the other side knows what app its for and at least for now,
        ::  the remote will call the mold.
        ::
        $:  %log
            ::  msg-signature=full-signature
            ::  route=path
            user-event=*
            private-event=*
        ==
        ::
        ::  creates a new child node under this
        [%create sub-id=@t type=@t =signature-type]
    ==
  --
::  the server state
::
++  server
  =<  node
  |%
  ::  the server state is a hierarchy of node objects, referred to by path
  ::
  ++  node
    $~  [~ 1 *snapshot:common *vase ~]
    $:  ::  the relevant parts of the server state should always be rebuildable
        ::  from the event log itself.
        ::
        event-log=(list [id=@ud =event-log-item:common])
        ::  the next event id from the
        ::
        next-event-id=@ud
        ::  the public snapshot; all state that can be 
        ::
        =snapshot:common
        ::  private applet state never shared with clients
        ::
        private-state=vase
        ::  the state of all child nodes
        ::
        children=(map @t node)
    ==
  --
::  the client state
::
++  client
  =<  node
  |%
  ::
  ++  node
    $~  [~ ~ ~ ~]
    $:  ::  an event log with snapshots in it.
        ::
        partial-event-log=(list [id=@ud item=event-item])
        ::  the current state of the node
        ::
        ::    The :partial-event-log may have %snapshots in them, but the head
        ::    may not be a snapshot so always have the current head rendered as
        ::    a snapshot.
        ::
        ::    However, we may not have any information about this node, only
        ::    understand that its exists in the graph.
        ::
        snapshot=(unit snapshot:common)
        ::  live children state we know about
        ::
        ::    When we see a %create event, we place a ~ in this node until we
        ::    follow it.
        ::
        children=(map @t (unit node))
        ::  archived children state
        ::
        ::    In the event log, when a %remove event occurs, that data gets
        ::    wiped on the server. On the client, removed nodes go to the
        ::    archive where they are no longer modifiable.
        ::
        ::    In the case that we never learned anything about the thread other
        ::    than its existence, we don't add anything here.
        ::
        archived=(map @t node)
    ==
  ::  state created from event log items that
  ::
  ++  top-state
    $:  invited=(set @p)
        community-name=@t
        original-host=@p
    ==
  ::  on the client, we may only have events 1,2,3 and then a snapshot of 10
  ::  and then events 11,12,13.
  ::
  ++  event-item
    $%  [%snapshot =snapshot:common]
        [%event =event-log-item:common]
    ==
  --
::
++  to-transport
  |%
  ++  event-log-item
    |=  e=event-log-item:common
    ^-  transport-event-log-item:common
    ::  the only thing we need to special case is %log
    ::
    ?.  ?=(%log -.e)
      e
    ::
    [%log q.user-event.e q.private-event.e]
  ::
  ++  snapshot
    |=  s=snapshot:common
    ^-  transport-snapshot:common
    [app-type top-state signature-type q.snapshot children]:s
  --
::
++  from-transport
  |%
  ++  event-log-item
    |=  [app=term e=transport-event-log-item:common]
    ^-  (unit event-log-item:common)
    ::
    ?.  ?=(%log -.e)
      `e
    ::
    =/  node-vase=vase           (~(got by app-map) app)
    ::
    =/  user-event-mold=vase     (slap node-vase [%limb %user-event])
    =/  private-event-mold=vase  (slap node-vase [%limb %private-event])
    ::
    =/  user-event=vase  (slam user-event-mold %noun user-event.e)
    =/  private-event=vase  (slam private-event-mold %noun private-event.e)
    ::
    `[%log user-event private-event]
  ::
  ++  snapshot
    |=  s=transport-snapshot:common
    ^-  snapshot:common
    ::
    =/  node-vase=vase           (~(got by app-map) app-type.s)
    ::
    =/  snapshot-mold=vase  (slap node-vase [%limb %snapshot])
    =/  snapshot-vase=vase  (slam snapshot-mold %noun raw-snapshot.s)
    ::
    [app-type.s top-state.s signature-type.s snapshot-vase children.s]
  --
::  a diff to be sent over the wire for transport; this contains no vases.
::
++  peer-diff
  $%  [%snapshot id=@u snapshot=transport-snapshot:common]
      [%event id=@u event=transport-event-log-item:common]
  ==




::  Imagine that I'm the client connecting for the first time. What gets sent?
::  I ask for (peer /). What gets returned? An event log or more likely a
::  snapshot which would theoretically be reconstructed from an event log. I
::  should get the set of 
::
::  Then I just get the events.



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
  |=  [route=path type=term =signature-type top-state=(unit top-state:common)]
  ^-  node:server
  ::
  =/  new-item-vase=vase       (~(got by app-map) type)
  ::
  =/  snapshot-type=vase       (slap new-item-vase [%limb %snapshot])
  =/  private-state-type=vase  (slap new-item-vase [%limb %private-state])
  ::
  =/  first-event=event-log-item:common
    ?~  top-state
      [%init route type signature-type]
    ::
    :*  %toplevel-init
        invited.u.top-state
        community-name.u.top-state
        original-host.u.top-state
        type
        signature-type
    ==
  ::
  :*  [[0 first-event] ~]
      1
      :*  type
          top-state
          signature-type
          (bunt-a-vase snapshot-type)
          ~
      ==
      (bunt-a-vase private-state-type)
      ~
  ==
::  +change-broadcast:
::
+$  change-broadcast
  $:  =path
      =peer-diff
  ==
::
++  verify-signature
  |=  $:  =signature-type
          route=path
          =full-signature
          noun=*
      ==
  ^-  ?
  ::  %inherit isn't a real signature type, but a directive to use the parent's
  ::  type. there should be no instances of it.
  ::
  ?>  !=(%inherit signature-type)
  ::  TODO: We don't want to go full signature verification until we have the
  ::  parts of the inter-ship replication system going, since we'll have to
  ::  write the code which 
  ::
  %.y
  ::  ::  if the signature-type is ship, you must have a ship signature
  ::  ::
  ::  ?:  =(%ship signature-type)
  ::    ?.  ?=([%ship *] full-signature)
  ::      %.n
  ::    ::
  ::    ~&  %todo-verify-ship-signature
  ::    %.y
  ::  ::  all other signatures are variants on ring signatures
  ::  ::
  ::  ?.  ?=([%ring *] full-signature)
  ::    %.n
  ::  ::  todo: this rest here.
  ::  ::
  ::  %.y
  
::  +apply: applies a message to the event logs
::
::    TODO: we need to thread the signatures through the system
::    now. :top-signature is the signature of [message-signature route
::    message] and message-signature is the signature of [route message].
::
::    We need the full path identity to be in the node and the signature and
::    checked to prevent weird replay attacks?
::
++  apply
  |=  $:  top-signature=full-signature
          message-signature=full-signature
          route=path
          message=vase
          original-state=node:server
      ==
  ^-  [(list change-broadcast) _original-state]
  ::
  =/  toplevel-sig-type=signature-type  signature-type.snapshot.original-state

  ::  TODO: Before we pass the data to the applets, we perform the verification
  ::  management ourselves, and error if we have incorrect signatures.
  ::
  ?.  %-  verify-signature  :*
        toplevel-sig-type
        /
        top-signature
        [message-signature route q.message]
      ==
    ~&  %invalid-toplevel-signature
    [~ original-state]
  ::
  |^  ::  OK, when the server receives a users message, we check the message with
      ::  the destination node's signature type. At this phase, we don't care if
      ::  the message is going to be forwarded by a %create command. (This is why
      ::  the event log records the route on a per-item basis; when you replay the
      ::  log, you need that information since the route of an individual message
      ::  may not match the final receiving node.
      ::
      =/  message-sig-type=(unit signature-type)
        (get-signature-type route original-state)
      ::
      ?~  message-sig-type
        ~&  %invalid-target-path
        [~ original-state]
      ::
      ~&  [%dest message-sig-type]
      ::
      ?.  %-  verify-signature  :*
            u.message-sig-type
            route
            message-signature
            [route q.message]
          ==
        ~&  %invalid-message-signature
        [~ original-state]
      ::  there are two things which cause broadcast changes: events sent as part
      ::  of the [%log ...] message from +on-process-event and the toplevel
      ::  [%accept-and-invite-member @p] return value.
      ::
      =/  ret=[=vase changes=(list change-broadcast) state=_original-state]
        %-  recurse  :*
          !>(top-signature)
          route
          route
          message
          [~ original-state]
        ==
      ::  the output of calling recurse is a vase which must be a
      ::  +output:toplevel-interface
      ::
      =/  ret-val=output:toplevel-interface
        ;;(output:toplevel-interface q.vase.ret)
      ::
      ?-    -.ret-val
          %accept
        ~&  %accepting-event
        [changes:ret state:ret]
      ::
          %reject
        ~&  %rejecting-event
        [~ original-state]
      ::
          %accept-and-invite-member
        ~&  [%accepting-event-and-inviting ship.ret-val]
        ?>  ?=(^ top-state.snapshot.state.ret)
        =.  invited.u.top-state.snapshot.state.ret
          (~(put in invited.u.top-state.snapshot.state.ret) ship.ret-val)
        ::
        %^  record-change  changes.ret  [state.ret /]
        [%toplevel-invite ship.ret-val]
      ==
  ::
  +$  changes-and-state
    $:  changes=(list change-broadcast)
        state=node:server
    ==
  ::  +recurse: applies a message to a node in a route
  ::
  ::    Returns a list of return messages (ignored at the toplevel) and the
  ::    modified node state. The flow of the apply is to dispatch to
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
  ++  recurse
    |=  $:  parent-event=vase
            route=path
            full-path=path
            message=vase
            changes-and-state
        ==
    ^-  [vase changes-and-state]
    ::
    ~&  [%full-path full-path]
    ::
    =/  app-vase=vase  (~(got by app-map) app-type.snapshot.state)
    ::  If we still have remaining path elements, dispatch on them.
    ::
    ?^  route
      ::
      ?~  sub-node=(~(get by children.state) i.route)
        ~&  [%four-oh-four i.route]
        [!>(~) changes state]
      ::
      =/  on-route=vase  (slap app-vase [%limb %on-route])
      =/  args  :(slop !>(route) parent-event private-state.state)
      =/  raw-result  (slam on-route args)
      ::  raw-result is a (unit *), where we abort processing if we get a sig
      ::  back
      ::
      ?:  =(~ q.raw-result)
        ~&  [%node-canceled-event ~]
        [!>(~) changes state]
      ::
      =/  child-event=vase  (slot 3 raw-result)
      ::
      =/  n=[return-value=vase changes-and-state]
        (recurse child-event t.route full-path message changes u.sub-node)
      =/  return-value  return-value.n
      =.  changes  changes.n
      =.  u.sub-node    state.n
      ::
      =.  children.state  (~(put by children.state) i.route u.sub-node)
      ::
      (process-child-returned app-vase return-value changes state)
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
      =/  args  :(slop message private-event.response snapshot.snapshot.state)
      =.  snapshot.snapshot.state  (slam apply-event-to-snapshot args)
      ::
      =/  nu=changes-and-state
        %^  record-change  changes  [state full-path]
        [%log message private-event.response]
      ::
      ~&  [%new-snapshot full-path snapshot.snapshot.state.nu]
      ::
      [return-event.response changes.nu state.nu]
    ::
        %create
      ::
      =/  new-route=path  (weld full-path [sub-id.response ~])
      ::
      =/  created=node:server
        (instantiate-node [new-route [type signature-type ~]:response])
      ::
      =/  n=[return-value=vase changes-and-state]
        (recurse child-event.response / new-route message changes created)
      =/  return  return-value.n
      =.  changes  changes.n
      =.  created    state.n
      ::
      ~&  [%created type=type.response sub-id=sub-id.response return=return]
      =.  children.state  (~(put by children.state) sub-id.response created)
      =.  children.snapshot.state  (~(put in children.snapshot.state) sub-id.response)
      ::
      =/  nu=changes-and-state
        %^  record-change  changes  [state full-path]
        [%create sub-id.response type.response signature-type.response]
      ::
      (process-child-returned app-vase return changes.nu state.nu)
    ::
        %return
      ::  when we receive a %return value, we pass the value up to the callers
      ::
      [return-event.response changes state]
    ==
  ::  +record-change: applies a :log-entry to the event log and the outbound changes.
  ::
  ++  record-change
    |=  $:  changes=(list change-broadcast)
            [state=node:server full-path=path]
            log-entry=event-log-item:common
        ==
    ^+  [changes state]
    ::
    =.  changes
      :_  changes
      :*  full-path
          %event
          next-event-id.state
          (event-log-item:to-transport log-entry)
      ==
    ::
    =.  event-log.state
      [[next-event-id.state log-entry] event-log.state]
    =.  next-event-id.state  +(next-event-id.state)
    ::
    [changes state]
  ::
  ++  process-child-returned
    |=  [app-vase=vase child-returned=vase changes=(list change-broadcast) state=node:server]
    ^-  [vase (list change-broadcast) node:server]
    ::
    =/  on-child-return=vase  (slap app-vase [%limb %on-child-return])
    =/  args=vase  :(slop child-returned private-state.state)
    ::
    =/  raw-result  (slam on-child-return args)
    ::
    =/  return-event=vase  (slot 2 raw-result)
    =.  private-state.state  (slot 3 raw-result)
    ::
    [return-event changes state]
  ::  +get-signature-type: returns the requested signature-type for a node
  ::
  ++  get-signature-type
    |=  [route=path state=node:server]
    ^-  (unit signature-type)
    ::
    ?~  route
      `signature-type.snapshot.state
    ::
    ?~  child-node=(~(get by children.state) i.route)
      ~
    ::
    =/  candidate  $(route t.route, state u.child-node)
    ?:  =(`%inherit candidate)
      `signature-type.snapshot.state
    ::
    candidate
  --
::  Situation: someone has opened a new +peer on a path; the first thing we
::  need to do is to send a complete snapshot of the current state to the
::  client.
::
++  get-peer-diff-snapshot
  |=  [route=path top-state=node:server]
  ^-  (unit peer-diff)
  ::
  |^  ^-  (unit peer-diff)
      ?~  maybe-state=(get-node-for route top-state)
        ~
      ::
      =/  event-id  (sub next-event-id.u.maybe-state 1)
      [~ %snapshot event-id (snapshot:to-transport snapshot.u.maybe-state)]
  ::
  ++  get-node-for
    |=  [route=path state=node:server]
    ^-  (unit node:server)
    ::
    ?~  route
      `state
    ::
    ?~  child-node=(~(get by children.state) i.route)
      ~
    ::
    $(route t.route, state u.child-node)
  --
::  +applies a diff to a client's state
::
++  apply-peer-diff
  |=  [route=path =peer-diff client-state=node:client]
  ^-  node:client
  ::
  ?^  route
    =/  child-state=node:client
      %_    $
          route  t.route
      ::
          client-state
        ?~  child-state=(~(get by children.client-state) i.route)
          ::  this is the first time we've even heard of this node
          *node:client
        ?~  u.child-state
          ::  we've heard about this node before, but know nothing about it
          *node:client
        u.u.child-state
      ==
    ::
    client-state(children (~(put by children.client-state) i.route `child-state))
  ::  apply this peer-diff to the client-state
  ::
  ?-    -.peer-diff
      ::  we have a completely new snapshot which we append to history
      ::
      %snapshot
    =/  snapshot=snapshot:common
      (snapshot:from-transport snapshot.peer-diff)
    ::
    %_    client-state
        partial-event-log
      :_  partial-event-log.client-state
      [id.peer-diff %snapshot snapshot]
    ::
        snapshot
      [~ snapshot]
    ==
  ::
      %event
    ::  todo: we have an event item, we need to apply it against the state.
    ::
    ~&  [%todo-apply-event peer-diff]
    client-state
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


::  toplevel container node
::
=/  toplevel
  %-  instantiate-node
    :*  /
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
  (apply [%ship ~zod 5] [%ship ~zod 5] / !>([%invite ~ponnys-podfer]) toplevel)
~&  [%changes ret1]
::  'our town' should have a 'shitposting' board
::
~&  %server---------2
=^  ret2  toplevel
  (apply [%ship ~zod 5] [%ship ~zod 5] / !>([%create 'shitposting' %board %unlinked]) toplevel)
~&  [%changes ret2]
::  time to start shitposting!
::
~&  %server---------3
=^  ret3  toplevel
  (apply [%ship ~zod 5] [%ship ~zod 5] /shitposting !>([%new-post [[%ship ~zod 5] 'subject' 'text']]) toplevel)
~&  [%changes ret3]

~&  %client---------1
::
=|  client-state=node:client
::
=/  snapshot-state=(unit peer-diff)  (get-peer-diff-snapshot /shitposting/1 toplevel)
::
=.  client-state  (apply-peer-diff /shitposting/1 (need snapshot-state) client-state)

~&  [%client-state client-state]

::  continue shitposting in the current thread!
::
~&  %server---------4
=^  ret4  toplevel
  (apply [%ship ~zod 5] [%ship ~zod 5] /shitposting/1 !>([%new-post [[%ship ~zod 5] 'reply' 'text reply']]) toplevel)
~&  [%changes ret4]

?>  ?=(^ ret4)
::  TODO: Actually apply diffs that come over the peer! This requires patches to work!
::
::  =.  client-state  (apply-peer-diff /shitposting/1 .i.ret4 client-state)
::  ~&  [%final-client-state client-state]

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



:: The system verifies that the signatures are valid before dispatch, but the
:: signatures are made available so that the system can perform its own
:: additional checks.
