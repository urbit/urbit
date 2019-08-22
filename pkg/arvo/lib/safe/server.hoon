/-  *safe-applet, common=safe-common, server=safe-server
/+  *safe-common, *safe-signatures
::
|%
::  +changes-and-state: internal type of +apply.
::
+$  changes-and-state
  $:  changes=(list server-to-client:common)
      state=node:server
  ==
--
::
|%
::
++  to-transport
  |%
  ++  event-log-item
    |=  e=event-log-item:common
    ^-  transport-event-log-item:common
    ::  the only thing we need to special case is %log
    ::
    ?-   -.e
        ?(%toplevel-init %toplevel-invite %init)
      e
    ::
        %log
      ::
      :*  %log
          ?~  user-event.e
            ~
          :*  ~
              msg-signature.u.user-event.e
              route.u.user-event.e
              q.user-event.u.user-event.e
          ==
      ::
          q.server-event.e
      ==
    ::
        %create
      :*  %create
          sub-id.e
          app-type.e
          signature-type.e
      ::
          ?~  server-event.e
            ~
          `q.u.server-event.e
      ==
    ==
  ::
  ++  snapshot
    |=  s=snapshot:common
    ^-  transport-snapshot:common
    [app-type top-state signature-type q.snapshot children]:s
  --
::  +process-sump: like arvo sump, translates vases into cards between applets
::
++  process-sump
  |=  wec/vase
  ^-  on-process-response
  ::
  =.  wec  (sped wec)
  =/  tag  (slot 2 wec)
  ?+    q.tag  !!
      %log
    =/  server-event  (slot 6 wec)
    =/  return-event  (slot 7 wec)
    [%log server-event return-event]
  ::
      %create
    =/  id  (slot 6 wec)
    =/  node-type  (slot 14 wec)
    =/  sig-type  (slot 30 wec)
    =/  unit-server-event  (slot 62 wec)
    =/  child-event  (slot 63 wec)
    ::
    =/  server-event=(unit vase)
      ?:  =(~ q.unit-server-event)
        ~
      [~ (slot 3 unit-server-event)]
    ::
    :*  %create
        ;;(@t q.id)
        ;;(@t q.node-type)
        ;;(signature-type q.sig-type)
        server-event
        child-event
    ==
  ::
      %return
    =/  event  (slot 3 wec)
    [%return event]
  ==
::  +child-sump: like arvo sump, translates vases into cards between applets
::
++  child-sump
  |=  wec/vase
  ^-  on-child-response
  ::
  =.  wec  (sped wec)
  =/  tag  (slot 2 wec)
  ?+    q.tag  !!
      %log
    =/  server-event  (slot 6 wec)
    =/  return-event  (slot 7 wec)
    [%log server-event return-event]
  ::
      %return
    =/  event  (slot 3 wec)
    [%return event]
  ==
::
++  instantiate-node
  |=  $:  app-map=(map @t vase)
          route=path
          type=term
          =signature-type
          top-state=(unit top-state:common)
      ==
  ^-  node:server
  ::
  =/  new-item-vase=vase       (~(got by app-map) type)
  ::
  =/  snapshot-type=vase       (slap new-item-vase [%limb %snapshot])
  =/  private-state-type=vase  (slap new-item-vase [%limb %private-state])
  ::
  :*  1
      :*  type
          top-state
          signature-type
          (bunt-a-vase snapshot-type)
          ~
      ==
      (bunt-a-vase private-state-type)
      ~
  ==
::  +get-snapshot-as-peer-diff: Returns a snapshot of the current state
::
::    Someone has opened a new +peer on a path; the first thing we need to do
::    is to send a complete snapshot of the current state to the client.
::
++  get-snapshot-as-peer-diff
  |=  [route=path top-state=node:server]
  ^-  (unit peer-diff:common)
  ::
  |^  ^-  (unit peer-diff:common)
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
::  +async-validate-message: validates that signatures are correct
::
++  async-validate-message
  |=  $:  our=@p
          now=@da
          signed-message=client-to-server:common
          state=node:server
      ==
  =/  m  (async ,?)
  ^-  form:m
  ::
  =/  toplevel-sig-type=signature-type  signature-type.snapshot.state
  ::
  ;<  ok=?  bind:m
    %-  async-verify-signature  :*
      our
      now
      invited:(need top-state.snapshot.state)
      toplevel-sig-type
      top-signature.signed-message
      [message-signature route message]:signed-message
    ==
  ::
  ?.  ok
    ~&  %invalid-toplevel-signature
    (pure:m %.n)
  ::
  ::  OK, when the server receives a users message, we check the message with
  ::  the destination node's signature type. At this phase, we don't care if
  ::  the message is going to be forwarded by a %create command. (This is why
  ::  the event log records the route on a per-item basis; when you replay the
  ::  log, you need that information since the route of an individual message
  ::  may not match the final receiving node.
  ::
  =/  message-sig-type=(unit signature-type)
    (get-signature-type route.signed-message state)
  ::
  ?~  message-sig-type
    ~&  %invalid-target-path
    (pure:m %.n)
  ::
  %-  async-verify-signature  :*
    our
    now
    invited:(need top-state.snapshot.state)
    u.message-sig-type
    message-signature.signed-message
    [route message]:signed-message
  ==
::  +apply-to-server: applies a message to the event logs
::
::    We need the full path identity to be in the node and the signature and
::    checked to prevent weird replay attacks?
::
++  apply-to-server
  |=  $:  now=@da
          app-map=(map @t vase)
          signed-message=client-to-server:common
          original-state=node:server
      ==
  ^-  [(list server-to-client:common) _original-state]
  ::
  |^  ::  there are two things which cause broadcast changes: events sent as part
      ::  of the [%log ...] message from +on-process-event and the toplevel
      ::  [%accept-and-invite-member @p] return value.
      ::
      =/  ret=[=vase changes=(list server-to-client:common) state=_original-state]
        %-  recurse  :*
          (slop !>(now) !>(top-signature.signed-message))
          route.signed-message
          route.signed-message
          message-signature.signed-message
          route.signed-message
          message.signed-message
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
        [changes:ret state:ret]
      ::
          %reject
        [~ original-state]
      ::
          %accept-and-invite-member
        ::
        %^  record-change  changes.ret  [state.ret /]
        [%toplevel-invite ship.ret-val]
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
            message-signature=full-signature
            original-path=path
            message=*
            changes-and-state
        ==
    ^-  [vase changes-and-state]
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
      =/  args  :(slop !>(route) parent-event snapshot.snapshot.state private-state.state)
      =/  raw-result  (sped (slam on-route args))
      ::  the raw-result is either a return-event or a child-event.
      ::
      ?:  =(%l q:(slot 2 raw-result))
        ::  the raw-result is a return vase. send it upwards.
        ::
        =/  return-vase=vase  (slot 3 raw-result)
        ~&  [%got-early-return-during-route return-vase]
        [return-vase changes state]
      ::  the raw-result is a child-event. send it downwards.
      ::
      =/  child-event=vase  (slot 3 raw-result)
      ::
      =/  n=[return-value=vase changes-and-state]
        (recurse child-event t.route full-path message-signature original-path message changes u.sub-node)
      =/  return-value  return-value.n
      =.  changes  changes.n
      =.  u.sub-node    state.n
      ::
      =.  children.state  (~(put by children.state) i.route u.sub-node)
      ::
      (process-child-returned full-path app-vase return-value changes state)
    ::  we've reached the node we're trying to talk to.
    ::
    ::  validate the incoming message as a user event.
    ::
    =/  user-event-mold=vase     (slap app-vase [%limb %user-event])
    =/  user-event=vase  (slam user-event-mold %noun message)
    ::  now process the event
    ::
    =/  on-process-event=vase  (slap app-vase [%limb %on-process-event])
    =/  args  :(slop parent-event user-event snapshot.snapshot.state private-state.state)
    =/  raw-result  (slam on-process-event args)
    ::
    =/  response=on-process-response  (process-sump (slot 2 raw-result))
    =.  private-state.state  (slot 3 raw-result)
    ::
    ?-    -.response
        %log
      ::
      =/  nu=changes-and-state
        %^  record-change  changes  [state full-path]
        [%log [~ message-signature original-path user-event] server-event.response]
      ::
      [return-event.response changes.nu state.nu]
    ::
        %create
      ::
      =/  new-route=path  (weld full-path [sub-id.response ~])
      ::
      =/  created=node:server
        (instantiate-node app-map [new-route [app-type signature-type ~]:response])
      ::
      =/  n=[return-value=vase changes-and-state]
        (recurse child-event.response / new-route message-signature original-path message changes created)
      =/  return  return-value.n
      =.  changes  changes.n
      =.  created    state.n
      ::
      =.  children.state  (~(put by children.state) sub-id.response created)
      ::
      =/  nu=changes-and-state
        %^  record-change  changes  [state full-path]
        [%create sub-id.response app-type.response signature-type.response server-event.response]
      ::
      (process-child-returned full-path app-vase return changes.nu state.nu)
    ::
        %return
      ::  when we receive a %return value, we pass the value up to the callers
      ::
      [return-event.response changes state]
    ==
  ::  +record-change: applies a :log-entry to the event log and the outbound changes.
  ::
  ++  record-change
    |=  $:  changes=(list server-to-client:common)
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
    =.  next-event-id.state  +(next-event-id.state)
    ::
    =.  snapshot.state
      (apply-event-log-item-to-state app-map log-entry snapshot.state)
    ::
    [changes state]
  ::
  ++  process-child-returned
    |=  $:  full-path=path
            app-vase=vase
            child-returned=vase
            changes=(list server-to-client:common)
            state=node:server
        ==
    ^-  [vase (list server-to-client:common) node:server]
    ::
    =/  on-child-return=vase  (slap app-vase [%limb %on-child-return])
    =/  args=vase  :(slop child-returned private-state.state)
    ::
    =/  raw-result  (slam on-child-return args)
    ::
    =/  response=on-child-response  (child-sump (slot 2 raw-result))
    =.  private-state.state  (slot 3 raw-result)
    ::
    ?-    -.response
        %log
      ::
      =/  nu=changes-and-state
        %^  record-change  changes  [state full-path]
        [%log ~ server-event.response]
      ::
      [return-event.response changes.nu state.nu]
    ::
        %return
      ::  when we receive a %return value, we pass the value up to the callers
      ::
      [return-event.response changes state]
    ==
  --
--
