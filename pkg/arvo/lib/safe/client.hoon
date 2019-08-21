/-  safe-applet, *safe-client, common=safe-common
/+  signatures=safe-signatures, *safe-common
::  Safe client library
::
::    The client library contains all the functionality for working with the
::    node:client data structure, which represents the client side view of the
::    world.
::
|%
::  +from-transport: changes
::
::    TODO: I've spent the entire project so far thinking in terms of transpor
::
++  from-transport
  |%
  ++  event-log-item
    |=  [app-map=(map @t vase) app=term e=transport-event-log-item:common]
    ^-  (unit event-log-item:common)
    ::
    ?-    -.e
        ?(%toplevel-init %toplevel-invite %init)
      `e
    ::
        %log
      ::
      =/  node-vase=vase           (~(got by app-map) app)
      ::
      =/  private-event-mold=vase  (slap node-vase [%limb %private-event])
      =/  private-event=vase  (slam private-event-mold %noun private-event.e)
      ::
      ?~  user-event.e
        `[%log ~ private-event]
      ::
      =/  user-event-mold=vase     (slap node-vase [%limb %user-event])
      =/  user-event=vase  (slam user-event-mold %noun user-event.u.user-event.e)
      ::
      `[%log [~ msg-signature.u.user-event.e route.u.user-event.e user-event] private-event]
    ::
        %create
      ::
      ::  Is this the simple case of creation without a private event? Then we can
      ::
      ?~  private-event.e
        `[%create sub-id.e app-type.e signature-type.e ~]
      ::
      =/  node-vase=vase           (~(got by app-map) app)
      ::
      =/  private-event-mold=vase  (slap node-vase [%limb %private-event])
      =/  private-event=vase  (slam private-event-mold %noun u.private-event.e)
      ::
      `[%create sub-id.e app-type.e signature-type.e `private-event]
    ==
  ::
  ++  snapshot
    |=  [app-map=(map @t vase) s=transport-snapshot:common]
    ^-  snapshot:common
    ::
    =/  node-vase=vase           (~(got by app-map) app-type.s)
    ::
    =/  snapshot-mold=vase  (slap node-vase [%limb %snapshot])
    =/  snapshot-vase=vase  (slam snapshot-mold %noun raw-snapshot.s)
    ::
    [app-type.s top-state.s signature-type.s snapshot-vase children.s]
  --
::
++  apply-evnet-log-item-to-children-state
  |=  [=event-log-item:common children=(map @t (unit node))]
  ^-  (map @t (unit node))
  ::
  ?+      -.event-log-item
        children
      %create
    (~(put by children) sub-id.event-log-item ~)
  ==
::  +apply-to-client-node: given a path, modify the node at that path
::
::    We have actions which walk through the
::
++  apply-to-client-node
  |=  [route=path client-state=node action=$-(node node)]
  ^-  node
  ::
  ?^  route
    =/  child-state=node
      %_    $
          route  t.route
      ::
          client-state
        ?~  child-state=(~(get by children.client-state) i.route)
          ::  this is the first time we've even heard of this node
          *node
        ?~  u.child-state
          ::  we've heard about this node before, but know nothing about it
          *node
        u.u.child-state
      ==
    ::
    client-state(children (~(put by children.client-state) i.route `child-state))
  ::
  (action client-state)
::
++  apply-peer-diff-to-node
  |=  [app-map=(map @t vase) =peer-diff:common =node]
  ^+  node
  ::
  ?-    -.peer-diff
      ::  we have a completely new snapshot which we append to history
      ::
      %snapshot
    ::
    =/  snapshot=snapshot:common
      (snapshot:from-transport app-map snapshot.peer-diff)
    ::  if the sequence id in this peer-diff is the same as the latest, we can
    ::  ignore this peer-diff because we already know its contents.
    ::
    ?:  ?&  ?=(^ partial-event-log.node)
            =(id.peer-diff id.i.partial-event-log.node)
        ==
      node
    ::
    ::  TODO: When we get a snapshot, we need to reconstruct the :archived
    ::  state here from the embedded snapshot, archiving all nodes that we have
    ::  data about which are missing from the snapshot.
    ::
    %_    node
        partial-event-log
      :_  partial-event-log.node
      [id.peer-diff %snapshot snapshot]
    ::
        snapshot
      [~ snapshot]
    ::
        children
      =/  pairs=(list (pair @t (unit ^node)))  ~(tap by children.node)
      ::  we remove any children which aren't in snapshot-children.
      ::
      =.  pairs  (skim pairs |=([a=@t *] (~(has in children.snapshot) a)))
      ::  we add any children which aren't in pairs
      ::
      =/  m=(map @t (unit ^node))  (my pairs)
      =/  to-add=(list @t)  ~(tap in children.snapshot)
      |-
      ?~  to-add
        m
      ::
      ?:  (~(has by m) i.to-add)
        $(to-add t.to-add)
      ::
      $(to-add t.to-add, m (~(put by m) i.to-add ~))
    ::
    ::  TODO: Also modify the archive state once I have archiving working.
    ==
  ::
      %event
    ::
    =/  event-item=event-log-item:common
      %-  need
      %^  event-log-item:from-transport  app-map
        app-type:(need snapshot.node)
      event.peer-diff
    ::
    %_    node
        partial-event-log
      :_  partial-event-log.node
      [id.peer-diff %event event-item]
    ::
        snapshot
      `(apply-event-log-item-to-state app-map event-item (need snapshot.node))
    ::
        children
      (apply-evnet-log-item-to-children-state event-item children.node)
    ::
    ::  TODO: Also modify the archive state once I have archiving working.
    ==
  ==
::  +applies a diff to a client's state
::
++  apply-to-client
  |=  [app-map=(map @t vase) msg=server-to-client:common client-state=node]
  ^-  node
  ::
  %^  apply-to-client-node  path.msg  client-state
  ::
  |=  =node
  (apply-peer-diff-to-node app-map peer-diff.msg node)
::
::  +signature-type-request-for: changes an abstract signature-type into a
::  signature-type-request for route.
::
::    This operation requires us to know information about the final 
::
++  signature-type-request-for
  |=  [route=path client-state=node]
  ^-  (each path (unit signature-type-request:common))
  ::
  =/  root-state  client-state
  ::
  |^  (search route client-state)
  ::  +search: attempts to find the node to operate on.
  ::
  ++  search
    |=  [route=path client-state=node]
    ^-  (each path (unit signature-type-request:common))
    ::
    =|  built-route=path
    |-
    ::
    ?^  route
      ?:  ?=(?(%unsubscribed %pending) subscribed.client-state)
        ::  if we're unsubscribed, we need to become subscribed
        ::
        [%& built-route]
      ::  if we don't know anything about children.client-state despite being
      ::  subscribed, then this is an invalid node.
      ?~  children-state=(~(get by children.client-state) i.route)
        [%| ~]
      ::  update the built-route so we're looking at the child node.
      ::
      =.  built-route  (weld built-route [i.route ~])
      ::  if we do know that children.client-state exists, but know nothing
      ::  about it, also block while we look it up.
      ::
      ?~  u.children-state
        ~&  [%no-u-children i.route]
        [%& built-route]
      ::
      =/  ret-val=(each path (unit signature-type-request:common))
        %_  $
          route         t.route
          client-state  u.u.children-state
        ==
      ::  if we're blocked, propagate the block
      ::
      ?:  ?=([%& *] ret-val)
        ~&  [%propagate-block i.route]
        [%& p.ret-val]
      ::
      ?~  p.ret-val
        =/  n  (get-for-node built-route client-state)
        ?-  -.n
          %&  [%& p.n]
          %|  [%| p.n]
        ==
      ::
      [%| p.ret-val]
    ::
    =/  n  (get-for-node built-route client-state)
    ?-  -.n
      %&  [%& p.n]
      %|  [%| p.n]
    ==
  ::
  ++  get-for-node
    |=  [built-route=path client-state=node]
    ^-  (each path (unit signature-type-request:common))
    ::  if we don't have the root state, we can't do anything here.
    ::
    ?~  snapshot.root-state
      [%& /]
    ::
    ?~  top-state.u.snapshot.root-state
      [%& /]
    ::
    =/  =top-state:common  u.top-state.u.snapshot.root-state
    ::
    =/  =snapshot:common   (need snapshot.client-state)
    ?-    signature-type.snapshot
        %ship
      [%| `[%ship ~]]
    ::
        %unlinked
      [%| `[%unlinked invited.top-state]]
    ::
        %community
      :*  %|
          ~
          %linked
          [community-name.top-state original-host.top-state /]
          invited.top-state
      ==
    ::
        %self
      :*  %|
          ~
          %linked
          [community-name.top-state original-host.top-state built-route]
          invited.top-state
      ==
    ::
        %inherit
      [%| ~]
    ==
  --
::  +build-signing-request:
::
::    XXX: Next paragraph is wrong
::
::
::    +sign-user-event returns either a path that we aren't subscribed to or a
::    completed signature. Since +sign-user-event requires information from the
::    server--mainly the node type and the signature type--we must abort trying
::    to perform a signature when we lack that information. Our caller must
::    instead perform the subscription and retry once it has the requisite
::    data.
::
::    TODO: It feels like a smell that app-map isn't part of a client-state,
::    which isn't reflected in the server-state. If we're going to let people
::    customize this thing, think about the source code being in the node?
::
::    TODO: What happens to old logs when the application vases change? This
::    was a large part of why I wanted the source code to the manipulation in
::    the nodes, even if it made upgrading harder.
::
++  build-signing-request
  |=  [route=path user-event=* client-state=node app-map=(map @t vase)]
  ^-  (each path (unit signing-request:common))
  ::
  =/  root-request  (signature-type-request-for / client-state)
  ?:  ?=([%& *] root-request)
    [%& p.root-request]
  ::
  ?~  p.root-request
    [%| ~]
  ::
  =/  path-request  (signature-type-request-for route client-state)
  ?:  ?=([%& *] path-request)
    [%& p.path-request]
  ::
  ?~  p.path-request
    [%| ~]
  ::
  =/  app-type=@t
    |-
    ?^  route
      $(route t.route, client-state (need (~(got by children.client-state) i.route)))
    app-type:(need snapshot.client-state)
  ::  validate the user-message against the +user-event mold for this applet
  ::
  ::    what we sign and send is not the exact user-event, but the one passed
  ::    through the applet's user-event mold for validation, since this data
  ::    will be explicitly validated on the server.
  ::
  =/  node-vase=vase        (~(got by app-map) app-type)
  =/  user-event-mold=vase  (slap node-vase [%limb %user-event])
  =/  user-event=vase       (slam user-event-mold %noun user-event)
  ::
  :*  %|
      ~
      u.p.root-request
      u.p.path-request
      route
      q.user-event
  ==
::  +get-data-at: returns the data for display at route
::
::    +get-data-at returns either a path that we aren't subscribed to which we
::    must subscribe to, or it returns an unit data, which is ~ if we know the
::    node doesn't currently exist, and the data if it does.
::
::    TODO: In the case of no data, we shouldn't return ~ until we've heard
::    back from the server that the path doesn't exist.
::
++  get-data-at
  |=  [route=path client-state=node]
  ::
  |^  ^-  (each path (unit [=snapshot:common archives=(list @t)]))
      =|  built-route=path
      |-
      ^-  (each path (unit [=snapshot:common archives=(list @t)]))
      ::
      ?^  route
        ?:  ?=(?(%unsubscribed %pending) subscribed.client-state)
          ::  if we're unsubscribed, we need to become subscribed
          ::
          [%& built-route]
        ::  TODO: archive checking goes here?
        ::
        ::  if we don't know anything about children.client-state despite being
        ::  subscribed, then this is an invalid node.
        ?~  children-state=(~(get by children.client-state) i.route)
          [%| ~]
        ::  update the built-route so we're looking at the child node.
        ::
        =.  built-route  (weld built-route [i.route ~])
        ::  if we do know that children.client-state exists, but know nothing
        ::  about it, also block while we look it up.
        ::
        ?~  u.children-state
          [%& built-route]
        ::
        %_  $
          route         t.route
          client-state  u.u.children-state
        ==
      ::
      [%| (get-for-node built-route client-state)]
  ::
  ++  get-for-node
    |=  [built-route=path client-state=node]
    ^-  (unit [=snapshot:common archives=(list @t)])
    ::  if we don't have the snapshot here, things have gone rather wrong.
    ::
    ?~  snapshot.client-state
      ~
    `[u.snapshot.client-state ~(tap in ~(key by archived.client-state))]
  --
--
