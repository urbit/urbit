/-  safe-client, safe-server
/+  tapp, stdio, safe-applets, *safe-server, *safe-client
=>
  |%
  +$  subscription-state
    $:  target=[her=ship app=term]
        =path
    ==
  +$  app-state
    $:  %0
        ::  communities we are hosting
        ::
        server-communities=(map name=@t node:safe-server)
        ::  communities we are subscribed to
        ::
        client-communities=(map [host=@p name=@t] node:safe-client)
        ::  our current local subscriptions
        ::
        ::    These are non-peer based subscriptions between a client copy of a
        ::    community and the server copy in the current application.
        ::
        local-subscriptions=(set [name=@t =path])
    ==
  +$  peek-data  _!!
  +$  in-poke-data
    $%  ::  a local action which can only be done by :our
        ::
        [%create-community name=@t initial-invitees=(set @p)]
        ::  a local action which can only be done by :our
        ::
        [%send-message host=@p name=@t =path data=*]
        ::  a poke which anyone can do.
        ::
        [%receive-message name=@t =client-to-server:common]
    ==
  +$  out-poke-data
    $%  ::  receive message to be sent to a foreign :safe server.
        ::
        [%receive-message name=@t =client-to-server:common]
        ::  unlink ourselves as soon as we start.
        ::
        [%drum-unlink =dock]
    ==
  +$  out-peer-data
    in-peer-data
  +$  in-peer-data
    $%  ::  from +peer-diff:common
        ::
        [%snapshot id=@u snapshot=transport-snapshot:common]
        ::  from +peer-diff:common
        ::
        [%event id=@u event=transport-event-log-item:common]
        ::  sent when the host community either doesn't exist or the requester
        ::  isn't allowed to access it.
        ::
        [%not-found ~]
    ==
  ++  tapp   (^tapp app-state peek-data in-poke-data out-poke-data in-peer-data out-peer-data)
  ++  stdio  (^stdio out-poke-data out-peer-data)
  --
::
=,  async=async:tapp
=,  tapp-async=tapp-async:tapp
=,  stdio
=*  default-tapp  default-tapp:tapp
::
|%
::  +set-subscription-state-on-client: modifies a client node's subscription
::
++  set-subscription-state-on-client
  |=  new-state=subscribed:safe-client
  |=  client-state=node:safe-client
  ^+  client-state
  client-state(subscribed new-state)
::  +apply-peer-diff-to-client: applies a peer diff
::
++  apply-peer-diff-to-client
  |=  =peer-diff:common
  |=  client-state=node:safe-client
  ^+  client-state
  ::
  =.  subscribed.client-state  %subscribed
  =.  client-state
    (apply-peer-diff-to-node safe-applets peer-diff client-state)
  ::
  client-state
::  +update-client-communities: ensure :full-route exists and run :fun on it.
::
++  update-client-communities
  |=  [host=@p name=@t full-route=path =app-state fun=$-(node:safe-client node:safe-client)]
  ^+  app-state
  ::
  %_      app-state
      client-communities
    ::  if we don't have any entries yet, we have to make the entire entry
    ::
    ?.  (~(has by client-communities.app-state) [host name])
      %+  ~(put by client-communities.app-state)  [host name]
      (apply-to-client-node full-route *node:safe-client fun)
    ::  otherwise, we modify the current one in place
    ::
    %+  ~(jab by client-communities.app-state)  [host name]
    |=  =node:safe-client
    (apply-to-client-node full-route node fun)
  ==

::  +local-subscribe: subscribes locally to a not
::
++  local-subscribe
  |=  [our=@p name=@t full-route=path =app-state]
  ^+  app-state
  ::  the server side must exist
  ::
  =/  server=node:safe-server  (~(got by server-communities.app-state) name)
  ::  already subscribed?
  ::
  ?:  (~(has in local-subscriptions.app-state) [name full-route])
    app-state
  ::
  =.  local-subscriptions.app-state
    (~(put in local-subscriptions.app-state) [name full-route])
  ::
  =/  full-server-state=(unit peer-diff:common)
    (get-snapshot-as-peer-diff full-route server)
  ::
  %-  update-client-communities  :*
    our
    name
    full-route
    app-state
    (apply-peer-diff-to-client (need full-server-state))
  ==
::  +send-message: sends a message to a node, validating the message.
::
::    To know how to construct the requested signature, +send-message will
::    first ensure that you're subscribed to the root node, the target node,
::    and any parent nodes above the target mode recursively on the %inherit
::    signature type.
::
++  send-message
  |=  [=bowl:gall host=@p name=@t =path msg=* =app-state]
  =/  m  tapp-async
  ^-  form:m
  ::
  |-
  =*  loop  $
  ::  Using our client copy of the state, perform verification.
  ::
  =/  community  (~(got by client-communities.app-state) [host name])
  ::
  =/  e  (sign-user-event our.bowl now.bowl eny.bowl path msg community safe-applets)
  ::  check if we made the signature. when we can't make the signature because
  ::  we're lacking information, we make a subscription and then try again.
  ::
  ?:  ?=([%& *] e)
    ::  if the missing information is on our own local server state, then we
    ::  can just go and fetch it synchronously.
    ::
    ?:  =(our.bowl host)
      =.  app-state  (local-subscribe our.bowl name p.e app-state)
      loop
    ::    TODO: This still isn't exactly correct? we need to not just
    ::    +peer-app, but wait for the first response from the peer-app.
    ::
    ::    Instead of "loop"-ing, we'll have to add this to some sort of queue,
    ::    where we then retry the +send-message when we get our first %diff
    ::    back.
    ::
    ::    TODO: The whole %subscribed/%pending thing in client-state needs a
    ::    way to remove %pending nodes which turn out to not actually exist.
    =.  app-state
      %-  update-client-communities  :*
        host
        name
        path
        app-state
        (set-subscription-state-on-client %pending)
      ==
    ::
    ;<  ~  bind:m  (peer-app [host %safe] (weld [name ~] path))
    ::
    loop
  ::
  ?:  =(host our.bowl)
    ::  we do the special case where we synchronously call ourselves for
    ::  messages to ourselves.
    ::
    ~&  %sending-to-self
    (receive-message host name p.e app-state)
  ::  sends to a remote server.
  ::
  ~&  %sending-remotely
  ;<  ~  bind:m  (poke-app [host %safe] %receive-message name p.e)
  (pure:m app-state)
::  +receive-message: receives a message to apply to our server state
::
::    This first applies a message to our server state, which may then produce
::    changes that need to be synced to clients who are subscribed.
::
++  receive-message
  |=  [host=@p name=@t =client-to-server:common =app-state]
  =/  m  tapp-async
  ^-  form:m
  ::
  ?~  community=(~(get by server-communities.app-state) name)
    ~&  [%no-such-colmmunity name]
    (pure:m app-state)
  ::
  =^  changes  u.community
    %-  apply-to-server  :*
      safe-applets
      client-to-server
      u.community
    ==
  ::
  =.  server-communities.app-state
    (~(put by server-communities.app-state) name u.community)
  ::  we need to emit the changes to all the local subscriptions, so filter
  ::  changes just to the set we're locally subscribed to
  ::
  =/  locally-subscribed-changes=(list server-to-client:common)
    %+  skim  changes
    |=  [=path =peer-diff:common]
    ^-  ?
    (~(has in local-subscriptions.app-state) [name path])
  ::  apply each of the changes per above to the community
  ::
  =.  client-communities.app-state
    %+  ~(jab by client-communities.app-state)  [host name]
    |=  =node:safe-client
    ^+  node
    ::
    ?~  locally-subscribed-changes
      node
    ::
    %_    $
        locally-subscribed-changes
      t.locally-subscribed-changes
    ::
        node
      ~&  [%applying-change i.locally-subscribed-changes]
      (apply-to-client safe-applets i.locally-subscribed-changes node)
    ==
  ::  broadcast each of the changes to all listeners.
  ::
  |-
  ^-  form:m
  =*  loop  $
  ::
  ?~  changes
    (pure:m app-state)
  ::
  ;<  ~  bind:m  (give-result (weld [name ~] path.i.changes) peer-diff.i.changes)
  ::
  loop(changes t.changes)
--
::
%-  create-tapp-all:tapp
^-  tapp-core-all:tapp
|_  [=bowl:gall =app-state]
::
++  handle-peek  handle-peek:default-tapp
::
++  handle-init
  =/  m  tapp-async
  ^-  form:m
  ::
  ;<  ~  bind:m  (poke-app:stdio [[our %hood] [%drum-unlink our dap]]:bowl)
  (pure:m app-state)

++  handle-poke
  |=  =in-poke-data
  =/  m  tapp-async
  ^-  form:m
  ?-    -.in-poke-data
      %create-community
    ::
    ?.  =(our.bowl src.bowl)
      ~&  [%remote-cant-create-community src.bowl name.in-poke-data]
      (pure:m app-state)
    ::  if the community already exists
    ::
    ?:  (~(has by server-communities.app-state) name.in-poke-data)
      ~&  [%community-already-exists name.in-poke-data]
      (pure:m app-state)
    ::  create the server side community
    ::
    ::    This is the community state which is the currently running 
    ::
    =/  initial-toplevel-server-state
      %-  instantiate-node
        :*  safe-applets
            /
            %auth
            %community
            :*  ~
                initial-invitees.in-poke-data
                name.in-poke-data
                our.bowl
        ==  ==
    ::
    =.  server-communities.app-state
      %+  ~(put by server-communities.app-state)  name.in-poke-data
      initial-toplevel-server-state
    ::  create the client side community
    ::
    ::    When we make a community, we also build a local copy of it as the
    ::    general users would see. We do this because sending messages to the
    ::    server requires a client copy to do the signing, and the person who
    ::    owns this server almost certainly wants to participate in the
    ::    community but their archive shouldn't be public.
    ::
    =.  app-state  (local-subscribe our.bowl name.in-poke-data / app-state)
    ::
    ~&  [%created-community name.in-poke-data initial-invitees.in-poke-data]
    ::
    ~&  [%client-communities client-communities.app-state]
    ::
    (pure:m app-state)
  ::
      %send-message
    ::  only we can send a message as ourselves
    ::
    ?.  =(our.bowl src.bowl)
      ~&  [%remote-cant-send-message src.bowl]
      (pure:m app-state)
    ::
    ~&  [%full-community client-communities.app-state]
    ::
    %-  send-message  :*
      bowl
      host.in-poke-data
      name.in-poke-data
      path.in-poke-data
      data.in-poke-data
      app-state
    ==
  ::
      %receive-message
    ::  note: anyone can call our receive-message message
    ::
    %-  receive-message  :*
      our.bowl
      name.in-poke-data
      client-to-server.in-poke-data
      app-state
    ==
  ==
::
++  handle-peer
  |=  =path
  =/  m  tapp-async
  ^-  form:m
  ::
  ~|  path
  ?>  ?=(^ path)
  ::
  =/  community-name=@t  i.path
  =/  route=^path        t.path
  ::  ensure the community exists
  ::
  ?~  community=(~(get by server-communities.app-state) community-name)
    ;<  ~  bind:m  (give-result path [%not-found ~])
    ;<  ~  bind:m  (quit-app [src.bowl %safe] path)
    (pure:m app-state)
  ::  ensure the requester has access
  ::
  ?.  (~(has in invited:(need top-state:(need snapshot.u.community))) src.bowl)
    ;<  ~  bind:m  (give-result path [%not-found ~])
    ;<  ~  bind:m  (quit-app [src.bowl %safe] path)
    (pure:m app-state)
  ::  send a full snapshot of route
  ::
  =/  full-snapshot=(unit peer-diff:common)
    (get-snapshot-as-peer-diff route u.community)
  ::  if the community doesn't exist, send a community not found event.
  ::
  ?~  full-snapshot
    ;<  ~  bind:m  (give-result path [%not-found ~])
    ;<  ~  bind:m  (quit-app [src.bowl %safe] path)
    (pure:m app-state)
  ::  send the snapshot as the first of many events
  ::
  ;<  ~  bind:m  (give-result path u.full-snapshot)
  ::
  (pure:m app-state)
::
++  handle-diff
  |=  [[her=ship app=term] =path data=in-peer-data]
  =/  m  tapp-async
  ^-  form:m
  ::
  ?>  ?=(^ path)
  ::
  =/  community-name=@t  i.path
  =/  route=^path        t.path
  ::
  ?:  ?=(%not-found -.data)
    ::  TODO: Some real error handling here. If we're pending for a send
    ::  operation, then we'll need to error the send operation.
    ::
    ~&  [%not-found community-name path]
    ::
    (pure:m app-state)
  ::  TODO: If this node is pending, we'll need to keep track of that before we
  ::  apply the diff
  ::
  =/  is-pending=?  %.n
  ::  our data is a peer diff, apply it to our state
  ::
  =.  app-state
    %-  update-client-communities  :*
      her
      community-name
      route
      app-state
      (apply-peer-diff-to-client data)
    ==
  ::
  ?:  is-pending
    ::  TODO: if we were pending for some reason, we'll have to notify what
    ::  we're waiting on to complete.
    ::
    (pure:m app-state)
  ::
  ::  TODO: MUCH LATER we probably want to forward on the update to any of our
  ::  http subscribed clients.
  (pure:m app-state)
::
::  TODO: Handle +quit so that it sets a community back to %unsubscribed.
::
++  handle-take  handle-take:default-tapp
--

