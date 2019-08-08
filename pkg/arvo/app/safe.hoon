/-  safe-gall, safe-client, safe-server
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
        ::  messages that we intend to send, but that we lack up-to-date data
        ::  in our client-communities state
        ::
        pending-messages-to-send=(jug [host=@p name=@t =path] msg=*)
    ==
  +$  peek-data  _!!
  +$  in-poke-data
    poke:safe-gall
  +$  out-poke-data
    $%  ::  receive message to be sent to a foreign :safe server.
        ::
        [%safe-poke %receive-message name=@t =client-to-server:common]
        ::  unlink ourselves as soon as we start.
        ::
        [%drum-unlink =dock]
    ==
  +$  out-peer-data
    peer:safe-gall
  +$  in-peer-data
    peer:safe-gall
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
  ?~  community=(~(get by client-communities.app-state) [host name])
    ::  If we don't know anything about this community, we need to go remote
    ::  subscribe to it.
    ::
    (enqueue-message-and-subscribe host name / `msg app-state)
  ::
  =/  e  (sign-user-event our.bowl now.bowl eny.bowl path msg u.community safe-applets)
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
    ::
    (enqueue-message-and-subscribe host name p.e `msg app-state)
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
  ;<  ~  bind:m  (poke-app [host %safe] %safe-poke %receive-message name p.e)
  (pure:m app-state)
::  +enqueue-message-and-subscribe
::
++  enqueue-message-and-subscribe
  |=  [host=@p name=@t =path maybe-msg=(unit *) =app-state]
  =/  m  tapp-async
  ^-  form:m
  ::
  =?  pending-messages-to-send.app-state  ?=(^ maybe-msg)
    (~(put ju pending-messages-to-send.app-state) [host name path] u.maybe-msg)
  ::
  ::  TODO: If we're already %pending, don't send a second peer.
  ::
  =.  app-state
    %-  update-client-communities  :*
      host
      name
      path
      app-state
      (set-subscription-state-on-client %pending)
    ==
  ::  TODO: Make library fun where peer will wait for first diff and return.
  ::
  ;<  ~  bind:m  (peer-app [host %safe] (weld [name ~] path))
  ::
  (pure:m app-state)
::  +retry-sending-messages:
::
++  retry-sending-messages
  |=  [=bowl:gall host=@p name=@t =path msgs=(list *) =app-state]
  =/  m  tapp-async
  ::
  |-  ^-  form:m
  =*  loop  $
  ::
  ?~  msgs
    (pure:m app-state)
  ::
  ;<  new-state=^app-state  bind:m
    (send-message bowl host name path i.msgs app-state)
  ::
  loop(msgs t.msgs, app-state new-state)
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
  ;<  ~  bind:m  (give-result (weld [name ~] path.i.changes) %safe-peer peer-diff.i.changes)
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
  ::
  =/  command  command.in-poke-data
  ::
  ?-    -.command
      %create-community
    ::
    ?.  =(our.bowl src.bowl)
      ~&  [%remote-cant-create-community src.bowl name.command]
      (pure:m app-state)
    ::  if the community already exists
    ::
    ?:  (~(has by server-communities.app-state) name.command)
      ~&  [%community-already-exists name.command]
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
                initial-invitees.command
                name.command
                our.bowl
        ==  ==
    ::
    =.  server-communities.app-state
      %+  ~(put by server-communities.app-state)  name.command
      initial-toplevel-server-state
    ::  create the client side community
    ::
    ::    When we make a community, we also build a local copy of it as the
    ::    general users would see. We do this because sending messages to the
    ::    server requires a client copy to do the signing, and the person who
    ::    owns this server almost certainly wants to participate in the
    ::    community but their archive shouldn't be public.
    ::
    =.  app-state  (local-subscribe our.bowl name.command / app-state)
    ::
    ~&  [%created-community name.command initial-invitees.command]
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
      host.command
      name.command
      path.command
      data.command
      app-state
    ==
  ::
      %receive-message
    ::  note: anyone can call our receive-message message
    ::
    %-  receive-message  :*
      our.bowl
      name.command
      client-to-server.command
      app-state
    ==
  ==
::
++  handle-peer
  |=  =path
  =/  m  tapp-async
  ^-  form:m
  ::
  ?:  =(/sole path)
    ;<  ~  bind:m  (poke-app:stdio [[our %hood] [%drum-unlink our dap]]:bowl)
    (pure:m app-state)
  ::
  ?>  ?=(^ path)
  ::
  =/  community-name=@t  i.path
  =/  route=^path        t.path
  ::  ensure the community exists
  ::
  ?~  community=(~(get by server-communities.app-state) community-name)
    ~&  [%no-community community-name]
    ;<  ~  bind:m  (give-result path %safe-peer %not-found ~)
    ;<  ~  bind:m  (quit-app [src.bowl %safe] path)
    (pure:m app-state)
  ::  ensure the requester has access
  ::
  ?.  (~(has in invited:(need top-state:(need snapshot.u.community))) src.bowl)
    ~&  [%bad-access path src.bowl]
    ;<  ~  bind:m  (give-result path %safe-peer %not-found ~)
    ;<  ~  bind:m  (quit-app [src.bowl %safe] path)
    (pure:m app-state)
  ::  send a full snapshot of route
  ::
  =/  full-snapshot=(unit peer-diff:common)
    (get-snapshot-as-peer-diff route u.community)
  ::  if the community doesn't exist, send a community not found event.
  ::
  ?~  full-snapshot
    ~&  [%node-not-found route]
    ;<  ~  bind:m  (give-result path %safe-peer %not-found ~)
    ;<  ~  bind:m  (quit-app [src.bowl %safe] path)
    (pure:m app-state)
  ::  send the snapshot as the first of many events
  ::
  ;<  ~  bind:m  (give-result path %safe-peer u.full-snapshot)
  ::
  (pure:m app-state)
::
++  handle-diff
  |=  [[her=ship app=term] =path tagged-data=in-peer-data]
  =/  m  tapp-async
  ^-  form:m
  ::
  ~&  [%handle-diff path tagged-data]
  ::
  ?>  ?=(^ path)
  ::
  =/  community-name=@t  i.path
  =/  route=^path        t.path
  ::
  =/  data  command.tagged-data
  ::
  ?:  ?=(%not-found -.data)
    ::  TODO: Some real error handling here. If we're pending for a send
    ::  operation, then we'll need to error the send operation.
    ::
    ~&  [%not-found community-name path]
    ::
    (pure:m app-state)
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
  ::
  ~&  [%new-app-state app-state]
  ::  for every pending message in the :pending-messages-to-send, try to send
  ::  it again.
  ::
  =/  to-send=(set *)
    (~(get ju pending-messages-to-send.app-state) [her community-name route])
  ::
  =.  pending-messages-to-send.app-state
    (~(del by pending-messages-to-send.app-state) [her community-name route])
  ::
  (retry-sending-messages bowl her community-name route ~(tap by to-send) app-state)
  ::  ::
  ::  ::  TODO: MUCH LATER we probably want to forward on the update to any of our
  ::  ::  http subscribed clients.
  ::  (pure:m app-state)
::
::  TODO: Handle +quit so that it sets a community back to %unsubscribed.
::
++  handle-take
  |=  =sign:tapp
  =/  m  tapp-async
  ^-  form:m
  ~&  [%s sign]
  (pure:m app-state)
--

