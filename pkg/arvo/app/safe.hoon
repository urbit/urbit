/-  safe-gall, safe-client, safe-server, ring-sur=ring, sole-sur=sole
/+  tapp, stdio, safe-applets, *safe-server, *safe-client, safe-signatures, ring-lib=ring
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
        ::  pending remote subscriptions?
        ::
        pending-community-subscriptions=(set [host=@p name=@t =path])
        ::  messages that we intend to send, but that we lack up-to-date data
        ::  in our client-communities state.
        ::
        pending-writes=(jug [host=@p name=@t =path] [route=path msg=*])
        ::  read operations here.
        ::
        pending-reads=(jug [host=@p name=@t =path] path)
    ==
  +$  peek-data  _!!
  +$  in-poke-data
    $%  [%safe-poke command=poke-command:safe-gall]
        [%sole-action *]
    ==
  +$  out-poke-data
    $%  ::  receive message to be sent to a foreign :safe server.
        ::
        [%safe-poke %receive-message name=@t =client-to-server:common]
    ==
  +$  out-peer-data
    out-peer:safe-gall
  +$  in-peer-data
    in-peer:safe-gall
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
    =.  app-state  (enqueue-write host name / path msg app-state)
    ::
    (subscribe bowl host name / app-state)
  ::
  =/  e  (build-signing-request path msg u.community safe-applets)
  ::  check if we made the signature. when we can't make the signature because
  ::  we're lacking information, we make a subscription and then try again.
  ::
  ?:  ?=([%& *] e)
    ~&  [%send-message-blocked-on p.e]
    =.  app-state  (enqueue-write host name p.e path msg app-state)
    ::
    (subscribe bowl host name p.e app-state)
  ::
  ;<  c-to-s=client-to-server:common  bind:m
    (async-sign-request our.bowl now.bowl eny.bowl p.e)
  ::
  ?:  =(host our.bowl)
    ::  we do the special case where we synchronously call ourselves for
    ::  messages to ourselves.
    ::
    (receive-message our.bowl now.bowl name c-to-s app-state)
  ::  sends to a remote server.
  ::
  ;<  ~  bind:m  (poke-app [host %safe] %safe-poke %receive-message name c-to-s)
  (pure:m app-state)
::  +enqueue-write
::
++  enqueue-write
  |=  [host=@p name=@t subscribe-path=path send-path=path msg=* =app-state]
  ^+  app-state
  ::
  %_    app-state
      pending-writes
    %+  ~(put ju pending-writes.app-state)
      [host name subscribe-path]
    [send-path msg]
  ==
::  +enqueue-read
::
++  enqueue-read
  |=  [host=@p name=@t subscribe-path=path read-path=path =app-state]
  ^+  app-state
  ::
  %_    app-state
      pending-reads
    (~(put ju pending-reads.app-state) [host name subscribe-path] read-path)
  ==
::  +subscribe: subscribes remotely or locally
::
++  subscribe
  |=  [=bowl:gall host=@p name=@t subscribe-path=path =app-state]
  =/  m  tapp-async
  ^-  form:m
  ::  we special case local subscriptions so that we don't peer on ourselves.
  ::
  ?:  =(our.bowl host)
    ::  the server side must exist
    ::
    =/  server=node:safe-server  (~(got by server-communities.app-state) name)
    ::  already subscribed?
    ::
    ?:  (~(has in local-subscriptions.app-state) [name subscribe-path])
      (pure:m app-state)
    ::
    =.  local-subscriptions.app-state
      (~(put in local-subscriptions.app-state) [name subscribe-path])
    ::
    =/  full-server-state=(unit peer-diff:common)
      (get-snapshot-as-peer-diff subscribe-path server)
    ::
    =.  app-state
      %-  update-client-communities  :*
        our.bowl
        name
        subscribe-path
        app-state
        (apply-peer-diff-to-client (need full-server-state))
      ==
    ::
    (unblock-pending bowl host name subscribe-path app-state)
  ::  TODO: If we're already %pending, don't send a second peer. This was what
  ::  caused the out of control message spam when I had the incorrect children
  ::  calculation. It's theoretically triggerable during natural usage, too.
  ::
  =.  app-state
    %-  update-client-communities  :*
      host
      name
      subscribe-path
      app-state
      (set-subscription-state-on-client %pending)
    ==
  ::  TODO: Make library fun where peer will wait for first diff and return.
  ::
  ;<  ~  bind:m  (peer-app [host %safe] (weld [name ~] subscribe-path))
  ::
  (pure:m app-state)
::  +unblock-pending:
::
++  unblock-pending
  |=  [=bowl:gall host=@p community-name=@t =path =app-state]
  =/  m  tapp-async
  ::  for every pending message in the :pending-writes, try to send
  ::  it again.
  ::
  =/  to-send=(set [route=^path msg=*])
    (~(get ju pending-writes.app-state) [host community-name path])
  ::
  =.  pending-writes.app-state
    (~(del by pending-writes.app-state) [host community-name path])
  ::  for all pending writes, try sending the message again.
  ::
  ;<  =^app-state  bind:m
    =/  msgs=(list [route=^path msg=*])  ~(tap by to-send)
    ::
    |-  ^-  form:m
    =*  loop  $
    ::
    ?~  msgs
      (pure:m app-state)
    ::
    ;<  new-state=^app-state  bind:m
      (send-message bowl host community-name route.i.msgs msg.i.msgs app-state)
    ::
    loop(msgs t.msgs, app-state new-state)
  ::  for all pending reads, try reading again.
  ::
  =/  to-read=(set ^path)
    (~(get ju pending-reads.app-state) [host community-name path])
  ::
  =.  pending-reads.app-state
    (~(del by pending-reads.app-state) [host community-name path])
  ::
  =/  reads=(list ^path)  ~(tap by to-read)
  ::
  |-  ^-  form:m
  =*  loop  $
  ::
  ?~  reads
    (pure:m app-state)
  ::
  ;<  new-state=^^app-state  bind:m
    (show-path bowl host community-name i.reads app-state)
  ::
  loop(reads t.reads, app-state new-state)
::  +receive-message: receives a message to apply to our server state
::
::    This first applies a message to our server state, which may then produce
::    changes that need to be synced to clients who are subscribed.
::
++  receive-message
  |=  [our=@p now=@da name=@t =client-to-server:common =app-state]
  =/  m  tapp-async
  ^-  form:m
  ::
  ?~  community=(~(get by server-communities.app-state) name)
    ~&  [%no-such-colmmunity name]
    (pure:m app-state)
  ::
  ;<  valid=?  bind:m
    (async-validate-message our now client-to-server u.community)
  ::
  ?.  valid
    ~&  [%ignoring-invalid-message client-to-server]
    (pure:m app-state)
  ::
  =^  changes  u.community
    %-  apply-to-server  :*
      now
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
    %+  ~(jab by client-communities.app-state)  [our name]
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
::
++  display
  |=  [=sole-effect:sole-sur]
  =/  m  (async ,~)
  ^-  form:m
  ::
  (give-result /sole %sole-effect sole-effect)
::
++  show-path
  |=  [=bowl:gall host=@p name=@t =path =app-state]
  =/  m  tapp-async
  ^-  form:m
  ::
  ?~  community=(~(get by client-communities.app-state) [host name])
    ::  If we don't know anything about this community, we need to go remote
    ::  subscribe to it.
    ::
    =.  app-state  (enqueue-read host name / path app-state)
    ::
    (subscribe bowl host name / app-state)
  ::
  =/  action  (nu-get-data-at path u.community)
  ::
  ?-    -.action
      %&
    ~!  action
    =.  app-state  (enqueue-read host name p.action path app-state)
    (subscribe bowl host name p.action app-state)
  ::
      %|
    ?~  p.action
      ;<  ~  bind:m  (display %txt "No such path {<path>}.")
      (pure:m app-state)
    ::  Calculate the signature line
    ::
    ::    We want to show information about what sort of signature will be
    ::    produced here, using red and our ship name for ship signatures, green
    ::    for unlinked, and yellow for signatures on a certain scope.
    ::
    =/  signature-line=styx
      =/  type
        (signature-type-request-for path u.community)
      ?:  ?=([%& *] type)
        ['<internal error>' ~]
      ?-  -.p.type
        %ship      [[[~ ~ `%r] (scot %p our.bowl) ~] ~]
        %linked    [[[~ ~ `%y] (spat route.scope.p.type) ~] ~]
        %unlinked  [[[~ ~ `%g] 'unlinked' ~] ~]
      ==
    ::
    =/  bold=styl  [`%br ~ ~]
    ::
    ;<  ~  bind:m
      %-  display
      :-  %mor  :~
        [%klr [[bold 'Path: ' ~] (spat path) ~]]
        [%klr [[bold 'Type: ' ~] app-type.snapshot.u.p.action ~]]
        [%klr [[bold 'Signatures: ' ~] signature-line]]
        [%tan [(cain snapshot.snapshot.u.p.action) ~]]
      ==
    (pure:m app-state)
  ==
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
  (pure:m app-state)

++  handle-poke
  |=  =in-poke-data
  =/  m  tapp-async
  ^-  form:m
  ::
  ?:  ?=([%sole-action *] in-poke-data)
    :: just ignore the sole actions?
    (pure:m)
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
      ::
      ;<  ~  bind:m  (display %txt "Community '{<name.command>}' already exists.")
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
    ::  TODO: +send-message will subscribe for us? No need to have this here?
    ::
    ::  create the client side community
    ::
    ::    When we make a community, we also build a local copy of it as the
    ::    general users would see. We do this because sending messages to the
    ::    server requires a client copy to do the signing, and the person who
    ::    owns this server almost certainly wants to participate in the
    ::    community but their archive shouldn't be public.
    ::
    ;<  =^app-state  bind:m  (subscribe bowl our.bowl name.command / app-state)
    ::  we send an in-band [%init ~] event so that our community recognizes us
    ::  as a moderator.
    ::
    ;<  =^^app-state  bind:m
      %-  send-message  :*
        bowl
        our.bowl
        name.command
        /
        [%init ~]
        app-state
      ==
    ::
    ;<  ~  bind:m  (display %txt "Created community '{<name.command>}'.")
    (pure:m app-state)
  ::
      %send-message
    ::  only we can send a message as ourselves
    ::
    ?.  =(our.bowl src.bowl)
      ~&  [%remote-cant-send-message src.bowl]
      (pure:m app-state)
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
      now.bowl
      name.command
      client-to-server.command
      app-state
    ==
  ::
      %show
    ::
    ::
    %-  show-path  :*
      bowl
      host.command
      name.command
      path.command
      app-state
    ==
  ::
      %debug
    ~&  [%debug-command command.command]
    ?+  command.command  (pure:m app-state)
        %test-sign
      =/  req=signature-type-request:common
        [%unlinked (sy [~zod ~nec ~fed ~])]
      ::
      ;<  =ring-signature:ring-sur  bind:m
        (sign-async:ring-lib our.bowl now.bowl eny.bowl 'hi there' ~ (silt ~zod ~nec ~))
::      ~&  [%sign (sign-message:safe-signatures our.bowl now.bowl eny.bowl req [%blah ~])]
      ~&  [%sig ring-signature]
      (pure:m app-state)
    ==
  ==
::
++  handle-peer
  |=  =path
  =/  m  tapp-async
  ^-  form:m
  ::
  ~&  [%received-peer path]
  ::
  ?:  ?=([%sole *] path)
    ::  we don't disconnect /sole so we have can print on it.
    ::
    ~&  %received-sole
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
  ::  TODO: We have intership communication issues here. We get into some sort
  ::  of loop where we receive a diff and that makes us send a diff and then
  ::  this loops.
  ::
  ::  Assuming ~nec is sending messages to ~zod:
  ::
  ::  - When trying to sign a message, we repeatedly try to subscribe to / on
  ::  ~zod. We're making repeated subscriptions then? And then we're never
  ::  really get to signing the outbound message?
  ::
  ::  - We don't deal in update-client-communities where we get multiple
  ::  snapshots which are the same. If we get snapshot 1, we shouldn't add
  ::  another shapshot 1.
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
  (unblock-pending bowl her community-name route app-state)
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

