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
::        [%receive-message ]
    ==
  +$  out-poke-data
    $%  [%noun =cord]
        [%drum-unlink =dock]
    ==
  +$  out-peer-data  ~
  +$  in-peer-data
    $%  [%comments comments=(list tape)]
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
  %_      app-state
      client-communities
    ::
    |^  ::  if we don't have any entries yet, we have to make the entire entry
        ::
        ?.  (~(has by client-communities.app-state) [our name])
          %+  ~(put by client-communities.app-state)  [our name]
          %^  apply-to-client-node  full-route  *node:safe-client
          set-local-subscription
        ::  otherwise, we modify the current one in place
        ::
        %+  ~(jab by client-communities.app-state)  [our name]
        |=  =node:safe-client
        %^  apply-to-client-node  full-route  node
        set-local-subscription
    ::
    ++  set-local-subscription
      |=  client-state=node:safe-client
      ^+  client-state
      ::
      =.  subscribed.client-state  %subscribed
      =.  client-state
        (apply-peer-diff-to-node safe-applets (need full-server-state) client-state)
      ::
      client-state
    --
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
  ::  Using our client copy of the state, perform verification.
  ::
  =/  community  (~(got by client-communities.app-state) [host name])
  ::
  =/  e  (sign-user-event our.bowl now.bowl eny.bowl path msg community safe-applets)
  ::
  ?:  ?=([%& *] e)
    ?:  =(our.bowl host)
      =.  app-state  (local-subscribe our.bowl name p.e app-state)
      $
    ::
    ~&  [%todo-must-remote-subscribe host name p.e]
    (pure:m app-state)
  ::
  ?:  =(host our.bowl)
    ::  we do the special case where we synchronously call ourselves for
    ::  messages to ourselves.
    ::
    ~&  %sending-to-self
    (receive-message host name p.e app-state)
  ::
  ~&  %todo-send-message-outbound
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
  ::  todo: we need to emit the changes to all foreign subscribers
  ::
  (pure:m app-state)
--
::
%-  create-tapp-all:tapp
^-  tapp-core-all:tapp
|_  [=bowl:gall =app-state]
::
++  handle-peek  handle-peek:default-tapp
++  handle-peer  handle-peer:default-tapp
++  handle-take  handle-take:default-tapp
::
++  handle-init
  =/  m  tapp-async
  ^-  form:m
  ::  ;<  success=?  bind:m  (bind-route:stdio [~ /dns/oauth] dap.bowl)
  ::  ~|  %dns-unable-to-bind-route
  ::  ?>  success
  ;<  ~  bind:m  (poke-app:stdio [[our %hood] [%drum-unlink our dap]]:bowl)
  (pure:m app-state)

++  handle-poke
  |=  =in-poke-data
  =/  m  tapp-async
  ^-  form:m
  ?-    -.in-poke-data
      %create-community
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
  ==

  ::  ?:  =(cord.in-poke-data 'pull')
  ::    ?~  subscription
  ::      (async-fail %no-subscription ~)
  ::    ;<  ~  bind:m  (pull-app [target path]:u.subscription)
  ::    (pure:m ~)
  ::  =/  target  [our.bowl %example-tapp-fetch]
  ::  ;<  ~  bind:m  (poke-app target %noun 'print')
  ::  ;<  ~  bind:m  (peer-app target /comments)
  ::  =.  subscription  `[target /comments]
  ::  ;<  ~  bind:m  (wait (add now.bowl ~s3))
::
++  handle-diff
  |=  [[her=ship app=term] =path data=in-peer-data]
  =/  m  tapp-async
  ^-  form:m
  ::  ?>  ?=(%comments -.data)
  ::  ~&  subscriber-got-data=(lent comments.data)
  (pure:m app-state)
--

