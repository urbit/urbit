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
    $%  [%create-community name=@t initial-invitees=(set @p)]
        [%send-message host=@p name=@t =path data=*]
    ==
  +$  out-poke-data  [%noun =cord]
  +$  out-peer-data  ~
  +$  in-peer-data
    $%  [%comments comments=(list tape)]
    ==
  ++  tapp   (^tapp app-state peek-data in-poke-data out-poke-data in-peer-data out-peer-data)
  ++  stdio  (^stdio out-poke-data out-peer-data)
  --
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
    (get-snapshot-as-peer-diff / server)
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
::  +send-message: sends a message to a node, validating the msg.
::
++  send-message
  |=  [=bowl:gall host=@p name=@t =path msg=* =app-state]
  ^+  app-state
  ::  Using our client copy of the state, perform verification.
  ::
  =/  community  (~(got by client-communities.app-state) [host name])
  ::
  =/  e  (sign-user-event our.bowl now.bowl eny.bowl path msg community safe-applets)
  ::
  ~&  [%outbound-message e]
  app-state
--
::
=,  async=async:tapp
=,  tapp-async=tapp-async:tapp
=,  stdio
%-  create-tapp-poke-diff:tapp
^-  tapp-core-poke-diff:tapp
|_  [=bowl:gall =app-state]
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
    =.  app-state
      %-  send-message  :*
        bowl
        host.in-poke-data
        name.in-poke-data
        path.in-poke-data
        data.in-poke-data
        app-state
      ==
    ~&  [%todo-impl-send-message [host name]:in-poke-data]
    (pure:m app-state)
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
  ?>  ?=(%comments -.data)
  ~&  subscriber-got-data=(lent comments.data)
  (pure:m app-state)
--

