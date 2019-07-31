/-  safe-server
/+  tapp, stdio, safe-applets, *safe-server
=>
  |%
  +$  subscription-state
    $:  target=[her=ship app=term]
        =path
    ==
  +$  state
    $:  %0
        server-communities=(map name=@t node:safe-server)
    ==
  +$  peek-data  _!!
  +$  in-poke-data
    $%  [%create-community name=@t initial-invitees=(set @p)]
    ==
  +$  out-poke-data  [%noun =cord]
  +$  out-peer-data  ~
  +$  in-peer-data
    $%  [%comments comments=(list tape)]
    ==
  ++  tapp   (^tapp state peek-data in-poke-data out-poke-data in-peer-data out-peer-data)
  ++  stdio  (^stdio out-poke-data out-peer-data)
  --
=,  async=async:tapp
=,  tapp-async=tapp-async:tapp
=,  stdio
%-  create-tapp-poke-diff:tapp
^-  tapp-core-poke-diff:tapp
|_  [=bowl:gall =state]
++  handle-poke
  |=  =in-poke-data
  =/  m  tapp-async
  ^-  form:m
  ?-    -.in-poke-data
      %create-community
    ::  if the community already exists
    ::
    ?:  (~(has by server-communities.state) name.in-poke-data)
      ~&  [%community-already-exists name.in-poke-data]
      (pure:m state)
    ::  create the server side community
    ::
    ::    This is the community state which is the currently running 
    ::
    =.  server-communities.state
      %+  ~(put by server-communities.state)  name.in-poke-data
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
    ::  create the client side community
    ::
    ::    When we make a community, we also build a local copy of it as the
    ::    general users would see. We do this because sending messages to the
    ::    server requires a client copy to do the signing, and the person who
    ::    owns this server almost certainly wants to participate in the
    ::    community but their archive shouldn't be public.
    ::
    ::  =.  client-communities.state
    ::
    ~&  [%created-community name.in-poke-data initial-invitees.in-poke-data]
    ::
    (pure:m state)
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
  (pure:m state)
--
