/-  *ballet
/+  tapp, stdio
::
::  Preamble
::
=>
  |%
  +$  state
    $:  next-election-num=@u
        open-elections=(map id election)
        closed-elections=(map id election)
    ==
  +$  peek-data  _!!
  +$  in-poke-data
    $%  ::  casts a ballot
        ::
        [%ballot-cast =id =ring-signature =vote]
        ::  creates a new election
        ::
        [%ballot-create =electorate =ballot closes=@da]
    ==
  +$  out-poke-data
    ~
  +$  in-peer-data
    ~
  +$  out-peer-data
    ::  For observers, our wire interface is:
    ::
    ::    /[election-num]
    ::
    election-diff
  ++  tapp   (^tapp state peek-data in-poke-data out-poke-data in-peer-data out-peer-data)
  ++  stdio  (^stdio out-poke-data out-peer-data)
  --
=,  async=async:tapp
=,  tapp-async=tapp-async:tapp
=,  stdio
::
::  The app
::
%-  create-tapp-poke-peer:tapp
^-  tapp-core-poke-peer:tapp
|_  [=bowl:gall =state]
::
::  Main function
::
++  handle-poke
  |=  =in-poke-data
  =/  m  tapp-async
  ^-  form:m
  ::
  ?-    -.in-poke-data
  ::
      %ballot-cast
    ~&  %ballot-cast
    ::  verify ballot is for a valid election
    ::
    ?~  election=(~(get by open-elections.state) id.in-poke-data)
      ~&  [%invalid-election id.in-poke-data]
      (pure:m state)
    ::  verify ballot has a valid signature
    ::
    ::    TODO: Right now, this is a no-op until the ring signature library is
    ::    completed.
    ::
    ?.  =,  in-poke-data
        (verify-signature ring-signature `[%election id] vote)
      ~&  [%invalid-signature ~]
      (pure:m state)
    ::  is this person trying to vote twice?
    ::
    ?:  (~(has by cast.u.election) ring-tag.ring-signature.in-poke-data)
      ~&  [%attempting-to-vote-twice ring-tag.ring-signature.in-poke-data]
      (pure:m state)
    ::  we have a valid vote. make it part of our state.
    ::
    =.  open-elections.state
      %+  ~(jab by open-elections.state)  id.in-poke-data
      |=  =^election
      %_    election
          cast
        %+  ~(put by cast.election)  ring-tag.ring-signature.in-poke-data
        [ring-signature.in-poke-data vote.in-poke-data]
      ==
    ::  broadcast this valid vote to the world
    ::
    ;<  ~  bind:m
      =,  in-poke-data
      (give-result `path`/(scot %u election-num.id) `election-diff`[%vote ring-signature vote])
    ::
    (pure:m state)
  ::
      %ballot-create
    =/  election-num  next-election-num.state
    =/  =id  [our.bowl election-num]
    ~&  [%this-ballot-id id]
    ::
    =.  next-election-num.state  +(next-election-num.state)
    =.  open-elections.state
      %+  ~(put by open-elections.state)  id
      ^-  election
      :*  id
          electorate.in-poke-data
          ballot.in-poke-data
          closes.in-poke-data
          cast=~
          tally=~
      ==
    ::
    (pure:m state)
  ==
::
++  handle-peer
  |=  =path
  =/  m  tapp-async
  ^-  form:m
  ~&  [%tapp-fetch-take-peer path]
  (pure:m state)
--
