/-  *ballet, *ring
/+  tapp, stdio, *ring
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
    ::    /[election-num]  <- subscribes to the results of a specific election
    ::
    election-diff
  ++  tapp   (^tapp state peek-data in-poke-data out-poke-data in-peer-data out-peer-data)
  ++  stdio  (^stdio out-poke-data out-peer-data)
  --
=>
  |%
  ::  validates that all answers are valid to the selected questions
  ::
  ++  valid-vote
    |=  [questions=(list question) =vote]
    ^-  ?
    ::
    ?:  &(=(~ questions) =(~ answers.vote))
      %.y
    ::
    ?~  questions
      %.n
    ?~  answers.vote
      %.n
    ::
    =/  current-valid=?
      ?-    -.i.answers.vote
          %check
        ?&  ?=(%check -.i.questions)
        ::
            =/  len  (lent descriptions.i.questions)
            (levy checked.i.answers.vote |=(a=@u (lth a len)))
        ==
      ::
          %radio
        ?&  ?=(%radio -.i.questions)
            (lth checked.i.answers.vote (lent descriptions.i.questions))
        ==
      ==
    ::
    ?:  =(%.n current-valid)
      %.n
    ::
    $(questions t.questions, answers.vote t.answers.vote)
  ::  adds the :vote to the running :tally
  ::
  ++  update-tally
    |=  [=tally =vote]
    ^+  tally
    ::
    ?~  tally
      ~
    ?~  answers.vote
      tally
    ::
    :_  $(tally t.tally, answers.vote t.answers.vote)
    ::
    =/  answers=(list @u)
      ?-  -.i.answers.vote
        %check  checked.i.answers.vote
        %radio  [checked.i.answers.vote ~]
      ==
    ::
    |-
    ?~  answers
      i.tally
    ::
    =.  i.tally
      %+  ~(put by i.tally)  i.answers
      ::
      ?~  previous-tally=(~(get by i.tally) i.answers)
        1
      +(u.previous-tally)
    ::
    $(answers t.answers)
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
    =/  electorate-keys=(set @udpoint)
      (~(run in electorate.u.election) public-key-for-ship)
    ::
    ?.  %-  verify  :*
          vote.in-poke-data
          [~ [%election id.in-poke-data]]
          electorate-keys
          ring-signature.in-poke-data
        ==
      ~&  [%invalid-signature ~]
      (pure:m state)
    ::  the signature must have a ring tag
    ::
    ?:  ?=(~ y.ring-signature.in-poke-data)
      ~&  [%missing-ring-tag ~]
      (pure:m state)
    ::  is this person trying to vote twice?
    ::
    ?:  (~(has by cast.u.election) u.y.ring-signature.in-poke-data)
      ~&  [%attempting-to-vote-twice u.y.ring-signature.in-poke-data]
      (pure:m state)
    ::  is this a valid ballot?
    ::
    ?.  (valid-vote questions.ballot.u.election vote.in-poke-data)
      ~&  [%invalid-ballot u.y.ring-signature.in-poke-data]
      (pure:m state)
    ::  we have a valid vote. make it part of our state.
    ::
    =.  open-elections.state
      %+  ~(jab by open-elections.state)  id.in-poke-data
      |=  =^election
      ~&  [%recording-vote-by u.y.ring-signature.in-poke-data vote.in-poke-data]
      %_    election
          cast
        %+  ~(put by cast.election)  u.y.ring-signature.in-poke-data
        [ring-signature.in-poke-data vote.in-poke-data]
      ::
          tally
        (update-tally tally.election vote.in-poke-data)
      ==
    ::
    =/  election-path=path  /(scot %u election-num.id.in-poke-data)
    ::  broadcast this valid vote to the world
    ::
    ;<  ~  bind:m
      =,  in-poke-data
      (give-result election-path `election-diff`[%vote ring-signature vote])
    ::
    =/  =^election  (~(got by open-elections.state) id.in-poke-data)
    ::  if everyone has voted, this election is over.
    ::
    ?:  =(~(wyt in electorate.election) ~(wyt by cast.election))
      ::  todo: trying to separate this into its own helper function nest-fails
      ::  at the |_ state?
      ::
      ::  notify everyone that the election is over.
      ::
      ;<  ~  bind:m
        (give-result election-path [%election-completed ~])
      ~&  [%election-closed id.in-poke-data]
      ::  todo: can't quit a wire in the current stdio?
      ::
      ::  move the election state from open to closed
      ::
      =/  election
        (~(got by open-elections.state) id.in-poke-data)
      ~&  [%election-results tally.election]
      =.  open-elections.state
        (~(del by open-elections.state) id.in-poke-data)
      =.  closed-elections.state
        (~(put by closed-elections.state) id.in-poke-data election)
      ::
      (pure:m state)
    ::
    (pure:m state)
  ::
      %ballot-create
    =/  election-num  next-election-num.state
    =/  =id  [our.bowl election-num]
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
          tally=(reap (lent questions.ballot.in-poke-data) ~)
      ==
    ::
    ~&  [%created-new-ballot id electorate.in-poke-data]
    ::  todo: figure out how to set a timer so we can close an election on a
    ::  timer.
    ::
    (pure:m state)
  ==
::
++  handle-peer
  |=  =path
  =/  m  tapp-async
  ^-  form:m
  ~&  [%tapp-fetch-take-peer path]
  ::
  ?.  ?=([@ ~] path)
    ~&  [%bad-path path]
    (pure:m state)
  ::
  ?~  election-id=(slaw %ud i.path)
    ~&  [%unparsable-election-id i.path]
    (pure:m state)
  ::
  ?^  election=(~(get by open-elections.state) u.election-id)
    ::  send the current state of the election as the opening statement
    ::
    ;<  ~  bind:m
      (give-result path [%snapshot u.election])
    ::
    (pure:m state)
  ::
  ?^  election=(~(get by closed-elections.state) u.election-id)
    ::  send the completed election
    ::
    ;<  ~  bind:m
      (give-result path [%snapshot u.election])
    ::  send the election completed message
    ::
    ;<  ~  bind:m
      (give-result path [%election-completed ~])
    ::
    ::  todo: also close the path.
    ::
    (pure:m state)
  ::
  ~&  [%invalid-electin-id u.election-id]
  ::  todo: close the invalid subscription
  ::
  (pure:m state)
--
