::  claz/pre-command: sanity-check command and gather prerequisites
::
/-  *claz
/+  *claz, ethio, strandio
=,  ethereum-types
=,  able:jael
::
|=  args=vase
=/  [url=@t =command]
  !<([@t command] args)
=/  m  (strand:strandio ,vase)
^-  form:m
?.  ?=(%generate -.command)  !!  ::TODO
|^  ;<  err=(unit tang)  bind:m
      ::  sanity-check command
      ::
      ?+  -.batch.command  (pure:(strand:strandio (unit tang)) ~)
        %invites  (check-invites +.batch.command)
      ==
    ?^  err  (strand-fail:strandio %claz-pre-command u.err)
    ::  gather prerequisites
    ::
    ~&  [%gonna-get-nonce url as.command]
    ;<  nonce=@ud  bind:m
      (get-next-nonce:ethio url as.command)
    ~&  [%got-nonce nonce]
    (pure:m !>([%nonce nonce]))
::
++  check-invites
  |=  [as=ship file=path]
  =/  m  (strand:strandio ,(unit tang))
  ^-  form:m
  =/  friends=(list ship)
    %+  turn
      (read-invites file)
    head
  ;<  err=(unit tang)  bind:m
    (are-available friends)
  ?^  err  (pure:m err)
  (has-invites-for as friends)
::
++  are-available
  |=  ships=(list ship)
  =/  m  (strand:strandio ,(unit tang))
  ^-  form:m
  ;<  responses=(list [@t @t])  bind:m
    %+  batch-read-contract-strict:ethio  url
    %+  turn  ships
    |=  =ship
    ^-  proto-read-request:rpc
    :+  `(scot %p ship)
      ::TODO  argument?
      azimuth:contracts:azimuth
    (rights:cal ship)
  =/  taken=(list ship)
    %+  murn  responses
    |=  [id=@t res=@t]
    ^-  (unit ship)
    =/  rights=[owner=address *]
      %+  decode-results:rpc  res
      ::NOTE  using +reap nest-fails
      [%address %address %address %address %address ~]
    ?:  =(0x0 owner.rights)  ~
    `(slav %p id)
  %-  pure:m
  ?:  =(~ taken)  ~
  :-  ~
  :~  leaf+"some ships already taken:"
      >taken<
  ==
::
++  has-invites-for
  |=  [as=ship ships=(list ship)]
  =/  m  (strand:strandio ,(unit tang))
  ^-  form:m
  =/  counts=(map ship @ud)
    %+  roll  ships
    |=  [s=ship counts=(map ship @ud)]
    =+  p=(^sein:title s)
    %+  ~(put by counts)  p
    +((~(gut by counts) p 0))
  ;<  pool=@ud  bind:m
    =/  n  (strand:strandio ,@ud)
    ;<  res=@t  bind:n
      %+  read-contract:ethio  url
      :+  `'pool'
        ::TODO pass in as argument
        delegated-sending:contracts:azimuth
      (get-pool:cal as)
    %-  pure:n
    (decode-results:rpc res [%uint]~)
  ;<  responses=(list [id=@t res=@t])  bind:m
    %+  batch-read-contract-strict:ethio  url
    %+  turn  ~(tap by counts)
    |=  [=ship @ud]
    ^-  proto-read-request:rpc
    :+  `(scot %p ship)
      ::TODO pass in as argument
      delegated-sending:contracts:azimuth
    (pools:cal pool ship)
  =/  missing=(list [star=ship have=@ud needed=@ud])
    %+  murn  responses
    |=  [id=@t res=@t]
    ^-  (unit [ship @ud @ud])
    =/  =ship
      (slav %p id)
    =/  pool-size=@ud
      (decode-results:rpc res [%uint]~)
    =/  need=@ud
      (~(got by counts) ship)
    ?:  (gte pool-size need)  ~
    `[ship pool-size need]
  ?:  =(~ missing)
    (pure:m ~)
  %+  strand-fail:strandio  %lacking-invites
  :~  leaf+"not enough invites from stars:"
      >missing<
  ==
--
