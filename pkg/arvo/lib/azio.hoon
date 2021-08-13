::  azio: asynchronous azimuth reads and related operations
::
/+  az=azio-azimuth, ec=azio-ecliptic,
    li=azio-linear, ds=azio-delegated-sending,
    strandio, *ethereum
::
^|
|_  $:  url=@ta
    $=  contracts
    $:  azimuth=address
        ecliptic=address
        linear=address
        delegated-sending=address
    ==  ==
++  azimuth
  =+  ~(. az url azimuth.contracts)
  |%
  ++  get-spawns-remaining
    |=  who=ship
    =/  m  (strand:strandio ,@ud)
    ^-  form:m
    ;<  count=@ud  bind:m
      (get-spawn-count:azimuth who)
    ;<  now=@da  bind:m
      get-time:strandio
    ;<  limit=@ud  bind:m
      (get-spawn-limit:ecliptic who now)
    %-  pure:m
    (sub limit (min count limit))
  ::
  ++  get-unspawned-children
    |=  who=ship
    =/  m  (strand:strandio ,(list ship))
    ^-  form:m
    ;<  kids-list=(list ship)  bind:m
      (get-spawned who)
    =/  kids  (~(gas in *(set ship)) kids-list)
    %-  pure:m
    %+  murn
      %+  gulf  0x1
      ?+  (clan:title who)  !!
        %czar  0xff
        %king  0xffff
      ==
    |=  k=@ud
    ^-  (unit ship)
    =+  kid=(cat 3 who k)
    ?:  (~(has in kids) kid)  ~
    `kid
  --
::
++  ecliptic
  ~(. ec url ecliptic.contracts)
::
++  linear
  ~(. li url linear.contracts)
::
++  delegated-sending
  =+  ~(. ds url delegated-sending.contracts)
  |%
  ++  get-planets-to-send
    |=  [as=ship amount=@ud]
    =/  m  (strand:strandio ,(list ship))
    ^-  form:m
    ;<  sponsor=ship  bind:m
      (get-sponsor:azimuth as)
    ;<  inviter=ship  bind:m
      (invited-by as)
    ;<  inviter-sponsor=ship  bind:m
      (get-sponsor:azimuth inviter)
    ;<  pool=ship  bind:m
      (get-pool as)
    ;<  stars=(list ship)  bind:m
      (get-pool-stars pool)
    ::  for each star from which we can invite,
    ::  figure out how many planets are available,
    ::  and determine usage preference based on sponsorship and spawncounts.
    ::
    =|  available=(list [prio=@ud avail=@ud =ship])
    |-  ^-  form:m
    =*  prioritize  $
    ?^  stars
      =*  star  i.stars
      ;<  spawnable=?  bind:m
        (is-spawn-proxy:azimuth star delegated-sending.contracts)
      ?.  spawnable
        prioritize(stars t.stars)
      ::
      ;<  live=?  bind:m
        (is-live:azimuth star)
      ?.  live
        prioritize(stars t.stars)
      ::
      ;<  invites=@ud  bind:m
        (pools pool star)
      ?:  =(0 invites)
        prioritize(stars t.stars)
      ::
      ;<  prio=@ud  bind:m
        =/  n  (strand:strandio ,@ud)
        ?:  =(sponsor star)
          (pure:n 0)
        ?:  =(inviter-sponsor star)
          (pure:n 1)
        ;<  spawned=@ud  bind:n
          (get-spawn-count:azimuth star)
        (pure:n (add 2 spawned))
      ::
      %=  prioritize
        available  [[prio invites star] available]
        stars      t.stars
      ==
    ::
    =.  available  (sort available dor)
    =|  planets=(list ship)
    |-  ^-  form:m
    =*  more-planets  $
    ?.  &(!=(0 amount) ?=(^ available))
      (pure:m planets)
    =*  star=[@ud avail=@ud =ship]  i.available
    ;<  spawnable=@ud  bind:m
      (get-spawns-remaining:azimuth ship.star)
    =/  avail=@ud  :(min amount avail.star spawnable)
    ?:  =(0 avail)
      more-planets(available t.available)
    ;<  unspawned=(list ship)  bind:m
      (get-unspawned-children:azimuth ship.star)
    ;<  eny=@uvJ  bind:m
      get-entropy:strandio
    =/  new-planets=(list ship)
      =-  (turn - head)
      %+  scag  avail
      =-  (sort - gor)
      %+  turn  unspawned
      |=(p=ship [p eny])
    %=  more-planets
      available  t.available
      planets    (weld planets new-planets)
      amount     (sub amount (lent new-planets))
    ==
  --
::
--
