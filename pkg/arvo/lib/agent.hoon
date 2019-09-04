|*  [state=mold in-poke-data=mold sign=mold]
|%
++  agent
  =>  |%
      +$  step  (quip move:agent:mall state)
      --
  $_  ^|
  |_  [bowl:mall state]
  ++  handle-init
    *step
  ::
  ++  handle-prep
    |~  old-state=vase
    *step
  ::
  ++  handle-poke
    |~  =in-poke-data
    *step
  ::
  ++  handle-peer
    |~  path
    *step
  ::
  ++  handle-pull
    |~  path
    *step
  ::
  ++  handle-peek
    |~  path
    *(unit (unit cage))
  ::
  ++  handle-mall
    |~  [wire internal-gift:mall]
    *step
  ::
  ++  handle-take
    |~  [wire =sign]
    *step
  ::
  ++  handle-lame
    |~  [term tang]
    *step
  --
++  agent-to-mall-agent
  |=  =a=agent
  |^  ^-  agent:mall
      |_  [=bowl:mall state=vase]
      ++  handle-init  (ag-step handle-init:(ag-core bowl state))
      ++  handle-prep  ((ag-pace ,vase) handle-prep:(ag-core bowl state))
      ++  handle-poke
        |=  in=cage
        ~&  poke-in=in
        ~&  !>(*in-poke-data)
        =/  data=(unit in-poke-data)  !<(in-poke-data q.in)
        ?~  data
          ~|  [%agent-received-malformed-poke our.bowl dap.bowl]
          !!
        (ag-step (handle-poke:(ag-core bowl state) u.data))
      ::
      ++  handle-peer  ((ag-pace ,path) handle-peer:(ag-core bowl state))
      ++  handle-pull  ((ag-pace ,path) handle-pull:(ag-core bowl state))
      ++  handle-peek  handle-peek:(ag-core bowl state)
      ++  handle-mall
        %-  (ag-pace ,[wire internal-gift:mall])
        handle-mall:(ag-core bowl state)
      ::
      ++  handle-take  ((ag-pace ,[wire vase]) handle-take:(ag-core bowl state))
      ++  handle-lame  ((ag-pace ,[term tang]) handle-lame:(ag-core bowl state))
      --
  ::
  ++  ag-core
    |=  [=bowl:mall =state=vase]
    =/  static-state  !<(state state-vase)
    ?~  static-state
      ~|  [%bad-state-type our.bowl dap.bowl]
      !!
    ~(. a-agent bowl u.static-state)
  ::
  ++  ag-step
    |=  =step:agent
    [-.step !>(+.step)]
  ::
  ++  ag-pace
    |*  in=mold
    |=  fun=$-(in step:agent)
    |=  =in
    (ag-step (fun in))
  --
--
