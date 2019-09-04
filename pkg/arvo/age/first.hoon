^-  agent:mall
=|  state=@
|_  bowl:mall
++  handle-init
  `..handle-init
::
++  handle-prep
  |=  =old-state=vase
  =/  old-state  !<(@ old-state-vase)
  ?~  old-state
    ~&  %prep-lost
    `..handle-init
  ~&  %prep-found
  `..handle-init(state u.old-state)
::
++  handle-poke
  |=  in-poke-data=cage
  ~&  >>  'ouchies!'
  ~&  >>>  in-poke-data
  ~&  >  state=state
  =.  state  +(state)
  `..handle-init
::
++  handle-peer
  |=  path
  `..handle-init
::
++  handle-pull
  |=  path
  `..handle-init
::
++  handle-peek
  |=  path
  *(unit (unit cage))
::
++  handle-mall
  |=  [wire internal-gift:mall]
  `..handle-init
::
++  handle-take
  |=  [wire vase]
  `..handle-init
::
++  handle-lame
  |=  [term tang]
  `..handle-init
::
++  handle-stay
  !>(state)
--
