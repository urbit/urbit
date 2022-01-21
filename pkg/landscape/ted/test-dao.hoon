/-  spider,
    metadata=metadata-store,
    *group,
    inv=invite-store,
    store=group-store,
    push-hook
/-  tx
/-  chain=dao-chain
/-  dao=uqbar-dao
/+  strandio, resource, view=group-view
=>
|%
++  strand    strand:spider
++  poke      poke:strandio
++  poke-our  poke-our:strandio
++  ships
  ^-  (list ship)
 ::  ~[~dopzod]
  ~[~zod ~dopzod]
++  spam-update
  |=  =update:chain
  =/  =cage
    chain-update+!>(update)
  (spam-poke cage)
++  zd  `@ux`~zod
++  dpzd  `@ux`~dopzod
++  mrzd  `@ux`~marzod
++  nft-addr  0x1
++  mint-tx
  :^  %mint  zd  nft-addr
  %+  turn  ~[dpzd]
  |=  ship=@ux
  [ship %nft ~zod (scot %p ship) ship %.n]
++  send-tx
  :^  %send  dpzd  mrzd
  %-  ~(gas by *(map account-id:tx asset:tx))
  %+  turn  ~[nft-addr]
  |=  =account-id:tx
  :-  account-id 
  ^-  asset:tx
  [%nft account-id `@ud`dpzd (scot %p dpzd) dpzd ~zod %.n]
::
++  spam-poke
  |=  =cage
  =/  m  (strand ,~)
  ^-  form:m
  =/  ships  ships
  |- 
  =*  loop  $
  ?~  ships  (pure:m ~)
  ;<  ~  bind:m  
    (poke:strandio [i.ships %uqbar-dao] cage)
  loop(ships t.ships)
--
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
:: =+  !<([~ =id:dao] arg)
;<  ~  bind:m  (spam-poke noun+!>(%nuke))
=/  members
  %-  ~(gas by *(map ship @ux))
  :~  [~zod `@ux`~zod]
      [~dopzod `@ux`~dopzod]
      [~marzod `@ux`~marzod]
  ==
;<  ~  bind:m
  (spam-update 0x1 %0 ~zod members (silt ~zod ~))
(pure:m *vase)
