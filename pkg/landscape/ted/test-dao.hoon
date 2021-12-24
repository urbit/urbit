/-  spider,
    metadata=metadata-store,
    *group,
    inv=invite-store,
    store=group-store,
    push-hook
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
  ~[~zod ~dopzod ~marzod]
++  spam-diff
  |=  [=id:dao =diff:dao]
  =/  =cage
    dao-update+!>([id diff]) 
  (spam-poke cage)
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
=+  !<([~ =id:dao] arg)
;<  ~  bind:m  (spam-poke noun+!>(%nuke))
;<  ~  bind:m
  (spam-diff id %create ~)
;<  ~  bind:m
  (spam-diff id %daoist ~zod %join 0v0)
;<  ~  bind:m
  (spam-diff id %daoist ~dopzod %join 0v0)
;<  ~  bind:m
  (spam-diff id %daoist ~marzod %join 0v0)
;<  ~  bind:m
  (spam-diff id %daoist ~zod %deputise %admin)
(pure:m *vase)
