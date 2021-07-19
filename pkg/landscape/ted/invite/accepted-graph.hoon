/-  spider, inv=invite-store, graph-view
/+  strandio
::
=*  strand        strand:spider
=*  fail          strand-fail:strand
=*  poke-our      poke-our:strandio
=*  flog-text     flog-text:strandio
::
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
=+  !<([~ =update:inv] arg)
?.  ?=(%accepted -.update)
  (pure:m !>(~))
;<  =bowl:spider  bind:m  get-bowl:strandio
=*  invite  invite.update
?:  =(our.bowl entity.resource.invite)
  ::  do not crash because that will kill the invitatory subscription
  (pure:m !>(~))
;<  ~  bind:m
  %+  poke-our  %spider
  =-  spider-start+!>([`tid.bowl ~ byk.bowl %graph-join -])
  %+  slop  !>(~)
  !>  ^-  action:graph-view
  [%join resource.invite ship.invite]
(pure:m !>(~))
