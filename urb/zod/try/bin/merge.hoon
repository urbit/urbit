!:
::  /=try=/bin/merge/hoon
::  Call with two desks and an optional germ as a merge option
::
=>  .(-< `who=@p`-<)
|=  [est=time eny=@uw]
|=  $:  pes=[ses=span des=span cas=span ~]
        pen=[sen=span den=span can=span ~]
        gem=$|([germ ~] ~)
    ==
^-  bowl
:_  ~
^-  (list gift)
:_  ~
=+  vsr=((hard dome) .^(%cv pes))
=+  ves=((hard dome) .^(%cv pen))
=+  ^=  sab  ^-  saba  :*
    (need (slaw 'p' ses.pes))
    des.pes
    [0 let.vsr]
    (flop (turn hit.vsr |=(a=frog q.a)))
    ==
=+  ^=  lum
    %-  ~(auld ze est ves)
    [?~(gem %fine -.gem) (need (slaw 'p' sen.pen)) den.pen sab]
?~  lum
  ^-  gift
  :*  %la  %leaf
      "{(trip des.pes)} failed to apply, please rerun with a merge option"
  ==
?~  u.lum
  `gift`[%la %leaf "{(trip den.pen)} is up to date"]
`gift`[%ok den.pen u.u.lum]
