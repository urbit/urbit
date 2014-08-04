!:
::  /=try=/bin/merge/hoon
::  Call with two desks and an optional germ as a merge option
::
=>  .(-< `who=@p`-<)
=>  .(+ =>(+ ^/=/main/=/lib/pony))
|=  [est=time eny=@uw]
|=  $:  pes=[ses=span des=span cas=span ~]
        pen=[sen=span den=span can=span ~]
        gem=$|([germ ~] ~)
    ==
=+  vsr=((hard dome) .^(%cv pes))
=+  ves=((hard dome) .^(%cv pen))
%-  (posh (add ~s1 est))
|=  tim=@da
^-  bowl
:_  ~
^-  (list gift)
:_  ~
=+  ran=((hard rang) .^(%cu /(scot %p who)/main/(scot %da tim)))
=+  ^=  sab  ^-  saba  :*
    (need (slaw 'p' ses.pes))
    des.pes
    [0 let.vsr]
    vsr
    ==
=+  ^=  lum
    %-  ~(auld ze est ves ran)
    [?~(gem %fine -.gem) (need (slaw 'p' sen.pen)) den.pen sab est]
?~  lum
  ^-  gift
  :*  %la  %leaf
      "{(trip des.pes)} failed to apply, please rerun with a merge option"
  ==
?~  u.lum
  `gift`[%la %leaf "{(trip den.pen)} is up to date"]
`gift`[%og den.pen u.u.lum]
