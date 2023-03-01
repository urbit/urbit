::    perform minimal norm change to permanently delete files
::
::  use =dry & for dry run
::
/+  *generators, *sole
=,  space:userlib
=,  clay
:-  %ask
|=  [[now=@da eny=@uvJ bec=beak] [target=path ~] dry=_|]
|^
=+  .^(=rang %cx /(scot %p p.bec)//(scot %da now)/rang)
=+  .^(=cone %cx /(scot %p p.bec)//(scot %da now)/domes)
=/  =beam  (need (de-beam target))                      ::  beam of target
=/  dusk=desk  q:beam                                   ::  desk to delete from
=+  .^(do=dome %cv target)                              ::  dome of that desk
=/  domes=(list [[=ship =desk] foam])                   ::  all domes on ship
  ~(tap by cone)
=+  .^(=cass %cw (en-beam beam(r da+now)))              ::  latest aeon
=/  used=(list [desk path])                             ::  desks using the target
  =-  (murn lobes -)                                    ::  match over lobes
  |=  lob=lobe
  ^-  (unit [desk path])
  =/  doms=(list [[=ship =desk] foam])  domes
  |-
  =*  dome-loop  $
  ?~  doms  ~
  ?:  =(0 let.i.doms)  dome-loop(doms t.doms)           ::  skip empty domes
  ?:  =(dusk desk.i.doms)  dome-loop(doms t.doms)       ::  skip target dome
  =/  latest=yaki                                       ::  only consider latest
    %-  ~(got by hut.rang)
    %-  ~(got by hit.i.doms)
    let.i.doms                                          ::  aeon of dome
  =/  yakies=(list [=path =lobe])  ~(tap by q.latest)   ::  latest yakis in dome
  |-
  =*  path-loop  $
  ?~  yakies  dome-loop(doms t.doms)
  ?:  =(lob lobe.i.yakies)                              ::  found a match
    `[desk.i.doms path.i.yakies]                        ::  return desk and path
  path-loop(yakies t.yakies)
?:  ?|  =(let.do ud.cass)                               ::  at dusk head
        !=(0 (lent used))                               ::  at other desk head
    ==
  =/  pax=path  +>+:target
  =/  hed=^beam  beam(r da+now, q dusk, s pax)
  =/  org=^beam  beam(q dusk, s +>+:target)
  =/  paths=(list path)                                 ::  paths blocking tomb
    ?~(used ~[target] (turn used tail))
  =/  all=(set desk)                                    ::  desks blocking tomb
    (silt ?~(used ~[dusk] (turn used head)))
  =/  prat=(list tank)                                  ::  printout of paths
    %+  turn  used
    |=  [=desk =path]
    [%leaf "{<desk>}: {<path>}"]
  =/  prom=$-([tint tape] sole-prompt)                  ::  styled |rm prompt
    |=  [=tint =tape]
    [%& %prompt (snoc *styx [[~ `tint ~] tape])]
  ::  print out the paths blocking the tomb
  %+  prints  (snoc prat leaf+"tomb blocked by:")
  ::  prompt for deletion using |rm
  %+  prompt   (prom %r "|rm from head of each desk instead (DANGER)? (y/N)")
  |=  rm-all=tape
  ?.  |(=("y" rm-all) =("Y" rm-all) =("yes" rm-all))
    no-product
  ::
  %+  prompt
    (prom %r "confirm deletion of {<paths>} from {<~(tap in all)>}? (y/N)")
  |=  confirm=tape
  ?.  |(=("y" confirm) =("Y" confirm) =("yes" confirm))
    no-product
  ::
  ?:  dry
    (print (crip "dry run: would remove {<paths>} from {<~(tap in all)>}") no-product)
  %+  produce  %helm-pans
  %+  turn  used
  |=  [=desk =path]
  %-  rm
  %-  en-beam
  beam(r da+now, q desk, s path)
::  no blocking paths, tombstone the target recursively
%-  produce
:-  %helm-pans
=-  ?:  dry  -
  %.  [%c %tomb %pick ~]
  (cury snoc -)
^-  (list note-arvo)
%-  zing
=-  (turn - notes)                                      ::  produce cards
=-  (turn lobes -)                                      ::  hashes
|=  =lobe
|^
|-  ^-  (set [ship desk tako norm path])
?~  domes
  ~
=/  =aeon  1
%-  ~(uni in $(domes t.domes))
|-  ^-  (set [ship desk tako norm path])
?:  (lth let.i.domes aeon)                            ::  only past aeons
  ~
=/  =tako  (~(got by hit.i.domes) aeon)
=/  paths
  (draw-tako ship.i.domes desk.i.domes +.i.domes tako)
(~(uni in paths) $(aeon +(aeon)))
::
++  draw-tako
  |=  [=ship =desk foam =tako]
  ^-  (set [^ship ^desk ^tako norm path])
  ~+
  =/  =yaki  (~(got by hut.rang) tako)
  =/  takos
    |-  ^-  (set [^ship ^desk ^tako norm path])
    ?~  p.yaki
      ~
    (~(uni in $(p.yaki t.p.yaki)) ^$(tako i.p.yaki))
  |-  ^-  (set [^ship ^desk ^tako norm path])
  ?~  q.yaki
    takos
  %-  ~(uni in $(q.yaki l.q.yaki))
  %-  ~(uni in $(q.yaki r.q.yaki))
  ^-  (set [^ship ^desk ^tako norm path])
  ?.  =(lobe q.n.q.yaki)
    ~
  [[ship desk tako (~(gut by tom) tako nor) p.n.q.yaki] ~ ~]
--
::  +lobes: recursively list hashes under target
::
++  lobes
  =|  lubs=(list lobe)
  |-  ^-  (list lobe)
  =+  b=.^(arch %cy target)
  ?:  ?=(^ fil.b)  (snoc lubs u.fil.b)
  %-  zing
  %+  turn  ~(tap by dir.b)
  |=  [kid=@ta ~]
  ^$(target (weld target /[kid]))
::  +notes: build cards for each path to tombstone
::
++  notes
  |=  norms=(set [ship desk tako norm path])
  ^-  (list note-arvo)
  %+  murn  ~(tap in norms)
  |=  [=ship =desk =tako =norm =path]
  ?:  ?=([~ %|] (~(fit of norm) path))
    ~
  %-  (slog leaf+"tomb: {<ship desk path `@uv`tako norm path>}" ~)
  ?:  dry
    ~
  `[%c %tomb %worn ship desk tako (~(put of norm) path %|)]
::    +info: toro into card
::
:: XX move to a shared library
++  info
  |=  tor=(unit toro)
  ^-  note-arvo
  ~|  [%tomb-error "tomb: failed to delete {<target>}"]
  [%c [%info (need tor)]]
::    +rm: remove a path from a desk
::
::  XX move to a shared library
++  rm
  |=  a=path
  =|  c=(list (unit toro))
  %-  info
  =-  %+  roll  -
      |=  [a=(unit toro) b=(unit toro)]
      (clap a b furl)
  |-  ^-  (list (unit toro))
  =+  b=.^(arch %cy a)
  ?:  ?=([^ ~] b)  (snoc c `(fray a))
  =?  c  ?=(^ fil.b)  (snoc c `(fray a))
  %-  zing
  %+  turn  ~(tap by dir.b)
  |=  [kid=@ta ~]
  ^$(a (weld a /[kid]))
--
