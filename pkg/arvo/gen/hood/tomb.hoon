::  Perform minimal norm change to delete a file, use =dry & for dry run
::
/+  *generators
=,  space:userlib
=,  clay
:-  %ask
|=  [[now=@da eny=@uvJ bec=beak] [target=path ~] dry=_|]
|^
=+  .^(=rang %cx /(scot %p p.bec)//(scot %da now)/rang)
=+  .^(=cone %cx /(scot %p p.bec)//(scot %da now)/domes)
=/  =beam  (need (de-beam target))
=/  dusk=desk  q:beam
=/  domes=(list [[=ship =desk] foam])
  ~(tap by cone)
=+  .^(=cass %cw (en-beam beam(r da+now)))
=+  .^(do=dome %cv target)
=/  used=(list [desk path])
  =-  (murn lobes -)
  |=  lob=lobe
  ^-  (unit [desk path])
  =/  doms=(list [[=ship =desk] foam])  domes
  |-
  =*  dome-loop  $
  ?~  doms  ~
  ?:  =(0 let.i.doms)  dome-loop(doms t.doms)
  ?:  =(dusk desk.i.doms)  dome-loop(doms t.doms)
  =/  latest=yaki
    %-  ~(got by hut.rang)
    %-  ~(got by hit.i.doms)
    let.i.doms
  =/  yakies=(list [=path =lobe])  ~(tap by q.latest)
  |-
  =*  path-loop  $
  ?~  yakies  dome-loop(doms t.doms)
  ?:  =(lob lobe.i.yakies)
    `[desk.i.doms path.i.yakies]
  path-loop(yakies t.yakies)
?:  |(=(let.do ud.cass) !=(0 (lent used)))
    %+  print    (rap 3 'used in ' (crip <?~(used dusk used)>) ' already.' ~)
    %+  prompt   [%& %prompt "|rm from head of {<dusk>}? (y/N) "]
    |=  in=tape
    ?.  |(=("y" in) =("Y" in) =("yes" in))
      %+  prompt   [%& %prompt "|rm from head of each desk instead? (y/N) "]
      |=  inn=tape
      ?.  |(=("y" inn) =("Y" inn) =("yes" inn))
        no-product
      (produce %helm-pans (turn used |=([=desk =path] (rm (en-beam beam(q desk, s path))))))
    (produce %helm-pans ~[(rm (en-beam beam(q dusk, s +>+:target)))])
::
%-  produce
:-  %helm-pans
=-  (snoc `(list note-arvo)`- [%c %tomb [%pick ~]])
%-  zing
=-  (turn - notes)
=-  (turn lobes -)
|=  =lobe
  |^
  |-  ^-  (set [ship desk tako norm path])
  ?~  domes
    ~
  =/  =aeon  1
  %-  ~(uni in $(domes t.domes))
  |-  ^-  (set [ship desk tako norm path])
  ?:  (lth let.i.domes aeon)
    ~
  =/  =tako  (~(got by hit.i.domes) aeon)
  =/  paths  (draw-tako ship.i.domes desk.i.domes +.i.domes tako)
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
::
++  lobes
  =|  lubs=(list lobe)
  |-  ^-  (list lobe)
  =+  b=.^(arch %cy target)
  ?:  ?=([^ ~] b)  (snoc lubs u.fil.b)
  %-  zing
  %+  turn  ~(tap by dir.b)
  |=  [kid=@ta ~]
  ^$(target (weld target /[kid]))
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
::
++  info
  |=  tor=(unit toro)
  ^-  note-arvo
  ~|  [%tomb-error "tomb: failed to delete {<target>}"]
  [%c [%info (need tor)]]
::
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
