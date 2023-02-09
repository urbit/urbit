:::  Perform minimal norm change to delete a file, use =dry & for dry run
::
::  TODO: recognize when it's going to fail because it's in the head of
::        a desk, and maybe offer to |rm
::
=,  clay
:-  %say
|=  [[now=@da eny=@uvJ bec=beak] [target=path ~] dry=_|]
:-  %helm-pans
|^
=+  .^(=rang %cx /(scot %p p.bec)//(scot %da now)/rang)
=+  .^(=cone %cx /(scot %p p.bec)//(scot %da now)/domes)
=/  domes=(list [[=ship =desk] foam])
  ~(tap by cone)
%+  welp  [%c %tomb %pick ~]~
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
    ::  TODO: maybe offer to |rm here?
    ::  ?:  =(tako (~(got by hit.do) aeon))  ~
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
=+  .^(=arch %cy target)
?~  fil.arch
  =/  dirs  ~(tap by dir.arch)
  %-  zing
  %+  turn  dirs
  |=  [kid=@ta ~]
  =/  paf=path  /[kid]
  =/  kud=path  `path`(weld target /[kid])
  ^$(target kud)
 (snoc lubs u.fil.arch)
::
++  notes
  |=  norms=(set [ship desk tako norm path])
  ^-  (list note-arvo)
  =/  dusk=desk  q:(need (de-beam target))
  %+  murn  ~(tap in norms)
  |=  [=ship =desk =tako =norm =path]
  ?.  =(desk dusk)
    ~
  ?:  ?=([~ %|] (~(fit of norm) path))
    ~
  %-  (slog leaf+"tomb: {<ship desk path `@uv`tako norm path>}" ~)
  ?:  dry
    ~
  `[%c %tomb %worn ship desk tako (~(put of norm) path %|)]
--
