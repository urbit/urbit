::  Perform minimal norm change to delete a file, use =dry & for dry run
::
::  TODO: recognize when it's going to fail because it's in the head of
::        a desk, and maybe offer to |rm
::
=,  clay
:-  %say
|=  [[now=@da eny=@uvJ bec=beak] [target=path ~] dry=_|]
:-  %helm-pans
=+  .^(=arch %cy target)
?~  fil.arch
  [%d %flog %text "tomb: not a file"]~  ::  should recurse
=/  =lobe  u.fil.arch
=+  .^(=rang %cx /(scot %p p.bec)//(scot %da now)/rang)
=+  .^(=cone %cx /(scot %p p.bec)//(scot %da now)/domes)
=/  domes=(list [[=ship =desk] dome])  ~(tap by cone)
=/  norms
  |^
  |-  ^-  (set [ship desk tako norm path])
  ?~  domes
    ~
  =/  n  1
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
    |=  [=ship =desk dome =tako]
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
^-  (list note-arvo)
%+  welp
  %+  murn  ~(tap in norms)
  |=  [=ship =desk =tako =norm =path]
  ?:  ?=([~ %|] (~(fit of norm) path))
    ~
  %-  (slog leaf+"tomb: {<ship desk path `@uv`tako norm path>}" ~)
  ?:  dry
    ~
  `[%c %tomb %worn ship desk tako (~(put of norm) path %|)]
?:  dry
  ~
[%c %tomb %pick ~]~
