::  Perform minimal norm change to delete a file, use =dry & for dry run
::
::  TODO: recognize when it's going to fail because it's in the head of
::        a desk, and maybe offer to |rm
::
:-  %say
|=  [[now=@da eny=@uvJ bec=beak] [target=path ~] dry=_|]
:-  %helm-pans
=+  .^(=arch %cy target)
?~  fil.arch
  [%d %flog %text "tomb: not a file"]~  ::  should recurse
=/  =lobe:clay  u.fil.arch
=+  .^(=rang:clay %cx /(scot %p p.bec)//(scot %da now)/rang)
=+  .^(=cone:clay %cx /(scot %p p.bec)//(scot %da now)/domes)
=/  domes=(list [[=ship =desk] =dome:clay])  ~(tap by cone)
=/  norms
  |^
  |-  ^-  (set [ship desk tako:clay norm:clay path])
  ?~  domes
    ~
  =/  n  1
  =/  =aeon:clay  1
  %-  ~(uni in $(domes t.domes))
  |-  ^-  (set [ship desk tako:clay norm:clay path])
  ?:  (lth let.dome.i.domes aeon)
    ~
  =/  =tako:clay  (~(got by hit.dome.i.domes) aeon)
  =/  paths  (draw-tako ship.i.domes desk.i.domes dome.i.domes tako)
  (~(uni in paths) $(aeon +(aeon)))
  ::
  ++  draw-tako
    |=  [=ship =desk =dome:clay =tako:clay]
    ^-  (set [^ship ^desk tako:clay norm:clay path])
    ~+
    =/  =yaki:clay  (~(got by hut.rang) tako)
    =/  takos
      |-  ^-  (set [^ship ^desk tako:clay norm:clay path])
      ?~  p.yaki
        ~
      (~(uni in $(p.yaki t.p.yaki)) ^$(tako i.p.yaki))
    |-  ^-  (set [^ship ^desk tako:clay norm:clay path])
    ?~  q.yaki
      takos
    %-  ~(uni in $(q.yaki l.q.yaki))
    %-  ~(uni in $(q.yaki r.q.yaki))
    ^-  (set [^ship ^desk tako:clay norm:clay path])
    ?.  =(lobe q.n.q.yaki)
      ~
    [[ship desk tako (~(gut by tom.dome) tako nor.dome) p.n.q.yaki] ~ ~]
  --
^-  (list note-arvo)
%+  welp
  %+  murn  ~(tap in norms)
  |=  [=ship =desk =tako:clay =norm:clay =path]
  ?:  ?=([~ %|] (~(fit de norm) path))
    ~
  %-  (slog leaf+"tomb: {<ship desk path `@uv`tako norm path>}" ~)
  ?:  dry
    ~
  `[%c %tomb %worn ship desk tako (~(put de norm) path %|)]
?:  dry
  ~
[%c %tomb %pick ~]~
