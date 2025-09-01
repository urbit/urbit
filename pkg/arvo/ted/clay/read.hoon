/-  spider
/+  strandio
=,  strand=strand:spider
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
::  Parse arguments as ship, desk, and path
::
=+  !<([~ =care:clay =ship =desk =case =target=path] arg)
::  Read the file, possibly asyncrhonously
::
;<  =bowl:spider  bind:m  get-bowl:strandio
;<  =riot:clay  bind:m
  (warp:strandio ship desk ~ %sing care case target-path)
?~  riot
  (pure:m !>('nothing'))
(pure:m q.r.u.riot)
