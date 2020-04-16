/-  spider
/+  strandio
=,  strand=strand:spider
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
::  Parse arguments as ship, desk, and path
::
=+  !<([=target=path ~] arg)
::  Read the file, possibly asyncrhonously
::
;<  =bowl:spider  bind:m  get-bowl:strandio
;<  =riot:clay  bind:m
  (warp:strandio our.bowl %home ~ %next %a [%da now.bowl] target-path)
?~  riot
  ~&  %nothing
  (pure:m !>("nothing"))
%-  (slog leaf+"got writ" (sell !<(vase q.r.u.riot)) ~)
(pure:m !<(vase q.r.u.riot))
