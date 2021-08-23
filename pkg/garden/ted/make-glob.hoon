/-  spider, docket
/+  strandio
=,  strand=strand:spider
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
=+  !<([~ [=desk dir=path]] arg)
;<  =bowl:spider  bind:m  get-bowl:strandio
=/  home=path  /(scot %p our.bowl)/[desk]/(scot %da now.bowl)
=+  .^(paths=(list path) %ct (weld home dir))
=/  =glob:docket
  %-  ~(gas by *glob:docket)
  %+  turn  paths
  |=  pax=path
  ^-  [path mime]
  :-  (slag (lent dir) pax)
  =/  mar=mark  (rear pax)
  =+  .^(vas=vase %cr (weld home pax))
  =+  .^(=tube:clay %cc (weld home /[mar]/mime))
  !<(mime (tube vas))
=/  =path  /(cat 3 'glob-' (scot %uv (sham glob)))/glob
  ~&  globbed+`(set ^path)`~(key by glob)
;<  ~  bind:m  (poke-our:strandio %hood drum-put+!>([path (jam glob)]))
(pure:m *vase)

