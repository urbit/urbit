/-  spider
/+  strandio
=,  strand=strand:spider
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
|^
=+  !<([~ =a=path =b=path] arg)
=/  a-mark=mark  -:(flop a-path)
=/  b-mark=mark  -:(flop b-path)
?.  =(a-mark b-mark)
  (strand-fail:strandio %files-not-same-type ~)
=/  a-beam  (need (de-beam a-path))
;<  =a=cage     bind:m  (get-file a-path)
;<  =b=cage     bind:m  (get-file b-path)
;<  =dais:clay  bind:m  (build-mark:strandio -.a-beam a-mark)
(pure:m (~(diff dais q.a-cage) q.b-cage))
::
++  get-file
  |=  =path
  =/  m  (strand ,cage)
  ^-  form:m
  =/  beam  (need (de-beam path))
  ;<  =riot:clay  bind:m
    (warp:strandio p.beam q.beam ~ %sing %x r.beam s.beam)
  ?~  riot
    (strand-fail:strandio %file-not-found >path< ~)
  (pure:m r.u.riot)
--
