/-  spider
/+  strandio
=,  strand=strand:spider
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
=+  !<([~ =spar:ames] arg)
;<  ~  bind:m
  (keen:strandio /keen spar)
;<  [* roar=(unit roar:ames)]  bind:m
  (take-tune:strandio /keen)
?~  roar
  (pure:m !>(~))
?~  data=q.dat.u.roar
  (pure:m !>(~))
::
;<  =bowl:spider  bind:m  get-bowl:strandio
;<  =dais:clay  bind:m
  (scry:strandio dais:clay /cb/[q.byk.bowl]/[p.u.data])
=/  res  (mule |.((vale.dais q.u.data)))
?.  ?=(%| -.res)
  (pure:m p.res)
~|(%keen-mark-fail (mean leaf+"-keen: ames vale fail {<mark>}" p.res))
