/-  spider
/+  strandio
=,  strand=strand:spider
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
=+  !<([~ arg=[ship path]] arg)
;<  dat=(unit (cask))  bind:m
  (keen:strandio arg)
?~  dat
  (pure:m !>(~))
;<  =bowl:spider  bind:m  get-bowl:strandio
=+  .^  =dais:clay  %cb
        /(scot %p our.bowl)/[q.byk.bowl]/(scot %da now.bowl)/[p.u.dat]
    ==
=/  res  (mule |.((vale.dais q.u.dat)))
?:  ?=(%| -.res)
  ~|(%keen-mark-fail (mean leaf+"-keen: ames vale fail {<mark>}" p.res))
(pure:m p.res)

