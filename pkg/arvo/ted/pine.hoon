/-  spider
/+  strandio
=,  strand=strand:spider
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
=+  !<([~ arg=[=ship path]] arg)
;<  =bowl:spider  bind:m  get-bowl:strandio
?:  =(our.bowl ship.arg)
  ~&  >>>  "fine: can't pine yourself"
  (pure:m !>(~))
;<  dat=(unit (cask))  bind:m  (pine:strandio arg)
?~  dat
  (pure:m !>(~))
=+  .^  =dais:clay  %cb
        /(scot %p our.bowl)/[q.byk.bowl]/(scot %da now.bowl)/[p.u.dat]
    ==
=/  res  (mule |.((vale.dais q.u.dat)))
?:  ?=(%| -.res)
  ~|(%pine-mark-fail (mean leaf+"-pine: ames vale fail {<mark>}" p.res))
(pure:m p.res)

