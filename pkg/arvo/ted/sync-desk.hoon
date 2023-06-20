/-  spider
/+  strandio
=,  strand=strand:spider
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
=+  !<([~ my=desk =ship her=desk] arg)
;<  ~  bind:m
  (keen:strandio /keen ship /c/s/1/[her]/late)
;<  [* roar=(unit roar:ames)]  bind:m
  (take-tune:strandio /keen)
?~  roar
  ~|([%cs-roar-null her=her] !!)
?~  data=q.dat.u.roar
  ~|([%cs-data-null her=her] !!)
::
=/  =cass:clay  ;;(cass:clay q.u.data)
;<  ~  bind:m
  (keen:strandio /keen ship /c/t/[(scot %ud ud.cass)]/[her])
;<  [* roar=(unit roar:ames)]  bind:m
  (take-tune:strandio /keen)
?~  roar
  ~|([%ct-roar-null case=cass her=her] !!)
?~  data=q.dat.u.roar
  ~|([%ct-roar-null case=cass her=her] !!)
=/  res  ;;((list path) q.u.data)
::
=|  sob=soba:clay
|-
?^  res
  =/  p=path  i.res
  ;<  ~  bind:m
    (keen:strandio /keen ship (weld /c/x/[(scot %ud ud.cass)]/[her] p))
  ;<  [* roar=(unit roar:ames)]  bind:m
    (take-tune:strandio /keen)
  ?~  roar
    ~|([%cx-roar-null case=cass her=her path=p] !!)
  ?~  data=q.dat.u.roar
    ~|([%cx-data-null case=cass her=her path=p] !!)
  =/  s  [p %ins p.u.data !>(q.u.data)]
  $(res t.res, sob [s sob])
;<  ps=(list path)  bind:m  (scry:strandio (list path) /ct/[my])
=/  ins=(set path)  (silt (turn sob head))
=/  dif=(set path)  (~(dif in (silt ps)) ins)
=.  sob  (weld (turn ~(tap by dif) |=(p=path [p %del ~])) sob)
;<  ~  bind:m
  (send-raw-card:strandio [%pass /sync-desk %arvo %c %info my %.y sob])
;<  ~  bind:m  (sleep:strandio ~s0)  ::  wait for merge to complete
;<  hash=@uv  bind:m  (scry:strandio @uv /cz/[my])
(pure:m !>(hash))
