/-  spider
/+  strandio
=>
|%
+$  vere-update  [cur=vere next=(unit vere)]
::
:: parse out the commit suffix for people on pre-release vere
:: these revisions look like /vere/~.2.7-de2d39b
:: we will have better pre-release (pace) handling later
++  parse-current-version
  |=  current=vere
  ^-  @t
  =/  v
    %+  rush
      (slav %ta (rear rev.current))
    ;~((glue hep) (star ;~(pose nud dot)) (star aln))
  ?~  v  (slav %ta (rear rev.current))
  (crip -.u.v)
::
++  is-equal-version
  |=  [latest=@t current=vere]
  =(latest (parse-current-version current))
--
=,  strand=strand:spider
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
;<  latest=cord  bind:m
  (fetch-cord:strandio "https://bootstrap.urbit.org/vere/live/last")
;<  =bowl:spider  bind:m  get-bowl:strandio
=/  cur=vere  .^(vere %$ /(scot %p our.bowl)//(scot %da now.bowl)/zen/ver)
=/  =vere-update
  ?:  (is-equal-version latest cur)
    [cur ~]
  =|  next=vere
  [cur `next(rev /vere/(scot %ta latest))]
%-  pure:m
!>(vere-update)
