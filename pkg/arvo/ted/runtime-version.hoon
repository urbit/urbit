/-  spider
/+  strandio
=>
|%
+$  vere-update  [cur=vere next=(unit vere)]
::
++  parse-current-pace
  |=  current=vere
  ^-  @t
  (snag 1 rev.current)
::
++  parse-current-version
  |=  current=vere
  ^-  @t
  (slav %ta (rear rev.current))
::
++  is-equal-version
  |=  [latest=@t current=vere]
  =(latest (parse-current-version current))
--
=,  strand=strand:spider
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
;<  =bowl:spider  bind:m  get-bowl:strandio
=/  cur=vere  .^(vere %$ /(scot %p our.bowl)//(scot %da now.bowl)/zen/ver)
=/  pace=tape
  ?:  =('once' (parse-current-pace cur))
    "live"
  (trip (parse-current-pace cur))
;<  latest=cord  bind:m
  (fetch-cord:strandio "https://bootstrap.urbit.org/vere/{pace}/last")
=/  =vere-update
  ?:  (is-equal-version latest cur)
    [cur ~]
  =|  next=vere
  [cur `next(rev /vere/(crip pace)/(scot %ta latest))]
%-  pure:m
!>(vere-update)
