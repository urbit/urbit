/+  strandio
=,  strand=strand:strandio
|%
::  +turf-confirm-install: self check and install domain
::
++  turf-confirm-install
  |=  =turf
  =/  m  (strand ,?)
  ^-  form:m
  ;<  good=?  bind:m  (self-check-http &+turf 5)
  ?.  good
    (pure:m |)
  ;<  ~       bind:m  (install-domain:strandio turf)
  (pure:m &)
::
::  +self-check-http: confirm our availability at .host on port 80
::
::    XX needs better success/failure predicates
::    XX bind route to self and handle request inside tx?
::
++  self-check-http
  |=  [=host:eyre max=@ud]
  =/  m  (strand ,?)
  ^-  form:m
  ::  XX also scry into eyre
  ::  q:.^(hart:eyre %e /(scot %p our)/host/real)
  =/  =hiss:eyre
    =/  url=purl:eyre
      [[sec=| por=~ host] [ext=`~.udon path=/static] query=~]
    [url %get ~ ~]
  =/  try=@ud  0
  |-  ^-  form:m
  =*  loop  $
  ?:  =(try max)
    (pure:m |)
  ;<  ~                     bind:m  (backoff:strandio try ~h1)
  ;<  rep=(unit httr:eyre)  bind:m  (hiss-request:strandio hiss)
  ?:  ?&  ?=(^ rep)
          |(=(200 p.u.rep) =(307 p.u.rep) =(301 p.u.rep))
      ==
    (pure:m &)
  ?.  ?|  ?=(~ rep)
          =(504 p.u.rep)
      ==
    (pure:m |)
  loop(try +(try))
--
