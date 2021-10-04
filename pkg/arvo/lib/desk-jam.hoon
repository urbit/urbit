=,  clay
|%
++  jam-desk
  |=  [our=ship =desk now=@da]
  ~>  %slog.0^leaf/"jamming desk {<desk>}"
  %-  jam
  %-  ?:(=(%base desk) remove-misc-dirs same)
  %-  ankh-to-map
  =<  ank
  .^(dome:clay %cv /(scot %p our)/[desk]/(scot %da now))
::
++  remove-misc-dirs
  |=  fiz=(map path page)
  ^-  (map path page)
  %-  ~(gas by *(map path page))
  %+  skip  ~(tap by fiz)
  |=  [p=path *]
  ?|  ?=([%tmp *] p)
      ?=([%tests *] p)
  ==
::
++  ankh-to-map
  =|  res=(map path page)
  =|  pax=path
  |=  a=ankh
  ^-  (map path page)
  =?  res  ?=(^ fil.a)  (~(put by res) pax [p q.q]:q.u.fil.a)
  =/  dir=(list [seg=@ta =ankh])  ~(tap by dir.a)
  |-  ^+  res
  ?~  dir  res
  $(dir t.dir, res ^$(pax (snoc pax seg.i.dir), a ankh.i.dir))
--
