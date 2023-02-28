=,  clay
|%
++  jam-desk
  |=  [our=ship =desk now=@da]
  ~>  %slog.0^leaf/"jamming desk {<desk>}"
  =+  .^(=rang:clay %cx /(scot %p our)//(scot %da now)/rang)
  =+  .^(=tako:clay %cs /(scot %p our)/[desk]/(scot %da now)/tako/~)
  %-  jam
  %-  ?:(=(%base desk) remove-misc-dirs same)
  %-  ~(run by q:(~(got by hut.rang) tako))
  ~(got by lat.rang)
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
--
