=,  clay
|%
++  jam-desk
  |=  [our=ship =desk now=@da tick=@ud]
  ~>  %slog.0^leaf/"jamming desk {<desk>}"
  =+  .^(=rang:clay %cx (en-bema [our %$ [da+now ud+tick]] /rang))
  =+  .^(=tako:clay %cs (en-bema [our desk [da+now ud+tick]] /tako/~))
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
