/-  sur=resource
=<  resource
|%
+$  resource  resource:sur
++  en-path
  |=  =resource
  ^-  path
  ~[%ship (scot %p entity.resource) name.resource]
::
++  de-path
  |=  =path
  ^-  resource
  (need (de-path-soft path))
::
++  de-path-soft
  |=  =path
  ^-  (unit resource)
  ?.  ?=([%ship @ @ *] path)
    ~
  =/  ship
    (slaw %p i.t.path)
  ?~  ship
    ~
  `[u.ship i.t.t.path]
::
++  enjs
  |=  =resource
  ^-  json
  =,  enjs:format
  %-  pairs
  :~  ship+(ship entity.resource)
      name+s+name.resource
  ==
::
++  enjs-path
  |=  =resource
  %-  spat
  (en-path resource)
::
++  dejs-path
  %-  su:dejs:format
  ;~  pfix
    (jest '/ship/')
    ;~((glue fas) ;~(pfix sig fed:ag) urs:ab)
  ==
::
++  dejs
  =,  dejs:format
  ^-  $-(json resource)
  |=  jon=json
  ~|  dejs+%resource
  %.  jon
  %-  ot
  :~  ship+(su ;~(pfix sig fed:ag))
      name+so
  ==
--
