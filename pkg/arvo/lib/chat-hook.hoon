/-  sur=chat-hook
^?
=<  [sur .]
=,  sur
|%
::
++  enjs
  |%
  ++  update
    |=  upd=^update
    =,  enjs:format
    ^-  json
    %+  frond  %chat-hook-update
    %-  pairs
    %+  turn  ~(tap by synced.upd)
    |=  [pax=^path shp=^ship]
    ^-  [cord json]
    [(spat pax) s+(scot %p shp)]
  --
++  dejs
  |%
  ::
  ++  action
    |=  jon=json
    ^-  ^action
    =,  dejs:format
    =<  (parse-json jon)
    |%
    ::
    ++  parse-json
      %-  of
      :~  [%add-owned add-owned]
          [%add-synced add-synced]
          [%remove pa]
      ==
    ::
    ++  add-owned
      %-  ot
      :~  [%path pa]
          [%allow-history bo]
      ==
    ::
    ++  add-synced
      %-  ot
      :~  [%ship (su ;~(pfix sig fed:ag))]
          [%path pa]
          [%ask-history bo]
      ==
    --
  --
--
