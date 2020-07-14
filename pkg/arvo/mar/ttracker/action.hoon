/-  ttracker
|_  act=action:ttracker
++  grow
  |%
  ++  tank  >act<
  --
::
++  grab
  |%
  ++  noun  action:ttracker
  ++  json
    |=  jon=^json
    %-  action:ttracker
    =<  (action jon)
    |%
    ++  action
      %-  of:dejs:format
      :~  facilities+facilities
      ==
    ::
    ++  facilities
    %-  ot:dejs:format
      :~  station+so:dejs:format
      ==
    --
  --
--