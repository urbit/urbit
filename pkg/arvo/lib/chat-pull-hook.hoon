/-  *chat-pull-hook
/+  *userspace
^?
|%
++  enjs
  |%
  ++  update
    |=  upd=^update
    =,  enjs:format
    ^-  json
    %+  frond  %chat-pull-hook-update
    %-  pairs
    %+  turn  ~(tap by tracking.upd)
    |=  [=rid ship=^ship]
    ^-  [cord json]
    [(spat (rid-to-path rid)) s+(scot %p ship)]
  --
::
++  dejs
  |%
  ++  action
    |=  jon=json
    ^-  ^action
    =,  dejs:format
    =<  (parse-json jon)
    |%
    ++  parse-json
      %-  of
      :~  [%add add]
          [%remove pa]
      ==
    ::
    ++  add
      %-  ot
      :~  [%ship (su ;~(pfix sig fed:ag))]
          [%path pa]
          [%ask-history bo]
      ==
    --
  --
--
