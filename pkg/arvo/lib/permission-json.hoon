/-  *permission-store
|%
++  permission-to-json
  |=  pem=permission-map
  =,  enjs:format
  ^-  json
  %+  frond  %permission-initial
  %-  pairs
  %+  turn  ~(tap by pem)
  |=  [pax=^path =permission]
  ^-  [cord json]
  :-  (spat pax)
  %-  pairs
  :~  [%kind s+kind.permission]
      [%who [%a (turn ~(tap in who.permission) ship)]]
  ==
::
++  ki
  =,  dejs:format
  ^-  $-(json kind)
  (su (perk %black %white ~))
::
++  json-to-set-path-kind
  =,  dejs:format
  %-  as
  %-  ot
  :~  [%path pa]
      [%kind ki]
  ==
::
++  json-to-perm-group-hook-action
  |=  jon=json
  =,  dejs:format
  =<  (parse-action jon)
  |%
  ++  parse-action
    %-  of
    :~  [%associate associate]
        [%dissociate dissociate]
    ==
  ::
  ++  associate
    %-  ot
    :~  [%group pa]
        [%permissions json-to-set-path-kind]
    ==
  ::
  ++  dissociate
    %-  ot
    :~  [%group pa]
        [%permissions (as pa)]
    ==
  ::
  --
--

