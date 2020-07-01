/-  *permission-store
|%
++  update-to-json
  |=  upd=permission-update
  ^-  json
  =,  enjs:format
  %+  frond  %permission-update
  %-  pairs
  :~
    ?:  =(%initial -.upd)
      ?>  ?=(%initial -.upd)
      [%initial (permission-to-json permissions.upd)]
    ::
    ::  %create
    ?:  =(%create -.upd)
      ?>  ?=(%create -.upd)
      :-  %create
      %-  pairs
      :~  [%path (path path.upd)]
          [%kind s+kind.permission.upd]
          [%who [%a (turn ~(tap in who.permission.upd) ship)]]
      ==
    ::
    ::  %delete
    ?:  =(%delete -.upd)
      ?>  ?=(%delete -.upd)
      [%delete (path path.upd)]
    ::
    ::  %add
    ?:  =(%add -.upd)
      ?>  ?=(%add -.upd)
      :-  %add
      %-  pairs
      :~  [%path (path path.upd)]
          [%who [%a (turn ~(tap in who.upd) ship)]]
      ==
    ::
    ::  %remove
    ?:  =(%remove -.upd)
      ?>  ?=(%remove -.upd)
      :-  %remove
      %-  pairs
      :~  [%path (path path.upd)]
          [%who [%a (turn ~(tap in who.upd) ship)]]
      ==
    ::
    ::  %noop
    [*@t *^json]
  ==
::
++  permission-to-json
  |=  pem=permission-map
  =,  enjs:format
  ^-  json
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

