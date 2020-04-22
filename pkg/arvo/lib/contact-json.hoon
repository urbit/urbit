/-  *contact-view, *contact-hook
/+  base64
|%
++  nu                                              ::  parse number as hex
  |=  jon/json
  ?>  ?=({$s *} jon)
  (rash p.jon hex)
::
++  hook-update-to-json
  |=  upd=contact-hook-update
  =,  enjs:format
  ^-  json
  %+  frond  %contact-hook-update
  %-  pairs
  %+  turn  ~(tap by synced.upd)
  |=  [pax=^path shp=^ship]
  ^-  [cord json]
  [(spat pax) s+(scot %p shp)]
::
++  rolodex-to-json
  |=  rolo=rolodex
  =,  enjs:format
  ^-  json
  %+  frond  %contact-initial
  %-  pairs
  %+  turn  ~(tap by rolo)
  |=  [pax=^path =contacts]
  ^-  [cord json]
  :-  (spat pax)
  (contacts-to-json pax contacts)
::
++  contacts-to-json
  |=  [=path con=contacts]
  ^-  json
  %-  pairs:enjs:format
  %+  turn  ~(tap by con)
  |=  [=ship =contact]
  ^-  [cord json]
  [(crip (slag 1 (scow %p ship))) (contact-to-json path ship contact)]
::
++  contact-to-json
  |=  [=path =ship con=contact]
  ^-  json
  %-  pairs:enjs:format
  :~  [%nickname s+nickname.con]
      [%email s+email.con]
      [%phone s+phone.con]
      [%website s+website.con]
      [%notes s+notes.con]
      [%color s+(scot %ux color.con)]
      [%avatar (avatar-to-json path ship avatar.con)]
  ==
::
++  edit-to-json
  |=  [=path =ship edit=edit-field]
  ^-  json
  %+  frond:enjs:format  -.edit
  ?-  -.edit
    %nickname  s+nickname.edit
    %email     s+email.edit
    %phone     s+phone.edit
    %website   s+website.edit
    %notes     s+notes.edit
    %color     s+(scot %ux color.edit)
    %avatar    (avatar-to-json path ship avatar.edit)
  ==
::
++  avatar-to-json
  |=  [=path =ship avat=(unit avatar)]
  ^-  json
  ?~  avat  ~
  ?-  -.u.avat
      %octt
    :-  %s
    %-  crip
    %-  zing
    :~  "/~groups/avatar"
        (trip (spat path))
        "/"
        (trip (scot %p ship))
    ==
  ::
      %url   s+url.u.avat
  ==
::
++  update-to-json
  |=  upd=contact-update
  =,  enjs:format
  ^-  json
  %+  frond  %contact-update
  %-  pairs
  :~
    ?:  ?=(%create -.upd)
      [%create (pairs [%path (path path.upd)]~)]
    ?:  ?=(%delete -.upd)
      [%delete (pairs [%path (path path.upd)]~)]
    ?:  ?=(%add -.upd)
      :-  %add
      %-  pairs
      :~  [%path (path path.upd)]
          [%ship (ship ship.upd)]
          [%contact (contact-to-json path.upd ship.upd contact.upd)]
      ==
    ?:  ?=(%remove -.upd)
      :-  %remove
      %-  pairs
      :~  [%path (path path.upd)]
          [%ship (ship ship.upd)]
      ==
    ?:  ?=(%edit -.upd)
      :-  %edit
      %-  pairs
      :~  [%path (path path.upd)]
          [%ship (ship ship.upd)]
          [%edit-field (edit-to-json path.upd ship.upd edit-field.upd)]
      ==
    [*@t *^json]
  ==
::
++  json-to-view-action
  |=  jon=json
  ^-  contact-view-action
  =,  dejs:format
  =<  (parse-json jon)
  |%
  ++  parse-json
    %-  of
    :~  [%create create]
        [%delete delete]
        [%remove remove]
        [%share share]
    ==
  ::
  ++  create
    %-  ot
    :~  [%path pa]
        [%ships (as (su ;~(pfix sig fed:ag)))]
        [%title so]
        [%description so]
    ==
  ::
  ++  delete  (ot [%path pa]~)
  ::
  ++  remove
    %-  ot
    :~  [%path pa]
        [%ship (su ;~(pfix sig fed:ag))]
    ==
  ::
  ++  share
    %-  ot
    :~  [%recipient (su ;~(pfix sig fed:ag))]
        [%path pa]
        [%ship (su ;~(pfix sig fed:ag))]
        [%contact cont]
    ==
  --
::
++  json-to-action
  |=  jon=json
  ^-  contact-action
  =,  dejs:format
  =<  (parse-json jon)
  |%
  ++  parse-json
    %-  of
    :~  [%create create]
        [%delete delete]
        [%add add]
        [%remove remove]
        [%edit edit]
    ==
  ::
  ++  create
    (ot [%path pa]~)
  ::
  ++  delete
    (ot [%path pa]~)
  ::
  ++  add
    %-  ot
    :~  [%path pa]
        [%ship (su ;~(pfix sig fed:ag))]
        [%contact cont]
    ==
  ::
  ++  remove
    %-  ot
    :~  [%path pa]
        [%ship (su ;~(pfix sig fed:ag))]
    ==
  ::
  ++  edit
    %-  ot
    :~  [%path pa]
        [%ship (su ;~(pfix sig fed:ag))]
        [%edit-field edit-fi]
    ==
  --
::
++  octet
  %-  ot:dejs:format
  :~  [%p ni:dejs:format]
      [%q so:dejs:format]
  ==
::
++  avat
  |=  jon=json
  ^-  avatar
  |^
  =/  =avatar  (parse-json jon)
  ?-  -.avatar
      %url   avatar
      %octt
    =.  octs.avatar  (need (de:base64 q.octs.avatar))
    avatar
  ==
  ::
  ++  parse-json
    %-  of:dejs:format
    :~  [%octt octt]
        [%url url]
    ==
  ::
  ++  octt
    %-  ot:dejs:format
    :~  [%content-type so:dejs:format]
        [%octs octet]
    ==
  ::
  ++  url  so:dejs:format
  --
::
++  cont
  %-  ot:dejs:format
  :~  [%nickname so:dejs:format]
      [%email so:dejs:format]
      [%phone so:dejs:format]
      [%website so:dejs:format]
      [%notes so:dejs:format]
      [%color nu]
      [%avatar (mu:dejs:format avat)]
  ==
::
++  edit-fi
  %-  of:dejs:format
  :~  [%nickname so:dejs:format]
      [%email so:dejs:format]
      [%phone so:dejs:format]
      [%website so:dejs:format]
      [%notes so:dejs:format]
      [%color nu]
      [%avatar (mu:dejs:format avat)]
  ==
--
