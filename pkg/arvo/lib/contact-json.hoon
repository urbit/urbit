/-  *contact-store
|%
++  nu                                              ::  parse number as hex
  |=  jon/json
  ?>  ?=({$n *} jon)
  (rash p.jon hex)
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
  (pairs [%contacts (contacts-to-json contacts)]~)
::
++  contacts-to-json
  |=  con=contacts
  ^-  json
  =,  enjs:format
  %-  pairs
  %+  turn  ~(tap by con)
  |=  [shp=^ship =contact]
  ^-  [cord json]
  :-  (crip (slag 1 (scow %p shp)))
  (contact-to-json contact)
::
++  contact-to-json
  |=  con=contact
  ^-  json
  =,  enjs:format
  %-  pairs
  :~  [%nickname s+nickname.con]
      [%email s+email.con]
      [%phone s+phone.con]
      [%website s+website.con]
      [%notes s+notes.con]
      [%color s+(scot %ux color.con)]
      [%avatar s+'TODO']
  ==
::
++  edit-to-json
  |=  edit=edit-field
  ^-  json
  ?-  -.edit
    %nickname  s+nickname.edit
    %email     s+email.edit
    %phone     s+phone.edit
    %website   s+website.edit
    %notes     s+notes.edit
    %color     s+(scot %ux color.edit)
    %avatar    s+'TODO'
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
          [%contact (contact-to-json contact.upd)]
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
          [%edit-field (edit-to-json edit-field.upd)]
      ==
    [*@t *^json]
  ==
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
  %-  ot:dejs:format
  :~  [%content-type so:dejs:format]
      [%octs octet]
  ==
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
