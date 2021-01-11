/-  sur=contact-store
=<  [sur .]
=,  sur
|%
++  nu                                              ::  parse number as hex
  |=  jon=json
  ?>  ?=([%s *] jon)
  (rash p.jon hex)
::
++  enjs
  =,  enjs:format
  |%
  ++  update
    |=  upd=^update
    ^-  json
    %+  frond  %contact-update
    %-  pairs
    :_  ~
    ^-  [cord json]
    ?-  -.upd
        %initial
      :-  %initial
      (pairs [%rolodex (rolo rolodex.upd)]~)
    ::
        %add
      :-  %add
      %-  pairs
      :~  [%ship (ship ship.upd)]
          [%contact (cont contact.upd)]
      ==
    ::
        %remove
      :-  %remove
      (pairs [%ship (ship ship.upd)]~)
    ::
        %edit
      :-  %edit
      %-  pairs
      :~  [%ship (ship ship.upd)]
          [%edit-field (edit edit-field.upd)]
      ==
    ==
  ::
  ++  rolo
    |=  =rolodex
    ^-  json
    %-  pairs
    %+  turn  ~(tap by rolodex)
    |=  [=^ship =contact]
    ^-  [cord json]
    [(scot %p ship) (cont contact)]
  ::
  ++  cont
    |=  =contact
    ^-  json
    %-  pairs
    :~  [%nickname s+nickname.contact]
        [%email s+email.contact]
        [%phone s+phone.contact]
        [%website s+website.contact]
        [%color s+(scot %ux color.contact)]
        [%avatar ?~(avatar.contact ~ s+u.avatar.contact)]
    ==
  ::
  ++  edit
    |=  field=edit-field
    ^-  json
    %+  frond  -.field
    ?-  -.field
      %nickname  s+nickname.field
      %email     s+email.field
      %phone     s+phone.field
      %website   s+website.field
      %color     s+(scot %ux color.field)
      %avatar    ?~(avatar.field ~ s+u.avatar.field)
    ==
  --
::
++  dej
  =,  dejs:format
  |%
  ++  update
    |=  jon=json
    ^-  ^update
    =<  (decode jon)
    |%
    ++  decode
      %-  of
      :~  [%initial initial]
          [%add add-contact]
          [%remove remove-contact]
          [%edit edit-contact]
      ==
    ::
    ++  initial  (op ;~(pfix sig fed:ag) cont)
    ::
    ++  add-contact
      %-  ot
      :~  [%ship (su ;~(pfix sig fed:ag))]
          [%contact cont]
      ==
    ::
    ++  remove-contact  (ot [%ship (su ;~(pfix sig fed:ag))]~)
    ::
    ++  edit-contact
      %-  ot
      :~  [%ship (su ;~(pfix sig fed:ag))]
          [%edit-field edit]
      ==
    ::
    ++  cont
      %-  ot
      :~  [%nickname so]
          [%email so]
          [%phone so]
          [%website so]
          [%color nu]
          [%avatar (mu so)]
      ==
    ::
    ++  edit
      %-  of
      :~  [%nickname so]
          [%email so]
          [%phone so]
          [%website so]
          [%color nu]
          [%avatar (mu so)]
      ==
    --
  --
--
