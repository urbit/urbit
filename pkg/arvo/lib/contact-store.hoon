/-  sur=contact-store
/+  res=resource
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
      [%initial (rolo rolodex.upd)]
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
    ::
        %allow
      :-  %allow
      (pairs [%beings (beng beings.upd)]~)
    ::
        %disallow
      :-  %disallow
      (pairs [%beings (beng beings.upd)]~)
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
  ::
  ++  beng
    |=  =beings
    ^-  json
    ?-  -.beings
      %ships  [%a (turn ~(tap in ships.beings) |=(s=^ship s+(scot %p s)))]
      %group  (enjs:res resource.beings)
    ==
  --
::
++  dejs
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
          [%allow beings]
          [%disallow beings]
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
    ++  beings
      %-  of
      :~  [%ships (as (su ;~(pfix sig fed:ag)))]
          [%group dejs:res]
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
