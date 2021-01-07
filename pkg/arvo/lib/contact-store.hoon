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
    |^  (frond %contact-update (pairs ~[(encode upd)]))
    ::
    ++  encode
      |=  upd=update-0
      ^-  [cord json]
      ?-  -.upd
          %initial
        ::  TODO: initial
        :-  %initial
        *json
          %add
        :-  %add
        %-  pairs
        :~  [%path (path path.upd)]
            [%ship (ship ship.upd)]
            [%contact (contact-to-json path.upd ship.upd contact.upd)]
        ==
      ::
          %remove
        :-  %remove
        %-  pairs
        :~  [%path (path path.upd)]
            [%ship (ship ship.upd)]
        ==
      ::
          %edit
        :-  %edit
        %-  pairs
        :~  [%path (path path.upd)]
            [%ship (ship ship.upd)]
            [%edit-field (edit-to-json edit-field.upd)]
        ==
      ==
    --
  ::
  ++  rolodex-to-json
    |=  rol=rolodex:store
    ^-  json
    %-  pairs:enjs:format
    %+  turn  ~(tap by rol)
    |=  [=ship =contact:store]
    ^-  [cord json]
    [(crip (slag 1 (scow %p ship))) (contact-to-json contact)]
  ::
  ++  contact-to-json
    |=  con=contact:store
    ^-  json
    %-  pairs:enjs:format
    :~  [%nickname s+nickname.con]
        [%email s+email.con]
        [%phone s+phone.con]
        [%website s+website.con]
        [%color s+(scot %ux color.con)]
        [%avatar ?~(avatar.edit ~ s+u.avatar.con)]
    ==
  ::
  ++  edit-to-json
    |=  edit=edit-field
    ^-  json
    %+  frond:enjs:format  -.edit
    ?-  -.edit
      %nickname  s+nickname.edit
      %email     s+email.edit
      %phone     s+phone.edit
      %website   s+website.edit
      %color     s+(scot %ux color.edit)
      %avatar    ?~(avatar.edit ~ s+u.avatar.edit)
    ==
  --
::
++  dej
  =,  dejs:formats
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
    ++  edit-fi
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
