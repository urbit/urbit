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
      :-  %initial
      %-  pairs
      :~  [%rolodex (rolo rolodex.upd)]
          [%is-public b+is-public.upd]
      ==
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
          [%timestamp (time timestamp.upd)]
      ==
    ::
        %allow
      :-  %allow
      (pairs [%beings (beng beings.upd)]~)
    ::
        %disallow
      :-  %disallow
      (pairs [%beings (beng beings.upd)]~)
    ::
        %set-public
      [%set-public b+public.upd]
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
        [%bio s+bio.contact]
        [%status s+status.contact]
        [%color s+(scot %ux color.contact)]
        [%avatar ?~(avatar.contact ~ s+u.avatar.contact)]
        [%cover ?~(cover.contact ~ s+u.cover.contact)]
        [%groups a+(turn ~(tap in groups.contact) (cork enjs-path:res (lead %s)))]
        [%last-updated (time last-updated.contact)]
    ==
  ::
  ++  edit
    |=  field=edit-field
    ^-  json
    %+  frond  -.field
    ?-  -.field
      %nickname      s+nickname.field
      %bio           s+bio.field
      %status        s+status.field
      %color         s+(scot %ux color.field)
      %avatar        ?~(avatar.field ~ s+u.avatar.field)
      %cover         ?~(cover.field ~ s+u.cover.field)
      %add-group     s+(enjs-path:res resource.field)
      %remove-group  s+(enjs-path:res resource.field)
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
          [%set-public bo]
      ==
    ::
    ++  initial
      %-  ot
      :~  [%rolodex (op ;~(pfix sig fed:ag) cont)]
          [%is-public bo]
      ==
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
          [%timestamp di]
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
          [%bio so]
          [%status so]
          [%color nu]
          [%avatar (mu so)]
          [%cover (mu so)]
          [%groups (as dejs:res)]
          [%last-updated di]
      ==
    ::
    ++  edit
      %-  of
      :~  [%nickname so]
          [%bio so]
          [%status so]
          [%color nu]
          [%avatar (mu so)]
          [%cover (mu so)]
          [%add-group dejs:res]
          [%remove-group dejs:res]
      ==
    --
  --
::
++  share-dejs
  =,  dejs:format
  |%
  ++  share  
      ^-  $-(json [%share ship])
      (of share+(su ;~(pfix sig fed:ag)) ~)
  --
--
