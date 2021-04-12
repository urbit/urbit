/-  sur=metadata-store
/+  resource
^?
=<  [. sur]
=,  sur
|%
++  enjs
  =,  enjs:format
  |%
  ::
  ++  initial-group
    |=  [group=resource assocs=^associations]
    %-  pairs
    :~  group+s+(enjs-path:resource group)
        associations+(associations assocs)
    ==
  ::
  ++  associations
    |=  =^associations
    =,  enjs:format
    ^-  json
    %-  pairs
    %+  turn  ~(tap by associations)
    |=  [=md-resource [group=resource =^metadatum]]
    ^-  [cord json]
    :-
    %-  crip
    ;:  weld
        (trip (spat (en-path:resource group)))
        (weld "/" (trip app-name.md-resource))
        (trip (spat (en-path:resource resource.md-resource)))
    ==
    %-  pairs
    :~  [%group s+(enjs-path:resource group)]
        [%app-name s+app-name.md-resource]
        [%resource s+(enjs-path:resource resource.md-resource)]
        [%metadata (^metadatum metadatum)]
    ==
  ::
  ++  metadatum
    |=  met=^metadatum
    ^-  json
    %-  pairs
    :~  [%title s+title.met]
        [%description s+description.met]
        [%color s+(scot %ux color.met)]
        [%date-created s+(scot %da date-created.met)]
        [%creator s+(scot %p creator.met)]
        [%module s+module.met]
        [%picture s+picture.met]
        [%preview b+preview.met]
        [%vip s+`@t`vip.met]
    ==
  ::
  ++  update
    |=  upd=^update
    ^-  json
    %+  frond  %metadata-update
    %-  pairs
    :~  ?-  -.upd
        %add
      :-  %add
      %-  pairs
      :~  [%group s+(enjs-path:resource group.upd)]
          [%app-name s+app-name.resource.upd]
          [%resource s+(enjs-path:resource resource.resource.upd)]
          [%metadata (metadatum metadatum.upd)]
      ==
        %updated-metadata
      :-  %add
      %-  pairs
      :~  [%group s+(enjs-path:resource group.upd)]
          [%app-name s+app-name.resource.upd]
          [%resource s+(enjs-path:resource resource.resource.upd)]
          [%metadata (metadatum metadatum.upd)]
      ==
    ::
        %remove
        :-  %remove
        %-  pairs
        :~  [%group s+(enjs-path:resource group.upd)]
            [%app-name s+app-name.resource.upd]
            [%resource s+(enjs-path:resource resource.resource.upd)]
        ==
    ::
        %associations
      [%associations (associations associations.upd)]
    ::
        %initial-group
      [%initial-group (initial-group +.upd)]
    ::
    ==  ==
  ::
  ++  hook-update
    |=  upd=^hook-update
    %+  frond  %metadata-hook-update
    %+  frond  -.upd
    %-  pairs
    ?-  -.upd
        %preview
      :~  [%group s+(enjs-path:resource group.upd)]
          [%channels (associations channels.upd)]
          [%members (numb members.upd)]
          [%channel-count (numb channel-count.upd)]
          [%metadata (metadatum metadatum.upd)]
      ==
        %req-preview
      ~[group+s+(enjs-path:resource group.upd)]
    ==
  --
::
++  dejs
  =,  dejs:format
  |%
  ++  action
    %-  of
    :~  [%add add]
        [%remove remove]
        [%initial-group initial-group]
    ==
  ::
  ++  initial-group
    |=  json
    [*resource *associations]
  ::
  ++  add
    %-  ot
    :~  [%group dejs-path:resource]
        [%resource md-resource]
        [%metadata metadatum]
    ==
  ++  remove
    %-  ot
    :~  [%group dejs-path:resource]
        [%resource md-resource]
    ==
  ::
  ++  nu
    |=  jon=json
    ?>  ?=([%s *] jon)
    (rash p.jon hex)
  ::
  ++  vip
    %-  su
    %-  perk
    :~  %reader-comments
        %member-metadata
        %$
    ==
  ::
  ++  metadatum
    ^-  $-(json ^metadatum)
    %-  ot
    :~  [%title so]
        [%description so]
        [%color nu]
        [%date-created (se %da)]
        [%creator (su ;~(pfix sig fed:ag))]
        [%module so]
        [%picture so]
        [%preview bo]
        [%vip vip]
    ==
  ::
  ++  md-resource
    ^-  $-(json ^md-resource)
    %-  ot
    :~  [%app-name so]
        [%resource dejs-path:resource]
    ==
  --
--
