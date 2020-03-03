/-  *metadata-store
|%
++  associations-to-json
  |=  =associations
  =,  enjs:format
  ^-  json
  %-  pairs
  %+  turn  ~(tap by associations)
  |=  [[=group-path =resource] =metadata]
  ^-  [cord json]
  :-
  %-  crip
  ;:  weld
      (trip (spat group-path))
      (weld "/" (trip app-name.resource))
      (trip (spat app-path.resource))
  ==
  %-  pairs
  :~  [%group-path (path group-path)]
      [%app-name s+app-name.resource]
      [%app-path (path app-path.resource)]
      [%metadata (metadata-to-json metadata)]
  ==
::
++  json-to-action
  |=  jon=json
  ^-  metadata-action
  =,  dejs:format
  =<  (parse-json jon)
  |%
  ++  parse-json
    %-  of
    :~  [%add add]
        [%remove remove]
    ==
  ::
  ++  add
    %-  ot
    :~  [%group-path pa]
        [%resource resource]
        [%metadata metadata]
    ==
  ++  remove
    %-  ot
    :~  [%group-path pa]
        [%resource resource]
    ==
  ::
  ++  nu
    |=  jon=json
    ?>  ?=({$s *} jon)
    (rash p.jon hex)
  ::
  ++  metadata
    %-  ot
    :~  [%title so]
        [%description so]
        [%color nu]
        [%date-created (se %da)]
        [%creator (su ;~(pfix sig fed:ag))]
    ==
  ++  resource
    %-  ot
    :~  [%app-name so]
        [%app-path pa]
    ==
  --
::
++  metadata-to-json
  |=  met=metadata
  ^-  json
  =,  enjs:format
  %-  pairs
  :~  [%title s+title.met]
      [%description s+description.met]
      [%color s+(scot %ux color.met)]
      [%date-created s+(scot %da date-created.met)]
      [%creator s+(scot %p creator.met)]
  ==
::
++  update-to-json
  |=  upd=metadata-update
  =,  enjs:format
  ^-  json
  %+  frond  %metadata-update
  %-  pairs
  :~  ?-  -.upd
      %add
    :-  %add
    %-  pairs
    :~  [%group-path (path group-path.upd)]
        [%app-name s+app-name.resource.upd]
        [%app-path (path app-path.resource.upd)]
        [%metadata (metadata-to-json metadata.upd)]
    ==
  ::
      %update-metadata
    :-  %update-metadata
    %-  pairs
    :~  [%group-path (path group-path.upd)]
        [%app-name s+app-name.resource.upd]
        [%app-path (path app-path.resource.upd)]
        [%metadata (metadata-to-json metadata.upd)]
    ==
  ::
      %remove
      :-  %remove
      %-  pairs
      :~  [%group-path (path group-path.upd)]
          [%app-name s+app-name.resource.upd]
          [%app-path (path app-path.resource.upd)]
      ==
  ::
      %associations
    [%associations (associations-to-json associations.upd)]
  ==  ==
--
