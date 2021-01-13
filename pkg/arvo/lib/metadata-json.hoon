/-  *metadata-store
/+  resource
^?
|%
++  associations-to-json
  |=  =associations
  =,  enjs:format
  ^-  json
  %-  pairs
  %+  turn  ~(tap by associations)
  |=  [=md-resource [group=resource =metadata]]
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
    :~  [%group dejs-path:resource]
        [%resource md-resource]
        [%metadata metadata]
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
  ++  metadata
    %-  ot
    :~  [%title so]
        [%description so]
        [%color nu]
        [%date-created (se %da)]
        [%creator (su ;~(pfix sig fed:ag))]
        [%module so]
    ==
  ::
  ++  md-resource
    ^-  $-(json ^md-resource)
    %-  ot
    :~  [%app-name so]
        [%resource dejs-path:resource]
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
      [%module s+module.met]
  ==
::
++  update-to-json
  |=  upd=metadata-update
  ^-  json
  =,  enjs:format
  %+  frond  %metadata-update
  %-  pairs
  :~  ?+  -.upd  *[cord json]
      %add
    :-  %add
    %-  pairs
    :~  [%group s+(enjs-path:resource group.upd)]
        [%app-name s+app-name.resource.upd]
        [%resource s+(enjs-path:resource resource.resource.upd)]
        [%metadata (metadata-to-json metadata.upd)]
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
    [%associations (associations-to-json associations.upd)]
  ==  ==
--
