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
    :-  %:  rap  3
            (spat (en-path:resource group))
            '/'
            app-name.md-resource
            (spat (en-path:resource resource.md-resource))
            ~
        ==
    %-  pairs
    :~  [%group s+(enjs-path:resource group)]
        [%app-name s+app-name.md-resource]
        [%resource s+(enjs-path:resource resource.md-resource)]
        [%metadata (^metadatum metadatum)]
    ==
  ::
  ++  edit-field
    |=  edt=^edit-field
    ^-  json
    %+  frond  -.edt
    ^-  json
    ?-  -.edt
      %color                                [%s `@t`(scot %ux color.edt)]
      ?(%title %description %picture %vip)  [%s `@t`+.edt]
      ?(%preview %hidden)                   [%b `?`+.edt]
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
      ::
        :-  %config
        ?+    -.config.met  o+~
            %graph
          %+  frond  %graph
          s+module.config.met
        ::
            %group
          %+  frond  %group
          ?~  feed.config.met
            ~
          ?~  u.feed.config.met
            o+~
          %-  pairs
          :~  [%app-name s+app-name.u.u.feed.config.met]
              [%resource s+(enjs-path:resource resource.u.u.feed.config.met)]
          ==
        ==
      ::
        [%picture s+picture.met]
        [%preview b+preview.met]
        [%hidden b+hidden.met]
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
    ::
        %edit
      :-  %edit
      %-  pairs
      :~  [%group s+(enjs-path:resource group.upd)]
          [%app-name s+app-name.resource.upd]
          [%resource s+(enjs-path:resource resource.resource.upd)]
          [%edit (edit-field edit-field.upd)]
      ==
    ::
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
        [%edit edit]
    ==
  ::
  ++  edit
    %-  ot
    :~  [%group dejs-path:resource]
        [%resource md-resource]
        [%edit edit-field]
    ==
  ::
  ++  edit-field
    %-  of
    :~  [%title so]
        [%description so]
        [%color nu]
        [%picture so]
        [%preview bo]
        [%hidden bo]
        [%vip vip]
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
        %admin-feed
        %host-feed
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
        [%config config]
        [%picture so]
        [%preview bo]
        [%hidden bo]
        [%vip vip]
    ==
  ::
  ++  config
    |=  jon=^json
    ^-  md-config
    ?~  jon
      [%group ~]
    ?>  ?=(%o -.jon)
    ?:  (~(has by p.jon) %graph)
      =/  mod
        (~(got by p.jon) %graph)
      ?>  ?=(%s -.mod)
      [%graph p.mod]
    =/  jin=json
      (~(got by p.jon) %group)
    :+  %group  ~
    ?~  jin
      ~  
    ?>  ?=(%o -.jin)
    ?.  ?&  (~(has by p.jin) 'app-name')
            (~(has by p.jin) 'resource')
        ==
      ~
    =/  app-name=^json  (~(got by p.jin) 'app-name')
    ?>  ?=(%s -.app-name)
    :+  ~
      p.app-name
    =/  res=^json  (~(got by p.jin) 'resource')
    (dejs-path:resource res)
  ::
  ++  md-resource
    ^-  $-(json ^md-resource)
    %-  ot
    :~  [%app-name so]
        [%resource dejs-path:resource]
    ==
  --
--
