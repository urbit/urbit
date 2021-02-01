/-  sur=group-view, spider
/+  resource, strandio, metadata=metadata-store
^?
=<  [. sur]
=,  sur
|%
++  dejs
  =,  dejs:format
  |%
  ++  action
    %-  of
    :~  join+join
    ==
  ::
  ++  join
    %-  ot
    :~  resource+dejs:resource
        ship+(su ;~(pfix sig fed:ag))
    ==
  --
::
++  enjs
  =,  enjs:format
  |%
  ++  update
    |=  upd=^update
    %+  frond  %group-view-update
    %+  frond  -.upd
    ?-  -.upd
      %initial  (initial +.upd)
      %progress  (progress +.upd)
    ==
  ::
  ++  progress
    |=  [rid=resource prog=^progress]
    %-  pairs
    :~  resource+s+(enjs-path:resource rid)
        progress+s+prog
    ==
  ::
  ++  initial
    |=  init=(map resource ^progress)
    %-  pairs
    %+  turn  ~(tap by init)
    |=  [rid=resource prog=^progress]
    :_  s+prog
    (enjs-path:resource rid)
  --
++  cleanup-md
  |=  rid=resource
  =/  m  (strand:spider ,~)
  ^-  form:m
  ;<  =associations:metadata  bind:m  
    %+  scry:strandio  associations:metadata
    %+  weld  /metadata-store/group 
    (snoc (en-path:resource rid) %noun)
  =/  assocs=(list [=md-resource:metadata association:metadata])
    ~(tap by associations) 
  |-  
  =*  loop  $
  ?~  assocs
    (pure:m ~)
  ;<  ~  bind:m
    %+  poke-our:strandio  %metadata-store
    metadata-action+!>([%remove rid md-resource.i.assocs])
  loop(assocs t.assocs)
--
