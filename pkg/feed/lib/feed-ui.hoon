/-  *feed-ui
/+  post
|%
++  dejs
  =,  dejs:format
  |%
  ++  update
    %-  of
    :~  [%add-post add-post]
    ==
  ::
  ++  add-post
    %-  ot
    :~  ship+(su ;~(pfix sig fed:ag))
        letter+letter
    ==
  ::
  ++  letter
    %-  ot
    :~  parent+(mu ni)
        ship+(su ;~(pfix sig fed:ag))
        contents+contents:dejs:post
        time+sd
    ==
  --
++  enjs
  =,  enjs:format
  |%
  ++  update
    |=  u=^update
    %+  frond  -.u
    ?+  -.u  ~
      %list  a+(turn p.u post:enjs:post)
      %feeds  a+(turn ~(tap in p.u) ship)
    ==
  ::
  ++  ship  
    |=  s=^^ship
    ^-  json
    s+(scot %p s)
  ::
  ++  id
    |=  i=id:^post
    ^-  json
    s+(scot %da i)
  --
--
