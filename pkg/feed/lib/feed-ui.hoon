/-  *feed-ui
/+  post
|%
++  dejs
  =,  dejs:format
  |%
  ++  uid  (su ;~(pfix (jest '0x') hex:ag))
  ++  request
    %-  ot
    :~  uid+uid
        action+action
    ==
  ::
  ++  action
    ^-  $-(json ^action)
    %-  of
    :~  [%add-post add-post]
        [%like like]
    ==
  ::
  ++  like
    %-  ot
    :~  ship+(su ;~(pfix sig fed:ag))
        id+id
    ==
  ::
  ++  add-post
    %-  ot
    :~  ship+(su ;~(pfix sig fed:ag))
        letter+letter
    ==
  ++  id  (su ;~(pfix sig (cook year when:^so)))
  ::
  ++  letter
    ^-  $-(json letter:post)
    %-  ou
    :~  host+(un (su ;~(pfix sig fed:ag)))
        parent+(uf ~ (mu id))
        author+(un (su ;~(pfix sig fed:ag)))
        contents+(un contents:dejs:post)
        time+(un di)
    ==
  --
++  enjs
  =,  enjs:format
  |%
  ::
  ++  response
    |=  r=^response
    %-  pairs
    :~  uid+s+(scot %ux p.r)
        :-  %data
      %+  frond  -.q.r
      ?-  -.q.r
        %ack  ~
      ==
    ==
  ::
  ++  update
    |=  u=^update
    ?-  -.u
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
