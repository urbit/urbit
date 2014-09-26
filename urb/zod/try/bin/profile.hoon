|%
++  doss                                          
  $:  sap=,@ud                                          ::  sample count
      hit=(map term ,@ud)                               ::  hit points
      cut=(map span hump)                               ::  cut points
  ==
::
++  hump
  $:  sap=,@ud                                          ::  sample count
      inn=(map span ,@ud)                               ::  calls into
      out=(map span ,@ud)                               ::  calls out of
  ==
::
++  pi
  |_  day=doss
  ++  heck                                              ::  report event
    |=  [nam=@tas day=doss]
    ^-  day
    =+  lam=(~(get by hit.day) nam)
    day(hit (~(put by hit.day) ?~(lam 1 +(u.lam))))
  ::
  ++  noon                                              ::  sample trace
    |=  pax=path
    =|  lax=(unit span)
    |-  ^-  day
    ?~  pax  day(sap +(sap.day))
    %=    $
        pax  t.pax
        lax  `i.pax
        cut.day
      %+  ~(put by cut.day)  i.pax
      ^-  hump
      =+  nax=`(unit span)`?~(t.pax ~ `i.t.pax)
      =+  hup=`hump`=+(hup=(~(get by cut) i.pax) ?^(hup u.hup [0 ~ ~]))
      :+  +(sap.hup)
        ?~  lax  inn.hup 
        =+(hag=(~(get by inn.hup) u.lax) ?~(hag 1 +(u.hag)))
      ?~  nax  out.hup 
      =+(hag=(~(get by out.hup) u.nax) ?~(hag 1 +(u.hag)))
    ==
  ::
  ++  tell                                              ::  produce dump
    ^-  (list tape)
    ;:  welp
      ^-  (list tape)
      [(welp "events: " (scow %ud sap.day)) ~]
    ::
      ^-  (list tape)
      %+  turn
        (~(tap by hit.day) ~)
      |=  [nam=term num=@ud]
      :(welp (trip nam) ": " (scow %ud num))
      ["" ~]
    ::
      ^-  (list tape)
      %-  welp
      %+  turn
        (~(tap by cut.day) ~)
      |=  [nam=term hup=hump]
      ^-  (list tape)
      ;:  welp
        [(welp "sector: " (trip nam)) ~]
      ::
        [(welp "weight: " (div (mul 1.000 sap.hup) sap.day)) ~]
      ::
        ["inn:" ~]
      ::
        %+  turn
          (~(tap by inn.hup) ~)
        |=  [nam=term num=@ud]
        ^-  tape
        :(welp "  " (trip nam) ": " (scow %ud num))
      ::
        ["out:" ~]
      ::
        %+  turn
          (~(tap by out.hup) ~)
        |=  [nam=term num=@ud]
        ^-  tape
        :(welp "  " (trip nam) ": " (scow %ud num))
      ==
    ==
  --
--
