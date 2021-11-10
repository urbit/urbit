/-  *dm-hook
|%
::
++  dejs
  =,  dejs:format
  |%
  ++  action
    |^
    %-  of
    :~  accept+ship
        decline+ship
        pendings+ships
        screen+bo
    ==
    ::
    ++  ship  (su ;~(pfix sig fed:ag))
    ::
    ++  ships  (as ship)
    --
  --
::
++  enjs
  =,  enjs:format
  |%
  ::
  ++  action
    |=  act=^action
    %+  frond  -.act
    ?-  -.act
      ?(%accept %decline)  (ship +.act)
      %pendings  a+(turn ~(tap in ships.act) ship)
      %screen   [%b +.act]
    ==
  --
--
        
