/-  *docket
|%
::
+$  lout
  $~  [%l %ud 0]
  $%  [%o p=(map term lout)]
      [%l p=aura q=@]
      [%a p=(list lout)]
  ==
::
+$  pool
  $%  [%tall =wall]
      [%wide =tape]
  ==
::
++  demo-lout
  ^-  lout
  :-  %o
  %-  ~(gas by *(map term lout))
  :~  ship+[%l %p ~zod]
      color+[%l %ux 0xee.5432]
      :-  %members
      :-  %a
      :~  l+t+%tlon
          l+t+%urbit
      ==
  ==
++  enlout
  =|  tab=@
  |=  =lout
  ^-  wall
  ?-  -.lout
      %o
    %-  zing
    %+  turn   ~(tap by p.lout)
    |=  [=term l=^lout]
    =/  res=wall  (enlout l)
    ?>  ?=(^ res)
    ^-  wall
    res(i "{(zing (reap tab " "))}{(trip term)}: {i.res}")
      %l
    ~[(scow [p q]:lout)]
      %a
    %-  zing
    %+  turn   p.lout
    |=  l=^lout
    =/  res=wall  (enlout l)
    ^-  wall
    ?>  ?=(^ res)
    res(i "{(zing (reap tab " "))}- {i.res}")
  ==
::
++  tan
  (enlout demo-lout)
--
