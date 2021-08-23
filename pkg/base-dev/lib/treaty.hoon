/-  *treaty
/+  dock=docket
|%
++  enjs
  =,  enjs:format
  |%
  ++  merge
    |=  [a=json b=json]
    ^-  json
    ?>  &(?=(%o -.a) ?=(%o -.b))
    [%o (~(uni by p.a) p.b)]
  ::
  ++  treaty
    |=  t=^treaty
    %+  merge  (docket:enjs:dock docket.t)
    %-  pairs
    :~  ship+s+(scot %p ship.t)
        desk+s+desk.t
        cass+(case case.t)
        hash+s+(scot %uv hash.t)
    ==
  ::
  ++  case
    |=  c=^case
    %+  frond  -.c
    ?-  -.c
      %da   s+(scot %da p.c)
      %tas  s+(scot %tas p.c)  
      %ud   (numb p.c)
    ==
  ++  foreign-desk
    |=  [s=^ship =desk]
    ^-  cord
    (crip "{(scow %p s)}/{(trip desk)}")
  ::
  ++  alliance
    |=  a=^alliance
    ^-  json
    :-  %a 
    %+  turn  ~(tap in a)
    |=  [=^ship =desk]
    ^-  json
    s+(foreign-desk ship desk)
  ::
  ++  treaty-update
    |=  u=update:^treaty
    ^-  json
    %+  frond  -.u
    ?-  -.u
      %add  (treaty treaty.u)
      %del  s+(foreign-desk +.u)
    ::
        %ini
      %-  pairs
      %+  turn  ~(tap by init.u)
      |=  [[s=^ship =desk] t=^treaty]
      [(foreign-desk s desk) (treaty t)]
    ==
  ::
  ++  ally-update
    |=  u=update:ally
    ^-  json
    %+  frond  -.u
    ?-  -.u
      ?(%add %del)  s+(scot %p ship.u)
    ::
        %ini
      %-  pairs
      %+  turn  ~(tap by init.u)
      |=  [s=^ship a=^alliance]
      [(scot %p s) (alliance a)]
    ::
        %new
      %-  pairs
      :~  ship+s+(scot %p ship.u)
          alliance+(alliance alliance.u)
      ==
    ==
  --
--

