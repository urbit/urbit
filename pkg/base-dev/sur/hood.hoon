=,  clay
=*  dude  dude:gall
|%
+$  pike
  $:  sync=(unit [=ship =desk])
      hash=@uv
      belt:tire
  ==
::
+$  pikes  (map desk pike)
::
::  $rung: reference to upstream commit
::
+$  rung  [=aeon =weft]
::
+$  seal        (list perm:gall)
+$  sync-state  [nun=@ta kid=(unit desk) let=@ud]
+$  sink        (unit [her=@p sud=desk kid=(unit desk) let=@ud])
::  +report-prep: get data required for reports
::
++  report-prep
  |=  [our=@p now=@da]
  =/  ego  (scot %p our)
  =/  wen  (scot %da now)
  :*  .^(rock:tire %cx /(scot %p our)//(scot %da now)/tire)
      .^(=cone %cx /(scot %p our)//(scot %da now)/domes)
      .^((map desk [ship desk]) %gx /[ego]/hood/[wen]/kiln/sources/noun)
      .^  (map [desk ship desk] sync-state)  %gx
          /[ego]/hood/[wen]/kiln/syncs/noun
      ==
  ==
::  +report-vats: report on all desk installations
::
++  report-vats
  |=  [our=@p now=@da]
  ^-  tang
  =/  desks  .^((set desk) %cd /(scot %p our)/base/(scot %da now))
  =/  prep  (report-prep our now)
  %+  turn  ~(tap in desks)
  |=(syd=desk (report-vat prep our now syd))
::  +report-vat: report on a single desk installation
::
++  report-vat
  |=  $:  $:  tyr=rock:tire  =cone  sor=(map desk [ship desk])
              zyn=(map [desk ship desk] sync-state)
          ==
          our=ship  now=@da  syd=desk
      ==
  ^-  tank
  =/  ego  (scot %p our)
  =/  wen  (scot %da now)
  =+  .^(=cass %cw /[ego]/[syd]/[wen])
  ?:  =(ud.cass 0)
    leaf+"desk does not yet exist: {<syd>}"
  ?:  =(%kids syd)
    =+  .^(hash=@uv %cz /[ego]/[syd]/[wen])
    leaf+"%kids %cz hash:     {<hash>}"
  =/  kel-path
    /[ego]/[syd]/[wen]/sys/kelvin
  ?.  .^(? %cu kel-path)
    leaf+"bad desk: {<syd>}"
  =+  .^(=waft %cx kel-path)
  :+  %rose  ["" "{<syd>}" "::"]
  ^-  tang
  =/  hash  .^(@uv %cz /[ego]/[syd]/[wen])
  =/  =sink
    ?~  s=(~(get by sor) syd)
      ~
    ?~  z=(~(get by zyn) syd u.s)
      ~
    `[-.u.s +.u.s +.u.z]
  =/  meb=(list @uv)
    ?~  sink  [hash]~
    (mergebase-hashes our syd now her.u.sink sud.u.sink)
  =/  dek  (~(got by tyr) syd)
  =/  =dome  (~(got by cone) our syd)
  =/  [on=(list [@tas ?]) of=(list [@tas ?])]
    (skid ~(tap by ren.dome) |=([* ?] +<+))
  =/  sat
    ?-  zest.dek
      %live  "running"
      %dead  "suspended"
    ::
        %held
      ?.  =(~ lac.dek)
        "suspended until permissions are granted"
      ?:  =(~ cop.dek)
        "suspended until next update"
      "suspended until next update, needs new permissions"
    ==
  =/  kul=tape
    %+  roll
      %+  sort
        ~(tap in (waft-to-wefts:clay waft))
      |=  [a=weft b=weft]
      ?:  =(lal.a lal.b)
        (lte num.a num.b)
      (lte lal.a lal.b)
    |=  [=weft =tape]
    (welp " {<[lal num]:weft>}" tape)
  =/  pen=(list $@(@tas [@tas @ud]))
    %+  weld  ~(tap in wic.dek)
    ?:  =(~ cop.dek)  ~
    `(list $@(@tas [@tas @ud]))`[%awaiting-perms]~
  :~  leaf/"/sys/kelvin:     {kul}"
      leaf/"base hash:        {?.(=(1 (lent meb)) <meb> <(head meb)>)}"
      leaf/"%cz hash:         {<hash>}"
      ::
      leaf/"app status:       {sat}"
      leaf/"force on:         {?:(=(~ on) "~" <on>)}"
      leaf/"force off:        {?:(=(~ of) "~" <of>)}"
      ::
      leaf/"publishing ship:  {?~(sink <~> <(get-publisher our syd now)>)}"
      leaf/"updates:          {?~(sink "local" "remote")}"
      leaf/"source ship:      {?~(sink <~> <her.u.sink>)}"
      leaf/"source desk:      {?~(sink <~> <sud.u.sink>)}"
      leaf/"source aeon:      {?~(sink <~> <let.u.sink>)}"
      leaf/"kids desk:        {?~(sink <~> ?~(kid.u.sink <~> <u.kid.u.sink>))}"
      leaf/"pending updates:  {<pen>}"
      leaf/" awaiting perms:  {<`(list perm:gall)`~(tap in cop.dek)>}"
      leaf/"missing perms:    {<`(list perm:gall)`~(tap in lac.dek)>}"
  ==
::  +report-kids: non-vat cz hash report for kids desk
::
++  report-kids
  |=  [our=ship now=@da]
  ^-  tank
  =/  syd  %kids
  =/  ego  (scot %p our)
  =/  wen  (scot %da now)
  ?.  (~(has in .^((set desk) %cd /[ego]//[wen])) syd)
    leaf/"no %kids desk"
  =+  .^(hash=@uv %cz /[ego]/[syd]/[wen])
  leaf/"%kids %cz hash:     {<hash>}"
::  +read-bill-foreign: read /desk/bill from a foreign desk
::
++  read-foreign
  |*  [=ship =desk =aeon =path =mold easy=*]
  ^-  mold
  ~|  +<
  =/  her  (scot %p ship)
  =/  syd  (scot %tas desk)
  =/  yon  (scot %ud aeon)
  ::
  =/  dom  .^(domo cv/~[her syd yon])
  =/  tak  ~|  aeons=~(key by hit.dom)
           (scot %uv (~(got by hit.dom) aeon))
  =/  yak  .^(yaki cs/~[her syd yon %yaki tak])
  =/  fil  (~(get by q.yak) path)
  ?~  fil  easy
  =/  lob  (scot %uv u.fil)
  =/  peg  .^(page cs/~[her syd yon %blob lob])
  ;;(mold q.peg)
::  +read-bill-foreign: read /desk/bill from a foreign desk
::
++  read-bill-foreign
  |=  [=ship =desk =aeon]
  ^-  (list dude)
  (read-foreign ship desk aeon /desk/bill (list dude) ~)
::  +read-bill: read contents of /desk/bill manifest
::
++  read-bill
  |=  [our=ship =desk now=@da]
  =/  pax  (en-beam [our desk da+now] /desk/bill)
  ?.  .^(? cu/pax)
    *(list dude)
  .^((list dude) cx/pax)
::  +read-seal-foreign: read /desk/seal from a foreign desk
::
++  read-seal-foreign
  |=  [=ship =desk =aeon]
  ^-  seal
  (read-foreign ship desk aeon /desk/seal seal *seal)
::  +read-seal: read contents of /desk/seal manifest
::
++  read-seal
  |=  [our=ship =desk now=@da]
  =/  pax  (en-beam [our desk da+now] /desk/seal)
  ?.  .^(? cu/pax)
    *seal
  .^(seal cx/pax)
::
++  get-remote-diff
  |=  [our=ship here=desk now=@da her=ship there=desk when=aeon]
  =+  .^(our-hash=@uv cz/[(scot %p our) here (scot %da now) ~])
  =+  .^(her-hash=@uv cz/[(scot %p her) there (scot %ud when) ~])
  !=(our-hash her-hash)
::
++  get-publisher
  |=  [our=ship =desk now=@da]
  ^-  (unit ship)
  =/  pax  /(scot %p our)/[desk]/(scot %da now)/desk/ship
  ?.  .^(? %cu pax)  ~
  `.^(ship %cx pax)
::
++  get-apps-live
  |=  [our=ship =desk now=@da]
  ^-  (list dude)
  %+  murn  (get-apps-have our desk now)
  |=([=dude live=?] ?.(live ~ `dude))
::  +get-apps-have: find which apps Gall is running on a desk
::
++  get-apps-have
  |=  [our=ship =desk now=@da]
  ^-  (list [=dude live=?])
  %~  tap  in
  .^((set [=dude live=?]) ge+/(scot %p our)/[desk]/(scot %da now))
::
++  mergebase-hashes
  |=  [our=@p syd=desk now=@da her=ship sud=desk]
  =/  her  (scot %p her)
  =/  ego  (scot %p our)
  =/  wen  (scot %da now)
  %+  turn  .^((list tako) %cs ~[ego syd wen %base her sud])
  |=(=tako .^(@uv %cs ~[ego syd wen %hash (scot %uv tako)]))
::
++  enjs
  =,  enjs:format
  |%
  ++  tim
    |=  t=@
    ^-  json
    (numb (fall (mole |.((unm:chrono:userlib t))) 0))
  ::
  ++  cass
    |=  c=^cass
    %-  pairs
    :~  ud+(numb ud.c)
        da+(tim da.c)
    ==
  ::
  ++  weft
    |=  w=^weft
    %-  pairs
    :~  name+s+lal.w
        kelvin+(numb num.w)
    ==
  ::
  ++  rung
    |=  r=^rung
    %-  pairs
    :~  aeon+(numb aeon.r)
        weft+(weft weft.r)
    ==
  ::
  ++  perm
    |=  p=perm:gall
    %-  pairs
    =/  mote  |=(u=(unit @) ?~(u ~ s+u.u))
    =/  burr  |=(b=burr:gall [desk+(mote desk.b) spur+(path spur.b) ~])
    =/  spar  |=(s=spar:gall [care+(mote care.s) (burr +.s)])
    ?+  p  [vane+s+-.p what+s+`@`?@(+.p +.p +<.p) ~]
        [%super ~]
      [what+s+'super' ~]
    ::
        [%write *]
      [what+s+-.p jump+b+jump.p dude+s+`@`dude.p ~]
    ::
        [%watch *]
      [what+s+-.p jump+b+jump.p dude+s+`@`dude.p path+(path path.p) ~]
    ::
        [%reads *]
      [what+s+-.p vane+s+vane.p (spar +>.p)]
    ::
        [%clay ?(%label %perms %liven) *]
      [vane+s+-.p what+s++<.p desk+(mote +>.p) ~]
    ::
        [%clay %write *]
      [vane+s+-.p what+s++<.p (burr +>.p)]
    ::
        [%clay ?(%local %peers) *]
      [vane+s+-.p what+s++<.p (spar +>.p)]
    ::
        [%gall %clear *]
      [vane+s+-.p what+s++<.p dude+(mote +>.p) ~]
    ==
  ::
  --
--
