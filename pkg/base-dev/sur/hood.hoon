=,  clay
=*  dude  dude:gall
|%
::  $rung: reference to upstream commit
::
+$  rung  [=aeon =weft]
::
::  XX
::  +$  vat  [=desk hash=@uv =cass =arak]
::  ::  +report-vats: report on all desk installations
::  ::
::  ++  report-vats
::    |=  [our=@p now=@da]
::    ^-  tang
::    =+  .^  raz=(list vat)
::            %gx  /(scot %p our)/hood/(scot %da now)/kiln/vats/noun
::        ==
::    :-  (report-kids our now)
::    (turn raz |=(v=vat (report-vat our now v)))
::  ::  +report-vat: report on a single desk installation
::  ::
::  ++  report-vat
::    |=  [our=ship now=@da vat]
::    ^-  tank
::    ?:  =(ud.cass 0)
::      leaf+"desk does not yet exist: {<desk>}"
::    =/  kel-path
::      /(scot %p our)/[desk]/(scot %da now)/sys/kelvin
::    ?.  .^(? %cu kel-path)
::      leaf+"bad desk: {<desk>}"
::    =+  .^(=weft %cx kel-path)
::    :+  %rose  ["" "{<desk>}" "::"]
::    ^-  tang
::    =/  meb  (mergebase-hashes our desk now arak)
::    =/  poz
::      ?~  rail.arak  "local"
::      ?:(paused.u.rail.arak "paused" "tracking")
::    =/  sat  ?:(liv.rein.arak "running" "suspended")
::    =/  pen
::      ?~  rail.arak  "~"
::      <(turn next.u.rail.arak |=([@ lal=@tas num=@] [lal num]))>
::    :~  leaf/"/sys/kelvin:      {<[lal num]:weft>}"
::        leaf/"base hash:        {?.(=(1 (lent meb)) <meb> <(head meb)>)}"
::        leaf/"%cz hash:         {<hash>}"
::        ::
::        leaf/"app status:       {sat}"
::        leaf/"force on:         {?:(=(~ add.rein.arak) "~" <add.rein.arak>)}"
::        leaf/"force off:        {?:(=(~ sub.rein.arak) "~" <sub.rein.arak>)}"
::        ::
::        leaf/"publishing ship:  {?~(rail.arak <~> <publisher.u.rail.arak>)}"
::        leaf/"updates:          {poz}"
::        leaf/"source ship:      {?~(rail.arak <~> <ship.u.rail.arak>)}"
::        leaf/"source desk:      {?~(rail.arak <~> <desk.u.rail.arak>)}"
::        leaf/"source aeon:      {?~(rail.arak <~> <aeon.u.rail.arak>)}"
::        leaf/"pending updates:  {pen}"
::    ==
::  +report-kids: non-vat cz hash report for kids desk
::
++  report-kids
  |=  [our=ship now=@da]
  ^-  tank
  =/  dek  %kids
  =/  ego  (scot %p our)
  =/  wen  (scot %da now)
  ?.  (~(has in .^((set desk) %cd /[ego]//[wen])) dek)
    leaf/"no %kids desk"
  =+  .^(hash=@uv %cz /[ego]/[dek]/[wen])
  leaf/"%kids %cz hash:     {<hash>}"
::  +read-kelvin-foreign: read /sys/kelvin from a foreign desk
::
++  read-kelvin-foreign
  |=  [=ship =desk =aeon]
  ^-  weft
  ~|  read-foreign-kelvin/+<
  =/  her  (scot %p ship)
  =/  syd  (scot %tas desk)
  =/  yon  (scot %ud aeon)
  ::
  =/  dom  .^(dome cv/~[her syd yon])
  =/  tak  (scot %uv (~(got by hit.dom) let.dom))
  =/  yak  .^(yaki cs/~[her syd yon %yaki tak])
  =/  lob  (scot %uv (~(got by q.yak) /sys/kelvin))
  =/  peg  .^(page cs/~[her syd yon %blob lob])
  ;;(weft q.peg)
::  +read-kelvin-local: read /sys/kelvin from a local desk
::
++  read-kelvin-local
  |=  [our=ship =desk now=@da]
  ^-  (unit weft)
  ~|  read-kelvin-local+desk
  =/  pax  (en-beam [our desk da+now] /sys/kelvin)
  ?.  .^(? cu/pax)
    ~
  [~ .^(weft cx/pax)]
::  +read-bill-foreign: read /desk/bill from a foreign desk
::
++  read-bill-foreign
  |=  [=ship =desk =aeon]
  ^-  (list dude)
  ~|  +<
  =/  her  (scot %p ship)
  =/  syd  (scot %tas desk)
  =/  yon  (scot %ud aeon)
  ::
  =/  dom  .^(dome cv/~[her syd yon])
  =/  tak  ~|  aeons=~(key by hit.dom)
           (scot %uv (~(got by hit.dom) aeon))
  =/  yak  .^(yaki cs/~[her syd yon %yaki tak])
  =/  fil  (~(get by q.yak) /desk/bill)
  ?~  fil  ~
  =/  lob  (scot %uv u.fil)
  =/  peg  .^(page cs/~[her syd yon %blob lob])
  ;;((list dude) q.peg)
::  +read-bill: read contents of /desk/bill manifest
::
++  read-bill
  |=  [our=ship =desk now=@da]
  =/  pax  (en-beam [our desk da+now] /desk/bill)
  ?.  .^(? cu/pax)
    *(list dude)
  .^((list dude) cx/pax)
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
::  XX
::  ++  get-apps-live
::    |=  [our=ship =desk now=@da]
::    ^-  (list dude)
::    %+  murn  (get-apps-have our desk now)
::    |=([=dude live=?] ?.(live ~ `dude))
::  +get-apps-have: find which apps Gall is running on a desk
::
++  get-apps-have
  |=  [our=ship =desk now=@da]
  ^-  (list [=dude live=?])
  %~  tap  in
  .^((set [=dude live=?]) ge+/(scot %p our)/[desk]/(scot %da now))
::  ::  +get-apps-want: find which apps should be running on a desk
::  ::
::  ++  get-apps-want
::    |=  [local=[our=ship =desk now=@da] duz=(list dude) =rein]
::    ^-  (list dude)
::    ?.  liv.rein  ~
::    ?.  |(=(`zuse+zuse (read-kelvin-local local)) =(%base desk.local))  ~
::    =.  duz  (skip duz ~(has in sub.rein))
::    =.  duz  (weld duz (skip ~(tap in add.rein) ~(has in (sy duz))))
::    duz
::  ::
++  mergebase-hashes
  |=  [our=@p syd=desk now=@da =ship sud=desk]
  =/  her  (scot %p ship)
  =/  ego  (scot %p our)
  =/  wen  (scot %da now)
  %+  turn  .^((list tako) %cs ~[ego syd wen %base her sud])
  |=(=tako .^(@uv %cs ~[ego syd wen %hash (scot %uv tako)]))
::
++  enjs
  =,  enjs:format
  |%
  ::  XX
  ::  ++  vats
  ::    |=  v=(list ^vat)
  ::    ^-  json
  ::    %-  pairs
  ::    %+  turn  v
  ::    |=  va=^vat
  ::    [desk.va (vat va)]
  ::
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
  ::  XX
  ::  ++  vat
  ::    |=  v=^vat
  ::    %-  pairs
  ::    :~  desk+s+desk.v
  ::        hash+s+(scot %uv hash.v)
  ::        cass+(cass cass.v)
  ::        arak+(arak arak.v)
  ::    ==
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
  ::  XX
  ::  ++  rein
  ::    |=  r=^rein
  ::    %-  pairs
  ::    :~  add+a+(turn ~(tap in add.r) (lead %s))
  ::        sub+a+(turn ~(tap in sub.r) (lead %s))
  ::    ==
  ::
  --
--
