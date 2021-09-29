=,  clay
=*  dude  dude:gall
|%
::  $snap: kiln snapshot
::
+$  snap  (map desk arak)
::  $diff: subscription update
::
+$  diff
  $%  [%block =desk =arak =weft blockers=(set desk)]
      [%reset =desk =arak]
      [%commit =desk =arak]
      [%merge-sunk =desk =arak =tang]
      [%merge-fail =desk =arak =tang]
      [%suspend =desk =arak]
      [%revive =desk =arak]
  ==
::  $arak: foreign vat tracker
::
::    .rail: upstream tracking state, if any
::    .rein: configuration for agents
::
+$  arak
  $:  rail=(unit rail)
      =rein
  ==
::  $rail: upstream tracking state
::
::    .publisher: Ship that originally published desk, if available
::    .paused: is tracking paused? or live
::    .ship: upstream ship (could be .our)
::    .desk: name of upstream desk
::    .aeon: next aeon to pull from upstream
::    .next: list of pending commits with future kelvins
::
+$  rail
  $:  publisher=(unit ship)
      paused=?
      =ship
      =desk
      =aeon
      next=(list rung)
  ==
::  $rung: reference to upstream commit
::
+$  rung  [=aeon =weft]
::  $rein: diff from desk manifest
::
::    .liv: suspended? if suspended, no agents should run
::    .add: agents not in manifest that should be running
::    .sub: agents in manifest that should not be running
::
+$  rein
  $:  liv=_&
      add=(set dude)
      sub=(set dude)
  ==
::
+$  vat  [=desk hash=@uv =cass =arak]
::  +report-vats: report on all desk installations
::
++  report-vats
  |=  [our=@p now=@da]
  ^-  tang
  =+  .^  raz=(list vat)
          %gx  /(scot %p our)/hood/(scot %da now)/kiln/vats/noun
      ==
  (turn raz |=(v=vat (report-vat our now v)))
::  +report-vat: report on a single desk installation
::
++  report-vat
  |=  [our=ship now=@da vat]
  ^-  tank
  ?:  =(ud.cass 0)
    leaf+"desk does not yet exist: {<desk>}"
  =/  kel-path
    /(scot %p our)/[desk]/(scot %da now)/sys/kelvin
  ?.  .^(? %cu kel-path)
    leaf+"bad desk: {<desk>}"
  =+  .^(=weft %cx kel-path)
  :+  %rose  ["" "{<desk>}" "::"]
  ^-  tang
  =/  meb  (mergebase-hashes our desk now arak)
  =/  poz
    ?~  rail.arak  "local"
    ?:(paused.u.rail.arak "paused" "tracking")
  =/  sat  ?:(liv.rein.arak "running" "suspended")
  =/  pen
    ?~  rail.arak  "~"
    <(turn next.u.rail.arak |=([@ lal=@tas num=@] [lal num]))>
  :~  leaf/"/sys/kelvin:      {<[lal num]:weft>}"
      leaf/"base hash:        {?.(=(1 (lent meb)) <meb> <(head meb)>)}"
      leaf/"%cz hash:         {<hash>}"
      ::
      leaf/"app status:       {sat}"
      leaf/"force on:         {?:(=(~ add.rein.arak) "~" <add.rein.arak>)}"
      leaf/"force off:        {?:(=(~ sub.rein.arak) "~" <sub.rein.arak>)}"
      ::
      leaf/"publishing ship:  {?~(rail.arak <~> <publisher.u.rail.arak>)}"
      leaf/"updates:          {poz}"
      leaf/"source ship:      {?~(rail.arak <~> <ship.u.rail.arak>)}"
      leaf/"source desk:      {?~(rail.arak <~> <desk.u.rail.arak>)}"
      leaf/"source aeon:      {?~(rail.arak <~> <aeon.u.rail.arak>)}"
      leaf/"pending updates:  {pen}"
  ==
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
  =/  bob  .^(blob cs/~[her syd yon %blob lob])
  ::
  ;;  weft
  ?-  -.bob
    %direct  q.q.bob
    %delta   q.r.bob
  ==
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
  =/  bob  .^(blob cs/~[her syd yon %blob lob])
  ::
  ;;  (list dude)
  ?-  -.bob
    %direct  q.q.bob
    %delta   q.r.bob
  ==
::  +read-bill: read contents of /desk/bill manifest
::
++  read-bill
  |=  [our=ship =desk now=@da]
  =/  pax  (en-beam [our desk da+now] /desk/bill)
  ?.  (~(has in .^((set ^desk) cd/~[(scot %p our) ~ (scot %da now)])) desk)
    *(list dude)
  ?.  .^(? cu/pax)
    *(list dude)
  .^((list dude) cx/pax)
::  +adjust-dudes: which agents should be started and stopped
::
::    Will ask Gall to start agents that it's already running
::    but that should be ok, and might be safer in case other
::    unprocessed moves would have turned them off.
::
++  adjust-dudes
  |=  $:  local=[our=ship =desk now=@da]
          =rein
      ==
  ^-  [jolt=(list dude) idle=(list dude)]
  =/  all=(list dude)  (read-bill local)
  =/  want  (get-apps-want all rein)
  =/  have  (get-apps-live local)
  [want (skip have ~(has in (sy want)))]
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
::  +get-apps-want: find which apps should be running on a desk
::
++  get-apps-want
  |=  [duz=(list dude) =rein]
  ^-  (list dude)
  =.  duz  (skip duz ~(has in sub.rein))
  =.  duz  (weld duz (skip ~(tap in add.rein) ~(has in (sy duz))))
  duz
::
++  mergebase-hashes
  |=  [our=@p =desk now=@da =arak]
  ?~  rail.arak
    ~
  =/  her  (scot %p ship.u.rail.arak)
  =/  ego  (scot %p our)
  =/  wen  (scot %da now)
  %+  turn  .^((list tako) %cs ~[ego desk wen %base her desk.u.rail.arak])
  |=(=tako .^(@uv %cs ~[ego desk wen %hash (scot %uv tako)]))
::
++  enjs
  =,  enjs:format
  |%
  ++  vats
    |=  v=(list ^vat)
    ^-  json
    %-  pairs
    %+  turn  v
    |=  va=^vat
    [desk.va (vat va)]
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
  ++  vat
    |=  v=^vat
    %-  pairs
    :~  desk+s+desk.v
        hash+s+(scot %uv hash.v)
        cass+(cass cass.v)
        arak+(arak arak.v)
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
  ++  rein
    |=  r=^rein
    %-  pairs
    :~  add+a+(turn ~(tap in add.r) (lead %s))
        sub+a+(turn ~(tap in sub.r) (lead %s))
    ==
  ::
  ++  arak
    |=  a=^arak
    %-  pairs
    :~  rail+?~(rail.a ~ (rail u.rail.a))
        rein+(rein rein.a)
    ==
  ::
  ++  rail
    |=  r=^rail
    %-  pairs
    :~  ship+s+(scot %p ship.r)
        publisher+?~(publisher.r ~ s+(scot %p u.publisher.r))
        desk+s+desk.r
        paused+b+paused.r
        aeon+(numb aeon.r)
        next+a+(turn next.r rung)
    ==
  --
--
