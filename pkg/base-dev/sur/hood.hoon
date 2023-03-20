=,  clay
=*  dude  dude:gall
|%
+$  pike
  $:  sync=(unit [=ship =desk])
      hash=@uv
      =zest
      wic=(set weft)
  ==
::
+$  pikes  (map desk pike)
::
::  $rung: reference to upstream commit
::
+$  rung  [=aeon =weft]
::
+$  sync-state  [nun=@ta kid=(unit desk) let=@ud]
+$  sink        (unit [her=@p sud=desk kid=(unit desk) let=@ud])
++  report-vats
  =|  $:  =cone
          sor=(map desk [ship desk])
          zyn=(map [desk ship desk] sync-state)
          desks=(set desk)
          =pikes
          =rock:tire:clay
          kel=weft
      ==
  |_  [our=@p now=@da]
  +*  ego  (scot %p our)
      wen  (scot %da now)
  ++  $
    |=  [? ? ? ? ?]
     (report-vats:abed +<)
  ++  abed 
    %=    ..abed
      cone  .^(^cone %cx /[ego]//[wen]/domes)
      sor  .^((map desk [ship desk]) %gx /[ego]/hood/[wen]/kiln/sources/noun)
      zyn  .^  (map [desk ship desk] sync-state)  %gx
             /[ego]/hood/[wen]/kiln/syncs/noun
           ==
      desks  .^((set desk) %cd /[ego]/base/[wen])
      pikes  .^(^pikes %gx /[ego]/hood/[wen]/kiln/pikes/kiln-pikes)
      rock  .^(rock:tire:clay %cx /[ego]//[wen]/tire)
      kel  (weft .^(* cx/(en-beam [our %base da+now] /sys/kelvin)))  
    ==
  ++  vat-info
    |=  desk=_`desk`%base
    =/  pike  (~(got by pikes) desk)
    =/  zest  -:(~(got by rock) desk)
    =/  kel-path  /[ego]/[desk]/[wen]/sys/kelvin
    =/  sink=sink
      ?~  s=(~(get by sor) desk)
        ~
      ?~  z=(~(get by zyn) desk u.s)
        ~
      `[-.u.s +.u.s +.u.z]
    =/  hash  .^(@uv %cz /[ego]/[desk]/[wen])
    =/  dek  (~(got by rock) desk)
    =/  =dome  (~(got by cone) our desk)
    =+  .^(=waft %cx kel-path)
    :*  &1  &2  &3  &4  &5  &6  &7  &8
        desk=desk
        ^=  running  =(%live zest)
        ^=  suspended  =(%dead zest)
        ^=  exists  !=(ud.cass 0):.^(=cass %cw /[ego]/[desk]/[wen])
        ^=  bad-desk  ?!(.^(? %cu kel-path))
        ^=  meb   ::  =(list @uv)
          ?~  sink  [hash]~
          (mergebase-hashes our desk now her.u.sink sud.u.sink)
        ^-  [on=(list [@tas ?]) of=(list [@tas ?])]
          (skid ~(tap by ren.dome) |=([* ?] +<+))
        ^=  sat
          ?-  zest.dek
            %live  "running"
            %dead  "suspended"
            %held  "suspended until next update"
          ==
        ^-  kul=tape
          %+  roll
            %+  sort
              ~(tap in (waft-to-wefts:clay waft))
            |=  [a=weft b=weft]
            ?:  =(lal.a lal.b)
              (lte num.a num.b)
            (lte lal.a lal.b)
          |=  [=weft =tape]
          (welp " {<[lal num]:weft>}" tape)
        ^=  blocking
            ?&  !=(%base desk)
                !=(%live zest.pike)
                !(~(has in wic.pike) kel) 
    ==      ==
  ++  report-vats
    |=  $:  verb=?
            show-suspended=?
            show-running=?
            show-blocking=?
            show-nonexistent=?
        ==
    %+  turn
      %+  skim
        %+  turn  ~(tap in desks)
        |=  =desk  (vat-info desk)
      |=  vat-info
      :: just unconditionally show "bad" desks, whatever that means
      ?|  bad-desk
          &(suspended show-suspended)
          &(running show-running)
          &(blocking show-blocking)
          &(!exists show-nonexistent)
      ==
    |=  =vat-info
    :+  %rose  [" " " " "::"]
    :-  leaf+"{<desk.vat-info>}"
    %-  flop
    %-  report-vat
    [verb vat-info]
  ++  report-vat
    |=  [verb=? vat-info]
    ^-  tang
    ?:  !exists
      ~[leaf+"desk does not yet exist: {<desk>}"]
    ?:  =(%kids desk)
      ~[leaf+"%kids %cz hash:     {<hash>}"]
    ?:  bad-desk
      ~[leaf+"bad desk: {<desk>}"]
    %-  flop
    ?.  verb
      :~  leaf/"/sys/kelvin:     {kul}"
          leaf/"app status:       {sat}"
          leaf/"publishing ship:  {?~(sink <~> <(get-publisher our desk now)>)}"
          leaf/"pending updates:  {<`(list [@tas @ud])`~(tap in wic.dek)>}"
      ==
    :~  leaf/"/sys/kelvin:     {kul}"
        leaf/"base hash:        {?.(=(1 (lent meb)) <meb> <(head meb)>)}"
        leaf/"%cz hash:         {<hash>}"
        ::
        leaf/"app status:       {sat}"
        leaf/"force on:         {?:(=(~ on) "~" <on>)}"
        leaf/"force off:        {?:(=(~ of) "~" <of>)}"
        ::
        leaf/"publishing ship:  {?~(sink <~> <(get-publisher our desk now)>)}"
        leaf/"updates:          {?~(sink "local" "remote")}"
        leaf/"source ship:      {?~(sink <~> <her.u.sink>)}"
        leaf/"source desk:      {?~(sink <~> <sud.u.sink>)}"
        leaf/"source aeon:      {?~(sink <~> <let.u.sink>)}"
        leaf/"kids desk:        {?~(sink <~> ?~(kid.u.sink <~> <u.kid.u.sink>))}"
        leaf/"pending updates:  {<`(list [@tas @ud])`~(tap in wic.dek)>}"
    ==
  ++  report-kids
    ^-  tank
    ?.  (~(has in .^((set desk) %cd /[ego]//[wen])) %kids)
      leaf/"no %kids desk"
    =+  .^(hash=@uv %cz /[ego]/kids/[wen])
    leaf/"%kids %cz hash:     {<hash>}"
  --
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
  =/  dom  .^(domo cv/~[her syd yon])
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
  --
--
