/%  kelvin  %kelvin
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
::  +truncate-hash: get last 5 digits of hash and convert to tape
::
++  truncate-hash
  |=  hash=@uv
  ^-  tape
  (slag 2 <`@uv`(mod hash 0v1.00000)>)
::  +report-prep: get data required for reports
::
++  report-prep
  |=  [our=@p now=@da]
  =/  ego  (scot %p our)
  =/  wen  (scot %da now)
  :*  .^(rock:tire %cx /[ego]//[wen]/tire)
      .^(=cone %cx /[ego]//[wen]/domes)
      .^((map desk [ship desk]) %gx /[ego]/hood/[wen]/kiln/sources/noun)
      .^  (map [desk ship desk] sync-state)  %gx
          /[ego]/hood/[wen]/kiln/syncs/noun
      ==
  ==
::  +report-vats: report on all desk installations
::
++  report-vats
  |=  [our=@p now=@da desks=(list desk) filt=@tas verb=?]
  ^-  tang
  =/  ego  (scot %p our)
  =/  wen  (scot %da now)
  =/  prep  (report-prep our now)
  ?~  filt
    %-  zing
    %+  turn  (flop desks)
    |=(syd=@tas (report-vat prep our now syd verb))
  =/  deks
    ?~  desks
      %+  sort
        (sort ~(tap in -.prep) |=([[a=@ *] b=@ *] !(aor a b)))
      |=([[a=@ *] [b=@ *]] ?|(=(a %kids) =(b %base)))
    %+  skip  ~(tap in -.prep)
    |=([syd=@tas *] =(~ (find ~[syd] desks)))
  ?:  =(filt %blocking)
    =/  base-wic
      %+  sort  ~(tap by wic:(~(got by -.prep) %base))
      |=([[* a=@ud] [* b=@ud]] (gth a b))
    ?~  base-wic  ~[leaf+"%base already up-to-date"]
    =/  blockers=(list desk)
      %+  turn
        %+  skip  ~(tap by -.prep)
        |=  [* =belt:tire]
        ?.  =(zest.belt %live)  &
        (~(has in wic.belt) i.base-wic)
      |=([syd=desk *] syd)
    ?~  blockers  ~[leaf+"No desks blocking upgrade, run |bump to apply"]
    :-  [%rose [" %" "To unblock upgrade run |suspend %" ""] blockers]
    %-  zing
    %+  turn  (flop blockers)
    |=(syd=desk (report-vat prep our now syd verb))
  ::
  %-  zing
  %+  turn
    ?+    filt  !!
    ::
        %exists
      %+  skip  deks
      |=([syd=desk *] =(ud:.^(cass %cw /[ego]/[syd]/[wen]) 0))
    ::
        %running
      %+  skim  deks
      |=([* [zest=@tas *]] =(zest %live))
    ::
        %suspended
      %+  skip  deks
      |=  [syd=@tas [zest=@tas *]]
      ?|  =(syd %kids)
          =(zest %live)
          =(ud:.^(cass %cw /[ego]/[syd]/[wen]) 0)
      ==
    ::
        %exists-not
      %+  skim  deks
      |=([syd=desk *] =(ud:.^(cass %cw /[ego]/[syd]/[wen]) 0))
    ==
  |=([syd=desk *] (report-vat prep our now syd verb))
::  +report-vat: report on a single desk installation
::
++  report-vat
  |=  $:  $:  tyr=rock:tire  =cone  sor=(map desk [ship desk])
              zyn=(map [desk ship desk] sync-state)
          ==
          our=ship  now=@da  syd=desk  verb=?
      ==
  ^-  tang
  =-  ::  hack to force wrapped rendering
      ::
      ::    edg=6 empirically prevents dedent
      ::
      %+  roll
        (~(win re -) [0 6])
      |=([a=tape b=(list @t)] [(crip a) b])
  ::
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
  ?.  verb
    =/  cut=(list tape)  (turn meb truncate-hash)
    =/  len  (lent cut)
    =/  base-hash
      ?:  =(0 len)  "~"
      ?:  =(1 len)  (head cut)
        "~[{`tape`(zing (join " " `(list tape)`cut))}]"
    :~  leaf/"/sys/kelvin:          {kul}"
        leaf/"base hash ends in:     {base-hash}"
        leaf/"%cz hash ends in:      {(truncate-hash hash)}"
        leaf/"app status:            {sat}"
        leaf/"pending updates:       {<pen>}"
    ==
  ::
  =/  [on=(list [@tas ?]) of=(list [@tas ?])]
    =/  =dome  (~(got by cone) our syd)
    (skid ~(tap by ren.dome) |=([* ?] +<+))
  :~  leaf/"/sys/kelvin:     {kul}"
      leaf/"base hash:        {?.(=(1 (lent meb)) <meb> <(head meb)>)}"
      leaf/"%cz hash:         {<hash>}"
      ::
      leaf/"app status:       {sat}"
      leaf/"force on:         {<(sort (turn on head) aor)>}"
      leaf/"force off:        {<(sort (turn of head) aor)>}"
      ::
      leaf/"publishing ship:  {?~(sink <~> <(get-publisher our syd now)>)}"
      leaf/"updates:          {?~(sink "local" "remote")}"
      leaf/"source ship:      {?~(sink <~> <her.u.sink>)}"
      leaf/"source desk:      {?~(sink <~> <sud.u.sink>)}"
      leaf/"source aeon:      {?~(sink <~> <let.u.sink>)}"
      leaf/"kids desk:        {?~(sink <~> ?~(kid.u.sink <~> <u.kid.u.sink>))}"
      leaf/"pending updates:  {<pen>}"
      leaf/" awaiting perms:  {=+(~(wyt in cop.dek) ?:(=(0 -) "~" (a-co:co -)))}"
      leaf/"missing perms:    {=+(~(wyt in lac.dek) ?:(=(0 -) "~" (a-co:co -)))}"
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
  .^((set [=dude live=?]) ge+/(scot %p our)/[desk]/(scot %da now)/$)
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
