/-  *bill
=,  clay
=*  dude  dude:gall
|%
::  $diff: subscription update
::
+$  diff
  $%  [%block =desk =arak =weft blockers=(set desk)]
      [%reset =desk =arak]
      [%merge =desk =arak]
      [%merge-sunk =desk =arak =tang]
      [%merge-fail =desk =arak =tang]
  ==
::  $arak: foreign vat tracker
::
::    .next is a list of pending commits with future kelvins
::
+$  arak
  $:  =ship
      =desk
      =aeon
      next=(list [=aeon =weft])
      =rein
  ==
::  $rein: diff from desk manifest
::
::    .add: agents not in manifest that should be running
::    .sub: agents in manifest that should not be running
::
+$  rein
  $:  add=(set dude)
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
  =+  .^(=weft %cx /(scot %p our)/[desk]/(scot %da now)/sys/kelvin)
  :+  %rose  ["" "{<desk>}" "::"]
  ^-  tang
  =-  ?:  =(~ next.arak)  -
      (snoc - leaf/"pending: {<next.arak>}")
  ^-  tang
  =/  meb  (mergebase-hashes our desk now arak)
  :~  leaf/"/sys/kelvin: {<[lal num]:weft>}"
      leaf/"base hash:   {?.(=(1 (lent meb)) <meb> <(head meb)>)}"
      leaf/"%cz hash:    {<hash>}"
      leaf/"remote aeon: {<aeon.arak>}"
      leaf/"force on:    {?:(=(~ add.rein.arak) "~" <add.rein.arak>)}"
      leaf/"force off:   {?:(=(~ sub.rein.arak) "~" <sub.rein.arak>)}"
  ==
::
++  read-kelvin-foreign
  |=  [=ship =desk =aeon]
  ^-  weft
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
::  +read-bill: read contents of /desk/bill manifest
::
++  read-bill
  |=  [our=ship =desk now=@da] 
  .^(bill cx+/(scot %p our)/[desk]/(scot %da now)/desk/bill)
::  +is-fish: should dill link .dude?
::
++  is-fish  |=([=dude =bill] .?((find ~[dude] (read-fish bill))))
::  +get-apps-diff: which agents should be started and stopped
::
++  get-apps-diff
  |=  [our=ship =desk now=@da =rein]
  ^-  [liv=(list dude) ded=(list dude)]
  =/  =bill  (read-bill our desk now)
  =/  wan  (sy (get-apps-want bill rein))
  =/  hav  (sy (get-apps-live our desk now))
  =/  liv  ~(tap in (~(dif in wan) hav))
  =/  ded  ~(tap in (~(dif in hav) wan))
  [liv ded]
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
  |=  [=bill =rein]
  ^-  (list dude)
  =/  duz  (read-apes bill)
  =.  duz  (skip duz ~(has in sub.rein))
  =.  duz  (weld duz ~(tap in add.rein))
  duz
::
++  mergebase-hashes
  |=  [our=@p =desk now=@da =arak]
  =/  her  (scot %p ship.arak)
  =/  ego  (scot %p our)
  =/  wen  (scot %da now)
  %+  turn  .^((list tako) %cs ~[ego desk wen %base her desk.arak])
  |=(=tako .^(@uv %cs ~[ego desk wen %hash (scot %uv tako)]))
--

