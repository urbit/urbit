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
::  $jump: changes to update source change requests
::
+$  jump
  $%  [%all all=(map dock dock)]        :: pending requests
      [%add old=dock new=dock]          :: new request
      [%yea old=dock new=dock]          :: approved
      [%nay old=dock new=dock]          :: denied
  ==
::  $rung: reference to upstream commit
::
+$  rung  [=aeon =weft]
::  #sync-record: source and destination of a kiln sync
::
+$  sync-record                         ::
  $:  syd=desk                          :: local desk
      her=ship                          :: foreign ship
      sud=desk                          :: foreign desk
  ==
::
+$  sync-state                          ::
  $:  nun=@ta                           :: nonce
      kid=(unit desk)                   :: has kids desk too?
      let=@ud                           :: next revision
      nit=(unit ?)                      :: automerge or default
      hav=(unit @ud)                    :: update available
      yea=?                             :: update approved
  ==
::
+$  sync-update
  $%  [%new for=sync-record rev=@ud]
      [%done for=sync-record rev=@ud]
      [%drop for=sync-record rev=@ud]
      [%pending pending=(set [for=sync-record rev=@ud])]
  ==
::
+$  seal        (list perm:gall)
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
      .^(cone %cx /[ego]//[wen]/domes)
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
  =+  prep=[tyr cone sor zyn]=(report-prep our now)
  ?:  =(%$ filt)
    %-  zing
    %+  turn
      ?^  desks
        (flop desks)
      %+  sort  ~(tap in ~(key by tyr.prep))
      |=  [a=desk b=desk]
      ?:  |(=(a %kids) =(b %base))  &
      ?:  |(=(a %base) =(b %kids))  |
      (aor b a)
    |=(syd=@tas (report-vat prep our now syd verb))
  =/  deks=(list [=desk =belt:tire])
    ?~  desks
      %+  sort  ~(tap by tyr.prep)
      |=  [[a=desk *] [b=desk *]]
      ?:  |(=(a %kids) =(b %base))  &
      ?:  |(=(a %base) =(b %kids))  |
      (aor b a)
    %+  murn  (flop desks)
    |=  des=desk
    ^-  (unit [=desk =belt:tire])
    ?~  got=(~(get by tyr.prep) des)
      ~
    `[des u.got]
  ?:  =(filt %blocking)
    =/  base-weft=(unit weft)
      %-  ~(rep in wic:(~(got by tyr.prep) %base))
      |=  [=weft out=(unit weft)]
      ?~  out
        `weft
      ?:  (lth num.weft num.u.out)
        out
      `weft
    ?~  base-weft  ~['%base already up-to-date']
    =/  blockers=(list desk)
      %+  sort
        ^-  (list desk)
        %+  murn  deks
        |=  [=desk =belt:tire]
        ^-  (unit ^desk)
        ?.  =(%live zest.belt)
          ~
        ?:  (~(has in wic.belt) u.base-weft)
          ~
        `desk
      aor
    ?~  blockers  ~['No desks blocking upgrade']
    %-  zing
    ^-  (list tang)
    :-  :~  %+  rap  3
            :~  'These desks are blocking upgrade to [%zuse '
                (scot %ud num.u.base-weft)
                ']:'
        ==  ==
    %+  turn  blockers
    |=(syd=desk (report-vat prep our now syd verb))
  ::
  %-  zing
  %+  turn
    ?+    filt  !!
        %exists
      %+  skip  deks
      |=  [syd=desk *]
      ?~  got=(~(get by cone.prep) our syd)
        &
      =(0 let.u.got)
    ::
        %running
      %+  skim  deks
      |=([* =belt:tire] =(zest.belt %live))
    ::
        %suspended
      %+  skip  deks
      |=  [syd=@tas =belt:tire]
      ?|  =(syd %kids)
          =(zest.belt %live)
          ?~  got=(~(get by cone.prep) our syd)
            &
          =(0 let.u.got)
      ==
    ::
        %exists-not
      %+  skim  deks
      |=  [syd=desk *]
      ?~  got=(~(get by cone.prep) our syd)
        |
      =(0 let.u.got)
    ==
  |=([syd=desk *] (report-vat prep our now syd verb))
::  +report-vat: report on a single desk installation
::
++  report-vat
  |=  $:  $:  tyr=rock:tire  =cone  sor=(map desk (pair ship desk))
              zyn=(map [desk ship desk] sync-state)
          ==
          our=ship  now=@da  syd=desk  verb=?
      ==
  |^  ^-  tang
  =/  ego  (scot %p our)
  =/  wen  (scot %da now)
  ?.  ((sane %tas) syd)
    ~[(cat 3 'insane desk: %' syd)]
  ?.  (~(has by cone) our syd)
    ~[(cat 3 'desk does not yet exist: %' syd)]
  =/  hash  .^(@uv %cz /[ego]/[syd]/[wen])
  ?:  =(%kids syd)
    ~[(cat 3 '%kids %cz hash:     ' (scot %uv hash))]
  =/  kel-path  /[ego]/[syd]/[wen]/sys/kelvin
  ?.  .^(? %cu kel-path)
    ~[(cat 3 'bad desk: %' syd)]
  =+  .^(=waft %cx kel-path)
  ^-  tang
  =/  =sink
    ?~  s=(~(get by sor) syd)
      ~
    ?~  z=(~(get by zyn) syd u.s)
      ~
    `[p.u.s q.u.s [kid let]:u.z]
  =/  meb=(list @uv)
    ?~  sink  ~[hash]
    %+  turn
      .^  (list tako)  %cs
        /[ego]/[syd]/[wen]/base/(scot %p her.u.sink)/[sud.u.sink]
      ==
    |=(=tako .^(@uv %cs /[ego]/[syd]/[wen]/hash/(scot %uv tako)))
  =/  dek  (~(got by tyr) syd)
  =/  sat
    ?-  zest.dek
      %live  'running'
      %dead  'suspended'
      ::
        %held
      ?.  =(~ lac.dek)
        'suspended until permissions are granted'
      ?:  =(~ cop.dek)
        'suspended until next update'
      'suspended until next update, needs new permissions'
    ==
  =/  kul=cord  (print-wefts (waft-to-wefts waft))
  =/  pen=cord
    ?:  =(~ cop.dek)  (print-wefts wic.dek)
    (rap 3 '[%awaiting-perms ' (print-wefts (waft-to-wefts waft)) ']' ~)
  =/  ese=cord
    ?:(.^(? %cx /[ego]//[wen]/esse/[syd]) 'yes' 'no')
  =/  pax=path  /(scot %p our)/[syd]/(scot %da now)
  ?.  verb
    :~  '::'
        =+  .^(exist=? cu+(weld pax /desk/bill))
        ?.  exist  '  /desk/bill:             missing'
        =+  .^(dudes=(list dude:gall) cx+(weld pax /desk/bill))
        (cat 3 '  /desk/bill:             ' (crip "{<dudes>}"))
        (cat 3 '  pending updates:        ' pen)
        (cat 3 '  source ship:            ' ?~(sink '~' (scot %p her.u.sink)))
        (cat 3 '  app status:             ' sat)
        (cat 3 '  essential desk:         ' ese)
        (cat 3 '  %cz hash ends in:       ' (print-shorthash hash))
        (cat 3 '  /sys/kelvin:            ' (print-wefts (waft-to-wefts waft)))
        (cat 3 '%' syd)
    ==
  ::
  =/  [on=(list @tas) of=(list @tas)]
    =/  [on=(list @tas) of=(list @tas)]
      %-  ~(rep by ren:(~(got by cone) our syd))
      |=  [[=dude:gall is-on=?] on=(list @tas) of=(list @tas)]
      ?:  is-on
        [[dude on] of]
      [on [dude of]]
    [(sort on aor) (sort of aor)]
  :~  '::'
      (cat 3 '  pending updates:  ' pen)
      (cat 3 '  awaiting perms:   ' (crip =+(~(wyt in cop.dek) ?:(=(0 -) "~" (a-co:co -)))))
      (cat 3 '  missing perms:    ' (crip =+(~(wyt in lac.dek) ?:(=(0 -) "~" (a-co:co -)))))
      %^  cat  3  '  kids desk:        '  ?~  sink  '~'
                                          ?~  kid.u.sink  '~'
                                          (cat 3 '%' u.kid.u.sink)
      (cat 3 '  source aeon:      ' ?~(sink '~' (scot %ud let.u.sink)))
      (cat 3 '  source desk:      ' ?~(sink '~' (cat 3 '%' sud.u.sink)))
      (cat 3 '  source ship:      ' ?~(sink '~' (scot %p her.u.sink)))
      (cat 3 '  updates:          ' ?~(sink 'local' 'remote'))
      %^  cat  3  '  publishing ship:  '  ?~  got=(get-publisher our syd now)
                                            '~'
                                          (scot %p u.got)
  ::
      (cat 3 '  force off:        ' (print-agents of))
      (cat 3 '  force on:         ' (print-agents on))
      (cat 3 '  app status:       ' sat)
      (cat 3 '  essential desk:   ' ese)
  ::
      (cat 3 '  %cz hash:         ' (scot %uv hash))
      (cat 3 '  base hash:        ' (print-mergebases meb))
      (cat 3 '  /sys/kelvin:      ' (print-wefts (waft-to-wefts waft)))
      (cat 3 '%' syd)
  ==
  ++  print-wefts
    |=  wefts=(set weft)
    ^-  @t
    ?:  =(~ wefts)
      '~'
    %+  roll  (sort ~(tap in wefts) aor)
    |=  [=weft out=@t]
    ?:  =('' out)
      (rap 3 '[%' lal.weft ' ' (scot %ud num.weft) ']' ~)
    (rap 3 out ' [%' lal.weft ' ' (scot %ud num.weft) ']' ~)
  ::
  ++  print-shorthash
    |=  hash=@uv
    ^-  @t
    (crip ((v-co:co 5) (end [0 25] hash)))
  ::
  ++  print-mergebases
    |=  hashes=(list @uv)
    ^-  @t
    ?~  hashes
      '~'
    ?~  t.hashes
      (scot %uv i.hashes)
    %+  roll  `(list @uv)`hashes
    |=  [hash=@uv out=@t]
    ?:  =('' out)
      (print-shorthash hash)
    (rap 3 out ' ' (print-shorthash hash) ~)
  ::
  ++  print-agents
    |=  agents=(list @tas)
    ^-  @t
    ?~  agents
      '~'
    %+  roll  `(list @tas)`agents
    |=  [agent=@tas out=@tas]
    ?:  =('' out)
      (cat 3 '%' agent)
    (rap 3 out ' %' agent ~)
  --
::  +report-kids: non-vat cz hash report for kids desk
::
++  report-kids
  |=  [our=ship now=@da]
  ^-  tank
  =/  syd  %kids
  =/  ego  (scot %p our)
  =/  wen  (scot %da now)
  ?.  (~(has in .^((set desk) %cd /[ego]//[wen])) syd)
    'no %kids desk'
  =+  .^(hash=@uv %cz /[ego]/[syd]/[wen])
  (cat 3 '%kids %cz hash:     ' (scot %uv hash))
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
  %+  turn  .^((list tako) %cs /[ego]/[syd]/[wen]/base/[her]/[sud])
  |=(=tako .^(@uv %cs /[ego]/[syd]/[wen]/hash/(scot %uv tako)))
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
