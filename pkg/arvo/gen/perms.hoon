::  Print permission information about desks.
::
::  Accepts an optional list of desks and show-all flag.
::  Returns info on all desks if no desks are specified.
::  Without show-all, only permissions not yet granted are shown
::  with show-all, the full bond is printed as-is.
::
:-  %say
|=  $:  [now=@da eny=@uvJ bec=beak]
        deks=(list desk)
        show-all=_|
    ==
=/  ego  (scot %p p.bec)
=/  wen  (scot %da now)
=/  des
  ?.  =(~ deks)  (silt deks)
  .^((set desk) %cd /[ego]//[wen])
=.  des  (~(del in des) %base)
=.  des  (~(del in des) %kids)
::
=-  [%tang -]
%-  flop
%+  roll  (sort ~(tap in des) aor)
|=  [=desk =tang]
?.  .^(? %cu /[ego]/[desk]/[wen]/sys/kelvin)     tang
=+  .^(=bond:ward:clay %cx /[ego]//[wen]/bond/[desk])
?:  &(=(~ peg) =(~ ped) =(~ peq) =(~ pew)):bond  tang
=/  in-peg
  |=  pes=(set perm:gall)
  %-  ~(rep in pes)
  |=  [p=perm:gall new-pes=(set perm:gall)]
  ?:  (have:guard:gall peg.bond p)  new-pes
  (~(put in new-pes) p)
=?  ped.bond  !show-all  (in-peg ped.bond)
=?  peq.bond  !show-all  (in-peg peq.bond)
=?  pew.bond  !show-all  (in-peg pew.bond)
;:  welp
  :~  '::'
      (crip "{<desk>}")
      (cat 3 '  granted:               ' (crip "{<~(tap in peg.bond)>}"))
  ==
  ?:  =(~ ped.bond)  ~
  :~((cat 3 '  required:              ' (crip "{<~(tap in ped.bond)>}")))
  ?:  =(~ peq.bond)  ~
  :~((cat 3 '  requested:             ' (crip "{<~(tap in peq.bond)>}")))
  ?:  =(~ pew.bond)  ~
  :~((cat 3 '  awaiting:              ' (crip "{<~(tap in pew.bond)>}")))
  tang
==
