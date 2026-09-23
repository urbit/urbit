::  Kiln: permission manager, grant required and requested permissions
::
::  Accepts an optional list of desks, =filt (%required %requested)
::  and =all flag.
::  filt filters which permissions are shown.
::  all prompts to grant all permission to the desk at once
::  Depending on argument, displays and allows to grant permissions
::  If no desks are specified, reports on all desks.
::
::
/-  *sole
/+  *generators
:-  %ask
|=  $:  [now=@da eny=@uvJ bec=beak]
        deks=(list desk)
        [filt=?(%required %requested %$) all=_|]
    ==
=*  our  p.bec
^-  (sole-result [%helm-grant-perms (jar desk perm:gall)])
|^
=/  des
  ?.  =(~ deks)  (silt deks)
  .^((set desk) %cd /(scot %p our)//(scot %da now))
=.  des  (~(del in des) %base)
=.  des  (~(del in des) %kids)
=.  des
  %-  silt
  %+  skim  ~(tap in des)
  |=  =desk
  .^(? %cu /(scot %p our)/[desk]/(scot %da now)/sys/kelvin)
(perm-to-desk (sort ~(tap in des) aor))
::
++  perm-to-desk
  |=  dek=(list desk)
  =/  data=(list [desk bond:ward:clay])
    %+  turn  dek
    |=  =desk
    :-  desk
    .^(=bond:ward:clay %cx /(scot %p our)//(scot %da now)/bond/[desk])
  ::
  =|  grants=(jar desk perm:gall)
  |-  ^-  (sole-result [%helm-grant-perms (jar desk perm:gall)])
  ?~  data
    ?~  grants  no-product
    %-  produce
    [%helm-grant-perms grants]
  =/  [=desk =bond:ward:clay]  i.data
  =/  missing-perms
    |=  pes=(set perm:gall)
    %+  skip  ~(tap in pes)
    (cury have:guard:gall peg.bond)
  =/  pew  ?:  ?=(?(%$ %required) filt)   (missing-perms pew.bond)  ~
  =/  peq  ?:  ?=(?(%$ %requested) filt)  (missing-perms peq.bond)  ~
  ?:  all
    =/  perms=(list perm:gall)  (welp pew peq)
    ?:  =(~ perms)  $(data t.data)
    =/  flag   ?:  ?=(%$ filt)  "required and requested"  (trip filt)
    %+  print  leaf+"y/n"
    %+  print  leaf+"{<perms>}"
    %+  print  leaf+"Grant all {flag} permission to {<desk>}?"
    %+  print  leaf+"::"
    %+  prompt  [%& %project "y/n: "]
    %+  parse   ;~(pose (cold %.y (mask "yY")) (cold %.n (mask "nN")))
    |=  grant=?
    =?  grants  grant  (~(put by grants) desk perms)
    ^$(data t.data)
  |-
  ?^  pew
    %+  print  leaf+"grant permission {<i.pew>}? y/n"
    %+  print  leaf+"{<desk>} requires permission:"
    %+  print  leaf+"::"
    %+  prompt  [%& %project "y/n: "]
    %+  parse   ;~(pose (cold %.y (mask "yY")) (cold %.n (mask "nN")))
    |=  grant=?
    =?  grants  grant  (~(add ja grants) desk i.pew)
    ^$(pew t.pew)
  ?^  peq
    %+  print  leaf+"grant permission {<i.peq>}? y/n"
    %+  print  leaf+"{<desk>} requested permission:"
    %+  print  leaf+"::"
    %+  prompt  [%& %project "y/n: "]
    %+  parse   ;~(pose (cold %.y (mask "yY")) (cold %.n (mask "nN")))
    |=  grant=?
    =?  grants  grant  (~(add ja grants) desk i.peq)
    ^$(peq t.peq)
  ^$(data t.data)
  ::
--