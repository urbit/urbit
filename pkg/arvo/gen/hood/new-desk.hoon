::  |new-desk: creates a minimal desk
::
/+  *generators
::
:-  %ask
|=  $:  [now=@da eny=@uvJ bek=beak]
        [=desk ~]
        [from=$~(%base desk) hard=_| gall=_|]
    ==
::
=;  make-new-desk
  ?.  ?&  !hard
          (~(has in .^((set ^desk) %cd (en-beam bek(q %$) /))) desk)
      ==
    (make-new-desk)
  %+  print    (rap 3 'the desk %' desk ' already exists. overwrite it?' ~)
  %+  prompt   [%& %prompt "overwrite? (y/N) "]
  |=  in=tape
  ?.  |(=("y" in) =("Y" in) =("yes" in))
    no-product
  (make-new-desk)
::
|.  %-  produce
:-  %helm-pass
%^  new-desk:cloy  desk
  ~
%-  ~(gas by *(map path page:clay))
|^  =-  (turn - mage)
    ^-  (list path)
    =/  common-files=(list path)  :~
        /mar/noun/hoon
        /mar/hoon/hoon
        /mar/txt/hoon
        /mar/kelvin/hoon
        /sys/kelvin
      ==
    =/  extra-files=(list path)  ?.  gall  [~]
      :~
        /mar/bill/hoon
        /mar/mime/hoon
        /mar/json/hoon
        /lib/skeleton/hoon
        /lib/default-agent/hoon
        /lib/dbug/hoon
      ==
    (weld common-files extra-files)
::
++  mage
  |=  =path
  :-  path
  ^-  page:clay
  :-  (rear path)
  ~|  [%missing-source-file from path]
  .^  *
    %cx
    (scot %p p.bek)
    from
    (scot %da now)
    path
  ==
--
