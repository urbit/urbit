::  |new-desk: creates a minimal desk
::
/+  *generators
::
:-  %ask
|=  $:  [now=@da eny=@uvJ bek=beak]
        [=desk ~]
        [from=$~(%base desk) hard=_|]
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
    :~  /mar/noun/hoon
        /mar/hoon/hoon
        /mar/txt/hoon
        /mar/kelvin/hoon
        /sys/kelvin
    ==
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
