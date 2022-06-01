::  |new-desk: creates a minimal desk
::
:-  %say
|=  $:  [now=@da eny=@uvJ bek=beak]
        [=desk ~]
        from=$~(%base desk)
    ==
::
?:  (~(has in .^((set ^desk) %cd (en-beam bek(q from) /))) desk)
  ~|  [%already-exists desk]
  !!
::
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
