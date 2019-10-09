/+  *custody, *csv
|=  [=key=path =loc=path]
%.  ~
%-  print-rows
%+  select
  !,  *hoon
  :*  (need ship.left)
      physical-form.left
  ==
%^    join
    (load:keys key-path)
  (load:locations loc-path)
!,  *hoon
?&  !=(~ ship.left)
    =(location.left location.right)
==
