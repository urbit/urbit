::  Perform minimal norm change to delete a file, use =dry & for dry run
::
::  TODO: recognize when it's going to fail because it's in the head of
::        a desk, and maybe offer to |rm
::
=,  clay
:-  %say
|=  [[now=@da eny=@uvJ bec=beak] [target=path ~] dry=_|]
:-  %helm-pans
=;  cards
?.  dry  cards
  %-  (slog leaf+"card: {<cards>}" ~)  ~
=|  lubs=(list note-arvo)
|-  ^-  (list note-arvo)
=+  .^(=arch %cy target)
?^  fil.arch  (snoc lubs [%c %tomb %lobe u.fil.arch])
%-  zing
%+  turn  ~(tap by dir.arch)
|=  [kid=@ta ~]
^$(target (weld target /[kid]))
