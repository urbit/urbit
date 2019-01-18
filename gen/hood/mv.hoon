::  Kiln: Move file in clay
::
::::  /hoon/mv/hood/gen
  ::
/?    310
=,  space:userlib
:-  %say
|=  {^ {input/path output/path ~} ~}
:-  %kiln-info
?.  =(-:(flop input) -:(flop output))
  ["Can't move to a different mark" ~]
=+  dir=.^(arch %cy input)
?~  fil.dir
  ~&  "No such file:"
  [<input> ~]
:-  "moved"  :-  ~
%+  furl  (fray input)
(foal output -:(flop input) [%noun .^(* %cx input)])
