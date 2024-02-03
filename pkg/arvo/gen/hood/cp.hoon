::  Kiln: copy file in clay
::
::::  /hoon/cp/hood/gen
  ::
/?    310
:-  %say
=,  space:userlib
|=  [[now=@da tick=@ud *] [input=path output=path ~] ~]
=.  input   (en-pick now tick input)
=.  output  (en-pick now tick output)
:-  %kiln-info
?.  =(-:(flop input) -:(flop output))
  ["Can't move to a different mark" ~]
=+  dir=.^(arch %cy input)
?~  fil.dir
  ~&  "No such file:"
  [<input> ~]
:-  "copied"
`(foal output -:(flop input) [%atom %t ~] .^(* %cx input))    ::  XX type
