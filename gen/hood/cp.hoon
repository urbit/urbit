::
::::  /hoon/cp/hood/gen
  ::
/?    310
:-  %say
|=  {^ {input/path output/path $~} $~}
?.  =(-:(flop input) -:(flop output))
  ~&  "Can't move to a different mark"
  ~
=+  dir=.^(arch %cy input)
?~  fil.dir
  ~&  "No such file:"
  ~&  <input>
  ~
:+  %kiln-info  "copied"
(foal output -:(flop input) [%atom %t ~] .^(* %cx input))    ::  XX type
