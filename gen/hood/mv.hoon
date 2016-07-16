::
::::  /hoon/mv/hood/gen
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
:+  %kiln-info  "moved"
%+  furl  (fray input)
(foal output -:(flop input) [%noun .^(* %cx input)])
