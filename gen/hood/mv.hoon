::
::::  /hoon/mv/hood/gen
  ::
/?    310
:-  %say
|=  {^ {input/path output/path $~} $~}
:-  %kiln-info
?.  =(-:(flop input) -:(flop output))
  ["Can't move to a different mark" *toro]
=+  dir=.^(arch %cy input)
?~  fil.dir
  ~&  "No such file:"
  [<input> *toro]
:-  "moved"
%+  furl  (fray input)
(foal output -:(flop input) [%noun .^(* %cx input)])
