::
::::  /hoon/mv/hood/gen
  ::
/?    310
:-  %say
|=  {^ {input/path output/path $~} $~}
:+  %kiln-info  "moved"
?>  =(-:(flop input) -:(flop output))
%+  furl  (fray input)
(foal output -:(flop input) [%noun .^(* %cx input)])
