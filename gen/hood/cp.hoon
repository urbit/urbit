::
::::  /hoon/cp/hood/gen
  ::
/?    310
:-  %say
|=  {^ {input/path output/path $~} $~}
:+  %kiln-info  "copied"
?>  =(-:(flop input) -:(flop output))
(foal output -:(flop input) [%atom %t ~] .^(* %cx input))    ::  XX type
