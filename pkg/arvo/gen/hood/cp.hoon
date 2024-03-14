::  Kiln: copy file in clay
::
::::  /hoon/cp/hood/gen
  ::
::  XX clay discards the type, so %noun is used
::  copy by lobe should be used, if implemented
::
/?    310
:-  %say
=,  space:userlib
|=  [^ [input=path output=path ~] r=_|]
:-  %kiln-info
^-  [mez=tape tor=(unit toro:clay)]
?.  r
  ?.  =(-:(flop input) -:(flop output))
    ["Can't move to a different mark" ~]
  ?~  =<(fil .^(arch %cy input))
    ~&  "No such file:"
    [<input> ~]
  :-  "copied"
  `(foal output -:(flop input) [%noun .^(* %cx input)])
?~  in-beam=(de-beam input)     ["bad input path" ~]
?~  =<(dir .^(arch %cy input))  ["input path isn't a directory" ~]
?~  out-beam=(de-beam output)   ["bad output path" ~]
=/  in-beak=beak   [p q r]:u.in-beam
=/  out-beak=beak  [p q r]:u.out-beam
=/  =soba:clay
  %+  murn  .^((list path) %ct input)
  |=  pax=path
  ?:  =(1 (sub (lent pax) (lent s.u.in-beam)))  ~
  =/  =cage
    :-  -:(flop pax)
    [%noun .^(* %cx (en-beam in-beak pax))]
  =/  =spur  (weld s.u.out-beam (slag (lent s.u.in-beam) pax))
  `[spur (feel (en-beam out-beak spur) cage)]
?~  soba  ["nothing to copy" ~]
["copied" `[q.out-beak [%& soba]]]
