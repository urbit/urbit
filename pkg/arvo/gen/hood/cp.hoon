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
|=  [[now=@da tick=@ud *] [input=path output=path ~] r=_|]
=.  input   (en-pick now tick input)
=.  output  (en-pick now tick output)
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
?~  in-bema=(de-bema input)     ["bad input path" ~]
?~  =<(dir .^(arch %cy input))  ["input path isn't a directory" ~]
?~  out-bema=(de-bema output)   ["bad output path" ~]
=/  in-beck=beck   -.u.in-bema
=/  out-beck=beck  -.u.out-bema
=/  =soba:clay
  %+  murn  .^((list path) %ct input)
  |=  pax=path
  ?:  =(1 (sub (lent pax) (lent s.u.in-bema)))  ~
  =/  =cage
    :-  -:(flop pax)
    [%noun .^(* %cx (en-bema in-beck pax))]
  =/  =spur  (weld s.u.out-bema (slag (lent s.u.in-bema) pax))
  `[spur (feel (en-bema out-beck spur) cage)]
?~  soba  ["nothing to copy" ~]
["copied" `[q.out-beck [%& soba]]]
