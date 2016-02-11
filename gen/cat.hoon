::  ConCATenate file listings
::  
::::  /hoon/cat/gen
  ::
/?    314
//  /%%/ls/subdir
//  /%/pretty
!:
::::
  ::
:-  %say
|=  {^ {arg/(list path)} $~}
=-  tang+(flop `tang`(zing -))
%+  turn  arg
|=  pax/path
^-  tang
=+  ark=.^(arch %cy pax)
?^  fil.ark
  ?:  =(%sched -:(flop pax))
    [>.^((map @da cord) %cx pax)<]~
  [leaf+(spud pax) (pretty-file .^(noun %cx pax))]
?-     dir.ark                                          ::  handle ambiguity
    $~
  [rose+[" " `~]^~[leaf+"~" (smyt pax)]]~
::
    {{@t $~} $~ $~}
  $(pax (welp pax /[p.n.dir.ark]))
::
    *
  =-  [palm+[": " ``~]^-]~
  :~  rose+[" " `~]^~[leaf+"*" (smyt pax)] 
      `tank`(subdir pax dir.ark)
  ==
==
