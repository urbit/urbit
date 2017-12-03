::  ConCATenate file listings
::
::::  /hoon/cat/gen
  ::
/?    310
//  /%%/ls/subdir
//  /%/pretty
::
::::
  ::
:-  %say
|=  {^ {arg/(list path)} vane/?($g $c)}
=-  tang+(flop `tang`(zing -))
%+  turn  arg
|=  pax/path
^-  tang
=+  ark=.^(arch (cat 3 vane %y) pax)
?^  fil.ark
  ?:  =(%sched -:(flop pax))
    [>.^((map @da cord) (cat 3 vane %x) pax)<]~
  [leaf+(spud pax) (pretty-file .^(noun (cat 3 vane %x) pax))]
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
      `tank`(subdir vane pax dir.ark)
  ==
==
