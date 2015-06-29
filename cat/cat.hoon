::  ConCATenate file listings
::  
::::  /hoon/cat/cat
  ::
//  /%%/ls/subdir
//  /%/pretty
!:
::::
  ::
|=  [^ [arg=(list path)] ~]
=-  tang/(flop `tang`(zing -))
%+  turn  arg
|=  pax=path
^-  tang
=+  ark=;;(arch .^(%cy pax))
?^  q.ark
  ?:  =(%sched -:(flop pax))
    [>;;((map ,@da cord) .^(%cx pax))<]~
  [leaf/(spud pax) (pretty-file .^(%cx pax))]
?-     r.ark                                          ::  handle ambiguity
    ~
  [rose/[" " `~]^~[leaf/"~" (smyt pax)]]~
    [[@t ~] ~ ~]
  $(pax (welp pax /[p.n.r.ark]))
    *
  =-  [palm/[": " ``~]^-]~
  :~  rose/[" " `~]^~[leaf/"*" (smyt pax)] 
      `tank`(subdir pax r.ark)
  ==
==
