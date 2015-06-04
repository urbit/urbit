::  ConCATenate file listings
::  
::::  /hook/gate/cat/cat
  ::
//  /%%/ls/subdir
//  /%/pretty
!:
::::
  ::
|=  [^ [arg=(list path)] ~]
=-  tang/(zing -)
%+  turn  arg
|=  pax=path
^-  tang
=+  ark=;;(arch .^(%cy pax))
?^  q.ark
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
