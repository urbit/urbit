::
::::  /hoon/tree/lib
  ::
/?    314
::
|%
++  getall                                              :: search in manx
  |=  tag/(list mane)
  |=  ele/manx  ^-  marl
  ?:  (lien tag |=(a/mane =(a n.g.ele)))
    ~[ele]
  (zing (turn c.ele ..$))
::
::  a.b_c.d => [[%a %b] [%c %d]]
::  a.b_c, a_b__c => [[%a %b] %c]
::  a_b_c, a__b_c => [%a [%b %c]]
++  read-schem                                          :: decode gapped noun
  =<  (cook to-noun (cook build-grove apex))
  |%
  ++  noun  $@(term {noun noun})       ::  shadow
  ::  improper list of possible entry points
  ++  grove  $@(term {gap/@ sealed/noun pending/grove})
  ++  apex  ;~(plug sym (star ;~(plug delim sym)))
  ++  delim  ;~(pose (cold 0 dot) (cook lent (plus cab)))
  ++  to-noun  |=(a/grove ?@(a a [sealed.a $(a pending.a)]))
  ++  build-grove
    |=  {a/grove b/(list {p/@u q/term})}  ^-  grove
    %+  roll  b  =<  .(acc a)
    |=  {{gap/@u v/term} acc/grove}  ^-  grove
    ?@  acc            [gap acc v]
    ?:  (gth gap gap.acc)  [gap (to-noun acc) v]
    acc(pending $(acc pending.acc))
  --
--
