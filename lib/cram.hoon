|%
++  static                                              :: freeze .mdh hoon subset
  |=  gen/twig  ^-  {inf/(map term dime) elm/manx}
  ?+    -.gen
      =/  gen  ~(open ap gen)
      ?:  =(gen ^gen)  ~|([%cram-dynamic -.gen] !!)
      $(gen gen)
  ::
    $xmn  [~ (single (shut gen))]
    ^     [(malt (frontmatter p.gen)) (single (shut q.gen))]
  ==
::
++  single                                              :: unwrap one-elem marl
  |=  xml/marl  ^-  manx
  ?:  ?=({* $~} xml)  i.xml
  ~|(%many-elems !!)
::
++  shut-mart                                           :: xml attrs
  |=({n/mane v/(list beer)} [n (turn v |=(a/beer ?^(a !! a)))])
::
++  shut                                                :: as xml constant
  |=  gen/twig  ^-  marl
  ?+    -.gen  ~|([%bad-xml -.gen] !!)
      $dbug  $(gen q.gen)
  ::
      $xmn
    [[n.g.p.gen (turn a.g.p.gen shut-mart)] $(gen [%xml c.p.gen])]~
  ::
      $xml
    ?~  p.gen  ~
    =-  (weld - $(p.gen t.p.gen))
    ?^  -.i.p.gen  $(gen [%xmn i.p.gen])
    ~|  [%shut-tuna -.i.p.gen]
    ?+  -.i.p.gen  !!
      $manx  ?>(?=($xmn -.p.i.p.gen) $(gen p.i.p.gen))
      $marl  ?>(?=($xml -.p.i.p.gen) $(gen p.i.p.gen))
    ==
  ==
::
::
++  frontmatter                                         :: parse ~[[%foo 1] [%bar ~s2]]
  |=  gen/twig  ^-  (list {term dime})
  ?:  ?=({$bust $null} gen)  ~
  ?:  ?=($dbug -.gen)  $(gen q.gen)
  ?.  ?=($conl -.gen)  ~|([%bad-frontmatter -.gen] !!)
  %+  turn  p.gen
  |=  gen/twig
  ?.  ?=(^ -.gen)
    =/  gen  ~(open ap gen)
    ?:  =(gen ^gen)  ~|([%bad-frontmatter-elem -.gen] !!)
    $(gen gen)
  =/  hed  (as-dime p.gen)
  ?.  =(%tas p.hed)  ~|([%bad-frontmatter-key-type p.hed] !!)
  [q.hed (as-dime q.gen)]
::
++  as-dime                                            :: %foo ~.foo 0vbar etc
  |=  gen/twig  ^-  dime
  ?:  ?=($dbug -.gen)  $(gen q.gen)
  ?.  ?=({?($rock $sand) @ @} gen)  ~|([%bad-literal gen] !!)
  +.gen
--
