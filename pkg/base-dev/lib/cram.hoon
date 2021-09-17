|%
++  static                                              :: freeze .mdh hoon subset
  |=  gen=hoon  ^-  [inf=(map term dime) elm=manx]
  ?+    -.gen
      =/  gen  ~(open ap gen)
      ?:  =(gen ^gen)  ~|([%cram-dynamic -.gen] !!)
      $(gen gen)
  ::
    %xray  [~ (single (shut gen))]
    ^     [(malt (frontmatter p.gen)) (single (shut q.gen))]
  ==
::
++  single                                              :: unwrap one-elem marl
  |=  xml=marl  ^-  manx
  ?:  ?=([* ~] xml)  i.xml
  ~|(%many-elems !!)
::
++  shut-mart                                           :: xml attrs
  |=([n=mane v=(list beer:hoot)] [n (turn v |=(a=beer:hoot ?^(a !! a)))])
::
++  shut                                                :: as xml constant
  |=  gen=hoon  ^-  marl
  ?+    -.gen  ~|([%bad-xml -.gen] !!)
      %dbug  $(gen q.gen)
  ::
      %xray
    [[n.g.p.gen (turn a.g.p.gen shut-mart)] $(gen [%mcts c.p.gen])]~
  ::
      %mcts
    ?~  p.gen  ~
    =-  (weld - $(p.gen t.p.gen))
    ?^  -.i.p.gen  $(gen [%xray i.p.gen])
    ~|  [%shut-tuna -.i.p.gen]
    ?+  -.i.p.gen  !!
      %manx  ?>(?=(%xray -.p.i.p.gen) $(gen p.i.p.gen))
      %marl  ?>(?=(%mcts -.p.i.p.gen) $(gen p.i.p.gen))
    ==
  ==
::
::
++  frontmatter                                         :: parse ~[[%foo 1] [%bar ~s2]]
  |=  gen=hoon  ^-  (list [term dime])
  ?:  ?=([%bust %null] gen)  ~
  ?:  ?=(%dbug -.gen)  $(gen q.gen)
  ?.  ?=(%clsg -.gen)  ~|([%bad-frontmatter -.gen] !!)
  %+  turn  p.gen
  |=  gen=hoon
  ?.  ?=(^ -.gen)
    =/  gen  ~(open ap gen)
    ?:  =(gen ^gen)  ~|([%bad-frontmatter-elem -.gen] !!)
    $(gen gen)
  =/  hed  (as-dime p.gen)
  ?.  =(%tas p.hed)  ~|([%bad-frontmatter-key-type p.hed] !!)
  [q.hed (as-dime q.gen)]
::
++  as-dime                                            :: %foo ~.foo 0vbar etc
  |=  gen=hoon  ^-  dime
  ?:  ?=(%dbug -.gen)  $(gen q.gen)
  ?.  ?=([?(%rock %sand) @ @] gen)  ~|([%bad-literal gen] !!)
  +.gen
--
