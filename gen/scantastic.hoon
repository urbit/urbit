:-  %say
|=  *
:-  %noun
=-  "hello, world"
|%
+$  scan  [=loop=(map =@ud =xray) =xray]
+$  xray
  $~  %noun
  $@  $?  %noun
          %void
      ==
  $%  ::  %bark: atom selection
      ::
      [%bark =(map atom aura)]
      ::  %bush: head-of-cell superposition
      ::
      [%bush wide=xray tall=xray]
      ::  %cell: ordered pair
      ::
      [%cell head=xray tail=xray]
      ::  %core: functional attribute battery
      ::
      [%core =vair =xray =(map term (map term xray))]
      ::  %face: namespace
      ::
      [%face =term =xray]
      ::  %fork: disordered superposition
      ::
      [%fork =(set xray)]
      ::  %hint: context-preserving type hint
      ::
      [%hint [=type =note] =xray]
      ::  %knot: recursion root
      ::
      [%knot =(set @ud) =xray]
      ::  %loop: recursion point
      ::
      [%loop index=@ud]
      ::  %rock: constant
      ::
      [%rock =atom =aura]
      ::  %root: atom/cell superposition
      ::
      [%root flat=xray deep=xray]
      ::  %sand: variable atom
      ::
      [%sand =aura]
      ::  %wood: tagged superposition
      ::
      [%wood =(map atom (pair aura xray))]
  ==
::
::  _ex: type analysis kernel
::
++  ex
  |_  scan
  ++  aura-merge
    |=  [=aura =aura]
    ?:  =(aura ^aura)  aura
    =/  byte  0
    |-  ^-  ^^aura
    ?:  (lte (met 3 aura) byte)  aura
    ?:  (lte (met 3 ^aura) byte)  ^aura
    ?:  !=((cut 3 [byte 1] aura) (cut 3 [byte 1] ^aura))
      (end 3 byte aura)
    $(byte +(byte))
  ::
  ++  bark-merge
    |=  [=one=(map atom aura) =two=(map atom aura)]
    ^-  (map atom aura)
    ?:  &(?=([* ~ ~] two-map) !?=([* ~ ~] one-map))
      $(one-map two-map, two-map one-map) 
    =/  list  ~(tap by one-map)
    |-  ^-  (map atom aura)
    ?~  list  two-map
    %=    $
        list  t.list
        two-map 
      %+  ~(put by two-map)
        p.i.list
      =+  (~(get by two-map) p.i.list)
      ?~  -  q.i.list
      (aura-merge u.- q.i.list)
    == 
  ::
  ++  wood-merge
    |=  [=one=(map atom [=aura =^xray]) =two=(map atom [=aura =^xray])]
    ^-  (map atom [=aura =^xray])
    ?:  &(?=([* ~ ~] two-map) !?=([* ~ ~] one-map))
      $(one-map two-map, two-map one-map) 
    =/  list  ~(tap by one-map)
    |-  ^-  (map atom [=aura =^xray])
    ?~  list  two-map
    %=    $
        list  t.list
        two-map 
      %+  ~(put by two-map)
        p.i.list
      =+  (~(get by two-map) p.i.list)
      ?~  -  q.i.list
      :-  (aura-merge aura.u.- aura.q.i.list)
      (merge(xray xray.u.-) xray.q.i.list)
    ==
  ::
  ++  merge
    |=  =new=^xray
    ::  (the superposition of .xray and .new-xray)
    ::
    ^-  ^xray
    ::  identify trivial cases
    ::
    ?:  ?=(%void xray)  new-xray
    ?:  ?=(%void new-xray)  xray
    ?:  |(?=(%noun xray) ?=(%noun new-xray))  %noun
    ?-  -.xray
      ::
      ::  %bark: atom selection
      ::
      %bark  ?+  -.new-xray  
                      $(xray new-xray, new-xray xray)
               %bark  [%bark (bark-merge map.new-xray map.xray)]
               %bush  [%root xray new-xray]
               %cell  [%root xray new-xray]
               %core  [%root xray new-xray]
               %root  [%root $(new-xray flat.new-xray) deep.new-xray]
               %sand  $(xray [%fork xray ~ ~])
               %wood  [%root xray new-xray]
             ==
      ::
      ::  %bush: head-of-cell superposition
      ::
      %bush  ?+  -.new-xray  
                      $(xray new-xray, new-xray xray)
               %bush  :+  %bush 
                        $(xray wide.xray, new-xray wide.new-xray)
                      $(xray tall.xray, new-xray tall.new-xray)
               %cell  $(xray [%fork xray ~ ~])
               %core  $(xray [%fork xray ~ ~])
               %root  [%root flat.new-xray $(new-xray deep.new-xray)]
               %sand  $(xray [%fork xray ~ ~])
               %wood  [%bush wide.xray $(xray tall.xray)]
             ==
      ::
      ::  %cell: unstructured cell
      ::
      %cell  ::  terminate cell collision
             ::
             ?:  ?=(%cell -.new-xray)
               :+  %cell 
                 $(xray head.xray, new-xray head.new-xray)
               $(xray tail.xray, new-xray tail.new-xray)
             ::  terminate core-cell collision
             ::
             ?:  ?=(%core -.new-xray)
               $(xray [%fork xray ~ ~])
             ::  normally, reverse
             ::
             $(xray new-xray, new-xray xray)
      ::
      ::  %core: functional attribute battery
      ::
      %core  ::  terminate core collision
             ::
             ?:  ?=(%core -.new-xray)
               $(xray [%fork xray ~ ~])
             ::  normally, reverse
             ::
             $(xray new-xray, new-xray xray)
      ::
      ::  %face: namespace
      ::
      %face  ::  if face matches, unify
             ::
             ?:  ?&(?=(%face -.new-xray) =(term.xray term.new-xray))
               [%face term.xray $(xray xray.xray, new-xray xray.xray)]
             ::  otherwise erase
             ::
             $(xray xray.xray)
      ::
      ::  %fork: unstructured superposition
      ::
      %fork  ::  forks are indigestible but merge with other forks
             ::
             ?:  ?=(%fork -.new-xray)
               [%fork (~(uni in set.xray) set.new-xray)]
             [%fork (~(put in set.xray) new-xray)]
      ::
      ::  %hint: type extension
      ::
      %hint  ::  merging a hint destroys it
             ::
             $(xray xray.xray)
      ::
      ::  %knot: recursion root
      ::
      %knot  ::  try to combine knots for cleanliness
             ::
             ?:  ?=(%knot -.new-xray)
               :+  %knot 
                 (~(uni in set.xray) set.new-xray) 
               $(xray xray.xray, new-xray xray.new-xray)
             [%knot set.xray $(xray xray.xray)]
      ::  %loop: recursion point
      ::
      %loop  ::  expand through loop
             ::
             $(xray (~(got by loop-map) index.xray))
      ::
      ::  %rock: atomic constant
      ::
      %rock  ::  reduce to trivial set
             ::
             $(xray [%bark [[atom.xray aura.xray] ~ ~]])
      ::
      ::  %root: atom-cell superposition
      ::
      %root  ?+  -.new-xray  
                      $(xray new-xray, new-xray xray)
               %bark  [%root $(xray flat.xray) deep.xray]
               %bush  [%root flat.xray $(xray deep.xray)]
               %cell  [%root flat.xray $(xray deep.xray)]
               %core  [%root flat.xray $(xray deep.xray)]
               %root  :+  %root
                        $(xray flat.xray, new-xray flat.new-xray)
                      $(xray deep.xray, new-xray deep.new-xray)
               %sand  [%root $(xray flat.xray) deep.xray]
               %wood  [%root flat.xray $(xray deep.xray)]
             ==        
      ::
      ::  %sand: atomic variable
      ::
      %sand  ?+  -.new-xray  
                      $(xray new-xray, new-xray xray)
               %bark  $(xray [%fork xray ~ ~])
               %bush  $(xray [%fork xray ~ ~])
               %cell  [%root xray new-xray]
               %core  [%root xray new-xray]
               %root  [%root $(new-xray flat.new-xray) deep.new-xray]
               %sand  $(xray [%fork xray ~ ~])
               %wood  [%root xray new-xray]
             ==        
      ::
      ::  %wood: tagged values
      ::
      %wood  ?+  -.new-xray  
                      $(xray new-xray, new-xray xray)
               %bark  [%root new-xray xray]
               %bush  [%bush wide.new-xray $(new-xray tall.new-xray)]
               %cell  $(xray [%fork xray ~ ~])
               %core  $(xray [%fork xray ~ ~])
               %root  [%root flat.new-xray $(new-xray deep.new-xray)]
               %sand  [%root new-xray xray]
               %wood  [%wood (wood-merge map.xray map.new-xray)]
    ==       ==
  --
--

