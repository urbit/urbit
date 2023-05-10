|_  [eny=@uv runs=@ud]
++  check
  |=  vax=vase
  =?  runs  =(0 runs)  100
  =+  size=1
  =+  sax=(slot 6 vax)
  =+  run-i=0
  =+  rng=~(. og eny)
  |-  ^-  ?
  ?:  =(run-i runs)
      ~&  success-runs+run-i
      %.y
  =+  sam=(~(fill quiz [size rng]) sax)
  =+  res=(slam vax sam)
  ?:  =(+:res %.y)
    :: Arbitrarily chosen growth pace.
    =+  new-size=(add +(size) (div (mul size 2) 21))
    $(run-i +(run-i), rng +:(rads:rng 1), size new-size)
  ~&  fail-with-sam+q.sam
  %.n
++  quiz  |_  [size=@ud rng=_og]
  ++  split-rng  ^+  [og og]
    =+  bit-size=256
    =^  bits-1  rng  (raws:rng bit-size)
    [~(. og bits-1) ~(. og (raw:rng bit-size))]
  ++  fill
    |=  sax=vase
    ^+  sax
    =+  new-rng=+:(rads:rng 1)
    ?+  p.sax  ~&(warn-unfill-sam+`type`p.sax sax)
      %noun            sax(q (gen-noun))
      [%atom p=* q=~]  sax(q (gen-atom p.p.sax))
      [%atom *]        sax
      [%cell p=* q=*]  =+  [rng-1 rng-2]=split-rng
                       %=  sax
                          q  :-  q:(fill(rng rng-2) (slot 2 sax))
                                 q:(fill(rng rng-1) (slot 3 sax))
                      ==
      [%face p=* q=*]  sax(q q:(fill [p=q.p.sax q=q.sax]))
    ==
  ++  gen-noun
    =+  start-size=size
    |.  ^-  noun
    ?:  (lte size 1)
      (rad:rng start-size)  :: leafs should be able to make large atoms.
    =^  ran  rng  (rads:rng 3)
    ?:  =(0 ran) :: 1/3 chance for a leaf.
      (rad:rng start-size)
    ?:  =(1 ran)  :: 1/3 chance for a identical subtrees.
      =+  subtree=$(size (div size 2))
      [subtree subtree]
    =+  [rng-1 rng-2]=split-rng :: 1/3 chance for different subtrees. 
    :-  $(size (div size 2), rng rng-1)
        $(size (div size 2), rng rng-2)
  ++  gen-atom
    |=  aur=@tas
    ^-  @
    (rad:rng size)
  --
--