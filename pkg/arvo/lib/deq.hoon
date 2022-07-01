|%
::
++  welt
  ~/  %welt
  |*  [a=(list) b=(list)]
  =>  .(a ^.(homo a), b ^.(homo b))
  |-  ^+  b
  ?~  a  b
  $(a t.a, b [i.a b])
++  afx
  |$  [val]
  $%  [%1 p=val ~]
      [%2 p=val q=val ~]
      [%3 p=val q=val r=val ~]
      [%4 p=val q=val r=val s=val ~]
  ==
++  pha
  |$  [val]
  $~  [%nul ~]
  $%  [%nul ~]
      [%one p=val]
      [%big p=(afx val) q=(pha val) r=(afx val)]
  ==
::
++  deq
  |*  val=mold
  |%
  ::
  ::  +|  %utilities
  ::
  ++  make-afx
    |=  ls=(list val)
    ?+  ls  ~|(bad-finger/(lent ls) !!)
     [* ~]        [%1 ls]
     [* * ~]      [%2 ls]
     [* * * ~]    [%3 ls]
     [* * * * ~]  [%4 ls]
    ==
  ::
  ++  afx-to-pha
    |=  =(afx val)
    ^-  (pha val)
    (apl *(pha val) +.afx)
  ::
  ::  +|  %left-biased-operations
  ::
  ::  +pop-left: remove leftmost value from tree
  ::
  ++  pop-left 
    |=  a=(pha val)
    ^-  [val=(unit val) pha=(pha val)]
    ?-  -.a
      %nul  ~^a
    ::
      %one  [`p.a nul/~]
    ::
        %big
      [`p.p.a (big-left +.+.p.a q.a r.a)]
   ==
  ::
  ++  apl
    |=  [a=(pha val) vals=(list val)]
    ^-  (pha val)
    =.  vals  (flop vals)
    |-  
    ?~  vals  a
    $(a (cons a i.vals), vals t.vals)
  ::
  ::
  ++  dip-left
    |*  state=mold
    |=  $:  a=(pha val)
            =state
            f=$-([state val] [(unit val) ? state])
        ==
    ^+  [state a]
    =/  acc  [stop=`?`%.n state=state]
    =|  new=(pha val)
    |-  
    ?:  stop.acc  
      :: cat new and old
      [state.acc (weld a new)]
    =^  val=(unit val)  a
      (pop-left a)
    ?~  val
      [state.acc new]
    =^  res=(unit ^val)  acc
      (f state.acc u.val)
    ?~  res  $
    $(new (snoc new u.res))
  ::
  ++  big-left
    |=  [ls=(list val) a=(pha val) sf=(afx val)]
    ^-  (pha val)
    ?.  =(~ ls)
      [%big (make-afx ls) a sf]
    =/  [val=(unit val) inner=_a]
      (pop-left a)
    ?~  val
      (afx-to-pha sf) 
    [%big [%1 u.val ~] inner sf]
  ::
  ++  cons
    =|  b=(list val)
    |=  [a=(pha val) c=val]
    ^-  (pha val)
    =.  b  [c b]
    |-  
    ?~  b  a
    ?-  -.a
    ::
        %nul  
      $(a [%one i.b], b t.b)
    ::
        %one
      %=  $  
         b  t.b
         a  [%big [%1 i.b ~] [%nul ~] [%1 p.a ~]]
      ==
    ::
        %big
      ?.  ?=(%4 -.p.a)
        %=    $
            b  t.b
        ::
            a
          ?-  -.p.a
            %1  big/[[%2 i.b p.p.a ~] q.a r.a]
            %2  big/[[%3 i.b p.p.a q.p.a ~] q.a r.a]
            %3  big/[[%4 i.b p.p.a q.p.a r.p.a ~] q.a r.a]
           ==
        ==
      =/  inner
        $(a q.a, b ~[s.p.a r.p.a q.p.a])
      =.  inner
        $(a inner, b t.b)
      big/[[%2 i.b p.p.a ~] inner r.a]
    ==
  ::
  ::  +|  %right-biased-operations
  ::
  ::  +snoc: append to end (right) of tree
  ::
  ++  snoc
    |=  [a=(pha val) b=val]
    ^+  a
    ?-  -.a
      %nul  [%one b]
    ::
        %one
      :-  %big
      :*  [%1 p.a ~]
          [%nul ~]
          [%1 b ~]
      ==
    ::
        %big
      ?-  -.r.a
      ::
          %1
        :-  %big
        [p.a q.a [%2 p.r.a b ~]]
      ::
          %2
        :-  %big
        [p.a q.a [%3 p.r.a q.r.a b ~]]
      ::
          %3
        :-  %big
        [p.a q.a [%4 p.r.a q.r.a r.r.a b ~]]
      ::
          %4
        =/  inner
          $(a q.a, b p.r.a)
        =.  inner
          $(a inner, b q.r.a)
        =.  inner
          $(a inner, b r.r.a)
        :-  %big
        :*  p.a
            inner
            [%2 s.r.a b ~]
        ==
      ==
    ==
  ::  +apr: append list to end (right) of tree
  ::
  ++  apr
    |=  [a=(pha val) vals=(list val)]
    ^-  (pha val)
    ?~  vals  a
    $(a (snoc a i.vals), vals t.vals)
  ::
  ::  +big-right: construct a tree, automatically balancing the right
  ::  side
  ++  big-right
    |=  [pf=(afx val) a=(pha val) ls=(list val)]
    ^-  (pha val)
    ?.  =(~ ls)
      [%big pf a (make-afx ls)]
    =/  [val=(unit val) inner=_a]
      (pop-right a)
    ?~  val
      (afx-to-pha pf) 
    [%big pf inner [%1 u.val ~]]
  ::
  ::  +pop-right: remove rightmost value from tree
  ::
  ++  pop-right 
    |=  a=(pha val)
    ^-  [val=(unit val) pha=(pha val)]
    ?-  -.a
      %nul  ~^a
    ::
      %one  [`p.a nul/~]
    ::
        %big
      =/  ls=(list val)  +.r.a
      =^  item  ls  (flop ls)
      [`item (big-right p.a q.a (flop ls))]
   ==
  ::
  ++  peek-right
    |=  a=(pha val)
    ?-  -.a
      %nul  ~
      %one  `p.a
      %big  (rear +.r.a)
   ==
  ::
  ::  +|  %manipulation
  ::  
  ::  +weld: concatenate two trees
  ::
  ::    O(log n)
  ++  weld
    =|  c=(list val)
    |=  [a=(pha val) b=(pha val)]
    ^-  (pha val)
    ?-  -.b
      %nul  (apr a c)
      %one  (snoc (apr a c) p.b)
    ::
        %big
      ?-  -.a
        %nul  (apl b c)
        %one  (cons (apl b c) p.a)
      ::
          %big
        :-  %big
        =-  [p.a - r.b]
        $(a q.a, b q.b, c :(welp +.r.a c +.p.b))
      ==
    ==
  ::  +tap: transform tree to list
  ::
  ++  tap
    =|  res=(list val)
    |=  a=(pha val)
    !.
    |^  ^+  res
    ?-  -.a
      %nul  ~
      %one  ~[p.a]
    ::
        %big
      =/  fst=_res
        (tap-afx p.a)
      =/  lst=_res
        (tap-afx r.a)
      =/  mid=_res
        $(a q.a)
      :(welp fst mid lst)
    ==
    ++  tap-afx
      |=  ax=(afx val)
      ^+  res
      ?-  -.ax
        %1  +.ax
        %2  +.ax
        %3  +.ax
        %4  +.ax
      ==
    --
  --
--
