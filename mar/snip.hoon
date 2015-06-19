::
::::  /hoon/core/elem/mar
  ::
/?  314
!:
|%
  ++  words  1
  ++  hedtal
    |=  a=marl  ^-  [hed=marl tal=marl]
    ?~  a  [~ ~]
    ?.  ?=(%h1 n.g.i.a)
      =+  had=$(a c.i.a)
      ?^  -.had  had
      $(a t.a)
    [c.i.a (limit words t.a)]
  ::
  ++  extract
    |=  a=marl  ^-  tape
    ?~  a  ~
    %-  weld  :_  $(a t.a)
    ?.  ?=(_:/(**) i.a)
      $(a c.i.a)
    v.i.a.g.i.a
  ::
  ++  limit
    |=  [lim=@u mal=marl]
    =<  res
    |-  ^-  [rem=@u res=marl]
    ?~  mal  [lim ~]
    ?~  lim  [0 ~]
    =+  ^-  [lam=@u hed=manx]
      ?:  ?=(_:/(**) i.mal)
        [lim :/(tay)]:(deword lim v.i.a.g.i.mal)
      [rem ele(c res)]:[ele=i.mal $(mal c.i.mal)]
    [rem - res]:[hed $(lim lam, mal t.mal)]
  ::
  ++  deword
    |=  [lim=@u tay=tape]  ^-  [lim=@u tay=tape]
    ?~  tay  [lim tay]
    ?~  lim  [0 ~]
    =+  wer=(dot 1^1 tay)
    ?~  q.wer
      [lim - tay]:[i.tay $(tay t.tay)]
    =+  nex=$(lim (dec lim), tay q.q.u.q.wer)
    [-.nex [(wonk wer) +.nex]]
--
::
!:
|_  own=manx
::
++  grow                                                         ::  convert to
  |%
  ++  mime
    =<  mime
    |%
    ++  elem  own
    ++  hymn  ;html:(head:title:"snip" body:"+{elem}")           ::  convert to %hymn
    ++  html  (crip (poxo hymn))                                 ::  convert to %html
    ++  mime  [/text/html (taco html)]                           ::  convert to %mime
    --
  --
++  garb  [%react-snip ~]
++  grab  |%                                                     ::  convert from
          ++  noun  manx                                         ::  clam from %noun
          ++  elem  
            |=  a=manx
            =+  (hedtal +.a)
            ;div:(h1:"*{hed}" div:"*{tal}")
--        --
