::
::::  /hoon/core/elem/mar
  ::
/?  314
!:
|%
  ++  words  1
  ++  hedtal
    =|  met=marl
    |=  a=marl  ^-  [hed=marl tal=marl]
    ?~  a  [~ ~]
    ?.  ?=(%h1 n.g.i.a)
      ?:  ?=(%meta n.g.i.a)
        $(a t.a, met [i.a met])
      =+  had=$(a c.i.a)
      ?^  -.had  had
      $(a t.a)
    [c.i.a (weld (flop met) (limit words t.a))]
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
|_  [hed=marl tal=marl]
::
++  grow                                                         ::  convert to
  |%
  ++  mime
    =<  mime
    |%
    ++  elem  ;div:(h1:"*{hed}" div:"*{tal}")                    ::  convert to %elem
    ++  hymn  ;html:(head:title:"snip" body:"+{elem}")           ::  convert to %hymn
    ++  html  (crip (poxo hymn))                                 ::  convert to %html
    ++  mime  [/text/html (taco html)]                           ::  convert to %mime
    --
  --
++  grab  |%                                                     ::  convert from
          ++  noun  ,[marl marl]                                 ::  clam from %noun
          ++  elem  |=(a=manx (hedtal +.a))
--        --
