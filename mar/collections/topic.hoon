::  /mar/collections/topic/hoon
::
|_  $=  top
    $:  tit/cord                                        ::  title
        who/ship                                        ::  author
        wen/@da                                         ::  created
        wed/@da                                         ::  editted
        wat/wain                                        ::  content
    ==                                                  ::
::
++  grow
  |%
  ++  mime
    :-  /text/x-collections-comment
    %-  as-octs:mimes:html
    (of-wain:format txt)
  ++  txt
    ^-  (list @t)
    :+  (cat 3 '> ' tit.top)
      (crip <[by=who.top on=wen.top ed=wed.top]>)
    wat.top
  --
::
++  grab
  |%
  ++  mime
    |=  {p/mite:eyre q/octs:eyre}
    (txt (to-wain:format q.q))
  ++  txt
    |=  txs/(pole @t)  ^+  top
    ?>  ?=([des=@t inf=@t wat=*] txs)
    =/  tit  (rash des.txs ;~(pfix (jest '> ') (cook crip (star next))))
    ::
    =/  inf  :: REVIEW this seems not v stable
      %-  %-  hard
          $:  %cltr
              [%ktts %by [%sand %p who=@p]]
              [%ktts %on [%sand %da wen=@da]]
              [%ktts %ed [%sand %da wed=@da]]
              ~
          ==
      (ream inf.txs)
    [tit who.inf wen.inf wed.inf wat.txs]
  --
++  grad  %txt
--
