::  /mar/collections/comment/hoon
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
    :*  tit.top
        (scot %p who.top)
        (scot %da wen.top)
        (scot %da wed.top)
        wat
    ==
  --
::
++  grab
  |%
  ++  mime
    |=  {p/mite:eyre q/octs:eyre}
    (txt (to-wain:format q.q))
  ++  txt
    |=  txs/(list @t)
    ?>  (gte (lent txs) 4)
    :*  (snag 0 txs)
        (slav %p (snag 1 txs))
        (slav %da (snag 2 txs))
        (slav %da (snag 3 txs))
        (slag 4 txs)
    ==
  --
++  grad  %txt
--
