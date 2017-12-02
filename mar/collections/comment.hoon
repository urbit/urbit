::  /mar/collections/comment/hoon
::
|_  $=  com
    $:  who/ship                                        ::  author
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
    :^    (scot %p who.com)
        (scot %da wen.com)
      (scot %da wed.com)
    wat
  --
::
++  grab
  |%
  ++  mime
    |=  {p/mite:eyre q/octs:eyre}
    (txt (to-wain:format q.q))
  ++  txt
    |=  txs/(list @t)
    ?>  (gte (lent txs) 3)
    :^    (slav %p (snag 0 txs))
        (slav %da (snag 1 txs))
      (slav %da (snag 2 txs))
    (slag 3 txs)
  --
++  grad  %txt
--
