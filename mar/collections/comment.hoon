::  /mar/collections/comment/hoon
::
/-  *collections
|_  com=comment
++  grow
  |%
  ++  mime
    :-  /text/x-collections-comment
    %-  as-octs:mimes:html
    (of-wain:format txt)
  ++  txt
    ^-  (list @t)
    :-  (scot %p who.com)
    wat.com
  --
::
++  grab
  |%
  ++  mime
    |=  {p/mite:eyre q/octs:eyre}
    (txt (to-wain:format q.q))
  ++  txt
    |=  txs/(pole @t)  ^+  com
    ?>  ?=([who=@t wat=*] txs)
    [(slav %p who.txs) wat.txs]
  --
++  grad  %txt
--
