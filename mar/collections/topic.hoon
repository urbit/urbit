::  /mar/collections/topic/hoon
::
/-  *collections
|_  top=topic
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
      (scot %p who.top)
    wat.top
  ::
  ++  elem  :: web display
    ;div
      ;pre: {(trip (of-wain:format txt))}
      ;hr;
      ;kids;  :: show comments
    ==
  ++  front  (my title+tit.top ~)  :: title in lists
  --
::
++  grab
  |%
  ++  mime
    |=  {p/mite:eyre q/octs:eyre}
    (txt (to-wain:format q.q))
  ++  txt
    |=  txs/(pole @t)  ^+  top
    ?>  ?=([des=@t who=@t wat=*] txs)
    =/  tit  (rash des.txs ;~(pfix (jest '> ') (cook crip (star next))))
    [tit (slav %p who.txs) wat.txs]
  --
++  grad  %txt
--
