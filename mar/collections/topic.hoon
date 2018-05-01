::  /mar/collections/topic/hoon
::
/-  *collections
|%
  ++  titled
    |=  a/wain
    ^-  wain
    ?:  =((scag 2 (trip -.a)) "# ")
      +.a
    a
--
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
  ++  md  (of-wain:format (titled wat.top))
  ++  elem  :: web display
    ;div
      ;pre: {(trip (of-wain:format (titled txt)))}
      ;hr;
      ;kids;  :: show comments
    ==
  ++  front  (my title+tit.top ~)  :: title in lists
  ++  json
    =,  enjs:format
    %-  pairs
    :~  title+[%s tit.top]
        who+[%s (scot %p who.top)]
        what+[%s (of-wain:format wat.top)]
    ==
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
