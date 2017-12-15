::  /mar/collections/config/hoon
::
/-  *collections
|_  con=config
::
::
++  grow
  |%
  ++  mime
    :-  /text/x-collections-config
    %-  as-octs:mimes:html
    (of-wain:format txt)
  ++  txt
    ^-  (list @t)
    :^    (cat 3 '> ' desc.con)
        (cat 3 'public: ' ?:(publ.con 'y' 'n'))
      (cat 3 'visible: ' ?:(visi.con 'y' 'n'))
    :-  'except:'
    %+  turn  (sort ~(tap in mems.con) aor)
    |=  a=@p
    (cat 3 '  ' (scot %p a))
  ++  elem  :: web display
    ;div
      ;pre: {(trip (of-wain:format txt))}
      ;list;  :: show topics
    ==
  ++  front  (my title+desc.con ~)  :: title in lists
  --
::
++  grab
  |%
  ++  mime
    |=  {p/mite:eyre q/octs:eyre}
    (txt (to-wain:format q.q))
  ++  txt
    |=  txs/(pole @t)
    ?>  ?=([des=@t pub=@t vis=@t %'except:' mem=*] txs)
    :^  (rash des.txs ;~(pfix (jest '> ') (cook crip (star next))))
        (rash pub.txs ;~(pfix (jest 'public: ') (flag %y %n)))
        (rash vis.txs ;~(pfix (jest 'visible: ') (flag %y %n)))
    %-  sy
    %+  turn  (skip mem.txs |=(a=@t =(a '')))  :: skip trailing blank
    (curr rash ;~(pfix (jest '  ~') fed:ag))
  --
++  grad  %txt
--
