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
    :*  (cat 3 '> ' desc.con)
        (cat 3 'public: ' ?:(publ.con 'y' 'n'))
        (cat 3 'visible: ' ?:(visi.con 'y' 'n'))
        (cat 3 'comments: ' ?:(comm.con 'y' 'n'))
        (cat 3 'xenopost: ' ?:(xeno.con 'y' 'n'))
        :-  'except:'
        %+  turn  (sort ~(tap in mems.con) aor)
        |=  a=@p
        (cat 3 '  ' (scot %p a))
    ==
  ++  elem  :: web display
    ;div
      ::;pre: {(trip (of-wain:format txt))}
      ;h1:  {(trip desc.con)}
      ::;div 
      ::  {(trip desc.con)}
      ::==
      ;list;  :: show topics
    ==
  ++  json
    =,  enjs:format
    %-  pairs
    :~  description+[%s desc.con]
        public+[%b publ.con]
        visible+[%b visi.con]
        comments+[%b comm.con]
        xenopost+[%b xeno.con]
        :-  %except
        :-  %a
        %+  turn
          ~(tap in mems.con)
        |=  a/@p
        [%s (scot %p a)]
    ==
  ++  front  (my title+desc.con ~)  :: title in lists
  --
::
++  grab
  |%
  ++  noun  config  :: validate over ames
  ++  mime
    |=  {p/mite:eyre q/octs:eyre}
    (txt (to-wain:format q.q))
  ++  txt
    |=  txs/(pole @t)
    ~&  %reading-config
    ?>  ?=([desc=@t publ=@t visi=@t comm=@t xeno=@t %'except:' mem=*] txs)
    :*  (rash desc.txs ;~(pfix (jest '> ') (cook crip (star next))))
        (rash publ.txs ;~(pfix (jest 'public: ') (flag %y %n)))
        (rash visi.txs ;~(pfix (jest 'visible: ') (flag %y %n)))
        (rash comm.txs ;~(pfix (jest 'comments: ') (flag %y %n)))
        (rash xeno.txs ;~(pfix (jest 'xenopost: ') (flag %y %n)))
        %-  sy
        %+  turn  (skip mem.txs |=(a=@t =(a '')))  :: skip trailing blank
        (curr rash ;~(pfix (jest '  ~') fed:ag))
    ==
  --
++  grad  %txt
--
