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
    (jam con)
::    (of-wain:format txt)
::  ++  txt
::    ^-  (list @t)
::    :*  (cat 3 '> ' desc.con)
::        (cat 3 'public: ' ?:(publ.con 'y' 'n'))
::        (cat 3 'visible: ' ?:(visi.con 'y' 'n'))
::        (cat 3 'kind: ' kind.con)
::::        (cat 3 'comments: ' ?:(comm.con 'y' 'n'))
::::        (cat 3 'xenopost: ' ?:(xeno.con 'y' 'n'))
::        :-  'except:'
::        %+  turn  (sort ~(tap in mems.con) aor)
::        |=  a=@p
::        (cat 3 '  ' (scot %p a))
::    ==
::  ++  elem  :: web display
::    ;div
::      ::;pre: {(trip (of-wain:format txt))}
::      ;h1:  {(trip desc.con)}
::      ::;div 
::      ::  {(trip desc.con)}
::      ::==
::      ;list;  :: show topics
::    ==
  ++  json
    =,  format
    |^
    %-  pairs:enjs
    :~  description+[%s desc.con]
        visible+[%b visi.con]
        read+(rule-to-json read.con)
        write-post+(rule-to-json write-post.con)
        write-reply+(rule-to-json write-reply.con)
    ==
    ++  rule-to-json
      |=  r=rule:clay
      ^-  ^json
      %-  pairs:enjs
      :~  mod+[%s mod.r]
          who+[%a (turn ~(tap in (whom-to-ship who.r)) ship:enjs)]
      ==
    ++  whom-to-ship
      |=  sw=(set whom:clay)
      ^-  (set ship)
      %-  ~(rep in sw)
      |=  [w=whom:clay out=(set ship)]
      ?:  -.w
        ?>  ?=(@p +.w)
        (~(put in out) +.w)
      out
    --
::        public+[%b publ.con]
::        kind+[%s kind.con]
::::        comments+[%b comm.con]
::::        xenopost+[%b xeno.con]
::        :-  %except
::        :-  %a
::        %+  turn
::          ~(tap in mems.con)
::        |=  a/@p
::        [%s (scot %p a)]
::    ==
::  ++  front  (my title+desc.con ~)  :: title in lists
  --
::
++  grab
  |%
  ++  noun  config  :: validate over ames
  ++  mime
    |=  {p/mite:eyre q/octs:eyre}
    (cue q.q)
::    (txt (to-wain:format q.q))
::  ++  txt
::    |=  txs/(pole @t)
::    ~&  %reading-config
::    ?>  ?=([desc=@t publ=@t visi=@t kind=@t %'except:' mem=*] txs)
::    :*  (rash desc.txs ;~(pfix (jest '> ') (cook crip (star next))))
::        (rash publ.txs ;~(pfix (jest 'public: ') (flag %y %n)))
::        (rash visi.txs ;~(pfix (jest 'visible: ') (flag %y %n)))
::        %+  rash  kind.txs 
::        %+  sear
::        |=  a=@t
::        ?.  ?=(?(%blog %fora %note) a)
::          ~
::        (some a)
::        ;~  pfix  (jest 'kind: ') 
::          ;~  pose  (jest 'blog')
::                    (jest 'fora')
::                    (jest 'note')
::          ==
::        ==
::::        (rash comm.txs ;~(pfix (jest 'comments: ') (flag %y %n)))
::::        (rash xeno.txs ;~(pfix (jest 'xenopost: ') (flag %y %n)))
::        %-  sy
::        %+  turn  (skip mem.txs |=(a=@t =(a '')))  :: skip trailing blank
::        (curr rash ;~(pfix (jest '  ~') fed:ag))
::    ==
  --
++  grad  %txt
--
::
