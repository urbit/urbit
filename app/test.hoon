::
=,  gall
=,  ford
=,  format
|_  {bowl $~}
++  peek  _~
++  made-a-core
  |=  {a/spur @uvH b/gage}
  :_  +>.$
  ?>  ?=([%tabl ^ ~] b)
  =/  cur  p.i.p.b
  %-  %-  slog
      ?+  -.cur  !!
        %|  (flop p.cur)
        %&  ~ ::[(sell q.p.cur)]~
      ==
  =/  nex/(list spur)
    =<(p ;;(,[%& %cont * p=(list spur)] q.i.p.b))
  ?~  nex  ~
  [ost (build-core nex)]~
::
++  build-core
  |=  [a=spur b=(list spur)]
  ~&  >>  (flop a)
  :^  %exec  a-core+a  our
  %-  some
  ^-  bilk
  :-  now-beak
  :~  %tabl
    [[%core now-beak a] [%$ %cont !>(b)]]
  ==
::
++  poke-noun
  |=  [%cores a=path]  ::TODO restore historical [%marks ~] handler
  :_  +>
  ?:  [dry=|]
    ~&((list-cores a) ~)
  [ost (build-core [- +]:(list-cores a))]~
::
++  list-cores
  |=  a/path  ^-  (list spur)
  =/  sup  (flop a)
  |-  ^-  (list spur)
  %-  zing
  %+  turn
    =-  (sort ~(tap by -) aor)
    dir:.^(arch %cy (en-beam now-beak sup))
  |=  [a=knot ~]  ^-  (list spur)
  =.  sup  [a sup]
  ?~  [fil:.^(arch %cy (en-beam now-beak [%hoon sup]))]
    ^$
  ~&  (flop sup)
  [sup ^$]
::
++  now-beak  %_(byk r [%da now])
--
