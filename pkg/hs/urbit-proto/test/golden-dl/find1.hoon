=/  a
  :*  p=1
      q=[1 2]
      r=`$-(@ @)`|=(a a)
      s=r=$
      q=[1 2 3]
      t=[u=4 v=5]
  ==
:*  p.a
    q.a
    +3.q.a
    r.a
    s.a
    r.s.a
    t.a
    u.t.a
    v.t.a
    +2.t.a
    +3.t.a
    +3.a
    +6.a
==
