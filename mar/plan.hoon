::
::::  /hoon/plan/mar
  ::
/?    310
/-    plan-data, plan-diff
!:
::::  ~fyr
  ::
|_  all/(map knot plan-data)
::
++  grow                                                ::  convert to
  |%
  ++  txt
    %+  turn  (sort (~(tap by all)) aor)
    |=  {a/knot b/iden c/(unit purf)}  ^-  cord
    %-  crip
    "{(trip a)}: {(trip b)}".
      "{?~(c "" ", {(earf u.c)}")}"
  --
++  grab  |%                                            ::  convert from
          ++  noun  (map knot plan-data)                ::  clam from %noun
          ++  txt
            =;  fel
              |=  a/wain  ^+  all
              (malt (turn a |=(b/cord (rash b fel))))
            ;~  plug
              urs:ab
              ;~(pfix col ace urs:ab)
              (punt ;~(pfix com ace aurf:urlp))
            ==
          ++  mime  |=({* a/octs} (txt (lore q.a)))     ::  XX mark translation
          --
++  grad
  |%  
  ++  form  %plan-diff
  ++  diff
    =|  out/plan-diff
    |=  neu/(map knot plan-data)  ^+  out               :: XXX map functions
    :-  =<  (malt `(list {knot $~})`(murn (~(tap by all)) .))
        |=  {a/knot *}  ^-  (unit {knot $~})
        ?:((~(has by neu) a) ~ (some [a ~]))
    =<  (malt (murn (~(tap by neu)) .))
    |=  {a/knot b/plan-data}  ^-  (unit {knot plan-data})
    ?:  =([~ b] (~(get by all) a))
      ~
    (some [a b])
  ::
  ++  pact
    |=  dif/plan-diff  ^+  all                          :: XXX map functions
    =;  neu  (~(uni by neu) put.dif)
    =+  del=(~(tap by del.dif))
    |-  ^+  all
    ?~  del  all
    $(del t.del, all (~(del by all) p.i.del))
  ::
  ++  can-join
    |=  {ali/plan-diff bob/plan-diff}  ^-  ?
    ?&  =(~ (~(int by `(map knot *)`del.ali) put.bob))  :: no del-put
        =(~ (~(int by `(map knot *)`put.ali) del.bob))  :: conflicts
        .=  (~(int by put.ali) put.bob)                 :: and all put
        (~(int by put.bob) put.ali)                     :: values match
    ==
  ::
  ++  join
    |=  {ali/plan-diff bob/plan-diff}
    ^-  (unit plan-diff)
    ?.  (can-join ali bob)
      ~
    (some [(~(uni by del.ali) del.bob) (~(uni by put.ali) put.bob)])
  --
--
