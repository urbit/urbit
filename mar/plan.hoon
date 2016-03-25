::
::::  /hoon/plan/mar
  ::
/?    310
/-    plan-diff
!:
::::  ~fyr
  ::
|_  all/(map knot iden)
::
++  grow                                                ::  convert to
  |%
  ++  txt
    %+  turn  (sort (~(tap by all)) aor)
    |=({a/knot b/iden} (crip "{(trip a)}: {(trip b)}"))
  --
++  grab  |%                                            ::  convert from
          ++  noun  (map knot iden)                     ::  clam from %noun
          ++  txt
            |=  a/wain
            %-  ~(gas by all)
            (turn a |=(b/cord (rash b ;~((glue (jest ': ')) urs:ab urs:ab))))
          ++  mime  |=({* a/octs} (txt (lore q.a)))     ::  XX mark translation
          --
++  grad
  |%  
  ++  form  %plan-diff
  ++  diff
    =|  out/plan-diff
    |=  neu/(map knot iden)  ^+  out                    :: XXX map functions
    :-  =<  (malt `(list {knot $~})`(murn (~(tap by all)) .))
        |=  {a/knot @}  ^-  (unit {knot $~})
        ?:((~(has by neu) a) ~ (some [a ~]))
    =<  (malt (murn (~(tap by neu)) .))
    |=  {a/knot b/iden}  ^-  (unit {knot iden})
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
