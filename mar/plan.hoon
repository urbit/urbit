::
::::  /hoon/plan/mar
  ::
/?    310
/-    plan-data, plan-diff
!:
::::  ~fyr
  ::
|_  all/{{who/cord loc/govt} usr/(map knot plan-data)}
::
++  grow                                                ::  convert to
  =+  all
  |%
  ++  txt
    ^-  wain
    :+  ?~(who 'Urbit User' who)
      (cat 3 'Location ' (moon ?~(loc /unknown loc)))
    %+  turn  (sort (~(tap by usr)) aor)
    |=  {a/knot b/iden c/(unit purf)}  ^-  cord
    %+  rap  3
    :^  a  ': '  b
    ?~(c ~ [', ' (earf u.c)])
  --
++  grab  |%                                            ::  convert from
          ++  noun  {{cord govt} (map knot plan-data)}  ::  clam from %noun
          ++  txt
            |^  |=  a/wain  ^+  all
                ?>  ?=({@t @t *} a)
                :-  [i.a (rash i.t.a loca)]
                (malt (turn t.t.a |=(b/cord (rash b acct))))
            ::
            ++  loca  ;~(pfix (jest 'Location ') (more fas urs:ab))
            ++  acct
              ;~  plug
                urs:ab
                ;~(pfix col ace urs:ab)
                (punt ;~(pfix com ace aurf:urlp))
              ==
            --
          ++  mime  |=({* a/octs} (txt (lore q.a)))     ::  XX mark translation
          --
++  grad
  |%  
  ++  form  %plan-diff
  ++  diff
    =|  out/plan-diff
    |=  neu/(map knot plan-data)  ^+  out               :: XXX map functions
    :-  =<  (malt `(list {knot $~})`(murn (~(tap by usr.all)) .))
        |=  {a/knot *}  ^-  (unit {knot $~})
        ?:((~(has by neu) a) ~ (some [a ~]))
    =<  (malt (murn (~(tap by neu)) .))
    |=  {a/knot b/plan-data}  ^-  (unit {knot plan-data})
    ?:  =([~ b] (~(get by usr.all) a))
      ~
    (some [a b])
  ::
  ++  pact
    |=  dif/plan-diff  ^+  usr.all                          :: XXX map functions
    =;  neu  (~(uni by neu) put.dif)
    =+  del=(~(tap by del.dif))
    |-  ^+  usr.all
    ?~  del  usr.all
    $(del t.del, usr.all (~(del by usr.all) p.i.del))
  ::
  ++  can-join
    |=  {ali/plan-diff bob/plan-diff}  ^-  ?
    ?&  ::|(!?=({{^ *} {^ *}} +<) =(u.inf.ali u.inf.bob)) :: compatible info
        =(~ (~(int by `(map knot *)`del.ali) put.bob))  :: no del-put
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
