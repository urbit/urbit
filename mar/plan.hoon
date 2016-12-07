::
::::  /hoon/plan/mar
  ::
/?    310
/-    plan-acct, plan-diff
::
::::  ~fyr
  ::
=,  eyre
=,  html
=,  format
|_  all/{{who/@txname loc/@txloc} acc/(map knot plan-acct)}
::
++  grow                                                ::  convert to
  =+  all
  |%
  ++  txt
    ^-  wain
    :+  (cat 3 'User ' ?~(who 'of Urbit' who))
      (cat 3 'Location ' ?~(loc %unknown loc))
    %+  turn  (sort (~(tap by acc)) aor)
    |=  {a/knot b/plan-acct}  ^-  cord
    %+  rap  3
    :^  a  ': '  usr.b
    ?~(url.b ~ [', ' (apix:en-purl u.url.b)])
  ::
  ++  plan-json
    %-  pairs:enjs  :~
      who+?~(who ~ s+who)
      loc+?~(loc ~ s+loc)
      acc+o+(~(run by acc) json-acct)
    ==
  ++  json-acct                       ::  helper                     
    |=  a/plan-acct  ^-  json
    =/  url  ?~(url.a ~ (tape:enjs (apix:en-purl u.url.a)))
    (pairs:enjs usr+s+usr.a url+url ~)
  --
++  grab  |%                                            ::  convert from
          ++  noun  {{cord cord} (map knot plan-acct)}  ::  clam from %noun
          ++  txt
            |^  |=  a/wain  ^+  all
                ?>  ?=({@t @t *} a)
                :-  [(rash i.a user) (rash i.t.a location)]
                (malt (turn t.t.a |=(b/cord (rash b account))))
            ::
            ++  user  ;~(pfix (jest 'User ') (cook crip (star prn)))
            ++  knot  (sear (flit |=(a/^knot !=('' a))) urs:ab)
            ++  location  ;~(pfix (jest 'Location ') (cook crip (star prn)))
            ++  account
              ;~  plug
                knot
                ;~(pfix col ace knot)
                (punt ;~(pfix com ace aurf:de-purl))
              ==
            --
          ++  mime  |=({* a/octs} (txt (to-wain q.a)))     ::  XX mark translation
          --
++  grad
  |%  
  ++  form  %plan-diff
  ++  diff
    |=  neu/_all  ^-  plan-diff                        :: XXX map functions
    :+  ?:(=(-.all -.neu) ~ (some -.neu))
      =<  (malt `(list {knot $~})`(murn (~(tap by acc.all)) .))
      |=  {a/knot *}  ^-  (unit {knot $~})
      ?:((~(has by acc.neu) a) ~ (some [a ~]))
    =<  (malt (murn (~(tap by acc.neu)) .))
    |=  {a/knot b/plan-acct}  ^-  (unit {knot plan-acct})
    ?:  =([~ b] (~(get by acc.all) a))
      ~
    (some [a b])
  ::
  ++  pact
    |=  dif/plan-diff  ^+  all                          :: XXX map functions
    :-  (fall inf.dif -.all)
    =;  neu  (~(uni by neu) put.dif)
    =+  del=(~(tap by del.dif))
    |-  ^+  acc.all
    ?~  del  acc.all
    $(del t.del, acc.all (~(del by acc.all) p.i.del))
  ::
  ++  can-join
    |=  {ali/plan-diff bob/plan-diff}  ^-  ?
    ?&  !&(?=({{^ *} {^ *}} +<) !=(u.inf.ali u.inf.bob)) :: compatible info
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
    %^    some
        (mate inf.ali inf.bob)
      (~(uni by del.ali) del.bob)
    (~(uni by put.ali) put.bob)
  --
--
