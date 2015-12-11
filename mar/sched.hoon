!:
::::  /hoon/core/sched/mar
  ::
|_  dat+(map @da cord)
++  grow                                                ::  convert to
  |%  ++  mime  [/text/x-sched (tact tape)]
      ++  tape  
        (zing `wall`(turn sorted-list |=({a+@da b+cord} "{<a>} {(trip b)}\0a")))
      ++  elem  =<  ;ul: *{(turn sorted-list .)}
                |=  {tym+@da ite+cord}  ^-  manx
                ;li: ;{b "{<tym>}"}: {(trip ite)}
      ++  sorted-list
        (sort (~(tap by dat)) |=({{l+@ @} {r+@ @}} (lth l r)))
  --
++  grab
  |%                                                    ::  convert from
  ++  mime
    |=  {p+mite q+octs}  ^+  dat
    =<  (mo (turn (lore q.q) .))
    |=  a+@t  ^-  {@da @t}
    %+  rash  a
    ;~  (glue ace)
      (cook |=(a+coin ?>(?=({$~ $da @} a) `@da`q.p.a)) nuck:so)
      (cook crip (star prn))
    ==
  --
++  grad  %mime
--
