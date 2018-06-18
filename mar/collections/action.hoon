::  /action/collections/mar
::
/-  collections
=,  api:collections
|_  act=action
::
++  grow
  |%
  ++  tank  >act<
  --
::
++  grab
  |%
  ++  noun  action
  ++  json
    =,  dejs:format
::     %+  cu  |=(action +<)
    =<  action
    |%
    ++  action
      %-  of  :~
        create+config
        change-config+config
        delete+(ot host+(su fed:ag) col+(se %da) ~)
        submit+(ot host+(su fed:ag) col+(se %da) tit+so wat+wain ~)
        comment+(ot host+(su fed:ag) col+(se %da) top+(se %da) com+null-or-da wat+wain ~)
        resubmit+(ot host+(su fed:ag) col+(se %da) top+(se %da) tit+so wat+wain ~)
        delete-topic+(ot host+(su fed:ag) col+(se %da) top+(se %da) ~)
        delete-comment+(ot host+(su fed:ag) col+(se %da) top+(se %da) com+(se %da) ~)
      ==
    ::
    ++  null-or-da
      %+  cu  |=(a=dime ?+(a !! [%n ~] ~, [%da @da] q.a))
      %+  cu  |=(a=coin ?+(a !! [%$ ^] p.a))
      (su nuck:so.hoon)
    ::
    ++  config
      %-  ot
      :~  description+so
          visible+bo
          read+rule-parse
          write-post+rule-parse
          write-reply+rule-parse
      ==
    ::
    ++  wain  (su (more newline (cook crip (star prn))))
    :: ++  newline  (just '\0a')
    ::XX getting sent \r by frontend
    ++  newline  ;~(pfix (punt (just '\0d')) (just '\0a'))
    ::
    ::
    ++  rule-parse
      |=  jon=json
      ^-  rule:clay
      %-  (hard rule:clay)
      ((ot ~[mod+so who+whom-parse]) jon)
    ++  whom-parse
      |=  jon=json
      ^-  (set whom:clay)
      =/  x  ((ar (su fed:ag)) jon)
      %-  (hard (set whom:clay))
      %-  ~(run in (sy x))
      |=  w=@
      [& w]
    ++  as  |*(a=fist (cu sy (ar a)))                     ::  array as set
    ++  se                                                ::  string as aura
      =,  wired
      |*  a=term
      %+  cu
        |=  b=cord  ^-  (odo:raid a)
        (slav a b)
      so
--  --
--
