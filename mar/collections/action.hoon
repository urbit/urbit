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
        create+create
        delete+(ot col+(se %da) ~)
        submit+(ot col+(se %da) tit+so wat+wain ~)
        comment+(ot col+(se %da) top+(se %da) com+null-or-da wat+wain ~)
        resubmit+(ot col+(se %da) top+(se %da) tit+so wat+wain ~)
        delete-topic+(ot col+(se %da) top+(se %da) ~)
        delete-comment+(ot col+(se %da) top+(se %da) com+(se %da) ~)
      ==
    ::
    ++  null-or-da
      %+  cu  |=(a=dime ?+(a !! [%n ~] ~, [%da @da] q.a))
      %+  cu  |=(a=coin ?+(a !! [%$ ^] p.a))
      (su nuck:so.hoon)
    ::
    ++  create
::       (ot wat+(cu (hard kind) so) des+so pub+bo vis+bo ses+(as (se %p)) ~)
      |=  a=json
      ~|  a
      =+  ^-  [wat=kind des=cord pub=? vis=? ses=(set @p)]
          %.  a
          :: change this to accept an array of @p
          (ot wat+(cu (hard kind) so) des+so pub+bo vis+bo ses+(su (cook sy (more ace fed:ag))) ~)
      [wat des pub vis ses]
    ::
    ++  wain  (su (more newline (cook crip (star prn))))
    :: ++  newline  (just '\0a')
    ::XX getting sent \r by frontend
    ++  newline  ;~(pfix (punt (just '\0d')) (just '\0a'))
    ::
    ::
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
