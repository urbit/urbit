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
        delete+delete
        submit+submit
        comment+comment
        resubmit+resubmit
        delete-topic+delete-topic
        delete-comment+delete-comment
      ==
    ::
    ++  null-or-da
      %+  cu  |=(a=dime ?+(a !! [%n ~] ~, [%da @da] q.a))
      %+  cu  |=(a=coin ?+(a !! [%$ ^] p.a))
      (su nuck:so.hoon)
    ::
    ++  resubmit
      |=  a=json
      =+  ^-  [col=@da top=@da tit=@t wat=wain]
          %.  a
          (ot col+(se %da) top+(se %da) tit+so wat+wain ~)
      [[~ col] top tit wat]
    ++  delete-topic
      |=  a=json
      =+  ^-  [col=@da top=@da]
          %.  a
          (ot col+(se %da) top+(se %da) ~)
      [[~ col] top]
    ++  delete-comment
      |=  a=json
      =+  ^-  [col=@da top=@da com=@da]
          %.  a
          (ot col+(se %da) top+(se %da) com+(se %da) ~)
      [[~ col] top com]
    ++  delete
      |=  a=json
      =+  ^-  col=@da
          %.  a
          (ot col+(se %da) ~)
      [[~ col]]
    ++  submit
      |=  a=json
      ^-  [coll-full @t wain]
      =/  host  
        %.  a
        (ot:dejs-soft:format hos+(se-soft %p) ~)
      =+  ^-  [col=@da tit=@t wat=wain]
          %.  a
          (ot col+(se %da) tit+so wat+wain ~)
      [[host col] tit wat]
    :: 
    ++  comment
      |=  a=json
      =/  host
      %.  a
      (ot:dejs-soft:format hos+(se-soft %p) ~)
      =+  ^-  [col=@da top=@da com=?(~ @da) wat=wain]
          %.  a
          (ot col+(se %da) top+(se %da) com+null-or-da wat+wain ~)
      [[host col] top com wat]
    ++  create
::       (ot wat+(cu (hard kind) so) des+so pub+bo vis+bo ses+(as (se %p)) ~)
      |=  a=json
      ~|  a
      =+  ^-  [desc=cord publ=? visi=? comm=? xeno=? ses=(set @p)]
          %.  a
          :: change this to accept an array of @p
          %-  ot  
          :::~  wat+(cu (hard kind) so) 
          :~  desc+so 
              publ+bo 
              visi+bo 
              comm+bo 
              xeno+bo 
              ses+(su (cook sy (more ace fed:ag)))
          ==
      [desc publ visi comm xeno ses]
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
    ++  se-soft                                                ::  string as aura
      =,  wired
      |*  a=term
      %+  cu
        |=  b=(unit cord)  ^-  (unit (odo:raid a))
        ?~  b  ~
        `(slav a (need b))
      so:dejs-soft:format
--  --
--
