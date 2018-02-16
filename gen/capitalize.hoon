::  to use, download UnicdoeData.txt and place it in `%/lib/unicode-data/txt`.
::
::::
::
::  part 1: parse the file into {uppers}
::
/-  unicode-data
/+  new-hoon
/=  case-table
  /;  |=  a=(list line:unicode-data)
      =,  new-hoon
      |^  %-  build-tree
          %-  flop
          (build-case-nodes a)
      ::
      :>  #
      :>  #  %case-nodes
      :>  #
      :>    transforms raw unicode data into sequential case nodes.
      +|
      ++  build-case-nodes
        :>  raw list of unicode data lines to a compact list of chardata
        |=  a=(list line:unicode-data)
        ^-  (list case-node:unicode-data)
        =<  out
        ::
        ::  todo: we don't have the final case range in the output of this
        ::  gate. this is because this algorithm doesn't work when the last
        ::  char is part of a range. this doesn't happen with the real one,
        ::  only the excerpts i was using for testing.
        ::
        %^  foldl:ls  a  *case-fold
        |=  [c=case-fold l=line:unicode-data]
        ^+  c
        =+  state=(line-to-case-state l)
        ?:  (is-adjacent state prev.c)
          c(prev state)
        =.  c  (add-range c)
        %=  c
          start
            ?:  &(!=(case.state %missing) !=(case.state %none))
              `state
            ~
          prev  state
        ==
      ::
      ++  line-to-case-state
        :>  creates an easy to merge form.
        |=  line:unicode-data
        ^-  case-state
        =/  out=case-state
          [code %none [%none ~] [%none ~] [%none ~]]
        ?:  =(code `@c`0)
          =.  case.out  %missing
          out
        =.  case.out
          ?+  gen  %none
            $lu  %upper
            $ll  %lower
            $lt  %title
          ==
        ::
        ::  several characters aren't described as $lu or $ll but have lower or
        ::  upper state, such as u+2161. detect this and fix it up.
        ::
        =?  case.out  &(=(case.out %none) !=(low ~))  %upper
        =?  case.out  &(=(case.out %none) !=(up ~))  %lower
        ::
        ::  calculate offsets
        ::
        =?  upper.out  !=(up ~)     (calculate-offset (need up) code)
        =?  lower.out  !=(low ~)
           (calculate-offset (need low) code)
        =?  title.out  !=(title ~)  (calculate-offset (need title) code)
        out
      ::
      ++  calculate-offset
        |=  [src=@c dst=@c]
        ^-  case-offset:unicode-data
        ?:  =(src dst)
          [%none ~]
        ?:  (gth src dst)
          [%add (sub src dst)]
        [%sub (sub dst src)]
      ::
      ++  is-adjacent
        :>  is {rhs} a continuation of {lhs}?
        |=  [lhs=case-state rhs=case-state]
        ^-  ?
        ?:  (lth point.rhs point.lhs)
          $(lhs rhs, rhs lhs)
        ?:  !=(point.rhs +(point.lhs))
          %.n
        ?:  !=(case.rhs case.lhs)
          (upper-lower-adjacent lhs rhs)
        ?:  =(case.lhs %none)
          %.n
        ?:  =(case.lhs %missing)
          %.n
        ?:  !=(upper.lhs upper.rhs)
          %.n
        ?:  !=(lower.lhs lower.rhs)
          %.n
        ?:  !=(title.lhs title.rhs)
          %.n
        %.y
      ::
      ++  upper-lower-adjacent
        :>    detects %upper-lower spans.
        :>
        :>  is {lhs} the same as {rhs}, but with opposite case?
        |=  [lhs=case-state rhs=case-state]
        ?:  &(=(case.lhs %upper) !=(case.rhs %lower))
          %.n
        ?:  &(=(case.lhs %lower) !=(case.rhs %upper))
          %.n
        ::
        ::  to simplify detection, if things are in the opposite order, redo
        ::  things flipped.
        ::
        ?:  =(case.lhs %lower)
          $(lhs rhs, rhs lhs)
        ?&  (is-upper-lower lhs)
            (is-lower-upper rhs)
        ==
      ::
      ++  is-upper-lower
        |=  i=case-state
        =(+.+.i [[%none ~] [%add 1] [%none ~]])
      ::
      ++  is-lower-upper
        |=  i=case-state
        =(+.+.i [[%sub 1] [%none ~] [%sub 1]])
      ::
      ++  is-none
        |=  i=case-state
        =(+.+.i [[%none ~] [%none ~] [%none ~]])
      ::
      ++  add-range
        |=  c=case-fold
        ^+  c
        ?~  start.c
          c
        ?:  (is-none u.start.c)
          c
        ?:  ?&  (gth point.prev.c point.u.start.c)
                (is-upper-lower u.start.c)
            ==
          =/  node=case-node:unicode-data
            [`@ux`point.u.start.c `@ux`point.prev.c [%uplo ~] [%uplo ~] [%uplo ~]]
          c(out [node out.c])
        =/  node=case-node:unicode-data
          [`@ux`point.u.start.c `@ux`point.prev.c +.+.u.start.c]
        c(out [node out.c])
      ::
      ++  case-fold
        :>  state that's part of the fold which generates the list of case-nodes
        $:  :>  resulting data to pass to treeify.
            out=(list case-node:unicode-data)
            :>  the start of a run of characters; ~ for not active.
            start=(unit case-state)
            :>  previous character state
            prev=case-state
        ==
      ::
      ++  case-state
        :>  a temporary model which we compress later in a second pass.
        $:  point=@c
            case=case-class
            upper=case-offset:unicode-data
            lower=case-offset:unicode-data
            title=case-offset:unicode-data
        ==
      ::
      ++  case-class
        :>  classification of an individual character.
        $?  $upper
            $lower
            $title
            $none
            $missing
        ==
      ::
      :>  #
      :>  #  %tree-building
      :>  #
      :>    builds a binary search tree out of the list
      +|
      ++  build-tree
        |=  a=(list case-node:unicode-data)
        ^-  case-tree:unicode-data
        ::  there's probably a bottom up approach that doesn't require walking
        ::  a list over and over again.
        ?~  a
          ~
        =+  len=(lent a)
        =+  [lhs rhs]=(split-at:ls (div len 2) a)
        ?~  rhs
          ?~  lhs
            ~
          [i.lhs ~ ~]
        =+  x=[i.rhs $(a lhs) $(a t.rhs)]
        x
      --
  /:  /===/lib/unicode-data  /&unicode-data&/txt/
::
::  part 2: utility core
::
|%
++  transform
  |=  [a=tape fun=$-(@c @c)]
  %-  tufa
  (turn (tuba a) fun)
::
++  to-upper
  :>  returns the uppercase of unicode codepoint {a}
  |=  a=@c
  ^-  @c
  ::  special case ascii to not perform map lookup.
  ?:  (lte a max-ascii)
    ?:  &((gte a 'a') (lte a 'z'))
      (sub a 32)
    a
  (apply-table a case-table %upper)
::
++  to-lower
  :>  returns the lowercase of unicode codepoint {a}
  |=  a=@c
  ^-  @c
  ?:  (lte a max-ascii)
    ?:  &((gte a 'A') (lte a 'Z'))
      (add 32 a)
    a
  (apply-table a case-table %lower)
::
++  apply-table
  :>    searches {table} and apples applies {type} to {a}.
  :>
  :>  this recursively walks the case tree {table}. if it finds an entry which
  :>  matches on {a}, it will apply the offset. otherwise, returns {a}.
  |=  [a=@c table=case-tree:unicode-data type=?($upper $lower $title)]
  ^-  @c
  ?~  table
    a
  ?:  (lth a start.n.table)
    $(table l.table)
  ?:  (gth a end.n.table)
    $(table r.table)
  ?.  &((lte start.n.table a) (lte a end.n.table))
    a
  %^  apply-offset  a  type
  ?-  type
    $upper  upper.n.table
    $lower  lower.n.table
    $title  title.n.table
  ==
::
++  apply-offset
  :>  applies an character offset to {a}.
  |=  [a=@c type=?($upper $lower $title) offset=case-offset:unicode-data]
  ^-  @c
  ?-  offset
    {$add *}   (add a a.offset)
    {$sub *}   (sub a s.offset)
    {$none *}  a
  ::
      {$uplo *}
    ?-  type
      $upper  (sub a 1)
      $lower  (add a 1)
      $title  (sub a 1)
    ==
  ==
::
++  max-ascii  `@c`0x7f
--
::
::  part 3: generator
::
:-  %say
|=  $:  [now=@da eny=@uvJ bec=beak]
        [n=tape $~]
        $~
    ==
:-  %tape  (transform n to-upper)
