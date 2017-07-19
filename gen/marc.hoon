::
::::  hoon/marc/gen
  ::
:-  %say
|=  *
:-  %noun
=>  |%
    ++  item  (pair mite (list flow))                   ::  xml node generator
    ++  colm  @ud                                       ::  column
    ++  flow  (each item twig)                          ::  node or generator
    ++  form  (unit $?($emph $bold $code))              ::  formatting
    ++  mite                                            ::  context
      $?  $down                                         ::  outer embed
          $flow                                         ::  regular flow; div
          $list                                         ::  unordered list
          $lime                                         ::  list item
          $lord                                         ::  ordered list
          $poem                                         ::  verse
          $bloc                                         ::  blockquote
          $code                                         ::  preformatted code
          $expr                                         ::  dynamic expression
      ==
    ++  trig                                            ::  line style
      $:  col/@ud                                       ::  start column 
          $=  sty                                       ::  style
          $?  $done                                     ::  \/ terminator
              $none                                     ::  end of input
              $lint                                     ::  + line item
              $lite                                     ::  - line item
              $head                                     ::  # heading
              $text                                     ::  anything else
      ==  ==                                            ::
    ++  word  @t                                        ::  source text
    --
|%
++  parse
  |=  {naz/hair los/tape}
  ^-  (like item)
  ::
  ::  err: error position
  ::  col: current control column
  ::  hac: stack of items under construction
  ::  cur: current item under construction
  ::  lub: current line stack
  ::
  =|  err/(unit hair)
  =/  col  q.naz
  =|  hac/(list item)
  =/  cur/item  [%flow ~]
  =|  lub/(unit (pair hair (list tape)))
  =<  $:line
  |%
  ::                                                    ::
  ++  $                                                 ::  complete parse
    ^-  (like item)
    ?^  err  [u.err ~]
    =-  [naz `[- [naz los]]]
    =.  hac  [cur hac]
    ?~  hac  !!
    |-  ^-  item
    ::  complete assembly by inverting stack
    ::
    =/  dis/item  [p.i.hac (flop q.i.hac)]
    ::
    ::  add as last entry in parent, or return
    ::
    ?~  t.hac  dis
    $(i.hac `item`i.t.hac(q [[%& dis] q.i.t.hac]), t.hac `(list item)`t.t.hac)
  ::                                                    ::
  ++  back                                              ::  column retreat
    |=  bac/@ud
    ^+  +>
    !!
  ::                                                    ::
  ++  snap                                              ::  capture line
    =|  nap/tape
    ^+  [nap +]
    !!
  ::                                                    ::
  ++  skip  +:snap                                      ::  discard line
  ++  look                                              ::  inspect line
    ^-  (unit trig)
    !!
  ++  made                                              ::  finish paragraph
    ^+  . 
    !!
  ::
::    ++  expr

      ::  sab: rule for embedded twig
      ::
::      =*  sab  (ifix [gay ;~(plug (star ace) (just `@`10))] tall:vast)
      ::  vex: product of parsing following twig
      ::
::      =/  vex/(like twig)  (sab naz los)
      ::  fail upward if parse failed
      ::
::      ?~  q.vex  ..^$(err `p.vex)
      ::  otherwise, add item and continue
::    ::
::    %=  $
::      naz  p.q.u.vex
::      los  q.q.u.vex
::      lap  :_(lap [%| %marl p.u.vex])
::    ==
  ::                                                    ::  
  ++  line  ^+  .                                       ::  body line loop
    ::  abort after first error
    ::
    ?:  !=(~ err)  .
    ::  pic: profile of this line
    ::
    =/  pic  look
    ::  if line is blank
    ::
    ?~  pic
      ::  break section
      ::
      line:made:skip
    ::  line is not blank
    ::
    =>  .(pic u.pic)
    ::  if end of input, complete
    ::
    ?:  ?=($none sty.pic)
      ..$(q.naz col.pic)
    ::  if end marker behind current column
    ::
    ?:  &(?=($done sty.pic) (lth col.pic col))
      ::  retract and complete
      ::
      (back(q.naz (add 2 col.pic)) col.pic)
    ::  bal: inspection copy of lub, current section
    ::
    =/  bal  lub
    ::  if within section
    ::
    ?^  bal
      ::  detect bad block structure
      ::
      ?:  ?|  ?=($head sty.pic)
              ?:  ?=(?($code $poem $expr) p.cur)
                (lth col.pic col)
              |(!=(%text sty.pic) !=(col.pic col))
          ==
        ..$(err `[p.naz col.pic])
      ::  accept line and continue
      :: 
      =^  nap  ..line  snap
      line(lub bal(q.u [nap q.u.bal]))
    ::  if column has retreated, adjust stack
    ::
    =.  ..$  ?:  (lth col.pic col)  ..$
        (back col.pic)
    ::  dif: columns advanced
    ::  erp: error position
    ::
    =/  dif  (sub col.pic col)
    =/  erp  [p.naz col.pic]
    =.  col  col.pic
    ::  nap: take first line
    ::
    =^  nap  ..$  snap
    ::  execute appropriate paragraph form
    ::
    =<  line(..$ abet:apex)
    |%
    ::                                                  ::  
    ++  abet                                            ::  accept line
      ..$(lub `[naz nap ~])
    ::                                                  ::
    ++  apex  ^+  .                                     ::  by column offset
      ?+  dif  fail
        $0  apse
        $2  expr
        $4  code
        $6  bloc
        $8  poem
      ==
    ::                                                  ::
    ++  apse  ^+  .                                     ::  by prefix style
      ?-  sty.pic
        $done  !!
        $head  head
        $lite  lite
        $lint  lint
        $text  text
      ==
    ::                                                  ::
    ++  bloc  apse:(push %bloc)                         ::  blockquote line
    ++  fail  .(err `erp)                               ::  set error position
    ++  push  |=(mite %_(+> hac [cur hac], cur [+< ~])) ::  deeper stack
    ++  expr  (push %expr)                              ::  hoon expression
    ++  code  (push %code)                              ::  code literal
    ++  poem  (push %poem)                              ::  verse literal
    ++  head  !!
    ++  lent                                            ::  list entry
      |=  ord/?
      ^+  +>
      ::  erase list marker
      ::
      =.  nap  =+(+(col) (runt [- ' '] (slag - nap)))
      ::  indent by 2
      ::
      =.  col  (add 2 col)
      ::  can't switch list types
      ::
      ?:  =(?:(ord %list %lord) p.cur)  fail
      ::  if not already in list, start list
      ::
      =+  ?:(ord %lord %list)
      ?:  =(- p.cur) 
        (push %lime)
      (push:(push -) %lime)
    ::
    ++  lint  (lent &)                                  ::  numbered list
    ++  lite  (lent |)                                  ::  unnumbered list
    ++  text                                            ::  plain text
      ^+  .
      ::  except in lists, continue in current flow
      ::
      ?.  ?=(?($list $lord) p.cur)  .
      ::  in lists, finish current and switch to text
      ::
      ?>  ?=(^ hac)
      .(hac [[p.i.hac [[%& cur] q.i.hac]] t.hac], cur [%flow ~])
    --
  --
--
