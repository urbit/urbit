
# communist manifesto


  =/  xml
    \/
      # communist manifesto

      there is a specter hanging over europe -- the specter
      of communism!
    \/

    - foo

    - foo 
::
::::  hoon/marc/gen
  ::
=>  |%
    ++  capo  (each mite head)                          ::  style
    ++  head  {n/mane a/(list n/mane v/(list beer))     ::  xml context
    ++  item  (pair capo (list item))                   ::  xml node generator
    ++  colm  @ud                                       ::  column
    ++  flow  (each item tube)                          ::  node or generator
    ++  form  (unit $?($emph $bold $code))              ::  formatting
    ++  mite  ?($body $list $lord)                      ::  div, ul, ol
    ++  mode                                            ::  inclusion mode
      $?  $cord                                         ::  ++cord (hard string)
          $manx                                         ::  ++manx (XML node)
          $marl                                         ::  ++marl (XML nodes)
          $tape                                         ::  ++tape (soft string)
          $text                                         ::  (list cord)
          $wall                                         ::  (list tape)
      ==                                                ::
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
    ++  tube  (pair mode twig)                          ::  inclusion
    ++  word  @t                                        ::  source text
    --
|%
++  parse
  |=  {naz/hair los/tape}
  =|  
  ^-  (like item)
  ::
  ::  err: yes iff error
  ::  col: current control column
  ::  hac: stack of items under construction
  ::  cur: current item under construction
  ::  lub: current line stack
  ::
  =|  err/_|
  =/  col  q.nax
  =|  hac/(list item)
  =/  cur/item  [[%& %text] ~]
  =|  lap/(list flow)
  =|  lub/(unit (pair hair (list tape)))
  =<  $:body
  |%
  ::                                                    ::
  ++  $                                                 ::  complete parse
    :-  naz
    ?:  err  ~
    :-  ~
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
    $(i.hac i.t.hac(q [dis q.i.t.hac]), t.hac t.t.hac)
  ::                                                    ::
  ++  home                                              ::  to head of line
    ^+  ..$
    ?.  =(' ' i.los)  .
    home(los t.los, q.naz +(q.naz))
  ::                                                    ::
  ++  fail                                              ::  syntax error
    ^+  ..$
    .(err &)
  ::                                                    ::
  ++  back                                              ::  column retreat
    ^+  . 
    !!
  ::                                                    ::
  ++  look                                              ::  inspect line
    ^-  (unit trig)
  ::                                                    ::
  ++  seam                                              ::  parse leaf block
    ^+  .
    !!
  ::                                                    ::
  ++  code                                              ::  finish code block
    ^+  .
   
  ::                                                    ::
  ++  snap                                              ::  produce line
    =|  nap/tape
    ^+  {nap ..$}
    !!
  ::                                                    ::
  ++  swim                                              ::  add line to buffer
    ^+  .
    !!
  ::                                                    ::  
  ++  made                                              ::  finish paragraph
    ^+  . 
    !!
  ::                                                    ::
  ++  know                                              ::  append to flow
    |=  mit
    ^+  +>
    !!
  ::                                                    ::
  ++  knew                                              ::  
    |=  sep/tape                                        
    ^+  +>
    !!
  ::                                                    ::  
  ++  push                                              ::

  ::                                                    ::  
  ++  body                                              ::  body line loop
    ^+  .
    ::  abort after first error
    ::
    ?:  err  .
    ::  pic: profile of this line
    ::
    =/  pic  look
    ::  if line is blank
    ::
    ?~  u.pic
      ::  break section
      ::
      body(..^$ made)
    ::  line is not blank
    ::
    =>  .(pic u.pic)
    ::  if end of input, complain
    ::
    ?:  ?=($none sty.pic)  fail(q.naz col.pic)
    ::  loop against profile
    ::
    |-  ^+  ..^$
    ::  if column has advanced
    :: 
    ?:  (gth col.pic col) 
      ::  dif: columns advanced
      ::
      =/  dif  (sub col.pic col)
      ?+    dif  
        ::  fail if advance is not 2, 4, 6 or 8
        ::
        ..^$(err &, q.naz col.pic)
          ::  hoon expression literal
          ::
          $2
        ::  sab: rule for embedded twig (producing (list manx))
        ::
        =*  sab  (ifix [gay ;~(plug (star ace) (just `@`10))] tall:vast)
        ::  vex: product of parsing following twig
        ::
        =/  vex/(like twig)  (sab naz los)
        ::  fail upward if parse failed
        ::
        ?~  q.vex  ..^$(err &, naz p.vex)
        ::  otherwise, add item and continue
        ::
        %=  body
          naz  p.q.u.vex
          los  q.q.u.vex
          lap  :_(lap [%| %marl p.u.vex])
        ==
          ::  preformatted text in code style
          ::
          $4
        ::
        ::
        body(..^$ poem:seam:take(col col.pic))
          ::
          ::  blockquote
          ::
          $6
        ::  repeat line loop with nested item
        ::
        $(..^$ (push(col (add 6 col)) [[%& %quot] ~]))
          ::
          ::  preformatted text in verse style
          ::
          $8
        body(..^$ poem:seam:take(col col.pic))
      ==
    ::  if column has retreated
    ::
    ?:  (lth col.pic col)
      ::  repeat with parsing stack adjusted
      ::
      $(..^$ back)
    ::
    ::  at 
    ::
    ?-    sty.pic
        ::
        $done  
      ..^$(q.naz (add 2 col))
      $none  ..^$(q.naz col)
      $lite  ?^(body(..^$ (push:(snip [%
      $lint  body(..^$ (push:(
    ==
    body(..^$ norm)
  ::
  ++  many
    |=  all/(list twig)
    ?~  all  !!
    |-  ^-  twig
    ?~  t.all
      i.all
    [i.all $(i.all i.t.all, t.all t.t.all)]
  --
