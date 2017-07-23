::
::::  hoon/marc/gen
  ::
:-  %say
|=  {^ {pax/path $~} $~}
:-  %noun
=<  (test pax)
=>  |%
    ++  item  (pair mite (list flow))                   ::  xml node generator
    ++  colm  @ud                                       ::  column
    ++  flow  manx                                      ::  node or generator
    ++  mite                                            ::  context
      $?  $down                                         ::  outer embed
          $list                                         ::  unordered list
          $lime                                         ::  list item
          $lord                                         ::  ordered list
          $poem                                         ::  verse
          $bloc                                         ::  blockquote
          $code                                         ::  preformatted code
          $head                                         ::  heading
          $expr                                         ::  dynamic expression
      ==                                                ::
    ++  trig                                            ::  line style
      $:  col/@ud                                       ::  start column 
          $=  sty                                       ::  style
          $?  $done                                     ::   terminator
              $none                                     ::  end of input
              $lint                                     ::  + line item
              $lite                                     ::  - line item
              $head                                     ::  # heading
              $text                                     ::  anything else
      ==  ==                                            ::
    ++  graf                                            ::  input fragment
      $%  {$bold p/tape}                                ::  bold
          {$talc p/tape}                                ::  italics
          {$code p/tape}                                ::  code literal
          {$text p/tape}                                ::  text symbol
          {$link p/(list manx) q/tape}                  ::  URL
      ==
    --
|%                                                      ::  
++  test                                                ::  test text parsing
  |=  pax/path
  ^-  tape
  ::  src: text file as (list cord)
  ::  txt: source as tape with newlines
  ::  vex: parsing result
  ::
  =/  src  .^(wain %cx pax)
  =*  txt  (zing (turn src |=(@t (weld (rip 3 +<) `tape`~[`@`10]))))
  =/  vex  (cram [1 1] txt)
  ::  print result as error or xml text
  ::
  ?~  q.vex
    "syntax error: line {(scow %ud p.p.vex)}, column {(scow %ud q.p.vex)}"
  (poxo p.u.q.vex)
::                                                      ::
++  cram                                                ::  parse unmark
  |=  {naz/hair los/tape}
  ^-  (like flow)
  ::
  ::  err: error position
  ::  col: current control column
  ::  hac: stack of items under construction
  ::  cur: current item under construction
  ::  lub: current block being read in
  ::
  =|  err/(unit hair)
  =/  col  q.naz
  =|  hac/(list item)
  =/  cur/item  [%down ~]
  =|  lub/(unit (pair hair (list tape)))
  =<  $:line
  |%
  ::                                                    ::
  ++  $                                                 ::  resolve
    ^-  (like flow)
    ::  if error position is set, produce error
    ::
    ?.  =(~ err)  [+.err ~]
    ::  all data was consumed
    ::
    =-  [naz `[- [naz los]]]
    |-  ^-  flow
    ::  fold all the way to top
    ::
    ?~  hac  fine
    $(..^$ fold)
  ::                                                    ::
  ++  back                                              ::  column retreat
    |=  luc/@ud
    ^+  +>
    ?:  =(luc col)  +>
    ::  nex: next backward step that terminates this context
    ::
    =/  nex/@ud
      ?-  p.cur
        $down  0
        $head  0
        $expr  2
        $list  0
        $lime  2
        $lord  0
        $poem  8
        $code  4
        $bloc  6
      ==
    ?:  (gth nex (sub col luc))
      ::  indenting pattern violation
      ::
      ..^$(err `[p.naz luc])
    $(..^$ fold, col (sub col nex))
  ::                                                    ::
  ++  fine                                              ::  item to flow
    ^-  flow
    =-  [[- ~] q.cur]
    ?+  p.cur  !!
      $down  %body
      $list  %ul
      $lord  %ol
      $lime  %li
      $bloc  %bq
      $code  %pre
    ==
  ::                                                    ::
  ++  fold  ^+  .                                       ::  complete and pop
    ?~  hac  .
    %=  .
      hac  t.hac
      cur  [p.i.hac [fine q.i.hac]]
    ==
  ::                                                    ::
  ++  snap                                              ::  capture raw line
    =|  nap/tape
    |-  ^+  [nap +>]
    ::  no unterminated lines
    ::
    ?~  los  [~ +>(err `naz)]
    ?:  =(`@`10 i.los)  
      ::  consume newline
      ::
      :_  +>(los t.los, naz [+(p.naz) 1])
      ::  trim trailing spaces
      ::
      |-  ^-  tape 
      ?:  ?=({$' ' *} nap) 
        $(nap t.nap) 
      (flop nap)
    ::  save byte and repeat
    ::
    $(los t.los, q.naz +(q.naz), nap [i.los nap])
  ::                                                    ::
  ++  skip  +:snap                                      ::  discard line
  ++  look                                              ::  inspect line
    ^-  (unit trig)
    ?~  los
      `[q.naz %none]
    ?:  =(`@`10 i.los)
      ~
    ?:  =(' ' i.los)
      look(los t.los, q.naz +(q.naz))
    :+  ~  q.naz
    ?:  =('\\' i.los)
      %done
    ?:  =('\\' i.los)
      %head
    ?:  ?=({$'-' $' ' *} los)
      %lite
    ?:  ?=({$'+' $' ' *} los)
      %lint
    %text
  ::                                                    ::
  ++  cape                                              ::  xml-escape
    |=  tex/tape
    ^-  tape
    ?~  tex  tex
    =+  $(tex t.tex)
    ?+  i.tex  [i.tex -]
      $34  ['&' 'q' 'u' 'o' 't' ';' -]
      $38  ['&' 'a' 'm' 'p' ';' -]
      $39  ['&' '#' '3' '9' ';' -]
      $60  ['&' 'l' 't' ';' -]
      $62  ['&' 'g' 't' ';' -]
    ==
  ::                                                    ::
  ++  clue                                              ::  tape to xml
    |=  tex/tape
    ^-  manx
    [[%$ [%$ tex] ~] ~]
  ::                                                    ::
  ++  cash                                              ::  escaped fence
    |*  tem/rule
    ;~  sfix
      %-  star
      =+  ;~(pose bas tem)
      ;~(pose ;~(less - prn) ;~(pfix bas -))
    ::
      tem
    ==
  ::                                                    ::
  ++  calm                                              ::  complete to space
    |*  sef/rule
    |=  tub/nail
    =/  vex  (sef tub)
    ?~  q.vex  vex 
    ?:  ?|  ?=($~ q.q.u.q.vex) 
            =(' ' i.q.q.u.q.vex)
            =(`@`10 i.q.q.u.q.vex)
        ==
      vex 
    [p=p.vex q=~]
  ::                                                    ::
  ++  cool                                              ::  reparsed fence
    |*  $:  ::  fex: fence delimiter
            ::  sab: main rule
            ::
            fex/rule
            sab/rule
        ==
    |=  {naz/hair los/tape}
    ^+  *sab
    ::  vex: fenced span
    ::
    =/  vex/(like tape)  (fex naz los)
    ?~  q.vex  vex
    ::  hav: reparse full fenced text
    ::
    =/  hav  ((full sab) [naz p.u.q.vex])
    ::  escapes may make error position drift
    ::
    ?~  q.hav  hav
    ::  the complete span with the main product
    ::
    :-  p.vex
    `[p.u.q.hav q.u.q.vex]
  ::                                                    ::
  ++  echo                                              ::  hoon literal
    |=  {naz/hair los/tape}
    ^-  (like tape)
    ::  vex: result of parsing wide twig
    ::
    =/  vex  (wide:vast naz los)
    ::  use result of expression parser
    ::
    ?~  q.vex  vex
    =-  [p.vex `[- q.u.q.vex]] 
    ::  but replace payload with bytes consumed
    ::
    |-  ^-  tape
    ?:  =(q.q.u.q.vex los)  ~
    ?~  los  ~
    [i.los $(los +.los)]
  ::                                                    ::
  ++  word                                              ::  flow unit
    ;~  pose
    ::  *bold literal*
    ::
      (stag %bold ;~(pfix tar (cash tar)))
    ::  _italic literal_
    ::
      (stag %talc ;~(pfix tar (cash cab)))
    ::  =expression
    ::
      (stag %code ;~(pfix tis echo))
    ::  ++arm
    ::
      (stag %code ;~(plug lus lus low (star ;~(pose nud low hep))))
    ::  [arbitrary *content*](url)
    ::
      %+  stag  %link
      ;~  plug
        ;~(pfix sel (cool (cash ser) down))
        ;~(pfix gay ;~(pfix pel (cash per)))
      ==
    ::  lowercase word, ending on word boundary
    ::
      (stag %text (calm (plus low)))
    ::  expression, ending on word boundary
    ::
      (stag %code (calm echo))
    ::  any word-shaped junk
    ::
      (stag %text (star ;~(less ace prn)))
    ==
  ::                                                    ::
  ++  down                                              ::  parse inline flow
    %+  knee  *(list manx)  |.  ~+
    %+  cook
      ::  collect raw flow into xml tags
      ::
      |=  gaf/(list graf)
      ^-  (list manx)
      ::  nap: collected words
      ::  max: collected tags
      ::
      =|  fip/(list tape)
      =|  max/(list manx)
      =<  (flop max:main)
      |%                                                ::
      ++  fill  ^+  .                                   ::  unify text block
                ::  txt: unconsumed text
                ::
                =/  txt/tape
                  =|  txt/tape
                  |-  ^+  txt
                  ?~  fip  txt
                  %=    $
                      fip  t.fip
                      txt  ?:  =(~ txt)
                             i.fip
                           (weld i.fip `tape`[' ' txt])
                  ==
                ?:  =(~ txt)  +
                %=  +
                  max  :_(max (clue txt))
                  fip  ~
                ==                                      ::
      ++  main  ^+  .                                   ::  flow to 
                ?~  gaf  fill
                ?:  ?=($text -.i.gaf)
                  main(gaf t.gaf, fip [p.i.gaf fip])
                ::  nex: first word in flow
                ::  mor: rest of flow
                ::
                =>  :-  [nex=i.gaf mor=t.gaf]
                    ::  consume accumulated text
                    ::
                    fill
                ::  convert and accumulate fragment
                ::
                =-  main(gaf mor, max [- max])
                ^-  manx
                ?-  -.nex
                  $bold  [[%b ~] (clue (cape p.nex)) ~]
                  $talc  [[%i ~] (clue (cape p.nex)) ~]
                  $code  [[%i ~] (clue (cape p.nex)) ~]
                  $link  [[%a [%href (cape q.nex)] ~] p.nex] 
                ==
      --
    (most whit word)
  ::                                                    ::
  ++  para                                              ::  paragraph
    (cook |=((list manx) `(list manx)`[[%p ~] +<]~) down)
  ::                                                    ::
  ++  whit                                              ::  whitespace
    (cold ' ' (plus ;~(pose (just ' ') (just `@`10))))
  ::                                                    ::
  ++  head                                              ::  parse heading
    %+  cook
      |=  $:  ::  a: list of #
              ::  b: tag flow of header line
              ::
              a/tape
              b/(list manx)
          ==
      ^-  (list manx)
      ::  hag: header tag, h1 through h6
      ::
      =/  hag  (cat 3 'h' (add '0' =+((lent a) ?:((gth - 6) 6 -))))
      ::  sid: header text flattened as id
      ::
      =/  sid  ^-  tape
        ::  assemble, normalize and kebab-case
        ::
        =-  %-  zing 
            %+  turn  `(list tape)`(flop -)
            |=  tape  ^-  tape
            %+  turn  `tape`+<
            |=  @tD
            ^-  @tD
            ?:  ?|  &((gte +< 'a') (lte +< 'z'))
                    &((gte +< '0') (lte +< '9'))
                ==
              +<
            ?:  &((gte +< 'A') (lte +< 'Z'))
              (add 32 +<)
            '-'
        ::  collect all text in header flow
        ::
        =|  ges/(list tape)
        |-  ^-  (list tape)
        ?~  b  ges
        %=    $
            b  t.b
            ges
          ?:  ?=({{$$ {$$ *} $~} $~} i.b)
            ::  capture text 
            ::
            [v.i.a.g.i.b ges]
          ::  descend into children
          ::
          $(b c.i.b)
        == 
      ::  header as tag with id attribute
      ::
      [[[%a [%id sid] ~] b] ~]
    ;~  plug
      ;~(sfix (star (just '#')) whit)
      down
    ==
  ::                                                    ::
  ++  made                                              ::  compose block
    ^+  . 
    ::  empty block, no action
    ::
    ?~  lub  .
    ::  if block is preformatted code
    ::
    ?:  ?=($code p.cur)
      ::  add blank line between blocks
      ::
      =.  q.cur
        ?~  q.cur  q.cur
        :_(q.cur (clue `@`10 ~))
      %=    .
          q.cur  
        %+  weld
          %+  turn
            q.u.lub
          |=  tape  ^-  flow
          ::  each line is text data with its newline
          ::
          (clue (weld (slag col +<) `tape`[`@`10 ~]))
        q.cur
      ==
    ::  if block is verse
    ::
    ?:  ?=($poem p.cur)
      ::  add break between stanzas
      ::
      =.  q.cur  ?~(q.cur q.cur [[[%br ~] ~] q.cur])
      %=    .
          q.cur  
        %+  weld
          %+  turn
            q.u.lub
          |=  tape  ^-  flow
          ::  each line is a paragraph
          ::
          :-  [%p ~]
          :_  ~
          (clue (weld (slag col +<) `tape`[`@`10 ~]))
        q.cur
      ==
    ::  yex: block recomposed, with newlines
    ::
    =/  yex/tape  
      (zing (turn (flop q.u.lub) |=(tape (weld +< `tape`[`@`10 ~]))))
    ::  XX expressions commented out
    ::
    ?<  ?=($expr p.cur)
    ::  vex: parse of paragraph
    ::
    =/  vex/(like (list manx))
      ::  either a one-line header or a paragraph
      ::
      %.([p.u.lub yex] ?:(?=($head p.cur) head para))
    ::  if error, propagate correctly
    ::
    ?~  q.vex  ..$(err `p.vex)
    ::  save good result
    ::
    ..$(q.cur (weld p.u.q.vex q.cur))
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
      ?:  ?|  ::  only one line in a heading
              ::
              =(%head p.cur)
              ?:  ?=(?($code $poem $expr) p.cur)
                ::  literals need to end with a blank line
                ::
                (lth col.pic col)
              ::  text flows must continue aligned
              ::
              |(!=(%text sty.pic) !=(col.pic col))
          ==
        ..$(err `[p.naz col.pic])
      ::  accept line and continue
      :: 
      =^  nap  ..$  snap
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
    =<  line:abet:apex
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
    ++  push  |=(mite %_(+> hac [cur hac], cur [+< ~])) ::  push context
    ++  expr  (push %expr)                              ::  hoon expression
    ++  code  (push %code)                              ::  code literal
    ++  poem  (push %poem)                              ::  verse literal
    ++  head  (push %head)                              ::  heading
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
      ::  push list item
      ::
      %.  %lime
      =<  push
      ::  push list context, unless we're in list
      ::
      =+  ?:(ord %lord %list)
      ?:  =(- p.cur)  ..push  (push -)
    ::
    ++  lint  (lent &)                                  ::  numbered list
    ++  lite  (lent |)                                  ::  unnumbered list
    ++  text                                            ::  plain text
      ^+  .
      ::  only in lists, fold
      ::
      ?.  ?=(?($list $lord) p.cur)  .
      .($ fold)
    --
  --
--
