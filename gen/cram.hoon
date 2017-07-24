::
::::  hoon/cram/gen
  ::
  ::  test generator for the cram markdown syntax
  ::
  ::  todo: integrate with ++sail and embed in hoon compiler
  ::
  ::  ++cram is a simple markdown-inspired parser that makes
  ::  common html tropes easy to type.  you can think of ++cram
  ::  as "rational markdown" or "markdown with syntax errors."
  ::  a document format should be easy to type and read, but
  ::  that doesn't mean it can't or have rigorous syntax.
  ::
  ::  tldr: ++cram is indent-oriented.  indent 2 spaces for 
  ::  a dynamic interpolation, 4 spaces for example code, 6
  ::  spaces for a blockquote and 8 spaces for verse.  separate
  ::  every semantic block by a blank line.  use - for 
  ::  unordered lists, + for ordered lists.  
  ::  
  ::  markdown link syntax works.  * means bold, _ means
  ::  italics, "" inserts smart quotes.  all enclosed
  ::  strings are reparsed; escape the terminator within
  ::  the string, eg, *star \* in bold text*.
  ::
  ::  markdown `literal` syntax is supported, but all hoon
  ::  constants are automatically marked as code.  also, any
  ::  hoon expression prefixed with # is a code literal. 
  ::
  ::  (++cram is a valid hoon parsing rule, but it does a lot
  ::  of custom processing internally, since the language is
  ::  context-sensitive.  we use a context-sensitive parser
  ::  to cut the lines into blocks, then reparse flow blocks
  ::  with normal hoon rules.  multipass parsing is the tax
  ::  humans have to pay for simple but human-friendly syntax.)
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
      $%  {$bold p/(list graf)}                         ::  *bold*
          {$talc p/(list graf)}                         ::  _italics_
          {$quod p/(list graf)}                         ::  "double quote"
          {$code p/tape}                                ::  code literal
          {$text p/tape}                                ::  text symbol
          {$link p/(list graf) q/tape}                  ::  URL
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
  ::  state of the parsing loop.  we maintain a construction 
  ::  stack for elements and a line stack for lines in the
  ::  current block.  a blank line causes the current block
  ::  to be parsed and thrown in the current element.  when
  ::  the indent column retreats, the element stack rolls up.
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
        $down  2
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
      ..^$(col luc, err `[p.naz luc])
    =.  ..^$  fold
    $(col (sub col nex))
  ::                                                    ::
  ++  fine                                              ::  item to flow
    ^-  flow
    ?:  ?=($head p.cur)
      ?>  ?=({* $~} q.cur)
      i.q.cur
    =-  [[- ~] (flop q.cur)]
    ?+  p.cur  !!
      $down  %div
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
    ?:  ?&  =('#' i.los)
            |-  ^-  ?
            ?~  t.los  |
            ?:  =(' ' i.t.los)  &
            ?:  =('#' i.t.los)  $(t.los t.t.los)
            |
        ==
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
      $39  ['&' 'a' 'p' 'o' 's' ';' -]
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
      ;~  pose 
        (cold ' ' whit)
        ;~(pose ;~(less - prn) ;~(pfix bas -))
      ==
    ::
      tem
    ==
  ::                                                    ::
  ++  cool                                              ::  reparse
    |*  $:  ::  fex: primary parser
            ::  sab: secondary parser
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
    ::  reparsed error position is always at start
    ::
    ?~  q.hav  [naz ~]
    ::  the complete span with the main product
    ::
    :-  p.vex
    `[p.u.q.hav q.u.q.vex]
  ::                                                    ::
  ++  echo                                              ::  hoon literal
    |*  sab/rule
    |=  {naz/hair los/tape}
    ^-  (like tape)
    ::  vex: result of parsing wide twig
    ::
    =/  vex  (sab naz los)
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
  ++  word                                              ::  flow parser
    %+  knee  *(list graf)  |.  ~+
    ;~  pose
    ::  whitespace
    ::
      (cold [%text " "]~ whit)
    ::  ordinary word
    ::
      %+  cook  |=(graf [+< ~])
      (stag %text ;~(plug ;~(pose low hig) (star ;~(pose nud low hig hep))))
    ::  naked \escape
    ::
      %+  cook  |=(@ [%text [+< ~]]~)
      ;~(pfix bas ;~(less ace prn))
    ::  *bold literal*
    ::
      %+  cook  |=(graf [+< ~])
      (stag %bold ;~(pfix tar (cool (cash tar) work)))
    ::  _italic literal_
    ::
      %+  cook  |=(graf [+< ~])
      (stag %talc ;~(pfix cab (cool (cash cab) work)))
    ::  "quoted text"
    ::
      %+  cook  |=(graf [+< ~])
      (stag %quod ;~(pfix doq (cool (cash doq) work)))
    ::  `classic markdown quote`
    ::
      %+  cook  |=(graf [+< ~])
      (stag %code ;~(pfix tec (cash tec)))
    ::  #twig
    ::
      %+  cook  |=(graf [+< ~])
      (stag %code ;~(pfix hax (echo wide:vast)))
    ::  ++arm
    ::
      %+  cook  |=(graf [+< ~])
      (stag %code ;~(plug lus lus low (star ;~(pose nud low hep))))
    ::  [arbitrary *content*](url)
    ::
      %+  cook  |=(graf [+< ~])
      %+  stag  %link
      ;~  plug
        ;~(pfix sel (cool (cash ser) work))
        ;~(pfix gay ;~(pfix pel (cash per)))
      ==
    ::  direct hoon constant
    ::
      %+  cook  |=(graf [+< ~])
      %+  stag  %code
      %-  echo
      ;~  pose
        bisk:so
        tash:so
        ;~(pfix dot perd:so)
        ;~(pfix sig ;~(pose twid:so (easy [%$ %n 0])))
        ;~(pfix cen ;~(pose sym buc pam bar qut nuck:so))
      ==
    ::  just a byte
    ::
      (cook |=(@ [%text [+< ~]]~) ;~(less ace prn))
    ==
  ::                                                    ::
  ++  work                                              ::  indefinite flow
    %+  cook
      |=((list (list graf)) (zing +<))
    (star word)
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
      ++  fill  %_  .                                   ::  unify text block
                  max  :_(max (clue (zing (flop fip))))
                  fip  ~
                ==                                      ::
      ++  main  ^+  .                                   ::  flow to 
                ?~  gaf  fill
                ?:  ?=($text -.i.gaf)
                  main(gaf t.gaf, fip [(cape p.i.gaf) fip])
                ::  nex: first word in flow
                ::  mor: rest of flow
                ::
                =>  :-  [nex=i.gaf mor=t.gaf]
                    ::  combine accumulated text
                    ::
                    fill
                ::  convert and accumulate fragment
                ::
                =-  main(gaf mor, max (weld - max))
                ^-  (list manx)
                ?-  -.nex
                  $bold  [[%b ~] $(gaf p.nex)]~
                  $talc  [[%i ~] $(gaf p.nex)]~
                  $code  [[%code ~] (clue (cape p.nex)) ~]~
                  $quod  ::  smart quotes
                         ::
                         %=    $
                             gaf
                           :-  [%text `@`226 `@`128 `@`156 ~] 
                           %+  weld  p.nex
                           `(list graf)`[%text `@`226 `@`128 `@`157 ~]~
                         ==
                  $link  [[%a [%href (cape q.nex)] ~] $(gaf p.nex)]~
                ==
      --
    work
  ::                                                    ::
  ++  para                                              ::  paragraph
    %+  cook
      |=((list manx) [[%p ~] +<]~) 
    ;~(pfix gay down)
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
      =*  hag  (cat 3 'h' (add '0' =+((lent a) ?:((gth - 6) 6 -))))
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
      [[[hag [%id sid] ~] b] ~]
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
          (clue (weld (slag (dec col) +<) `tape`[`@`10 ~]))
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
          (clue (weld (slag (dec col) +<) `tape`[`@`10 ~]))
        q.cur
      ==
    ::  yex: block recomposed, with newlines
    ::
    =/  yex/tape
      =/  hel  (flop q.u.lub)
      |-  ^-  tape
      ?~  hel  ~
      ?~  t.hel  i.hel
      (weld i.hel `tape`[`@`10 $(hel t.hel)])
    ::  XX live expressions stubbed out
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
    ::  finish tag if it's a header
    ::
    =<  ?.  =(%head p.cur)  ..$  fold
    ::  save good result, clear buffer
    ::
    ..$(lub ~, q.cur (weld p.u.q.vex q.cur))
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
    =.  ..$  ?.  (lth col.pic col)  ..$  (back col.pic)
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
