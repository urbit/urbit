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
=>
|%
::
++  freeze
  |=  manx:twig  ^-  manx
  :-  [n.g (turn a.g freeze-mart)]
  %+  turn  c
  |=(a/tuna:twig ?@(-.a !! (freeze a)))
::
++  freeze-mart
  |=  {n/mane v/(list beer)}  ^-  {mane tape}
  [n (turn v |=(a/beer ?^(a !! a)))]
::
::
::+|
::
--
::|=  inp/cord
::=<  (steam-marl (rash inp apex:(sail &)))
=<  |=(pax/path (test pax))
=>  |%
    ++  item  (pair mite marl:twig)                     ::  xml node generator
    ++  colm  @ud                                       ::  column
    ++  flow  marl:twig                                 ::  node or generator
    ++  mite                                            ::  context
      $?  $down                                         ::  outer embed
          $rule                                         ::  horizontal ruler
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
          sty/trig-style                                ::  style
      ==                                                ::
    ++  trig-style                                      ::
      $?  $done                                         ::  end of input
          $rule                                         ::  --- horizontal ruler
          $lint                                         ::  + line item
          $lite                                         ::  - line item
          $head                                         ::  # heading
          $bloc                                         ::  > block-quote
          $expr                                         ::  ! interpolation
          $text                                         ::  anything else
      ==                                                ::
    ++  graf                                            ::  input fragment
      $%  {$bold p/(list graf)}                         ::  *bold*
          {$talc p/(list graf)}                         ::  _italics_
          {$quod p/(list graf)}                         ::  "double quote"
          {$code p/tape}                                ::  code literal
          {$text p/tape}                                ::  text symbol
          {$link p/(list graf) q/tape}                  ::  URL
          {$expr p/tuna:twig}                        ::  interpolated hoon
      ==
    --
|%                                                      ::
++  test                                                ::  test text parsing
  |=  pax/path
  ^-  tape
  ::
  ::  src: text file as (list cord)
  ::  txt: source as tape with newlines
  ::  vex: parsing result
  ::
  =/  src  .^(wain %cx pax)
  =.  src  ['---' src]
  =/  txt  (zing (turn src |=(@t (weld (rip 3 +<) `tape`~[`@`10]))))
  =/  vex  (cram [1 1] txt)
  ::
  ::  print result as error or xml text
  ?~  q.vex
    "syntax error: line {(scow %ud p.p.vex)}, column {(scow %ud q.p.vex)}"
  ?:  [freeze=|]  (poxo (freeze p.u.q.vex))
  (poxo ;;(manx q:(slap !>(..zuse) (steam p.u.q.vex))))
::
++  join                                                ::  glue tapes together
  |=  {a/char b/(list tape)}
  ?~  b  ""
  |-  ^-  tape
  ?~  t.b  i.b
  (welp i.b a $(b t.b))
::
++  cram                                                ::  parse unmark
  %+  stag  [%div ~]  ::REVIEW
  |=  {naz/hair los/tape}
  %+  pfix  (hrul:cram-parsers +<)
  |=  nail  ^-  (like marl:twig)
  $:line:(cram-main q.naz +<)
::
++  cram-main                                           ::  parsing loop logic
  =*  parse  cram-parsers
  |=  {top/@u naz/hair los/tape}
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
  =/  col  top
  =|  hac/(list item)
  =/  cur/item  [%down ~]
  =|  lub/(unit (pair hair (list tape)))
  |%
  ::
  ++  $                                                 ::  resolve
    ^-  (like flow)
    ::
    ::  if error position is set, produce error
    ?.  =(~ err)  [+.err ~]
    ::
    ::  all data was consumed
    =-  [naz `[- [naz los]]]
    =>  made
    |-  ^-  flow
    ::
    ::  fold all the way to top
    ?~  hac  fine
    $(..^$ fold)
  ::
  ++  cur-indent
    ?-  p.cur
      $down  2
      $rule  0
      $head  0
      $expr  2
      $list  0
      $lime  2
      $lord  0
      $poem  8
      $code  4
      $bloc  2
    ==
  ++  back                                              ::  column retreat
    |=  luc/@ud
    ^+  +>
    ?:  =(luc col)  +>
    ::
    ::  nex: next backward step that terminates this context
    =/  nex/@ud  cur-indent  ::REVIEW code and poem blocks are handled elsewhere
    ?:  (gth nex (sub col luc))
      ::
      ::  indenting pattern violation
      ~&  indent-pattern-violation+[p.cur nex col luc]
      ..^$(col luc, err `[p.naz luc])
    =.  ..^$  fold
    $(col (sub col nex))
  ::
  ++  fine                                              ::  item to flow
    ^-  flow
    ?:  ?=(?($down $head $expr) p.cur)
      (flop q.cur)
    =-  [[- ~] (flop q.cur)]~
    ?-  p.cur
      $rule  %hr
      $list  %ul
      $lord  %ol
      $lime  %li
      $code  %pre
      $poem  %div ::REVIEW actual container element?
      $bloc  %blockquote
    ==
  ::
  ++  fold  ^+  .                                       ::  complete and pop
    ?~  hac  .
    %=  .
      hac  t.hac
      cur  [p.i.hac (concat-code (weld fine q.i.hac))]
    ==
  ::
  ++  concat-code                                       ::  merge continuous pre
    |=  a/flow
    ?~  a  a
    ?.  ?=({$pre *} -.i.a)  a
    |-
    ?~  t.a  a
    ?.  ?=({$pre $~} -.i.t.a)  a
    ::  add blank line between blocks
    $(t.a t.t.a, c.i.a (welp c.i.t.a ;/("\0a") c.i.a))
  ::
  ++  snap                                              ::  capture raw line
    =|  nap/tape
    |-  ^+  [nap +>]
    ::
    ::  no unterminated lines
    ?~  los  ~&(%unterminated-line [~ +>(err `naz)])
    ?.  =(`@`10 i.los)
      ?:  (gth col q.naz)
        ?.  =(' ' i.los)
          ~&(expected-indent+[col naz los] [~ +>(err `naz)])
        $(los t.los, q.naz +(q.naz))
      ::
      ::  save byte and repeat
      $(los t.los, q.naz +(q.naz), nap [i.los nap])
    ::
    ::  consume newline
    :_  +>(los t.los, naz [+(p.naz) 1])
    ::
    ::  trim trailing spaces
    |-  ^-  tape
    ?:  ?=({$' ' *} nap)
      $(nap t.nap)
    (flop nap)
  ::
  ++  skip                                      ::  discard line
    |-  ^+  +
    ::
    ::  no unterminated lines
    ?~  los  ~&(%unterminated-line +(err `naz))
    ?.  =(`@`10 i.los)
      ::
      ::  eat byte and repeat
      $(los t.los)
    ::
    ::  consume newline
    +(los t.los, naz [+(p.naz) 1])
  ::
  ++  look                                              ::  inspect line
    ^-  (unit trig)
    (wonk (look:parse naz los))
  ::                                                   ::
  ++  made                                              ::  compose block
    ^+  .
    ::
    ::  empty block, no action
    ?~  lub  .
    ::
    ::  if block is preformatted code
    ?:  ?=($code p.cur)
      =-  fold(lub ~, q.cur (weld - q.cur), col (sub col 4))
      %+  turn  q.u.lub
      |=  tape  ^-  mars
      ::
      ::  each line is text data with its newline
      ;/("{+<}\0a")
    ::
    ::  if block is verse
    ?:  ?=($poem p.cur)
      ::
      ::  add break between stanzas
      =.  q.cur  ?~(q.cur q.cur [[[%br ~] ~] q.cur])
      =-  fold(lub ~, q.cur (weld - q.cur), col (sub col 8))
      %+  turn  q.u.lub
      |=  tape  ^-  manx
      ::
      ::  each line is a paragraph
      :-  [%p ~]
      :_  ~
      ;/("{+<}\0a")
    ::
    ::  yex: block recomposed, with newlines
    =/  yex/tape  (join '\0a' (flop q.u.lub))
    ::
    ::  vex: parse of paragraph
    =/  vex/(like marl:twig)
      ::
      ::  either a one-line header or a paragraph
      %.  [p.u.lub yex]
      ?-  p.cur
        $rule  (full ;~(pfix gay hrul)):parse
        $expr  expr:parse
        $head  head:parse
        @      para:parse
      ==
    ::
    ::  if error, propagate correctly
    ?~  q.vex  ~&(%err-prop ..$(err `p.vex))
    ::
    ::  finish tag if it's a header or rule
    =<  ?:(?=(?($head $rule) p.cur) fold ..$)
    ::
    ::  save good result, clear buffer
    ..$(lub ~, q.cur (weld p.u.q.vex q.cur))
  ::
  ++  line  ^+  .                                       ::  body line loop
    ::
    ::  abort after first error
    ?:  !=(~ err)  .
    ::
    ::  pic: profile of this line
    =/  pic  look
    ::
    ::  if line is blank
    ~&  line+[pic -.cur (flop (turn hac head))]
    ?~  pic
      ::
      ::  break section
      line:made:skip
    ::
    ::  line is not blank
    =>  .(pic u.pic)
    ::
    ::  if end of input, complete
    ?:  |(?=($done sty.pic) (lth col.pic top))
      ..$(q.naz col.pic)
    ::
    ::  bal: inspection copy of lub, current section
    =/  bal  lub
    ::
    ::  if within section
    ?~  bal  (new-container pic)
    ::
    ::  detect unspaced new containers
    ?:  ?&  ?=(?($down $lime $bloc) p.cur)
            |(!=(%text sty.pic) (gth col.pic col))
        ==
      (new-container:made pic)
    ::
    ::  first line of container is legal
    ?~  q.u.bal
      =^  nap  ..$  snap
      line(lub bal(q.u [nap q.u.bal]))
    ::
    ::  detect bad block structure
    ?.  ?-    p.cur
        ::
        ::  can't(/directly) contain text
            ?($rule $lord $list)  ~|(bad-leaf-container+p.cur !!)
        ::
        ::  only one line in a header
            $head  |
        ::
        ::  literals need to end with a blank line
            ?($code $poem $expr)  (gte col.pic col)
        ::
        ::  text flows must continue aligned
            ?($down $list $lime $lord $bloc)  =(col.pic col)
        ==
      ~&  bad-block-structure+[p.cur col col.pic]
      ..$(err `[p.naz col.pic])
    ::
    ::  accept line and continue
    =^  nap  ..$  snap
    line(lub bal(q.u [nap q.u.bal]))
  ::
  ++  new-container
    |=  pic/trig
    ::
    ::  if column has retreated, adjust stack
    =.  +>.$  ?.  (lth col.pic col)  +>.$  (back col.pic)
    ::
    ::  dif: columns advanced
    ::  erp: error position
    ::
    =/  dif  (sub col.pic col)
    =/  erp  [p.naz col.pic]
    =.  col  col.pic
    ::
    ::  execute appropriate paragraph form
    =<  line:abet:apex
    |%
    ::
    ++  abet                                            ::  accept line
      ::
      ::  nap: take first line
      ..$(lub `[naz ~])
    ::
    ++  apex  ^+  .                                     ::  by column offset
      ?+  dif  ~&  offset+dif  fail
        $0  apse                                        ::  unindented forms
        $4  (push %code)                                ::  code literal
        $8  (push %poem)                                ::  verse literal
      ==
    ::
    ++  apse  ^+  .                                     ::  by prefix style
      ?-  sty.pic
        $done  !!                                       ::  blank
        $rule  (push %rule)                             ::  horizontal ruler
        $head  (push %head)                             ::  heading
        $bloc  (entr %bloc)                             ::  blockquote line
        $expr  (entr %expr)                             ::  hoon expression
        $lite  (lent %list)                             ::  unnumbered list
        $lint  (lent %lord)                             ::  numbered list
        $text  text                                     ::  anything else
      ==
    ::
    ++  fail  .(err `erp)                               ::  set error position
    ++  push  |=(mite %_(+> hac [cur hac], cur [+< ~])) ::  push context
    ++  entr                                            ::  enter container
      |=  typ/mite
      ^+  +>
      ::
      ::  indent by 2
      =.  col  (add 2 col)
      ::
      ::  "parse" marker
      =.  los  (slag (sub col q.naz) los)
      =.  q.naz  col
      ::
      (push typ)
    ::
    ++  lent                                            ::  list entry
      |=  ord/?($lord $list)
      ^+  +>
      ::  can't switch list types
      ?:  =(?-(ord $list %lord, $lord %list) p.cur)
        fail
      ::
      ::  push list item
      =<  (entr %lime)
      ::
      ::  push list context, unless we're in list
      ?:(=(ord p.cur) ..push (push ord))
    ::
    ++  text                                            ::  plain text
      ^+  .
      ::
      ::  only in lists, fold
      ?.  ?=(?($list $lord) p.cur)  .
      .(^$ fold)
    --
  --
::
++  cram-parsers                                        ::  individual parsers
  |%
  ++  look
    %+  cook  |=(a/(unit trig) a)
    ;~  pfix  (star ace)
      %+  here
        |=({a/pint b/?($~ trig-style)} ?~(b ~ `[q.p.a b]))
      ;~  pose
        (full (easy %done))                             ::  end of input
        (cold ~ (just `@`10))                           ::  blank line
        (cold %rule ;~(plug hep hep hep))               ::  --- horizontal ruler
        (cold %head ;~(plug (star hax) ace))            ::  # heading
        (cold %lite ;~(plug hep ace))                   ::  - line item
        (cold %lint ;~(plug lus ace))                   ::  + line item
        (cold %bloc ;~(plug gar ace))                   ::  > block-quote
        (cold %expr ;~(plug zap ace))                   ::  ! interpolation
        (easy %text)                                    ::  anything else
      ==
    ==
  ::
  ++  cash                                              ::  escaped fence
    |*  tem/rule
    %-  echo
    %-  star
    ;~  pose
      whit
      ;~(plug bas tem)
      ;~(less tem prn)
    ==
  ::
  ++  cool                                              ::  reparse
    |*  $:  ::  fex: primary parser
            ::  sab: secondary parser
            ::
            fex/rule
            sab/rule
        ==
    |=  {naz/hair los/tape}
    ^+  *sab
    ::
    ::  vex: fenced span
    =/  vex/(like tape)  (fex naz los)
    ?~  q.vex  vex
    ::
    ::  hav: reparse full fenced text
    =/  hav  ((full sab) [naz p.u.q.vex])
    ::
    ::  reparsed error position is always at start
    ?~  q.hav  [naz ~]
    ::
    ::  the complete span with the main product
    :-  p.vex
    `[p.u.q.hav q.u.q.vex]
  ::
  ++  echo                                              ::  hoon literal
    |*  sab/rule
    |=  {naz/hair los/tape}
    ^-  (like tape)
    ::
    ::  vex: result of parsing wide twig
    =/  vex  (sab naz los)
    ::
    ::  use result of expression parser
    ?~  q.vex  vex
    =-  [p.vex `[- q.u.q.vex]]
    ::
    ::  but replace payload with bytes consumed
    |-  ^-  tape
    ?:  =(q.q.u.q.vex los)  ~
    ?~  los  ~
    [i.los $(los +.los)]
  ::
  ++  word                                              ::  flow parser
    %+  knee  *(list graf)  |.  ~+
    %+  cook  |=(a/?(graf (list graf)) ?+(a a {@ *} [a]~))
    ;~  pose
    ::
    ::  ordinary word
    ::
      (stag %text ;~(plug ;~(pose low hig) (star ;~(pose nud low hig hep))))
    ::
    ::  naked \escape
    ::
      (stag %text ;~(pfix bas (cook trip ;~(less ace prn))))
    ::
    ::  *bold literal*
    ::
      (stag %bold (ifix [tar tar] (cool (cash tar) work)))
    ::
    ::  _italic literal_
    ::
      (stag %talc (ifix [cab cab] (cool (cash cab) work)))
    ::
    ::  "quoted text"
    ::
      (stag %quod (ifix [doq doq] (cool (cash doq) work)))
    ::
    ::  `classic markdown quote`
    ::
      (stag %code (ifix [tec tec] (cash tec)))
    ::
    ::  ++arm
    ::
      (stag %code ;~(plug lus lus low (star ;~(pose nud low hep))))
    ::
    ::  [arbitrary *content*](url)
    ::
      %+  stag  %link
      ;~  (glue gay)
        (ifix [sel ser] (cool (cash ser) work))
        (ifix [pel per] (cash per))
      ==
    ::
    ::  #twig
    ::
      ;~  plug
        (stag %text (cold " " whit))
        (stag %code ;~(pfix hax (echo wide:vast)))
        (easy ~)
      ==
    ::
    ::  direct hoon constant
    ::
      ;~  plug
        (stag %text (cold " " whit))
      ::
        %+  stag  %code
        %-  echo
        ;~  pose
          bisk:so
          tash:so
          ;~(pfix dot perd:so)
          ;~(pfix sig ;~(pose twid:so (easy [%$ %n 0])))
          ;~(pfix cen ;~(pose sym buc pam bar qut nuck:so))
        ==
      ::
        (easy ~)
      ==
    ::
    ::  whitespace
    ::
      (stag %text (cold " " whit))
    ::
    ::  {interpolated} sail
    ::
      (stag %expr inline-embed:(sail:vast |))
    ::
    ::  just a byte
    ::
      (stag %text (cook trip ;~(less ace prn)))
    ==
  ::
  ++  work  (cook zing (star word))                     ::  indefinite flow
  ::
  ++  down                                              ::  parse inline flow
    %+  knee  *flow  |.  ~+
    =-  (cook - work)
    ::
    ::  collect raw flow into xml tags
    |=  gaf/(list graf)
    ^-  flow
    =<  main
    |%
    ++  main
      ^-  flow
      ?~  gaf  ~
      ?.  ?=($text -.i.gaf)
        (weld (item i.gaf) $(gaf t.gaf))
      ::
      ::  fip: accumulate text blocks
      =/  fip/(list tape)  [p.i.gaf]~
      |-  ^-  flow
      ?~  t.gaf  [;/((zing (flop fip))) ~]
      ?.  ?=($text -.i.t.gaf)
        [;/((zing (flop fip))) ^$(gaf t.gaf)]
      $(gaf t.gaf, fip :_(fip p.i.t.gaf))
    ::
    ++  item
      |=  nex/graf
      ^-  flow  ::CHECK can be tuna:twig?
      ?-  -.nex
        $text  !!  :: handled separately
        $expr  [p.nex]~
        $bold  [[%b ~] ^$(gaf p.nex)]~
        $talc  [[%i ~] ^$(gaf p.nex)]~
        $code  [[%code ~] ;/(p.nex) ~]~
        $quod  ::
               ::  smart quotes
               %=    ^$
                   gaf
                 :-  [%text (tufa ~-~201c. ~)]
                 %+  weld  p.nex
                 `(list graf)`[%text (tufa ~-~201d. ~)]~
               ==
        $link  [[%a [%href q.nex] ~] ^$(gaf p.nex)]~
      ==
    --
  ::
  ++  hrul                                              ::  empty besides fence
    (cold ~ ;~(plug hep hep hep (star hep)))
  ::
  ++  para                                              ::  paragraph
    %+  cook
      |=(flow [[%p ~] +<]~)
    ;~(pfix gay down)  ::REVIEW does this mean comments work?
  ::
  ++  expr                                              ::  expression
    ;~(pfix gay a-marl:(sail:vast &))
  ::
  ++  whit                                              ::  whitespace
    (cold ' ' (plus ;~(pose (just ' ') line)))
  ::
  ++  line  (just '\0a')  ::TODO lookahead
  ++  head                                              ::  parse heading
    %+  cook
      |=  a/manx:twig  ^-  marl:twig
      =.  a.g.a  :_(a.g.a [%id (sanitize-to-id c.a)])
      [a]~
    ::
    ;~  plug
      ::
      :: # -> 1 -> %h1, ### -> 3 -> %h3, etc
      :(cook |=(a/@u /(crip "h{<a>}")) lent (stun [1 6] hax))
    ::
      ;~(pfix whit down)
    ==
  ::
  ++  sanitize-to-id                                    ::  # text into elem id
    |=  a/(list tuna:twig)  ^-  tape
    =;  raw/tape
      %+  turn  raw
      |=  @tD
      ^-  @tD
      ?:  ?|  &((gte +< 'a') (lte +< 'z'))
              &((gte +< '0') (lte +< '9'))
          ==
        +<
      ?:  &((gte +< 'A') (lte +< 'Z'))
        (add 32 +<)
      '-'
    ::
    ::  collect all text in header flow
    |-  ^-  tape
    ?~  a  ~
    %+  weld
      ^-  tape
      ?-    i.a
        {{$$ {$$ *} $~} $~}                             ::  text node contents
          (murn v.i.a.g.i.a |=(a/beer ?^(a ~ (some a))))
        {^ *}  $(a c.i.a)                               ::  concatenate children
        {@ *}  ~                                        ::  ignore interpolation
      ==
    $(a t.a)
  --
++  lines-with-md
  %-  star
  ;~  pfix  gay
    ;~  pose
      :(cook poxo freeze cram)
      ;~(sfix ;~(less hrul:cram-parsers (star prn)) (just '\0a'))
    ==
  ==
--
