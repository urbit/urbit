=>
|%
+$  range  (pair pint tape)
+$  match  (map @u range)
--
=<
|%
::
::  This outer core represents the main interface for executing regular
::  expressions.  Most of its arms produce a +$range (a tape annotated
::  with location data) or a +$match (a map from capture numbers to
::  +$range).  These functions should serve your basic purposes, but for
::  more complex operations you may use the inner core ++on (see below).
::
++  valid   ::  Determine if a regular expression is valid
  |=  [regex=tape]  ^-  ?
  !=(~ (purse:on regex))
::
++  run   ::  Find the first match of a pattern in a subject
  |=  [regex=tape text=tape]  ^-  (unit match)
  =+  pan=(parse:on regex)
  =+  ini=(init:on text)
  |-
  =+  sat=(long:on (pan ini))
  ?~  sat
    ?~  q.tub.ini  ~
    $(ini (skip:on ini))
  `gru.u.sat
::
++  ran   ::  Find the first match; crash if there is none
  |=  [regex=tape text=tape]  ^-  match
  =+  mat=(run regex text)
  ?~(mat !! u.mat)
::
++  rut   ::  Find the text of the first match
  |=  [regex=tape text=tape]  ^-  (unit range)
  =+  mat=(run regex text)
  ?~(mat ~ `(~(got by u.mat) 0))
::
++  rat   ::  Find the text of the first match; crash if there is none
  |=  [regex=tape text=tape]  ^-  range
  =+  mat=(ran regex text)
  (~(got by mat) 0)
::
++  all   ::  Find all non-empty, non-overlapping matches
  |=  [regex=tape text=tape]  ^-  (list match)
  =+  pan=(parse:on regex)
  =+  ini=(init:on text)
  =+  lis=`(list match)`~
  |-
  =+  sat=(long:on (pan ini))
  ?~  sat
    ?~  q.tub.ini  (flop lis)
    $(ini (skip:on ini))
  ?:  =(p.tub.ini p.tub.u.sat)
    ?~  q.tub.ini  (flop lis)
    $(ini (skip:on ini))
  $(ini u.sat(gru ~), lis [gru.u.sat lis])
::
++  alt   ::  Find the text of all non-empty, non-overlapping matches
  |=  [regex=tape text=tape]  ^-  (list range)
  %+  turn  (all regex text)
  |=  [mat=match]  (~(got by mat) 0)
::
++  is    ::  If regex validates text, produces nil; otherwise, produces
          ::  the location of the first non-matching character
  |=  [regex=tape text=tape]  ^-  (unit hair)
  =+  pan=(parse:on regex)
  =+  ini=(init:on text)
  |-
  ?~  q.tub.ini  ~
  =+  sat=(long:on (pan ini))
  ?~  sat  `p.tub.ini
  $(ini u.sat(gru ~))
::
++  as    ::  Produce nil, crashing if regex does not validate text
  |=  [regex=tape text=tape]  ^-  ~
  ?~((is regex text) ~ !!)
::
++  has   ::  Determine whether a pattern has a match
  |=  [regex=tape text=tape]  ^-  ?
  ?~  (run regex text)
    %.n
  %.y
::
++  from  ::  Find the next match after a certain point
  |=  [regex=tape where=hair text=tape]  ^-  (unit match)
  =+  pan=(parse:on regex)
  =+  ini=(init:on text)
  |-
  ?:  (past:of p.tub.ini where)
    ?~  q.tub.ini  ~
    $(ini (skip:on ini))
  |-
  =+  sat=(long:on (pan ini))
  ?~  sat
    ?~  q.tub.ini  ~
    $(ini (skip:on ini))
  `gru.u.sat
::
++  fort  ::  Find the text of the next match after a certain point
  |=  [regex=tape where=hair text=tape]  ^-  (unit range)
  =+  mat=(from regex where text)
  ?~(mat ~ `(~(got by u.mat) 0))
::
++  sub   ::  Replace the first match with a string
  |=  [regex=tape repl=tape text=tape]  ^-  tape
  (subf regex |=(* repl) text)
::
++  gsub  ::  Replace all matches with a string
  |=  [regex=tape repl=tape text=tape]  ^-  tape
  (gsubf regex |=(* repl) text)
::
++  subf  ::  Replace the first match by a function
  |=  [regex=tape repl=$-(tape tape) text=tape]  ^-  tape
  =+  mat=(rut regex text)
  ?~  mat  text
  %+  weld  q:(lant:of p.p.u.mat [[1 1] text])
  %+  weld  (repl q.u.mat)
  q:(whet:of q.p.u.mat [[1 1] text])
::
++  gsubf   ::  Replace all matches by a function
  |=  [regex=tape repl=$-(tape tape) text=tape]  ^-  tape
  =+  lis=(alt regex text)
  =+  tub=`nail`[[1 1] text]
  |-  ^-  tape
  ?~  lis  q.tub
  %+  weld  q:(lant:of p.p.i.lis tub)
  %+  weld  (repl q.i.lis)
  $(tub (whet:of q.p.i.lis tub), lis t.lis)
--
::
|%  ::  Internal operations
::
::  Patterns are construed as a function that produces a promise-based
::  stream of successive matches.  This function accepts a +$state which
::  is somewhat more complex than a tape; the ++on core is used for
::  creating and operating on these states and patterns.
::
::  Applying a pattern to a match state produces a +$promise, which is a
::  trap producing a +$product.  +$product is either nil or a tuple,
::  containing the state of the next match that was detected, alongside
::  another promise that will produce the next +$product.  The first
::  match state is not necessarily the correct match; use ++long:on to
::  extract the leftmost-longest match from a promise, according to
::  Posix rules.
::
::  ++parse:of is the main entry point for creating patterns, but it
::  needs an appropriate sample constructed by ++start:of; this logic
::  is handled by ++parse:on.
::
::  When using patterns directly, remember that they only match text
::  beginning at the given state.  Use ++init:on and ++skip:on to test
::  matches downstream in the subject text.
::
+$  state
  $:  tub=nail        ::  Remaining text to be matched
      las=(unit @t)   ::  The previous character in the subject text
      gru=match       ::  Capture groups that have been matched so far
  ==
+$  product  (unit (pair state promise))
+$  promise  _^&(|.(`(unit (pair state _^&(.)))``[*state .]))
+$  pattern  _^|(|=(state *promise))
::
++  on  ::  Basic operations on patterns and states
  |%
  ++  parse   ::  Parse regex string
    |=  [regex=tape]  ^-  pattern
    parse:(start:of regex)
  ::
  ++  purse   ::  Unitized parse
    |=  [regex=tape]  ^-  (unit pattern)
    purse:(start:of regex)
  ::
  ++  init  ::  Create an initial state for subject text
    |=  [sut=tape]  ^-  state
    =|  sat=state
    %=  sat
      tub  [[1 1] sut]
      gru  ~
    ==
  ::
  ++  skip  ::  Advance a state's subject text by one character
    |=  [sat=state]  ^-  state
    ?~  q.tub.sat  sat
    sat(tub (skip:of tub.sat), las `i.q.tub.sat)
  ::
  ++  long  ::  Get the leftmost-longest match from a promise
    |=  [per=promise]  ^-  (unit state)
    =+  pro=(per)
    ?~  pro  ~
    =+  sat=p.u.pro
    =.  per  q.u.pro
    |-  ^-  (unit state)
    =+  pro=`product`(per)
    ?~  pro  `sat
    %=  $
      per  q.u.pro
      sat  =|  n=@ud
           =/  lim=@ud
             .+  %+  max
             (roll ~(tap in ~(key by gru.sat)) max)
             (roll ~(tap in ~(key by gru.p.u.pro)) max)
           |-
           ?:  =(n lim)  sat
           =+  a=(~(get by gru.sat) n)
           ?~  a
             ?:  (~(has by gru.p.u.pro) n)
               p.u.pro
             $(n +(n))
           =+  b=(~(get by gru.p.u.pro) n)
           ?~  b  sat
           ?:  (past:of p.p.u.a p.p.u.b)  sat
           ?:  (past:of p.p.u.b p.p.u.a)  p.u.pro
           ?:  (past:of q.p.u.a q.p.u.b)  p.u.pro
           ?:  (past:of q.p.u.b q.p.u.a)  sat
           $(n +(n))
    ==
  --
::
++  of  ::  Pattern parsing operations
  ::
  ::  Parsing is done by constructing an initial sample with ++start
  ::  and evaluating the ++parse arm.  ++parse begins at the top level
  ::  with ++top, making reference to the middle level ++mid and the
  ::  bottom level ++bot, which recurs back to ++top in the case of
  ::  capture groups.
  ::
  ::  The least tightly-binding regex operator is |, but there is also
  ::  the case-sensitivity operator (?i), which is effective beyond
  ::  subsequent | operators but not outside an enclosing group of ().
  ::  Because (?i) can occur in the middle of an alternated pattern,
  ::  like "a|b(?i)c|d", we parse the top level into a list of lists,
  ::  then join the ends of these lists together by catenation into a
  ::  flat list, which is then joined by alternation.
  ::
  ::  "a|b(?i)c|d"  ->  {(a or b), ([Cc] or [Dd])}
  ::                ->  (a or b[Cc] or [Dd]}
  ::
  ::  The middle level of parsing handles catenated sequences; for
  ::  example "a(bcd)*e" is a sequence of three bottom-level regexes:
  ::  a, (bcd)*, and e.  The bottom level of parsing handles everything
  ::  else (characters, anchors, capture groups, etc) with optional
  ::  repetition.  Capture groups recur to the top level, containing
  ::  the effects of any case-sensitivity operators.
  ::
  |_  $:  reg=tape  ::  The pattern being parsed
          cas=?     ::  Are we parsing a case-sensitive pattern?
          arp=?     ::  Will a `)` in the pattern match literally?
      ==
  ++  start
    |=  [regex=tape]  ^-  _..start
    ..start(reg regex)
  ++  parse  ^-  pattern
    (capt 0 (scan reg top))
  ++  purse  ^-  (unit pattern)
    =+  pan=(rust reg top)
    ?~  pan  ~
    `(capt 0 u.pan)
  ::
  +$  posix
    $:  neg=?
      $?  %ascii  %alpha  %alnum  %blank
          %cntrl  %digit  %graph  %lower
          %print  %punct  %space  %upper
          %word   %xdigit
      ==
    ==
  ::
  ++  skip  ::  Advance a nail by one character
    |=  [tub=nail]  ^-  nail
    ?~  q.tub  tub
    [(lust i.q.tub p.tub) t.q.tub]
  ::
  ++  none  ::  Promise that produces nothing
    ^&  |.  `(unit (pair state _^&(.)))`~
  ::
  ++  once  ::  Empty pattern (matches "" once)
    ^-  pattern
    |=  [sat=state]  `promise`|.(`[sat none])
  ::
  ++  cont  ::  Combine two patterns sequentially
    |=  [pan=pattern pun=pattern]  ^-  pattern
    |=  [sat=state]  ^-  promise
    =+  per=(pan sat)
    |.  ^-  product
    =+  for=(per)
    ?~  for  ~
    =+  mid=(pun p.u.for)
    |-  ^-  product
    =+  aft=(mid)
    ?~  aft  ^$(per q.u.for)
    `[p.u.aft ..$(mid q.u.aft)]
  ::
  ++  fork  ::  Combine two patterns alternatively
    |=  [pan=pattern pun=pattern]  ^-  pattern
    |=  [sat=state]  ^-  promise
    =+  per=(pan sat)
    |.  ^-  product
    =+  pro=(per)
    ?~  pro  ((pun sat))
    `[p.u.pro ..$(per q.u.pro)]
  ::
  ++  some  ::  Repeat a pattern
    |=  [pan=pattern lo=@u hi=(unit @u)]  ^-  pattern
    =|  n=@u
    |-  ^-  pattern
    ?:  =(lo n)
      ?~(hi (many pan) (much pan (sub u.hi lo)))
    (cont pan $(n +(n)))
  ::
  ++  much  ::  Repeat a pattern, bounded
    |=  [pan=pattern hi=@u]  ^-  pattern
    =|  n=@u
    |-  ^-  pattern
    ?:  =(hi n)  once
    (cont (fork pan once) $(n +(n)))
  ::
  ++  many  ::  Repeat a pattern, unbounded
    |=  [pan=pattern]  ^-  pattern
    |=  [sat=state]  ^-  promise
    =+  per=(pan sat)
    |.  ^-  product
    =+  for=(per)
    ?~  for  `[sat none]
    ?:  =(p.tub.sat p.tub.p.u.for)
      `[p.u.for ..$(per q.u.for)]
    =+  mid=^$(sat p.u.for)
    |-  ^-  product
    =+  aft=(mid)
    ?~  aft  `[p.u.for ..^$(per q.u.for)]
    `[p.u.aft ..$(mid q.u.aft)]
  ++  text  ::  Create a pattern that matches literal text
    |=  [tet=tape]  ^-  pattern
    ?:  cas
      |=  [sat=state]  ^-  promise
      |.  ^-  product
      ?~  tet  `[sat none]
      ?~  q.tub.sat  ~
      ?.  =(i.tet i.q.tub.sat)  ~
      $(tet t.tet, sat (skip:on sat))
    =.  tet  (cass tet)
    |=  [sat=state]  ^-  promise
    |.  ^-  product
    ?~  tet  `[sat none]
    ?~  q.tub.sat  ~
    ?.  .=  i.tet
        ?:  &((gte i.q.tub.sat 'A') (lte i.q.tub.sat 'Z'))
          (add i.q.tub.sat 32)
        i.q.tub.sat
      ~
    $(tet t.tet, sat (skip:on sat))
  ++  capt  ::  Capture a pattern's matched text
    |=  [n=@u pan=pattern]  ^-  pattern
    |=  [sat=state]  ^-  promise
    =+  per=(pan sat)
    |.
    =+  pro=(per)
    ?~  pro  ~
    :+  ~
      %=  p.u.pro
          gru
        %-  ~(put by gru.p.u.pro)
        :+  n  [p.tub.sat p.tub.p.u.pro]
        q:(lant p.tub.p.u.pro tub.sat)
      ==
    ..$(per q.u.pro)
  ++  back  ::  Create a backreference pattern
    |=  [n=@u]  ^-  pattern
    |=  [sat=state]
    =+  mat=(~(get by gru.sat) n)
    ?~  mat  none
    ((text q.u.mat) sat)
  ::
  ::
  ::  Helper functions
  ::
  ++  rant
    ::  Determine if an atom is in a certain range
    |=  [n=@ lo=@ hi=@]
    &((gte n lo) (lte n hi))
  ++  past
    ::  Determine whether hair `b` comes after hair `a`
    |=  [a=hair b=hair]  ^-  ?
    ?|  (lth p.a p.b)
        &(=(p.a p.b) (lth q.a q.b))
    ==
  ++  prev
    ::  Get the last character before a certain point
    |=  [har=hair tub=nail]  ^-  (unit @t)
    ?~  q.tub  ~
    ?.  (past p.tub har)  ~
    =+  c=i.q.tub
    %.  tub
    |=  [tub=nail]
    ?.  (past p.tub har)  `c
    ?~  q.tub  `c
    $(c i.q.tub, tub (skip tub))
  ++  lant  ::  Get the text of a nail up to a certain hair
    |=  [wer=hair tub=nail]  ^-  nail
    :-  p.tub
    =+  beh=""
    |-
    ?~  q.tub  (flop beh)
    ?.  (past p.tub wer)  (flop beh)
    $(tub (skip tub), beh [i.q.tub beh])
  ++  whet  ::  Get the text of a nail after a certain heir
    |=  [wer=hair tub=nail]  ^-  nail
    ?.  (past p.tub wer)  tub
    $(tub (skip tub))
  ++  bond  ::  Determine if a state is at a word boundary
    |=  [sat=state]  ^-  ?
    ?~  las.sat
      ?~  q.tub.sat  %.n
      (memb i.q.tub.sat [| %word])
    ?~  q.tub.sat  (memb u.las.sat [| %word])
    ?!  .=
    (memb i.q.tub.sat [| %word])
    (memb u.las.sat [| %word])
  ++  left  ::  Determine if a state is at the beginning of a word
    |=  [sat=state]  ^-  ?
    ?~  q.tub.sat  %.n
    ?&  (memb i.q.tub.sat [| %word])
        |(?=(~ las.sat) (memb u.las.sat [& %word]))
    ==
  ++  rite  ::  Determine if a state is at the end of a word
    |=  [sat=state]  ^-  ?
    ?~  las.sat  %.n
    ?&  (memb u.las.sat [| %word])
        |(?=(~ q.tub.sat) (memb i.q.tub.sat [& %word]))
    ==
  ++  what  ::  Determine a group's capture number by hair position
    |=  [wer=hair]  ^-  @u
    =|  n=@u
    =+  tub=`nail`[[1 1] reg]
    |^
      ?.  (past p.tub wer)  n
      ?~  q.tub  n
      =+  vex=(non tub)
      ?~  q.vex
        $(tub (skip tub), n ?:(=('(' i.q.tub) +(n) n))
      $(tub q.u.q.vex)
    ++  non  ::  Skip over a non-capturing (?...) expression
      %+  knee  *~
      |.  ~+
      %+  cold  ~
      ;~(pose (ifix [(jest '(?') par] nom) cla)
    ++  nom  ::  Skip over the contents of a non-capturing expression
      %+  knee  *~
      |.  ~+
      %+  cold  ~
      ;~  pose
        ;~  plug
          ;~  pose
            (ifix [pal par] nom)
            ;~(plug bas next)
            ;~(less (mask "\\()[") next)
            cla
          ==
        ::
          nom
        ==
      ::
        (easy ~)
      ==
    --
  ++  memb  ::  Determine if a character is a member of a POSIX class
    |=  [=char =posix]  ^-  ?
    .=  !neg.posix
    ?-  +.posix
      %ascii   (rant char 0 127)
      %alnum   ?|  (rant char 'A' 'Z')
                   (rant char 'a' 'z')
                   (rant char '0' '9')
               ==
      %alpha   ?|  (rant char 'A' 'Z')
                   (rant char 'a' 'z')
               ==
      %blank   |(=(char ' ') =(char '\09'))
      %cntrl   |((rant char '\00' '\1f') =(char '\7f'))
      %digit   (rant char '0' '9')
      %graph   (rant char '!' '~')
      %lower   (rant char 'a' 'z')
      %print   (rant char ' ' '~')
      %punct   ?|  (rant char '!' '/')
                   (rant char ':' '@')
                   (rant char '[' '`')
                   (rant char '{' '~')
               ==
      %space   |(=(char ' ') (rant char '\09' '\0d'))
      %upper   (rant char 'A' 'Z')
      %word    ?|  (rant char 'A' 'Z')
                   (rant char 'a' 'z')
                   (rant char '0' '9')
                   =('_' char)
               ==
      %xdigit  ?|  (rant char '0' '9')
                   (rant char 'A' 'F')
                   (rant char 'a' 'f')
               ==
    ==
  ++  top   ::  Top-level parsing:  alternation and regex mode
    %+  cook
      |=  [lis=(lest (lest pattern))]
      =;  end=(lest pattern)
        |-  ^-  pattern
        ?~  t.end  i.end
        (fork i.end $(end t.end))
      |-  ^-  (lest pattern)
      ?~  t.lis  i.lis
      =/  sal=(lest pattern)  ?~(t.t.lis i.t.lis $(lis t.lis))
      |-  ^-  (lest pattern)
      ?~  t.i.lis  [(cont i.i.lis i.sal) t.sal]
      [i.i.lis $(i.lis t.i.lis)]
    |-
    %+  knee  *(lest (lest pattern))
    |.  ~+
    ;~  plug
      (most bar mid)
    ::
      ;~  pose
        ;~(pfix (jest '(?i)') %=(^$ cas %.n))
        ;~(pfix (jest '(?-i)') %=(^$ cas %.y))
        (easy ~)
      ==
    ==
  ++  mid   ::  Mid-level parsing:  catenation
    %+  knee  *pattern
    |.  ~+
    ;~  pose
      (cook cont ;~(plug ;~(pose str bot) mid))
      nil
    ==
  ++  bot   ::  Bottom-level parsing element
    %+  cook
      |=  [pan=pattern ran=(unit [@u (unit @u)])]
      ?~  ran  pan
      (some pan u.ran)
    ;~  plug
      ;~  pose
        (cook text ;~(plug lit (easy ~)))  :: literal or escaped character
        cap                                :: capture group
        bak                                :: backtrack
        cla                                :: character class
        ank                                :: control chars
        luk                                :: lookahead
        (cold any dot)                     :: match forward
        (cook text ;~(pfix bas ;~(plug next (easy ~))))  :: non-special escape
      ==
    ::
      (punt rep)
    ==
  ++  bak
    (cook back ;~(pfix bas dit))
  ++  any  ^-  pattern
    |=  [sat=state]  ^-  promise
    |.  ^-  product
    ?~  q.tub.sat  ~
    `[(skip:on sat) none]
  ++  ank
    ;~  pose
      %-  cold  :_  ket
      ^-  pattern
      |=  [sat=state]  ^-  promise
      |.  ^-  product
      ?~(las.sat `[sat none] ~)
    ::
      %-  cold  :_  buc
      ^-  pattern
      |=  [sat=state]  ^-  promise
      |.  ^-  product
      ?~(q.tub.sat `[sat none] ~)
    ::
      %-  cold  :_  (jest '\\b')
      ^-  pattern
      |=  [sat=state]  ^-  promise
      |.  ^-  product
      ?:((bond sat) `[sat none] ~)
    ::
      %-  cold  :_  (jest '\\B')
      ^-  pattern
      |=  [sat=state]  ^-  promise
      |.  ^-  product
      ?:((bond sat) ~ `[sat none])
    ::
      %-  cold  :_  (jest '\\<')
      ^-  pattern
      |=  [sat=state]  ^-  promise
      |.  ^-  product
      ?:((left sat) `[sat none] ~)
    ::
      %-  cold  :_  (jest '\\>')
      ^-  pattern
      |=  [sat=state]  ^-  promise
      |.  ^-  product
      ?:((rite sat) `[sat none] ~)
    ==
  ++  nil  ::  Parse empty pattern
    (easy once)
  ++  str  ::  Parse a long string of literal text
    (cook text (plus ;~(less ;~(sfix lit rep) lit)))
  ++  lit  ;~(pose cha esc)
  ++  cha  ::  Literal character
    ;~  less
      ?:  arp
        (mask "^$.|?+*([\{\\")
      (mask "^$.|?+*()[\{\\")
    ::
      next
    ==
  ++  esc   ::  Escape code
    ;~  pfix  bas
      ;~  pose
        %-  sear  :_  next
        %~  get  by
        %-  malt
        :~  ['a' '\07']    :: Bell
            ['t' '\09']    :: Horizontal tab
            ['n' '\0a']    :: Newline
            ['v' '\0b']    :: Vertical tab
            ['f' '\0c']    :: Form feed
            ['r' '\0d']    :: Carriage return
            ['e' '\1b']    :: Escape
        ==
      ::
        ;~  pfix  (jest 'x')
          %-  cook  :_  ;~(plug hit hit)
          |=  [a=@u b=@u]  ^-  @t
          (add (mul 16 a) b)
        ==
      ::
        ;~  pfix  (jest '0')
          %-  cook  :_  ;~(plug cit cit cit)
          |=  [a=@u b=@u c=@u]  ^-  @t
          (add (add (mul 64 a) (mul 8 b)) c)
        ==
      ::
        ;~  pfix  (jest 'c')
          %-  cook  :_  (shim '\00' '\7f')
          |=  [c=@t]
          =.  c  ?:((rant c 'a' 'z') (sub c 32) c)
          ?:  =(0 (dis c '\40'))
            (add c '\40')
          (sub c '\40')
        ==
      ==
    ==
  ++  rep   ::  Repetition quantifier
    ;~  pose
      (cold [0 `1] wut)
      (cold [0 ~] tar)
      (cold [1 ~] lus)
    ::
      %+  ifix  [kel ker]
      ;~  pose
        ;~(plug ;~(pose dem (easy 0)) ;~(pfix com (punt dem)))
        (cook |=(n=@u [n `n]) dem)
      ==
    ==
  ++  cap   ::  Capture group
    %+  cook
      |=  [wer=hair pan=pattern]
      (capt (what wer) pan)
    %+  ifix  [pal par]
    ;~  plug
      (here |=([a=pint *] p.a) (easy ~))
      top(arp %.n)
    ==
  ++  cla   ::  Character class
    |^
      %+  sear
        |=  [neg=? lis=(list elem)]  ^-  (unit pattern)
        =*  ok=(unit pattern)  `(make neg lis)
        ?:  |-
            ?~  lis  %.y
            ?@  i.lis  $(lis t.lis)
            ?^  +.i.lis  $(lis t.lis)
            ?:  (lte -.i.lis +.i.lis)  $(lis t.lis)
            %.n
          `(make neg lis)
        ~
      ;~  pose
        (cook |=(pos=[~ posix] [| ~[pos]]) ;~(less sel pec))
        (ifix [sel ser] ;~(plug (fuss '^' '') hed))
      ==
    ++  hed
      ;~  pose
        ;~  plug
          ;~  less
            ;~(pfix ser hep pec)
            ;~(pfix ser hep cil hep ;~(pose pec cil))
            ;~(pose ;~(plug ser ;~(pfix hep cil)) ser)
          ==
        ::
          tel
        ==
      ::
        tel
      ==
    ++  tel
      %+  knee  *(list elem)
      |.  ~+
      ;~  pose
        ;~  plug
          ;~  pose
            ;~(less ;~(pfix pec hep ;~(pose pec cil)) pec)
          ::
            ;~  less
              pec
              puc
              ;~(pfix cil hep pec)
              ;~(pfix cil hep cil hep ;~(pose pec cil))
              ;~(pose ;~(plug cil ;~(pfix hep cil)) cil)
            ==
          ==
        ::
          tel
        ==
      ::
        (easy ~)
      ==
    ++  cil   ::  Literal character for character classes
      ;~(pose esc sec ;~(less bas ser next))
    ++  sec   ::  Special escape character in this context
      ;~(pose (cold '\08' (jest '\\b')) ;~(pfix bas next))
    +$  elem  ?(@t [@t @t] [~ posix])
    ++  make
      |=  [neg=? lis=(list elem)]  ^-  pattern
      |=  [sat=state]  ^-  promise
      |.  ^-  product
      ?~  q.tub.sat  ~
      ?:  .=  neg
          |-
          ?~  lis  %.n
          ?:  (pass i.q.tub.sat i.lis)  %.y
          $(lis t.lis)
        ~
      `[sat(tub (skip tub.sat)) none]
    ++  pass
      ?:  cas
        |=  [=char =elem]  ^-  ?
        ?-  elem
          @          =(elem char)
          [@ @]      &((lte -.elem char) (lte char +.elem))
          [~ posix]  (memb char +.elem)
        ==
      |=  [=char =elem]  ^-  ?
      =+  p=pass(cas %.y)
      ?|  (p char elem)
          &((rant char 'A' 'Z') (p (add char 32) elem))
          &((rant char 'a' 'z') (p (sub char 32) elem))
      ==
    ++  pec   ::  Perl- or POSIX-style class
      ;~  pose
        %-  sear  :_  ;~(pfix bas next)
        %~  get  by  ^-  (map @t [~ posix])
        %-  malt  ^-  (list [@t ~ posix])
        :~  ['d' `|^%digit]  ['D' `&^%digit]
            ['w' `|^%word]   ['W' `&^%word]
            ['s' `|^%space]  ['S' `&^%space]
            ['u' `|^%upper]  ['U' `&^%upper]
            ['l' `|^%lower]  ['L' `&^%lower]
        ==
      ::
        %+  ifix  [(jest '[:') (jest ':]')]
        ;~  plug  (easy ~)  (fuss '^' '')
          %-  perk
          :~  %ascii  %alpha  %alnum  %blank
              %cntrl  %digit  %graph  %lower
              %print  %punct  %space  %upper
              %word   %xdigit
          ==
        ==
      ==
    ++  puc   ::  Invalid POSIX-style class
      %+  ifix  [(jest '[:') (jest ':]')]
      ;~(plug (fuss '^' '') (star ;~(less col next)))
    --
  ++  luk   ::  Lookahead, positive or negative
    ;~  pose
      %+  ifix  [(jest '(?=') par]
      %-  cook  :_  top(arp %.n)
      |=  [pan=pattern]  ^-  pattern
      |=  [sat=state]  ^-  promise
      |.  ^-  product
      ?~  ((pan sat))  ~
      `[sat none]
    ::
      %+  ifix  [(jest '(?!') par]
      %-  cook  :_  top(arp %.n)
      |=  [pan=pattern]  ^-  pattern
      |=  [sat=state]  ^-  promise
      |.  ^-  product
      ?~  ((pan sat))  `[sat none]
      ~
    ==
  --
--

