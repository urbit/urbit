/-  *plum
::
::  This library includes `plume`, the actual pretty printing logic,
::  and a handful of utilities for constructing plums.
::
::  Generally, you'll just use `plume` like this:
::
::    ~(tall plume plum)  ::  Pretty print `plum` in tall mode.
::    ~(flat plume plum)  ::  Pretty print `plum` in wide mode.
::
::  There is probably no reason to look at the utility routines unless
::  you are writing something to generate `plum`s.
::
::  This is the pretty-printer.  Use the `flat` arm to render a plum
::  into a single line and use the `tall` arm to get a nice multi-line
::  rendering that switches to wide mode if there's enough space.
::
::  For details about how this works and what exactly it does in various
::  cases, take a look at the docs for `plum`, `plumfmt`, and at the
::  docs on the arms of this door.
::
^?  |%
++  plume
  |_  =plum
  ::
  ::  An line, indented by `indent` spaces.
  ::
  +$  line  [indent=@ud text=tape]
  ::
  ::  An sequence of indented lines.
  ::
  +$  block  (list line)
  ::
  ::  +flat: print as a single line
  ::
  ++  flat
    ^-  wain
    text:linear
  ::
  ::  +tall: print as multiple lines
  ::
  ++  tall
    ^-  wain
    %+  turn  window
    |=  line
    (crip (runt [indent ' '] text))
  ::
  ::  +adjust: adjust lines to right
  ::
  ++  adjust
    |=  [tab=@ud =block]  ^-  ^block
    (turn block |=([@ud tape] [(add tab +<-) +<+]))
  ::
  ::  Prepend `n` spaces to a tape.
  ::
  ++  prepend-spaces
    |=  [n=@ t=tape]  ^-  tape
    (runt [n ' '] t)
  ::
  ::  +window: print as list of tabbed lines
  ::
  ++  window
    ^-  block
    ~+                                                  ::  for random access
    ?@  plum  [0 (trip plum)]~                          ::  trivial text
    ?-  -.plum
      ::
      ::  %para: Line-wrappable paragraph. This is a stub; it should
      ::  wrap text to 40 characters.
      ::
      %para
        [0 +:linear]~
      ::
      ::  %sbrk: nested subexpression
      ::
      ::  This is an opportunity to switch to wide mode. First, try
      ::  rendered in wide mode. If that's possible and the result
      ::  isn't too big, use that. Otherwise recurse into the subplum
      ::  without switching to wide mode.
      ::
      %sbrk
        =/  sub  kid.plum
        ?+    sub
            window(plum sub)
          [%tree *]
            =/  wideresult
              ?~(wide.fmt.sub ~ [~ u=linear])
            ?:  ?&(?=(^ wideresult) (lte length.u.wideresult 40))
              [0 text.u.wideresult]~
            window(plum sub)
        ==
      ::
      ::  %tree: Try to render a text tree in tall mode.
      ::
      ::  We want to render this in tall mode. First, verify that there
      ::  the plum has a tall render (if not, fall back to `linear`
      ::  formatting), then render all the subplums, and then render
      ::  them in one of three ways:
      ::
      ::  - If the `plumfmt` contains an `indef` and that indef has
      ::    no prefix, then this is a variable-arity rune with a
      ::    terminator: Use vertical formatting.
      ::
      ::  - If the `plumfmt` contains an `indef` and that indef DOES have
      ::    a prefix, then this is something that looks like a core: Use
      ::    `core-like` formatting.
      ::
      ::  - Otherwise, this is a rune with a fixed number of arguments
      ::    Render the subplums using backstep indentation.
      ::
      ::  There's also a special case where something has exactly one sub-plum.
      ::  where something has exactly one sub-block. For example, we
      ::  want this output:
      ::
      ::      |-
      ::      foo
      ::
      %tree
        ?~  tall.fmt.plum  [0 text:linear]~
        =/  prelude  (trip intro.u.tall.fmt.plum)
        |^  =/  blocks   (turn kids.plum |=(=^plum window(plum plum)))
            =/  prelude  (trip intro.u.tall.fmt.plum)
            ?~  indef.u.tall.fmt.plum
              ?:  =(1 (lent blocks))
                [[0 prelude] (zing blocks)]
              (backstep prelude blocks)
            =/  prefix  (trip sigil.u.indef.u.tall.fmt.plum)
            =/  finale  (trip final.u.indef.u.tall.fmt.plum)
            ?~  blocks  %+  weld
                          ?~(prelude ~ [0 prelude]~)
                        ?~(finale ~ [0 finale]~)
            ?~  prefix  (running prelude blocks finale)
            (core-like prelude prefix blocks finale)
        --
    ==
  ::
  ::  Render a plum in tall-mode using backstep indentation. Here,
  ::  we are rendering things that look something like this:
  ::
  ::      :+  foo
  ::        bar
  ::      baz
  ::
  ++  backstep
    |=  [prelude=tape blocks=(list block)]
    ^-  block
    %-  zing
    =/  nkids  (lent blocks)
    =/  idx  1
    |-  ^-  (list block)
    ?~  blocks  ~
    :_  $(blocks t.blocks, idx +(idx))
    ^-  block
    =/  indent  (mul 2 (sub nkids idx))
    ?.  =(1 idx)  (adjust indent i.blocks)
    (rune-inline-with-block prelude indent i.blocks)
  ::
  ::  To make things look a bit nicer, we want to put the first
  ::  sub-block on the same line as the rune. We want this:
  ::
  ::      :-  foo
  ::      baz
  ::
  ::  Instead of this:
  ::
  ::      :-
  ::          foo
  ::      baz
  ::
  ::  This handles the "foo" case.
  ::
  ++  rune-inline-with-block
    |=  [rune=tape indent=@ blk=block]
    ^-  block
    =.  indent  (max indent (add 2 (lent rune)))
    =.  blk     (adjust indent blk)
    ?~  rune  blk
    ?~  blk   [0 rune]~
    :_  t.blk
    :-  0
    %+  weld  rune
    =/  spaces-btwn  (sub indent.i.blk (lent rune))
    (prepend-spaces spaces-btwn text.i.blk)
  ::
  ::  Render a tall hoon with running indentation. Here, we are
  ::  rendering things that look sopmething like:
  ::
  ::      :~  foo
  ::          bar
  ::          baz
  ::      ==
  ::
  ::  So, there's basically three cases here: Either the prelude
  ::  is a rune, the prelude is empty, or prelude is some other
  ::  random-ass thing.
  ::
  ::  - If there is no prelude, then just combine all of the
  ::    sub-blocks together unaltered.
  ::  - If it's a rune (two-characters wide), then combine the
  ::    rune and the first line into one line (separated by two
  ::    spaces) and indent the rest of the lines by four spaces.
  ::  - If the rune is some other random-ass thing (has a length
  ::    that isn't 0 or 2), then render the prelude alone on the
  ::    first line and then combine the sub-blocks together,
  ::    all indented by another two spaces.
  ::
  ::  Regardless, if there's a finale, stick it on the end without
  ::  any indentation.
  ::
  ++  running
    |=  [prelude=tape blocks=(list block) finale=tape]
    ^-  block
    =/  result=block  (zing blocks)
    =.  result
      ?+    (lent prelude)
          [[0 prelude] (adjust 2 result)]         ::  unusual prelude
        %0                                        ::  empty prelude
          result
        %2                                        ::  rune prelude
          (rune-inline-with-block prelude 4 result)
      ==
    ?~  finale  result
    (snoc result [0 finale])
  ::
  ::  This renders sub-blocks where each sub-block needs to be
  ::  prefixed by some tape. For example:
  ::
  ::      |%
  ::      ++  foo
  ::        bar
  ::      ++  baz
  ::        qux
  ::      --
  ::
  ++  core-like
    |=  [prelude=tape prefix=tape blocks=(list block) finale=tape]
    ^-  block
    =/  clear  (add 2 (lent prefix))
    =/  result
      ^-  block
      %-  zing
      ^-  (list block)
      %+  turn  blocks
      |=  blk=block
      ^-  block
      ^+  +<
      =*  tab  ?~(blk 0 (sub clear (min clear indent.i.blk)))
      =.  blk  (adjust tab blk)
      ?~  blk  ~
      :_  t.blk
      :-  0
      %+  weld  prefix
      (runt [(sub indent.i.blk (lent prefix)) ' '] text.i.blk)
    =.  result
      ?~  finale  result
      (snoc result [0 finale])
    ?~  prelude  result
    [[0 prelude] result]
  ::
  ::  +linear: Render a plum onto a single line, even if it only has a
  ::  wide form.
  ::
  ++  linear
    ^-  [length=@ud text=tape]
    ~+                                                  ::  ~+ for random access
    ?@  plum  [(met 3 plum) (trip plum)]                ::  Just a cord.
    ?-  -.plum
      ::
      ::  This is already in wide mode, so %sbrk nodes don't matter here.
      ::
      %sbrk
        linear(plum kid.plum)
      ::
      ::  %para: To write a wrappable text paragraph to a single line,
      ::  we just combine all the lines into one, interspersing single
      ::  spaces chars.
      ::
      %para
        |-  ^-  [length=@ud text=tape]
        ?~  lines.plum  [0 ~]
        =/  next  $(lines.plum t.lines.plum)
        =/  this  [length=(met 3 i.lines.plum) text=(trip i.lines.plum)]
        :-  (add +(length.this) length.next)
        (weld text.this `tape`[' ' text.next])
      ::
      ::  Render a text tree to a single line.
      ::
      %tree
        |^  ^-  [length=@ud text=tape]
            ?~  wide.fmt.plum  (force-wide window)
            =/  body  (render-body delimit.u.wide.fmt.plum kids.plum)
            ?~  enclose.u.wide.fmt.plum  body
            (wrap-with-enclose u.enclose.u.wide.fmt.plum body)
        ::
        ::  Given a list of subplums and a delimiter, render all the
        ::  subplums onto a single line, and combine them into a single
        ::  string by interspersing the delimiter.
        ::
        ++  render-body
           |=  [delimit=cord kids=(list ^plum)]
           =/  stop  (trip delimit)
           |-  ^-  [length=@ud text=tape]
           ?~  kids  [0 ~]
           =/  next  $(kids t.kids)
           =/  this  linear(plum i.kids)
           ?~  text.next  this
           :-  :(add length.this (lent stop) length.next)
           :(weld text.this stop text.next)
        ::
        ::  Wrap a wide-form-rendered result with the `enclose`  cords
        ::  from its `plumefmt`.
        ::
        ++  wrap-with-enclose
          |=  [clamps=(pair cord cord) body=[length=@ text=tape]]
          ^-  [length=@ud text=tape]
          ::
          =/  close  [(trip -.clamps) (trip +.clamps)]
          :-  :(add length.body (lent -.close) (lent +.close))
          :(weld -.close text.body +.close)
        ::
        ::  Given the result of rendering a plum in tall form, combine
        ::  all the lines into one by separating each by two spaces.
        ::
        ++  force-wide
          |=  render=(list [@ud text=tape])
          ^-  [length=@ud text=tape]
          ::
          ?~  render  [0 ~]
          =/  next  (force-wide t.render)
          :-  :(add (lent text.i.render) 2 length.next)
          ?~(text.next text.i.render :(weld text.i.render "  " text.next))
        --
    ==
  --
::
::  Convenience function to build a `plumfmt` for a rune with a fixed
::  number of parameters.
::
++  fixed
  |=  rune=@ta
  ^-  plumfmt
  [wide=`[' ' `[(cat 3 +< '(') ')']] tall=`[+< ~]]
::
::  Same as `fixed` but only outputs in `tall` mode.
::
++  tall-fixed
  |=  rune=cord
  ^-  (unit [cord (unit [cord cord])])
  `[rune ~]
::
::  Convenience function to build a the `tall` part of a `plumfmt` for
::  a running-style rune (one that takes a variable number of parameters
::  and has a terminator).
::
++  tall-running
  |=  [rune=cord sigil=cord term=cord]
  ^-  (unit [cord (unit [cord cord])])
  `[rune `[sigil term]]
::
::  Convenience function for rendering a rune into a plum. This takes
::  a rune, an optional tall-form terminator, optionally a short-form (if
::  you don't supply a short-form, it'll just construct the standard
::  wide-form (e.g. "?~(x x ~)") for you, and a list of sub-plums.
::
++  rune
  |=  $:  rune=cord
          term=(unit cord)
          short=(unit [cord cord cord])
          kids=(list plum)
      ==
  ^.  plum
  |^  :-  %sbrk
      :+  %tree
        :-  (rune-wide-form rune short)
        ?~  term  (tall-fixed rune)
        (tall-running rune '' u.term)
      kids
  ::
  ::  If you just give this a rune, it'll build the standard wide-form.
  ::  Otherwise, it'll just use the one that you gave it.
  ::
  ++  rune-wide-form
    |=  [rune=cord short=(unit [fst=cord mid=cord lst=cord])]
    ^-  (unit (pair cord (unit [cord cord])))
    =*  fst  (cat 3 rune '(')
    =*  std  `[' ' `[fst ')']]
    ?~  short  std
    `[mid.u.short `[fst.u.short lst.u.short]]
  --
::
::  Just a helper function for constructing a wide-form %tree plum.
::
++  simple-wide
  |=  [init=cord sep=cord end=cord kids=(list plum)]
  ^-  plum
  =/  fmt=plumfmt  [wide=[~ sep [~ init end]] tall=~]
  [%tree fmt kids]
::
::  Convenience function that builds a plum for a subexpression. The
::  `%sbrk` tells the pretty-printer that this is a valid place to
::  switch from tall mode to wide mode.
::
++  subtree
  |=  [p=plumfmt q=(list plum)]
  ^-  plum
  [%sbrk [%tree p q]]
::
::  Convenience for generating plums that look like s-expressions. Useful
::  for quickly getting decent-looking debug output.
::
++  sexp
  |=  [sym=cord kids=(list plum)]
  ^-  plum
  =/  head=cord     (cat 3 '(' sym)
  =/  headspc=cord  (cat 3 head ' ')
  =/  symcol=cord  (cat 3 sym ':')
  =/  fmt=plumfmt   [[~ ' ' [~ headspc ')']] [~ symcol [~ '' '']]]
  ?~  kids  (cat 3 '(' (cat 3 sym ')'))
  [%sbrk [%tree fmt kids]]
--
