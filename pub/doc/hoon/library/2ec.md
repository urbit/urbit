section 2eC, parsing (custom rules)
===================================

### `++cold`

Replace with constant

    ++  cold                                                ::  replace w/ constant
      ~/  %cold
      |*  [cus=* sef=_rule]
      ~/  %fun
      |=  tub=nail
      =+  vex=(sef tub)
      ?~  q.vex
        vex
      [p=p.vex q=[~ u=[p=cus q=q.u.q.vex]]]
    ::

Parser modifier. Accepts a rule `sef` and produces a parser that
produces a constant `cus`, if `sef` is successful.

`cus` is a constant [noun]().

`sef` is a [`++rule`]().

        ~zod/try=> ((cold %foo (just 'a')) [[1 1] "abc"])
        [p=[p=1 q=2] q=[~ u=[p=%foo q=[p=[p=1 q=2] q="bc"]]]]
        ~zod/try=> ((cold %foo (just 'a')) [[1 1] "bc"])
        [p=[p=1 q=1] q=~]

------------------------------------------------------------------------

### `++cook`

Apply gate

    ++  cook                                                ::  apply gate
      ~/  %cook
      |*  [poq=_,* sef=_rule]
      ~/  %fun
      |=  tub=nail
      =+  vex=(sef tub)
      ?~  q.vex
        vex
      [p=p.vex q=[~ u=[p=(poq p.u.q.vex) q=q.u.q.vex]]]
    ::

Parser modifier. Produces a parser that takes a (successful) result of a
rule `sef` and slams it through `poq`.

`poq` is a [gate]().

`sef` is a [`++rule`]().

        ~zod/try=> ((cook ,@ud (just 'a')) [[1 1] "abc"])
        [p=[p=1 q=2] q=[~ u=[p=97 q=[p=[p=1 q=2] q="bc"]]]]
        ~zod/try=> ((cook ,@tas (just 'a')) [[1 1] "abc"])
        [p=[p=1 q=2] q=[~ u=[p=%a q=[p=[p=1 q=2] q="bc"]]]]
        ~zod/try=> ((cook |=(a=@ +(a)) (just 'a')) [[1 1] "abc"])
        [p=[p=1 q=2] q=[~ u=[p=98 q=[p=[p=1 q=2] q="bc"]]]]
        ~zod/try=> ((cook |=(a=@ `@t`+(a)) (just 'a')) [[1 1] "abc"])
        [p=[p=1 q=2] q=[~ u=[p='b' q=[p=[p=1 q=2] q="bc"]]]]

------------------------------------------------------------------------

### `++easy`

Always parse

    ++  easy                                                ::  always parse
      ~/  %easy
      |*  huf=*
      ~/  %fun
      |=  tub=nail
      ^-  (like ,_huf)
      [p=p.tub q=[~ u=[p=huf q=tub]]]
    ::

Parser generator. Produces a parser that succeeds with given noun `huf`
without consuming any text.

    ~zod/try=> ((easy %foo) [[1 1] "abc"])
    [p=[p=1 q=1] q=[~ [p=%foo q=[p=[p=1 q=1] q="abc"]]]]
    ~zod/try=> ((easy %foo) [[1 1] "bc"])
    [p=[p=1 q=1] q=[~ [p=%foo q=[p=[p=1 q=1] q="bc"]]]]
    ~zod/try=> ((easy 'a') [[1 1] "bc"])
    [p=[p=1 q=1] q=[~ [p='a' q=[p=[p=1 q=1] q="bc"]]]]

------------------------------------------------------------------------

### `++fail`

Never parse

    ++  fail  |=(tub=nail [p=p.tub q=~])                    ::  never parse

Produces an [edge]() at the same text position ([hair]()) with a failing
result (`q=~`).

`tub` is a [`++nail`]().

    ~zod/try=> (fail [[1 1] "abc"])
    [p=[p=1 q=1] q=~]
    ~zod/try=> (fail [[p=1.337 q=70] "Parse me, please?"])
    [p=[p=1.337 q=70] q=~]

------------------------------------------------------------------------

### `++full`

Parse to end

    ++  full                                                :: parse to end 
      |*  sef=_rule
      |=  tub=nail
      =+  vex=(sef tub)
      ?~(q.vex vex ?:(=(~ q.q.u.q.vex) vex [p=p.vex q=~]))
    ::

Accepts a [`++nail`](), `tub`, and produces a parser that succeeds only
when a `tub` success consumes the remainder of the [tape]().

`sef` is a [`++rule`]().

    ~zod/try=> ((full (just 'a')) [[1 1] "ab"])
    [p=[p=1 q=2] q=~]
    ~zod/try=> ((full (jest 'ab')) [[1 1] "ab"])
    [p=[p=1 q=3] q=[~ u=[p='ab' q=[p=[p=1 q=3] q=""]]]]
    ~zod/try=> ((full ;~(plug (just 'a') (just 'b'))) [[1 1] "ab"])
    [p=[p=1 q=3] q=[~ u=[p=[~~a ~~b] q=[p=[p=1 q=3] q=""]]]]

------------------------------------------------------------------------

### `++funk`

Add to tape

    ++  funk                                                ::  add to tape first
      |*  [pre=tape sef=_rule]
      |=  tub=nail
      (sef p.tub (weld pre q.tub))
    ::

Parser modifier: prepend text to tape before applying parser.

`pre` is a [`++tape`]()

`sef` is a [`++rule`]()

    ~zod/try=> ((funk "abc prefix-" (jest 'abc')) [[1 1] "to be parsed"])
    [p=[p=1 q=4] q=[~ [p='abc' q=[p=[p=1 q=4] q=" prefix-to be parsed"]]]]
    ~zod/try=> ((funk "parse" (just 'a')) [[1 4] " me"])
    [p=[p=1 q=4] q=~]

------------------------------------------------------------------------

### `++here`

Place-based apply

    ++  here                                                ::  place-based apply
      ~/  %here
      |*  [hez=_|=([a=pint b=*] [a b]) sef=_rule]
      ~/  %fun
      |=  tub=nail
      =+  vex=(sef tub)
      ?~  q.vex
        vex
      [p=p.vex q=[~ u=[p=(hez [p.tub p.q.u.q.vex] p.u.q.vex) q=q.u.q.vex]]]
    ::

Parser modifier. Similar to [`++cook`](), produces a parser that takes a
(successful) result of `sef` and slams it through `hez`. `hez` accepts a
[`++pint`]() `a` and a noun `b`, which is what the parser parsed.

`hez` is a [gate]().

`sef` is a [`++rule`]()

    ~zod/try=> (scan "abc" (star alf))
    "abc"
    ~zod/try=> (scan "abc" (here |*(^ +<) (star alf)))
    [[[p=1 q=1] p=1 q=4] "abc"]
    ~zod/try=> (scan "abc" (star (here |*(^ +<) alf)))
    ~[[[[p=1 q=1] p=1 q=2] ~~a] [[[p=1 q=2] p=1 q=3] ~~b] [[[p=1 q=3] p=1 q=4] ~~c]]

------------------------------------------------------------------------

### `++inde`

Indentation block

    ++  inde  |*  sef=_rule                                 :: indentation block
      |=  nail  ^+  (sef)
      =+  [har tap]=[p q]:+<
      =+  lev=(fil 3 (dec q.har) ' ')
      =+  eol=(just `@t`10)
      =+  =-  roq=((star ;~(pose prn ;~(sfix eol (jest lev)) -)) har tap)
          ;~(simu ;~(plug eol eol) eol)
      ?~  q.roq  roq
      =+  vex=(sef har(q 1) p.u.q.roq)
      =+  fur=p.vex(q (add (dec q.har) q.p.vex))
      ?~  q.vex  vex(p fur)
      =-  vex(p fur, u.q -)
      :+  &3.vex
        &4.vex(q.p (add (dec q.har) q.p.&4.vex))
      =+  res=|4.vex
      |-  ?~  res  |4.roq
      ?.  =(10 -.res)  [-.res $(res +.res)]
      (welp [`@t`10 (trip lev)] $(res +.res))
    ::

Apply rule to indented block starting at current column number, omitting
the leading whitespace.

`sef` is a [`++rule`]()

    ~zod/try=> (scan "abc" (inde (star ;~(pose prn (just `@`10)))))
    "abc"
    ~zod/try=> (scan "abc" (star ;~(pose prn (just `@`10))))
    "abc"
    ~zod/try=> (scan "  abc\0ade" ;~(pfix ace ace (star ;~(pose prn (just `@`10)))))
    "abc
        de"
    ~zod/try=> (scan "  abc\0ade" ;~(pfix ace ace (inde (star ;~(pose prn (just `@`10))))))
    ! {1 6}
    ! exit
    ~zod/try=> (scan "  abc\0a  de" ;~(pfix ace ace (inde (star ;~(pose prn (just `@`10))))))
    "abc
        de"

------------------------------------------------------------------------

### `++jest`

Match a cord

    ++  jest                                                ::  match a cord
      |=  daf=@t
      |=  tub=nail
      =+  fad=daf
      |-  ^-  (like ,@t)
      ?:  =(0 daf)
        [p=p.tub q=[~ u=[p=fad q=tub]]]
      ?:  |(?=(~ q.tub) !=((end 3 1 daf) i.q.tub))
        (fail tub)
      $(p.tub (lust i.q.tub p.tub), q.tub t.q.tub, daf (rsh 3 1 daf))
    ::

Match and consume a cord.

`daf` is a `@t`

    ~zod/try=> ((jest 'abc') [[1 1] "abc"])
    [p=[p=1 q=4] q=[~ [p='abc' q=[p=[p=1 q=4] q=""]]]]
    ~zod/try=> (scan "abc" (jest 'abc'))
    'abc'
    ~zod/try=> (scan "abc" (jest 'acb'))
    ! {1 2}
    ! 'syntax-error'
    ! exit
    ~zod/try=> ((jest 'john doe') [[1 1] "john smith"])
    [p=[p=1 q=6] q=~]
    ~zod/try=> ((jest 'john doe') [[1 1] "john doe"])
    [p=[p=1 q=9] q=[~ [p='john doe' q=[p=[p=1 q=9] q=""]]]]

------------------------------------------------------------------------

### `++just`

Match a char

    ++  just                                                ::  XX redundant, jest
      ~/  %just                                             ::  match a char
      |=  daf=char
      ~/  %fun
      |=  tub=nail
      ^-  (like char)
      ?~  q.tub
        (fail tub)
      ?.  =(daf i.q.tub)
        (fail tub)
      (next tub)
    ::

Match and consume a single character.

`daf` is a [`++char`]()

    ~zod/try=> ((just 'a') [[1 1] "abc"])
    [p=[p=1 q=2] q=[~ [p=~~a q=[p=[p=1 q=2] q="bc"]]]]
    ~zod/try=> (scan "abc" (just 'a'))
    ! {1 2}
    ! 'syntax-error'
    ! exit
    ~zod/try=> (scan "a" (just 'a'))
    ~~a
    ~zod/try=> (scan "%" (just '%'))
    ~~~25.

------------------------------------------------------------------------

### `++knee`

Recursive parsers

    ++  knee                                                ::  callbacks
      |*  [gar=* sef=_|.(rule)]
      |=  tub=nail
      ^-  (like ,_gar)
      ((sef) tub)
    ::

Used for recursive parsers, which would otherwise be infinite when
compiled.

`gar` is a noun.

`sef` is a [gate]() that accepts a [`++rule`]()

    ~zod/try=> |-(;~(plug prn ;~(pose $ (easy ~))))
    ! rest-loop
    ! exit
    ~zod/try=> |-(;~(plug prn ;~(pose (knee *tape |.(^$)) (easy ~))))
    < 1.obo
      [ c=c=tub=[p=[p=@ud q=@ud] q=""]
          b
        < 1.bes
          [ c=tub=[p=[p=@ud q=@ud] q=""]
            b=<1.tnv [tub=[p=[p=@ud q=@ud] q=""] <1.ktu [daf=@tD <414.fvk 101.jzo 1.ypj %164>]>]>
            a=<1.fvg [tub=[p=[p=@ud q=@ud] q=""] <1.khu [[les=@ mos=@] <414.fvk 101.jzo 1.ypj %164>]>]>
            v=<414.fvk 101.jzo 1.ypj %164>
          ]
        >
          a
        ... 450 lines omitted ...
      ]
    >
    ~zod/try=> (scan "abcd" |-(;~(plug prn ;~(pose (knee *tape |.(^$)) (easy ~)))))
    [~~a "bcd"]

------------------------------------------------------------------------

### `++mask`

Match char

    ++  mask                                                ::  match char in set
      ~/  %mask
      |=  bud=(list char)
      ~/  %fun
      |=  tub=nail
      ^-  (like char)
      ?~  q.tub
        (fail tub)
      ?.  (lien bud |=(a=char =(i.q.tub a)))
        (fail tub)
      (next tub)
    ::

Parser generator. Matches the next character if it is in a list of
characters.

`bud` is a list of [`++char`]()

    ~zod/try=> (scan "a" (mask "cba"))
    ~~a
    ~zod/try=> ((mask "abc") [[1 1] "abc"])
    [p=[p=1 q=2] q=[~ [p=~~a q=[p=[p=1 q=2] q="bc"]]]]
    ~zod/try=> ((mask "abc") [[1 1] "bbc"])
    [p=[p=1 q=2] q=[~ [p=~~b q=[p=[p=1 q=2] q="bc"]]]]
    ~zod/try=> ((mask "abc") [[1 1] "dbc"])
    [p=[p=1 q=1] q=~]

------------------------------------------------------------------------

### `++next`

Consume char

    ++  next                                                ::  consume a char
      |=  tub=nail
      ^-  (like char)
      ?~  q.tub
        (fail tub)
      =+  zac=(lust i.q.tub p.tub)
      [zac [~ i.q.tub [zac t.q.tub]]]
    ::

Consume any character, producing it as a result.

`tub` is a [`++nail`]()

    ~zod/try=> (next [[1 1] "ebc"])
    [p=[p=1 q=2] q=[~ [p=~~e q=[p=[p=1 q=2] q="bc"]]]] 
    ~zod/try=> (next [[1 1] "john jumps jones"])
    [p=[p=1 q=2] q=[~ [p=~~j q=[p=[p=1 q=2] q="ohn jumps jones"]]]]

------------------------------------------------------------------------

### `++sear`

Conditional `++cook`

    ++  sear                                                ::  conditional cook
      |*  [pyq=_|=(* *(unit)) sef=_rule]
      |=  tub=nail
      =+  vex=(sef tub)
      ?~  q.vex
        vex
      =+  gey=(pyq p.u.q.vex)
      ?~  gey
        [p=p.vex q=~]
      [p=p.vex q=[~ u=[p=u.gey q=q.u.q.vex]]]
    ::

Conditional [`++cook`](). Slams the result through a gate that produces
a unit; if that unit is empty, fail.

`tub` is a [`++nail`](/doc/hoon/library/1#++nail)

    ~zod/try=> ((sear |=(a=* ?@(a (some a) ~)) (just `a`)) [[1 1] "abc"])
    [p=[p=1 q=2] q=[~ u=[p=97 q=[p=[p=1 q=2] q="bc"]]]]
    ~zod/try=> ((sear |=(* ~) (just 'a')) [[1 1] "abc"])
    [p=[p=1 q=2] q=~]

------------------------------------------------------------------------

### `++shim`

Char in range

    ++  shim                                                ::  match char in range
      ~/  %shim
      |=  [les=@ mos=@]
      ~/  %fun
      |=  tub=nail
      ^-  (like char)
      ?~  q.tub
        (fail tub)
      ?.  ?&((gte i.q.tub les) (lte i.q.tub mos))
        (fail tub)
      (next tub)
    ::

Match characters within a range.

`les` and `mos` are atoms, `@`.

    ~zod/try=> ((shim 'a' 'z') [[1 1] "abc"])
    [p=[p=1 q=2] q=[~ [p=~~a q=[p=[p=1 q=2] q="bc"]]]]
    ~zod/try=> ((shim 'a' 'Z') [[1 1] "abc"])
    [p=[p=1 q=1] q=~]
    ~zod/try=> ((shim 'a' 'Z') [[1 1] "Abc"])
    [p=[p=1 q=2] q=[~ [p=~~~41. q=[p=[p=1 q=2] q="bc"]]]]

------------------------------------------------------------------------

### `++stag`

Add label

    ++  stag                                                ::  add a label
      ~/  %stag
      |*  [gob=* sef=_rule]
      ~/  %fun
      |=  tub=nail
      =+  vex=(sef tub)
      ?~  q.vex
        vex
      [p=p.vex q=[~ u=[p=[gob p.u.q.vex] q=q.u.q.vex]]]
    ::

Add a label to an edge parsed by a rule.

`gob` is a noun.

`sef` is a rule.

    ~zod/try=> ((stag %foo (just 'a')) [[1 1] "abc"])
    [p=[p=1 q=2] q=[~ u=[p=[%foo ~~a] q=[p=[p=1 q=2] q="bc"]]]]
    ~zod/try=> ((stag "xyz" (jest 'abc')) [[1 1] "abc"])
    [p=[p=1 q=4] q=[~ u=[p=["xyz" 'abc'] q=[p=[p=1 q=4] q=""]]]]
    ~zod/try=> ((stag 10.000 (shim 0 100)) [[1 1] "abc"])
    [p=[p=1 q=2] q=[~ u=[p=[10.000 ~~a] q=[p=[p=1 q=2] q="bc"]]]]

------------------------------------------------------------------------

### `++stet`

Add faces

    ++  stet
      |*  leh=(list ,[?(@ [@ @]) _rule])
      |-
      ?~  leh
        ~
      [i=[p=-.i.leh q=+.i.leh] t=$(leh t.leh)]
    ::

Add `[p q]` faces to range-parser pairs in a list.

`leh` is a list of range-parsers.

    ~zod/try=> (stet (limo [[5 (just 'a')] [1 (jest 'abc')] [[1 1] (shim 0 200)] 
    [[1 10] (cold %foo (just 'a'))]~]))
    ~[
      [p=5 q=<1.lrk [tub=[p=[p=@ud q=@ud] q=""] <1.nqy [daf=@tD <394.imz 97.kdz 1.xlc %164>]>]>]
      [p=1 q=<1.lrk [tub=[p=[p=@ud q=@ud] q=""] <1.nqy [daf=@tD <394.imz 97.kdz 1.xlc %164>]>]>]
      [p=[1 1] q=<1.lrk [tub=[p=[p=@ud q=@ud] q=""] <1.nqy [daf=@tD <394.imz 97.kdz 1.xlc %164>]>]>]
      [p=[1 10] q=<1.lrk [tub=[p=[p=@ud q=@ud] q=""] <1.nqy [daf=@tD <394.imz 97.kdz 1.xlc %164>]>]>]
    ]
    ~zod/try=> [[[1 1] (just 'a')] [[2 1] (shim 0 200)] ~]
    [ [[1 1] <1.tnv [tub=[p=[p=@ud q=@ud] q=""] <1.ktu [daf=@tD <414.fvk 101.jzo 1.ypj %164>]>]>]
      [[2 1] <1.fvg [tub=[p=[p=@ud q=@ud] q=""] <1.khu [[les=@ mos=@] <414.fvk 101.jzo 1.ypj %164>]>]>]
      ~
    ]
    ~zod/try=> (stet (limo [[[1 1] (just 'a')] [[2 1] (shim 0 200)] ~]))
    ~[
      [p=[1 1] q=<1.lrk [tub=[p=[p=@ud q=@ud] q=""] <1.nqy [daf=@tD <394.imz 97.kdz 1.xlc %164>]>]>] 
      [p=[2 1] q=<1.lrk [tub=[p=[p=@ud q=@ud] q=""] <1.nqy [daf=@tD <394.imz 97.kdz 1.xlc %164>]>]>]
    ]

------------------------------------------------------------------------

### `++stew`

Switch by first

    ++  stew                                                ::  switch by first char
      ~/  %stew
      |*  leh=(list ,[p=?(@ [@ @]) q=_rule])                ::  char/range keys
      =+  ^=  wor                                           ::  range complete lth
          |=  [ort=?(@ [@ @]) wan=?(@ [@ @])]
          ?@  ort
            ?@(wan (lth ort wan) (lth ort -.wan))
          ?@(wan (lth +.ort wan) (lth +.ort -.wan))
      =+  ^=  hel                                           ::  build parser map
          =+  hel=`(tree $_(?>(?=(^ leh) i.leh)))`~
          |-  ^+  hel
          ?~  leh
            ~
          =+  yal=$(leh t.leh)
          |-  ^+  hel
          ?~  yal
            [i.leh ~ ~]
          ?:  (wor p.i.leh p.n.yal)
            =+  nuc=$(yal l.yal)
            ?>  ?=(^ nuc)
            ?:  (vor p.n.yal p.n.nuc)
              [n.yal nuc r.yal]
            [n.nuc l.nuc [n.yal r.nuc r.yal]]
          =+  nuc=$(yal r.yal)
          ?>  ?=(^ nuc)
          ?:  (vor p.n.yal p.n.nuc)
            [n.yal l.yal nuc]
          [n.nuc [n.yal l.yal l.nuc] r.nuc]
      ~%  %fun  ..^$  ~
      |=  tub=nail
      ?~  q.tub
        (fail tub)
      |-
      ?~  hel
        (fail tub)
      ?:  ?@  p.n.hel
            =(p.n.hel i.q.tub)
          ?&((gte i.q.tub -.p.n.hel) (lte i.q.tub +.p.n.hel))
        ::  (q.n.hel [(lust i.q.tub p.tub) t.q.tub])
        (q.n.hel tub)
      ?:  (wor i.q.tub p.n.hel)
        $(hel l.hel)
      $(hel r.hel)
    ::

Parser generator. From an associative list of characters or character
ranges to rules, construct a map, and parse tapes only with rules
associated with a range the tape's first character falls in.

------------------------------------------------------------------------

### `++stir`

Parse repeatedly

    ++  stir                                                ::  parse repeatedly 
      ~/  %stir
      |*  [rud=* raq=_|*([a=* b=*] [a b]) fel=_rule]
      ~/  %fun
      |=  tub=nail
      ^-  (like ,_rud)
      =+  vex=(fel tub)
      ?~  q.vex
        [p.vex [~ rud tub]]
      =+  wag=$(tub q.u.q.vex)
      ?>  ?=(^ q.wag)
      [(last p.vex p.wag) [~ (raq p.u.q.vex p.u.q.wag) q.u.q.wag]]
    ::

Parse with rule as many times as possible, and fold over results with a
binary gate.

`rud` is a noun.

`raq` is a gate that takes two nouns and produces a cell.

`fel` is a rule.

    ~zod/try=> (scan "abc" (stir *@ add prn))
    294
    ~zod/try=> (roll "abc" add)
    b=294

------------------------------------------------------------------------

### `++stun`

Parse several times

    ++  stun                                                ::  parse several times
      |*  [[les=@ mos=@] fel=_rule]
      |=  tub=nail
      ^-  (like (list ,_(wonk (fel))))
      ?:  =(0 mos)
        [p.tub [~ ~ tub]]
      =+  vex=(fel tub)
      ?~  q.vex
        ?:  =(0 les)
          [p.vex [~ ~ tub]]
        vex
      =+  ^=  wag  %=  $
                     les  ?:(=(0 les) 0 (dec les))
                     mos  ?:(=(0 mos) 0 (dec mos))
                     tub  q.u.q.vex
                   ==
      ?~  q.wag
        wag
      [p.wag [~ [p.u.q.vex p.u.q.wag] q.u.q.wag]]

Parse bounded number of times.

`[les=@ mos=@]` is a cell of atoms indicating the bounds.

`fel` is a rule.

    ~zod/try=> ((stun [5 10] prn) [1 1] "aquickbrownfoxran")
    [p=[p=1 q=11] q=[~ [p="aquickbrow" q=[p=[p=1 q=11] q="nfoxran"]]]]
    ~zod/try=> ((stun [5 10] prn) [1 1] "aquickbro")
    [p=[p=1 q=10] q=[~ [p="aquickbro" q=[p=[p=1 q=10] q=""]]]]
    ~zod/try=> ((stun [5 10] prn) [1 1] "aqui")
    [p=[p=1 q=5] q=~]

------------------------------------------------------------------------
