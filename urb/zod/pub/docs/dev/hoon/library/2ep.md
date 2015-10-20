section 2eP, diff
=================

A- more or less low priority and/or currently in the wrong section
anyway.

------------------------------------------------------------------------

------------------------------------------------------------------------

### `++berk`

Invert diff patches

    ++  berk                                                ::  invert diff patch
      |*  bur=(urge)
      |-  ^+  bur
      ?~  bur  ~
      :_  $(bur t.bur)
      ?-  -.i.bur
        &  i.bur
        |  [%| q.i.bur p.i.bur]
      ==
    ::

Inverts a list of changes `bur`. Skips stay constant and replaces are
swapped. Produces a `bur`.

`bur` is a [`++urge`]().

    ~zod/try=> (berk `(urge)`~[`10 %|^[~[2] ~[3 4]] `5])
    ~[[%.y p=10] [%.n p=~[3 4] q=~[2]] [%.y p=5]]
    ~zod/try=> (lurk "somes" `(urge char)`~[`1 [%| "o" "a"] `3])
    "sames"
    ~zod/try=> (berk `(urge char)`~[`1 [%| "o" "a"] `3])
    ~[[%.y p=1] [%.n p="a" q="o"] [%.y p=3]]
    ~zod/try=> (lurk "sames" (berk `(urge char)`~[`1 [%| "o" "a"] `3]))
    "somes"

------------------------------------------------------------------------

### `++diff`

Generate patch

    ++  diff                                                ::  generate patch
      |=  pum=umph
      |=  [old=* new=*]  ^-  udon
      :-  pum
      ?+  pum  ~|(%unsupported !!)
        %a  [%d (nude old new)]
        %b  =+  [hel=(cue ((hard ,@) old)) hev=(cue ((hard ,@) new))]
            [%d (nude hel hev)]
        %c  =+  [hel=(lore ((hard ,@) old)) hev=(lore ((hard ,@) new))]
            [%c (lusk hel hev (loss hel hev))]
      ==
    ::

Produces a patch between two nouns, by change type

`pum` is an [`++umph`]().

    ~zod/try=> ((diff %a) 20 21)
    [p=%a q=[%d p=[%1 p=21] q=[%1 p=20]]]
    ~zod/try=> ((diff %a) [1 2 3] [1 2 4])
    [ p=%a
        q
      [ %d
        p=[p=[%0 p=2] q=[p=[%0 p=6] q=[%1 p=4]]] 
        q=[p=[%0 p=2] q=[p=[%0 p=6] q=[%1 p=3]]]
      ]
    ]
    ~zod/try=> ~04hh
    [1 2]
    ~zod/try=> ~0ph
    [1 1]
    ~zod/try=> ((diff %b) 0v4hh 0vph)
    [p=%b q=[%d p=[p=[%0 p=2] q=[%0 p=2]] q=[p=[%0 p=3] q=[%1 p=2]]]]
    ~zod/try=> ((diff %c) (role 'sam' 'les' 'les' 'kor' ~) (role 'sam' 'mor' 'kor' ~))
    [p=%c q=[%c p=~[[%.y p=1] [%.n p=~[7.562.604 7.562.604] q=~[7.499.629]] [%.y p=1]]]]
    ~[[%.y p=0] [%.y p=0] [%.y p=1] [%.n p=<|les les|> q=<|mor|>] [%.y p=1]]
    ~zod/try=> (,[%c %c (urge cord)] ((diff %c) (role 'sam' 'les' 'les' 'kor' ~) (role 'sam' 'mor' 'kor' ~)))
    [%c %c ~[[%.y p=1] [%.n p=<|les les|> q=<|mor|>] [%.y p=1]]]

------------------------------------------------------------------------

### `++loss`

Longest subsequence

    ++  loss                                                ::  longest subsequence
      ~/  %loss
      |*  [hel=(list) hev=(list)]
      |-  ^+  hev
      =+  ^=  sev
          =+  [inx=0 sev=*(map ,@t (list ,@ud))]
          |-  ^+  sev
          ?~  hev  sev
          =+  guy=(~(get by sev) i.hev)
          $(hev t.hev, inx +(inx), sev (~(put by sev) i.hev [inx ?~(guy ~ u.guy)]))
      =|  gox=[p=@ud q=(map ,@ud ,[p=@ud q=_hev])]
      =<  abet
      =<  main
      |%
      ++  abet  =.(q.rag ?:(=([& 0] p.rag) q.rag [p.rag q.rag]) (flop q.rag))
      ++  hink                                              ::  extend fits top
        |=  [inx=@ud goy=@ud]  ^-  ?
        |(=(p.gox inx) (lth goy p:(need (~(get by q.gox) inx))))
      ::
      ++  lonk                                              ::  extend fits bottom
        |=  [inx=@ud goy=@ud]  ^-  ?
        |(=(0 inx) (gth goy p:(need (~(get by q.gox) (dec inx)))))
      ::
      ++  lune                                              ::  extend
        |=  [inx=@ud goy=@ud]
        ^+  +>
        %_    +>.$
            gox
          :-  ?:(=(inx p.gox) +(p.gox) p.gox)
          %+  ~(put by q.gox)  inx
          [goy (snag goy hev) ?:(=(0 inx) ~ q:(need (~(get by q.gox) (dec inx))))]
        ==
      ::
      ++  merg                                              ::  merge all matches
        |=  gay=(list ,@ud)
        ^+  +>
        =+  ^=  zes
            =+  [inx=0 zes=*(list ,[p=@ud q=@ud])]
            |-  ^+  zes
            ?:  |(?=(~ gay) (gth inx p.gox))  zes
            ?.  (lonk inx i.gay)  $(gay t.gay)
            ?.  (hink inx i.gay)  $(inx +(inx))
            $(inx +(inx), gay t.gay, zes [[inx i.gay] zes])
        |-  ^+  +>.^$
        ?~(zes +>.^$ $(zes t.zes, +>.^$ (lune i.zes)))
      ::
      ++  main
        |-  ^+  +
        ?~  hel
          ?~  hev
            ?>(?=(~ lcs) +)
          $(hev t.hev, rag (done %| ~ [i.hev ~]))
        ?~  hev
          $(hel t.hel, rag (done %| [i.hel ~] ~))
        ?~  lcs
          +(rag (done %| (flop hel) (flop hev)))
        ?:  =(i.hel i.lcs)
          ?:  =(i.hev i.lcs)
            $(lcs t.lcs, hel t.hel, hev t.hev, rag (done %& 1))
          $(hev t.hev, rag (done %| ~ [i.hev ~]))
        ?:  =(i.hev i.lcs)
          $(hel t.hel, rag (done %| [i.hel ~] ~))
        $(hel t.hel, hev t.hev, rag (done %| [i.hel ~] [i.hev ~]))
      --

Finds a subsequence of repeated elements within two [`++list`]()s, using
several internal helper arms. Produces a [`++tape`]().

`hel` is a [`++list`]() of characters.

`hev` is a [++list\`]() of characters.

    ~zod/try=> (loss "sam" "sem")
    "sm"
    ~zod/try=> (loss "samo" "semo")
    "smo"
    ~zod/try=> (loss "sakmo" "semo")
    "smo"
    ~zod/try=> (loss "ferdinham" "ferdilapos
    ~ <syntax error at [1 30]>
    ~zod/try=> (loss "ferdinham" "ferdilapos")
    "ferdia"

------------------------------------------------------------------------

### `++locz`

Find common

    ++  locz                                                ::  trivial algorithm
      |=  [hel=tape hev=tape]
      ^-  tape
      =+  [leh=(lent hel) veh=(lent hev)]
      =-  (flop q.yun)
      ^=  yun
      |-  ^-  [p=@ud q=tape]
      ~+
      ?:  |(=(0 leh) =(0 veh))  [0 ~]
      =+  [dis=(snag (dec leh) hel) dat=(snag (dec veh) hev)]
      ?:  =(dis dat)
        =+  say=$(leh (dec leh), veh (dec veh))
        [+(p.say) [dis q.say]]
      =+  [lef=$(leh (dec leh)) rig=$(veh (dec veh))]
      ?:((gth p.lef p.rig) lef rig)
    ::

Finds a subsequence of repeated elements within two [`++list`](/doc/hoon/library/1#++list)s,
producing a [\`++tape]().

    ~zod/try=> (locz "samukot" "semelkot")
    "smkot"
    ~zod/try=> (locz "samukot" "samelkot")
    "samkot"

------------------------------------------------------------------------

### `++lore`

Split on `\n`

    ++  lore                                                ::  atom to line list
      ~/  %lore
      |=  lub=@
      =|  tez=(list ,@t)
      |-  ^+  tez
      ?:  =(0 lub)  (flop tez)
      =+  ^=  meg
          =+  meg=0
          |-  ^-  @ud
          =+  gam=(cut 3 [meg 1] lub)
          ?:(|(=(10 gam) =(0 gam)) meg $(meg +(meg)))
      =+  res=(rsh 3 +(meg) lub)
      ?:  &(=(0 (cut 3 [meg 1] lub)) !=(0 res))
        !!
      $(lub res, tez [(end 3 meg lub) tez])
    ::

Split on newlines, ascii `10`

    ~zod/try=> (lore 'soke\0alas\0amep')
    <|soke las mep|>
    ~zod/try=> (lore '|=  a=@\0a=+  b=(add a 5)\0a(mix b a)')
    <||=  a=@ =+  b=(add a 5) (mix b a)|>
    ~zod/try=> `wain`[(fil 3 80 ' ') (lore '|=  a=@\0a=+  b=(add a 5)\0a(mix b a)')]
    <|                                                                                
      |=  a=@
      =+  b=(add a 5)
      (mix b a)
    |>

------------------------------------------------------------------------

### `++role`

Join with `\n`

    ++  role                                                ::  line list to atom
      |=  tez=(list ,@t)
      (rap 3 (turn tez |=(a=@t (cat 3 a 10))))
    ::

Join line list with newlines.

    ~zod/try=> (role 'sep' 'tek' 'lap' ~)
    3.230.709.852.558.292.782.985.274.739
    ~zod/try=> `@t`(role 'sep' 'tek' 'lap' ~)
    '''
    sep
    tek
    lap
    '''

------------------------------------------------------------------------

### `++lump`

Change with `++udon`

    ++  lump                                                ::  apply patch
      |=  [don=udon src=*]
      ^-  *
      ?+    p.don  ~|(%unsupported !!)
          %a
        ?+  -.q.don  ~|(%unsupported !!)
          %a  q.q.don
          %c  (lurk ((hard (list)) src) p.q.don)
          %d  (lure src p.q.don)
        ==
      ::
          %c
        =+  dst=(lore ((hard ,@) src))
        %-  role
        ?+  -.q.don  ~|(%unsupported !!)
          %a  ((hard (list ,@t)) q.q.don)
          %c  (lurk dst p.q.don)
        ==
      ==
    ::

Use udon to change noun

    ~zod/try=> (lump [%a %a 20 25] 20)
    25
    ~zod/try=> (lump [%a %d [[%0 1] [%0 1]] [%0 2]] 20)
    [20 20]
    ~zod/try=> (lump [%c %a ~['sa' 'le'] ~['sa' 'lo']] 'sa\0ale')
    11.473.670.267.251
    ~zod/try=> (,@t (lump [%c %a ~['sa' 'le'] ~['sa' 'lo']] 'sa\0ale'))
    '''
    sa
    lo
    '''
    ~zod/try=> (,@t (lump [%c %c `1 [%| ~['le'] ~['lo' 'ma']] ~] 'sa\0ale'))
    '''
    sa
    ma
    lo
    '''

------------------------------------------------------------------------

### `++lure`

Patch `a`

    ++  lure                                                ::  apply tree diff
      |=  [a=* b=upas]
      ^-  *
      ?^  -.b
        [$(b -.b) $(b +.b)]
      ?+  -.b  ~|(%unsupported !!)
        %0  .*(a [0 p.b])
        %1  .*(a [1 p.b])
      ==

Patch a by references to axis and literal.

    ~zod/try=> (lure ~[1 2] [[%0 2] [%1 3] [%0 7]])
    [1 3 0]
    ~zod/try=> (lure ~[1 2 4] [[%0 2] [%1 3] [%0 7]])
    [1 3 4 0]

------------------------------------------------------------------------

### `++limp`

Reverse patch

    ++  limp                                                ::  invert patch
      |=  don=udon  ^-  udon
      :-  p.don
      ?+  -.q.don  ~|(%unsupported !!)
        %a  [%a q.q.don p.q.don]
        %c  [%c (berk p.q.don)]
        %d  [%d q.q.don p.q.don]
      ==
    ::

Reverse a patch (preprocessor unchanged)

    ~zod/try=> (limp [%a %a 20 40])
    [p=%a q=[%a p=40 q=20]]
    ~zod/try=> (limp [%c %c ~[`20 [%| ~[52 53] ~[51]] `6]])
    [p=%c q=[%c p=~[[%.y p=20] [%.n p=~[51] q=~[52 53]] [%.y p=6]]]]
    ~zod/try=> (limp [%a %d [[%0 1] [%0 1]] [%0 2]])
    [p=%a q=[%d p=[%0 p=2] q=[p=[%0 p=1] q=[%0 p=1]]]]

------------------------------------------------------------------------

### `++hump`

Prep for diff

    ++  hump                                                ::  general prepatch
      |=  [pum=umph src=*]  ^-  *
      ?+  pum  ~|(%unsupported !!)
        %a  src
        %b  (cue ((hard ,@) src))
        %c  (lore ((hard ,@) src))
      ==
    ::

Prep atom for diff: leave alone, cue, or split by newlines.

    ~zod/try=> (hump %a ~)
    0
    ~zod/try=> (hump %a 40)
    40
    ~zod/try=> (hump %c 40)
    [40 0]
    ~zod/try=> (hump %c 'as')
    [29.537 0]
    ~zod/try=> (hump %c 'as\0alok')
    [29.537 7.040.876 0]
    ~zod/try=> (hump %b 0vph)
    [1 1]

------------------------------------------------------------------------

### `++husk`

Atomize post diff

    ++  husk                                                ::  unprepatch
      |=  [pum=umph dst=*]  ^-  *
      ?+  pum  ~|(%unsupported !!)
        %a  dst
        %b  (jam dst)
        %c  (role ((hard (list ,@)) dst))
      ==
    ::

Re-atomize after diff: leave alone, jam, or join with newlines.

    ~zod/try=> (husk %a 0)
    0
    ~zod/try=> (husk %a 40)
    40
    ~zod/try=> (husk %c [40 0])
    2.600
    ~zod/try=> (rip 3 (,@ (husk %c [40 0])))
    ~[40 10]
    ~zod/try=> (husk %c [%as 0])
    684.897
    ~zod/try=> (husk %c [%as 0])
    684.897
    ~zod/try=> (,@t (husk %c [%as 0]))
    '''
    as
    '''
    ~zod/try=> (husk %c [%as %lok 0])
    2.932.876.065.272.673
    ~zod/try=> (,@t (husk %c [%as %lok 0]))
    '''
    as
    lok
    '''
    ~zod/try=> (husk %b [1 1])
    817
    ~zod/try=> (,@uv (husk %b [1 1]))
    0vph
    ~zod/try=> ~0ph
    [1 1]

------------------------------------------------------------------------

### `++lurk`

Apply list patch

    ++  lurk                                                ::  apply list patch
      |*  [hel=(list) rug=(urge)]
      ^+  hel
      =+  war=`_hel`~
      |-  ^+  hel
      ?~  rug  (flop war)
      ?-    -.i.rug
          &
        %=   $
          rug  t.rug
          hel  (slag p.i.rug hel)
          war  (weld (flop (scag p.i.rug hel)) war)
        ==
      ::
          |
        %=  $
          rug  t.rug
          hel  =+  gur=(flop p.i.rug)
               |-  ^+  hel
               ?~  gur  hel
               ?>(&(?=(^ hel) =(i.gur i.hel)) $(hel t.hel, gur t.gur))
          war  (weld q.i.rug war)
        ==
      ==
    ::

Amend list using an urge: list of `[%& {number skipped}]` and
`[%| old new]`

    ~zod/try=> (lurk "hema" `(urge char)`~[`1 [%| "e" "ru"] `2])
    "hurma"
    ~zod/try=> (lurk "koltep" `(urge char)`~[`3 [%| "et" ""] `1])
    "kolp"

------------------------------------------------------------------------

### `++lusk`

`lcs` to list patch

    ++  lusk                                                ::  lcs to list patch
      |*  [hel=(list) hev=(list) lcs=(list)]
      =+  ^=  rag
          ^-  $%  [& p=@ud]
                  [| p=_lcs q=_lcs]
              ==
          [%& 0]
      =>  .(rag [p=rag q=*(list ,_rag)])
      =<  abet  =<  main
      |%
      ++  abet  =.(q.rag ?:(=([& 0] p.rag) q.rag [p.rag q.rag]) (flop q.rag))
      ++  done
        |=  new=_p.rag
        ^+  rag
        ?-  -.p.rag
          |   ?-  -.new
                |  [[%| (weld p.new p.p.rag) (weld q.new q.p.rag)] q.rag]
                &  [new [p.rag q.rag]]
              ==
          &   ?-  -.new
                |  [new ?:(=(0 p.p.rag) q.rag [p.rag q.rag])]
                &  [[%& (add p.p.rag p.new)] q.rag]
              ==
        ==
      ::
      ++  main
        |-  ^+  +
        ?~  hel
          ?~  hev
            ?>(?=(~ lcs) +)
          $(hev t.hev, rag (done %| ~ [i.hev ~]))
        ?~  hev
          $(hel t.hel, rag (done %| [i.hel ~] ~))
        ?~  lcs
          +(rag (done %| (flop hel) (flop hev)))
        ?:  =(i.hel i.lcs)
          ?:  =(i.hev i.lcs)
            $(lcs t.lcs, hel t.hel, hev t.hev, rag (done %& 1))
          $(hev t.hev, rag (done %| ~ [i.hev ~]))
        ?:  =(i.hev i.lcs)
          $(hel t.hel, rag (done %| [i.hel ~] ~))
        $(hel t.hel, hev t.hev, rag (done %| [i.hel ~] [i.hev ~]))
      --

Using a common sequence, generate urge from two lists

    ~zod/try=> (lusk "hamok" "hasok" "haok")
    ~[[%.y p=2] [%.n p="m" q="s"] [%.y p=2]]
    ~zod/try=> (lusk "hamok" "hasok" "hak")
    ~[[%.y p=2] [%.n p="om" q="os"] [%.y p=1]]
    ~zod/try=> (lusk "telroga" "tesomga" "teoga") 
    ~[[%.y p=2] [%.n p="rl" q="s"] [%.y p=1] [%.n p="" q="m"] [%.y p=2]]
    ~zod/try=> (lurk "telroga" `(urge char)`~[[%.y p=2] [%.n p="rl" q="s"] [%.y p=1] [%.n p="" q="m"] [%.y p=2]])
    "tesomga"

------------------------------------------------------------------------

### `++nude`

Tree change

    ++  nude                                                ::  tree change
      |=  [a=* b=*]
      ^-  [p=upas q=upas]
      =<  [p=(tred a b) q=(tred b a)]
      |%
      ++  axes                                              ::  locs of nouns
        |=  [a=@ b=*]  ^-  (map ,* axis)
        =+  c=*(map ,* axis)
        |-  ^-  (map ,* axis)
        =>  .(c (~(put by c) b a))
        ?@  b
          c
        %-  ~(uni by c)
        %-  ~(uni by $(a (mul 2 a), b -.b))
        $(a +((mul 2 a)), b +.b)
      ::
      ++  tred                                              ::  diff a->b
        |=  [a=* b=*]  ^-  upas
        =|  c=(unit ,*)
        =+  d=(axes 1 a)
        |-  ^-  upas
        =>  .(c (~(get by d) b))
        ?~  c
          ?@  b
            [%1 b]
          =+  e=^-(upas [$(b -.b) $(b +.b)])
          ?-  e
            [[%1 *] [%1 *]]  [%1 [p.p.e p.q.e]]
            *  e
          ==
        [%0 u.c]
      --

Generate tree diff from two nouns.

    ~zod/try=> (nude 40 20)
    [p=[%1 p=20] q=[%1 p=40]]
    ~zod/try=> (nude [5 5] 5)
    [p=[%0 p=3] q=[p=[%0 p=1] q=[%0 p=1]]]
    ~zod/try=> (nude "sam" "sal")
    [ p=[p=[%1 p=115] q=[p=[%1 p=97] q=[p=[%1 p=108] q=[%0 p=15]]]]
      q=[p=[%1 p=115] q=[p=[%1 p=97] q=[p=[%1 p=109] q=[%0 p=15]]]]
    ]

------------------------------------------------------------------------
