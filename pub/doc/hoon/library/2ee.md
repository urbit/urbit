section 2eE, parsing (composers)
================================

### `++bass`

    ++  bass
      |*  [wuc=@ tyd=_rule]
      %+  cook
        |=  waq=(list ,@)
        %+  roll
          waq
        =|([p=@ q=@] |.((add p (mul wuc q))))
      tyd
    ::

Parser modifier:
[LSB](http://en.wikipedia.org/wiki/Least_significant_bit) ordered list
as atom of a base.

`wuc` is an [atom]().

`tyd` is a [rule]().

    ~zod/try=> (scan "123" (bass 10 (star dit)))
    q=123
    ~zod/try=> (scan "123" (bass 8 (star dit)))
    q=83
    ~zod/try=> `@ub`(scan "123" (bass 8 (star dit)))
    0b101.0011

------------------------------------------------------------------------

### `++boss`

    ++  boss
      |*  [wuc=@ tyd=_rule]
      %+  cook
        |=  waq=(list ,@)
        %+  reel
          waq
        =|([p=@ q=@] |.((add p (mul wuc q))))
      tyd
    ::

Parser modifier:
[LSB](http://en.wikipedia.org/wiki/Least_significant_bit) ordered list
as atom of a base.

`wuc` is an [atom]().

`tyd` is a [rule]().

    ~zod/try=> (scan "123" (boss 10 (star dit)))
    q=321
    ~zod/try=> `@t`(scan "bam" (boss 256 (star alp)))
    'bam'
    ~zod/try=> `@ux`(scan "bam" (boss 256 (star alp)))
    0x6d.6162

------------------------------------------------------------------------

### `++ifix`

    ++  ifix
      |*  [fel=[p=_rule q=_rule] hof=_rule]
      ;~(pfix p.fel ;~(sfix hof q.fel))
    ::

Parser modifier: surround with pair of rules, output of which is
discarded.

`fel` is a pair of [rule]()s.

`hof` is a [rule]().

    ~zod/try=> (scan "-40-" (ifix [hep hep] dem))
    q=40
    ~zod/try=> (scan "4my4" (ifix [dit dit] (star alf)))
    "my"

------------------------------------------------------------------------

### `++more`

    ++  more
      |*  [bus=_rule fel=_rule]
      ;~(pose (most bus fel) (easy ~))
    ::

Parser modifier: using a delimiter rule, parse a list of matches.

`bus` is a [rule]().

`fel` is a [rule]().

    ~zod/try=> (scan "" (more ace dem))
    ~
    ~zod/try=> (scan "40 20" (more ace dem))
    [q=40 ~[q=20]]
    ~zod/try=> (scan "40 20 60 1 5" (more ace dem))
    [q=40 ~[q=20 q=60 q=1 q=5]]

------------------------------------------------------------------------

### `++most`

    ++  most
      |*  [bus=_rule fel=_rule]
      ;~(plug fel (star ;~(pfix bus fel)))
    ::

Parser modifier: using a delimiter rule, parse a list of at least one
match.

`bus` is a [rule]().

`fel` is a [rule]().

    ~zod/try=> (scan "40 20" (most ace dem))
    [q=40 ~[q=20]]
    ~zod/try=> (scan "40 20 60 1 5" (most ace dem))
    [q=40 ~[q=20 q=60 q=1 q=5]]
    ~zod/try=> (scan "" (most ace dem))
    ! {1 1}
    ! exit

------------------------------------------------------------------------

### `++plus`

    ++  plus  |*(fel=_rule ;~(plug fel (star fel)))

Parser modifier: parse list of at least one match

`fel` is a [rule]().

    ~zod/try=> (scan ">>>>" (cook lent (plus gar)))
    4
    ~zod/try=> (scan "-  - " (plus ;~(pose ace hep)))
    [~~- "  - "]
    ~zod/try=> `tape`(scan "-  - " (plus ;~(pose ace hep)))
    "-  - "
    ~zod/try=> `(pole ,@t)`(scan "-  - " (plus ;~(pose ace hep)))
    ['-' [' ' [' ' ['-' [' ' ~]]]]]

------------------------------------------------------------------------

### `++slug`

    ++  slug
      |*  raq=_|*([a=* b=*] [a b])
      |*  [bus=_rule fel=_rule]
      ;~((comp raq) fel (stir rud raq ;~(pfix bus fel)))
    ::

Parser modifier: By composing with a gate, parse a delimited list of
matches.

`bus` is a [rule]().

`fel` is a [rule]().

    ~zod/try=> (scan "20+5+110" ((slug add) lus dem))
    135
    ~zod/try=> `@t`(scan "a b c" ((slug |=(a=[@ @t] (cat 3 a))) ace alp))
    'abc'

------------------------------------------------------------------------

### `++star`

    ++  star                                                ::  0 or more times
      |*  fel=_rule
      (stir `(list ,_(wonk *fel))`~ |*([a=* b=*] [a b]) fel)

Parser modifier: parse list of matches.

`fel` is a [rule]().

        ~zod/try=> (scan "aaaaa" (just 'a'))
        ! {1 2}
        ! 'syntax-error'
        ! exit
        ~zod/try=> (scan "aaaaa" (star (just 'a')))
        "aaaaa"
        ~zod/try=> (scan "abcdef" (star (just 'a')))
        ! {1 2}
        ! 'syntax-error'
        ! exit
        ~zod/try=> (scan "abcabc" (star (jest 'abc')))
        <|abc abc|>
        ~zod/try=> (scan "john smith" (star (shim 0 200)))
        "john smith"

------------------------------------------------------------------------
