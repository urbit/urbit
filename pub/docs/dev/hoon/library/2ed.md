section 2eD
===========

### `++bend`

Conditional composer

    ++  bend                                                ::  conditional comp
      ~/  %bend
      |*  raq=_|*([a=* b=*] [~ u=[a b]])
      ~/  %fun
      |*  [vex=edge sab=_rule]
      ?~  q.vex
        vex
      =+  yit=(sab q.u.q.vex)
      =+  yur=(last p.vex p.yit)
      ?~  q.yit
        [p=yur q=q.vex]
      =+  vux=(raq p.u.q.vex p.u.q.yit)
      ?~  vux
        [p=yur q=q.vex]
      [p=yur q=[~ u=[p=u.vux q=q.u.q.yit]]]
    ::

Parsing composer: connects the edge `vex` with the subsequent rule `sab`
as an optional suffix, using the gate `raq` to compose or reject its
result. If there is no suffix, or if the suffix fails to be composed
with the current result, the current result is produced. Used to map a
group of rules to a specified output.

`raq` is a [gate]().

`sab` is a [rule]().

`vex` is an [edge]().

    ~zod/try=> (;~((bend |=([a=char b=char] ?.(=(a b) ~ (some +(a))))) prn prn) [1 1] "qs")
    [p=[p=1 q=3] q=[~ u=[p=~~q q=[p=[p=1 q=2] q="s"]]]]
    ~zod/try=> (;~((bend |=([a=char b=char] ?.(=(a b) ~ (some +(a))))) prn prn) [1 1] "qqq")
    [p=[p=1 q=3] q=[~ u=[p=~~r q=[p=[p=1 q=3] q="q"]]]]
    ~zod/try=> (scan "aa" ;~((bend |=([a=char b=char] ?.(=(a b) ~ (some +(a))))) prn prn))
    ~~b
    ~zod/try=> (scan "ba" ;~((bend |=([a=char b=char] ?.(=(a b) ~ (some +(a))))) prn prn))
    ! {1 3}
    ! exit
    ~zod/try=> `(unit ,@tas)`(scan "" ;~((bend) (easy ~) sym))
    ~
    ~zod/try=> `(unit ,@tas)`(scan "sep" ;~((bend) (easy ~) sym))
    [~ %sep]

------------------------------------------------------------------------

### `++comp`

Arbitrary compose

    ++  comp
      ~/  %comp
      |*  raq=_|*([a=* b=*] [a b])                          ::  arbitrary compose
      ~/  %fun
      |*  [vex=edge sab=_rule]
      ?~  q.vex
        vex
      =+  yit=(sab q.u.q.vex)
      =+  yur=(last p.vex p.yit)
      ?~  q.yit
        [p=yur q=q.yit]
      [p=yur q=[~ u=[p=(raq p.u.q.vex p.u.q.yit) q=q.u.q.yit]]]
    ::

Parsing composer: connects the edge `vex` with a following rule `sab`,
combining the contents of `vex` with the result of `sab` using a binary
gate `raq`. Used to fold over the results of several rules.

`raq` is a [gate]() that accepts a cell of two nouns, `a` and `b`, and
produces a cell of two nouns.

`sab` is a [rule]().

`vex` is an [edge]().

    ~zod/try=> (scan "123" ;~((comp |=([a=@u b=@u] (add a b))) dit dit dit))
    6
    ~zod/try=> (scan "12" ;~((comp |=([a=@u b=@u] (add a b))) dit dit dit))
    ! {1 3}
    ! exit

------------------------------------------------------------------------

### `++glue`

Skip delimiter

    ++  glue                                                ::  add rule
      ~/  %glue
      |*  bus=_rule
      ~/  %fun
      |*  [vex=edge sab=_rule]
      (plug vex ;~(pfix bus sab))
    ::

Parsing composer: connects an edge `vex` with a following rule `sab` by
parsing the rule `bus` (the delimiting symbol) and throwing out the
result.

`bus` is a [rule]().

`sab` is a [rule]().

`vex` is an [edge]().

    ~zod/try=> (scan "200|mal|bon" ;~((glue bar) dem sym sym))
    [q=200 7.102.829 7.237.474]
    ~zod/try=> `[@u @tas @tas]`(scan "200|mal|bon" ;~((glue bar) dem sym sym))
    [200 %mal %bon]
    ~zod/try=>  (scan "200|;|bon" ;~((glue bar) dem sem sym))
    [q=200 ~~~3b. 7.237.474]
    ~zod/try=>  (scan "200.;.bon" ;~((glue dot) dem sem sym))
    [q=200 ~~~3b. 7.237.474]

------------------------------------------------------------------------

### `++less`

Parse unless

    ++  less                                                ::  no first and second
      |*  [vex=edge sab=_rule]
      ?~  q.vex
        =+  roq=(sab)
        [p=(last p.vex p.roq) q=q.roq]
      vex(q ~)
    ::

Parsing composer: if an edge `vex` reflects a success, fail. Otherwise,
connect `vex` with the following rule.

`sab` is a [rule]().

`vex` is an [edge]().

    ~zod/try=> (scan "sas-/lo" (star ;~(less lus bar prn)))
    "sas-/lo"
    ~zod/try=> (scan "sas-/l+o" (star ;~(less lus bar prn)))
    ! {1 8}
    ! exit
    ~zod/try=> (scan "sas|-/lo" (star ;~(less lus bar prn)))
    ! {1 5}
    ! exit

------------------------------------------------------------------------

### `++pfix`

Discard first rule

    ++  pfix                                                ::  discard first rule
      ~/  %pfix
      (comp |*([a=* b=*] b))
    ::

Parsing composer: connects an [edge]() `vex` with two subsequent rules,
ignoring the result of the first and producing the result of the second.

`vex` is an [edge]().

    ~zod/try=> `@t`(scan "%him" ;~(pfix cen sym))
    'him'
    ~zod/try=> (scan "+++10" ;~(pfix (star lus) dem))
    q=10

------------------------------------------------------------------------

### `++plug`

Parse to tuple

    ++  plug                                                ::  first then second
      ~/  %plug
      |*  [vex=edge sab=_rule]
      ?~  q.vex
        vex
      =+  yit=(sab q.u.q.vex)
      =+  yur=(last p.vex p.yit)
      ?~  q.yit
        [p=yur q=q.yit]
      [p=yur q=[~ u=[p=[p.u.q.vex p.u.q.yit] q=q.u.q.yit]]]
    ::

Parsing composer: connects `vex` with a following rule `sab`, producing
a cell of both the results. See also: the monad applicator [;\~]() for a
more detailed explanation.

`sab` is a [rule]().

`vex` is an [edge]().

    ~zod/try=> (scan "1..20" ;~(plug dem dot dot dem))
    [q=1 ~~~. ~~~. q=20]
    ~zod/try=> (scan "moke/~2014.1.1" ;~(plug sym fas nuck:so))
    [1.701.539.693 ~~~2f. [% p=[p=~.da q=170.141.184.500.766.106.671.844.917.172.921.958.400]]]
    ~zod/try=> ;;(,[@tas @t ~ %da @da] (scan "moke/~2014.1.1" ;~(plug sym fas nuck:so)))
    [%moke '/' ~ %da ~2014.1.1]

------------------------------------------------------------------------

### `++pose`

Parse options

    ++  pose                                                ::  first or second
      ~/  %pose
      |*  [vex=edge sab=_rule]
      ?~  q.vex
        =+  roq=(sab)
        [p=(last p.vex p.roq) q=q.roq]
      vex

Parsing composer: if `vex` reflects a failure, connect it with the
following rule `sab`. See also: the monad applicator [;\~]()

`sab` is a [rule]().

`vex` is an [edge]().

    ~zod/try=> `@t`(scan "+" ;~(pose lus tar cen))
    '+'
    ~zod/try=> `@t`(scan "*" ;~(pose lus tar cen))
    '*'
    ~zod/try=> `@t`(scan "%" ;~(pose lus tar cen))
    '%'
    ~zod/try=> `@t`(scan "-" ;~(pose lus tar cen))
    ! {1 1}
    ! exit

------------------------------------------------------------------------

### `++simu`

First and second

    ++  simu                                                ::  first and second
      |*  [vex=edge sab=_rule]
      ?~  q.vex
        vex
      =+  roq=(sab)
      roq
    ::

Parsing composer: if an edge `vex` reflects a failure, fail. Otherwise,
connect `vex` with the following rule.

`sab` is a [rule]().

`vex` is an [edge]().

    ~zod/try=> (scan "~zod" scat:vast)
    [%dtzy p=%p q=0]
    ~zod/try=> (scan "%zod" scat:vast)
    [%dtzz p=%tas q=6.582.138]
    ~zod/try=> (scan "%zod" ;~(simu cen scat:vast))
    [%dtzz p=%tas q=6.582.138]
    ~zod/try=> (scan "~zod" ;~(simu cen scat:vast))
    ! {1 1}
    ! exit

------------------------------------------------------------------------

### `++sfix`

Discard second rule

    ++  sfix                                                ::  discard second rule
      ~/  %sfix
      (comp |*([a=* b=*] a))

Parsing composer: connects `vex` with two subsequent rules returning the
result of the first and discarding the result of the second.

`a` is the result of parsing the first [rule]().

`b` is the result of of parsing the second [rule]().

    ~zod/try=> `@t`(scan "him%" ;~(sfix sym cen))
    'him'
    ~zod/try=> (scan "10+++" ;~(sfix dem (star lus)))
    q=10

------------------------------------------------------------------------
