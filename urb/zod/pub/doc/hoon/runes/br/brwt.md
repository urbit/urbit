barwut, `|?`, %brwt
============================

`%lead` trap

`|?` is a synthetic rune that produces a dry [`%lead`]() trap. `%lead`
traps are used when we want to pass one core to another core, as both
the sample and context of the core being passed must must be hidden in
order to avoid type conflicts with a core that has a different sample
and context.

Produces
--------

Twig: `[%brwt q=twig]`

Sample
------

`q` is a [twig]().

Tall form
---------

    |?  p

Wide form
---------

    |?(p)

Irregular form
--------------

None

Examples
--------

    ~zod/try=> |?(20)
    < 1?lld
      [[[@da @ta] [@p @ta] *''] @n <250.yum 41.int 414.hhh 100.xkc 1.ypj %164>]
    >
    ~zod/try=> +:|?(20)
    ! -axis.3
    ! peek-park
    ! exit
    ~zod/try=> +:|.(20)
    [ [[~2014.10.22..19.39.41..0440 ~.~2014.10.22..19.39.41..0440] [~zod ~.~zod] <||>]
      ~
      <250.yum 41.int 414.hhh 100.xkc 1.ypj %164>
    ]

A lead core's payload cannot be read or written.
