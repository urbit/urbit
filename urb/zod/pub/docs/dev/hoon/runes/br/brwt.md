`|?`
====

`|-` with hidden context

Similar to `|-` except for that the [context]() of the core being
passed is hidden in order to avoid type conflicts with a core
that has a different context. In urbit language, `|?` produces a
[dry]() [%lead]() trap. 

Produces
--------

Twig: `[%brwt q=twig]`

Accepts
-------

`p` is a [++twig]().

Tall form
---------

    |?  p

Wide form
---------

    |?(p)

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

A lead core's context cannot be read or written.
