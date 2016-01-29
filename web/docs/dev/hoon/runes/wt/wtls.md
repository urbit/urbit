wutlus, `?+`, %wtls
============================

Switch with default

`?+` is a synthetic rune that selects a case in `q` for the actual type
of `p` with a default case, `q`. The labels in `r` must have the same
[icon]() as `p` The list of cases, `r` must be terminated by `==`.

See also
--------

wuthep, `?-`, %wthp
============================

Produces
--------

Twig: `[%wtls p=wing q=twig r=tine]`

Sample
------

`p` is a [`++wing`](). `q` is a [twig](). `r` is a [`++tine`]().

Tall form
---------

Kingside:

    ?+  p
      q
      p.i.r      q.i.r
      p.i.t.r    q.i.t.r
      p.i.t.t.r  q.i.t.t.r
    ==

Queenside:

    ?+    p
      q
        p.i.r      
      q.i.r
        p.i.t.r    
      q.i.t.r
        p.i.t.t.r  
      q.i.t.t.r
    ==

Wide form
---------

    ?+(p p.i.r q.i.r, p.i.t.r q.i.t.r, p.i.t.t.r q.i.t.t.r)

Irregular form
--------------

None

Examples
--------

    ~zod/try=> 
      =cor  |=  typ=@ta
            ?+  typ  0
            %a  1
            %b  2
            ==
    new var %cor
    ~zod/try=> 
      (cor 'a')
    1
    ~zod/try=> 
      (cor 'c')
    0

Here is a simple example of `?+` showing that although the cases must
match the icon of its input, all possible cases don't need to be covered
since `?+` has a default case. We create a core, `cor` that takes an
input `typ`, a `@ta`. Calling `cor` selects one of our cases when it is
covered, or the default.

    ~zod/try=> 
      ?+  'a'  0
          %a  1
          ==
    1
    ~zod/try=> 
      ?+  [0 'a']  0
          %a  1
          ==
    ! poke-mack-fail
    ! /~zod/try/~2014.11.2..16.45.31..7b2a:<[1 1].[3 7]>
    ! mint-vain
    ! ford: call ~hidper-sommur
    ! poking %poke-txt
    ! error in app %shell on ~zod at instance /shell/terminal
    ~zod/try=> 
      ?+  [0 1]  0
          [0 %a]  1
          ==
    0

This example shows how `?+` can fail. Our input icon must match the icon
of our cases.

Equivalent to
-------------

    ?-  p
      p.i.r      q.i.r
      p.i.t.r    q.i.t.r
      p.i.t.t.r  q.i.t.t.r
      *          q
    ==
