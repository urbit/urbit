semhep, `;-`
=====================

`++sail` tape

`;-` is a virtual rune used within [`++sail`]() to interpolate a tape.

See also
--------

The `%a` case inside of [`++tuna`]().

Produces
--------

Twig: [`++tape`]()

Sample
------

`p` is a [twig]().

Tall form
---------

    ;-  a

Wide form
---------

    -{a}

(within quoted form)

Irregular form
--------------

None

Examples
--------

    ~zod/try=> 
    =+  c="contents!"
    ;div: -{c}
    [[%div ~] [[%~. [%~. "contents!"] ~] ~] ~]
    ~zod/try=> 
    %-  poxo
    =+  c="contents!"
    ;div: -{c}
    "<div>contents!</div>"

Here we add `c` to our context as a `++tape`, and interpolate it within
our `;div` using the irregular form of `;-`. By using [`++poxo`]() we
can see our result in XML form.
