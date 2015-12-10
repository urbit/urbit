`;-`
====

Interpolate string

Interpolates a string (aka [++tape]()) into a [`++marl`](), our datastructure for XML.

Produces
--------

Twig: tape

Sample
------

`p` is a [`++twig`]().

Tall form
---------

    ;-  a

Wide form
---------

    -{a}

(within quoted form)

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
