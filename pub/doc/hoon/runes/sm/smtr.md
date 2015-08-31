semtar, `;*`
=====================

`++sail` list `++manx`

`;*` is a virtual rune used within [`++sail`]() to interpolate a list of
marl.

See also
--------

The `%c` case inside of [`++tuna`]().

Produces
--------

Twig: [`++marl`]()

Sample
------

`p` is a twig

Tall form
---------

    ;*  a

Wide form
---------

    *{a}

(within quoted form)

Irregular form
--------------

None

Examples
--------

    ~zod/try=> ;div  ;*  ~[;hi; ;p;]
               ==
    [[%div ~] [[%hi ~] ~] [[%p ~] ~] ~]
    ~zod/try=> ;div:"*{~[;hi; ;p;]}"
    [[%div ~] [[%hi ~] ~] [[%p ~] ~] ~]
    ~zod/try=> ;div:"a*{~[;hi; ;p;]}b"
    [ [%div ~]
      [[%~. [%~. "a"] ~] ~]
      [[%hi ~] ~]
      [[%p ~] ~] 
      [[%~. [%~. "b"] ~] ~]
      ~
    ]
    ~zod/try=> (poxo ;div:"a*{~[;hi; ;p;]}b")
    "<div>a<hi></hi><p></p>b</div>"

Here we use the tall, wide and irregular forms of `;*` to interpolate a
list of `++marl` into our containing [`++manx`]() `;div`.
