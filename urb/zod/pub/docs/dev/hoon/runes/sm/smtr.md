`;*`
====

Interpolates a list of XML nodes within our XML template syntax (aka sail)


`;*` is a virtual rune used within [`++sail`]() to interpolate a list of
marl.

See also
--------

The `%c` case inside of [`++tuna`]().

Produces
--------

Twig: [`++marl`]()

Accepts
-------

`p` is a twig

Tall form
---------

    ;*  a

Wide form
---------

    *{a}

(within quoted form)

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
