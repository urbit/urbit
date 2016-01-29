semzaz
===============

`++sail` tag

`semzaz`, is a virtual rune frequently referred to as [`++sail`]() in
reference to `++sail`:vast which parses it. It is the most basic part of
the templating structure frequently used to produce [`++manx`](). In its
simplest and most common form we see `semzaz` as
`;tagname(property1 "value1", property2 "value2")`.

Produces
--------

Twig

Sample
------

`p` is a mane, `q` is a list of pairs of `++mane` to twig producing a
tape, `r` is a list of twigs producing `manx`

Tall form
---------

    ;p
      i.r
      i.t.r
    ==

    ;p 
      =p.i.q  q.i.q
      =p.i.t.q  q.i.t.q
      i.r
      i.t.r
    ==

    ;p(p.i.q q.i.q, p.i.t.q q.i.t.q)
      i.r
      i.t.r
    ==

Wide form
---------

    ;p;

    ;p:(i.r i.t.r i.t.t.r)

    ;p(p.i.q q.i.q, p.i.t.q q.i.t.q):"{...}"

    ;p(p.i.q q.i.q, p.i.t.q q.i.t.q):(i.r i.t.r i.t.t.r)

    ;p(p.i.q q.i.q, p.i.t.q q.i.t.q):"{...}"

Within quoted form,

    `;{p}`, `{p i.r i.t.r}`, `;{p(p.i.q q.i.q, p.i.t.q q.i.t.q) i.r i.t.r}`

Irregular form
--------------

None

Examples
--------

    ~zod/try=> ;html  ;head;
                      ;body;
               == 
    [[%html ~] [[%head ~] ~] [[%body ~] ~] ~]
    ~zod/try=> ;html:(head body)
    [[%html ~] [[%head ~] ~] [[%body ~] ~] ~]
    ~zod/try=> ;html:";{head meta(charset "utf-8")};{body p p}"
    [ [%html ~]
      [[%head ~] [[%meta [%charset "utf-8"] ~] ~] ~]
      [[%body ~] [[%p ~] ~] [[%p ~] ~] ~]
      ~
    ]
    ~zod/try=> (poxo ;html:";{head meta(charset "utf-8")};{body p p}")
    "<html><head><meta charset="utf-8"></meta></head><body><p></p><p></p></body></html>"

Here we create the most basic HTML structure using nested `dotzaz`, and
eventually print it properly using [`++poxo`]().
