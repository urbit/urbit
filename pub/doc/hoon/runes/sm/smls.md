semlus, `;+`
=====================

`++sail` manx

`;+` is a virtual rune used within [`++sail`]() to interpolate a manx.
`;+` is useful when you need to dynamically produce a tag in a `++sail`
block.

See also
--------

The `%b` case inside of [`++tuna`]().

Produces
--------

Twig: [`++manx`]()

Sample
------

`p` is a twig

Tall form
---------

    ;+  a

Wide form
---------

    +{a}

(within quoted form)

Irregular form
--------------

None

Examples
--------

    ~zod/try=> 
    =+  tag=;p(class "doc")
    ;div  ;+  tag
    ==
    [[%div ~] [[%p [%class "doc"] ~] ~] ~]
    ~zod/try=> 
    %-  poxo
    =+  tag=;p(class "doc")
    ;div  ;+  tag
    ==
    "<div><p class="doc"></p></div>"

Here we add `tag` to our context as a `;p` tag and use `;+` to add it to
our `;div`. Using [`++poxo`]() we can print our result as XML.
