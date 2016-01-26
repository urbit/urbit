semcen, `;%`
=====================

`++sail` list

`;%` is a virtual rune used within [`++sail`]() for passing a list of
child nodes to a gate. `;%` is used for transforming a list of child
elements inside a [`++manx`]().

See also
--------

The `%e` case inside of [`++tuna`]().

Produces
--------

Twig: [`++marl`]()

Sample
------

`p` is a [twig]().

Tall form
---------

    ;%  p

Wide form
---------

    %{p}

(within quoted form)

Irregular form
--------------

None

Examples
--------

    ~zod/try=> 
    ;div:"%{|=(a=marl (weld a a))} repeat"
    [ [%div ~]
      ~[
        [g=[n=%$ a=~[[n=%$ v=" repeat"]]] c=~]
        [g=[n=%$ a=~[[n=%$ v=" repeat"]]] c=~]
      ]
    ]
    ~zod/try=> 
    (poxo ;div:"%{|=(a=marl (weld a a))} repeat")
    "<div> repeat repeat</div>"

In this simple example we use a gate, `|=(a=marl (weld a a))` that
produces a `++marl` twice. Using the irregular form of `;%` we produce a
repeated child node `'repeat'`. [`++poxo`]() prints our result in XML.

    ~zod/try=> 
    ;=
      ;%  |=(a=marl (turn a |=(b=manx ;script(src (poxo b));)))
      ; /gep/hart.js
      ; /main/lib/urb.js
      ; //cdnjs.cloudflare.com/ajax/libs/jquery/2.1.1/jquery.js
    ==
    ~[
      [[%script [%src "/gep/hart.js
    "] ~] ~]
      [[%script [%src "/main/lib/urb.js
    "] ~] ~]
      [ [ %script
          [%src "//cdnjs.cloudflare.com/ajax/libs/jquery/2.1.1/jquery.js
    "]
          ~
        ]
        ~
      ]
    ]
    ~zod/try=> 
    %-  many:poxo  :_  ~
    ;=
      ;%  |=(a=marl (turn a |=(b=manx ;script(src (poxo b));)))
      ; /gep/hart.js
      ; /main/lib/urb.js
      ; //cdnjs.cloudflare.com/ajax/libs/jquery/2.1.1/jquery.js
    ==
    "<script src="/gep/hart.js"></script><script src="/main/lib/urb.js"></script><script src="//cdnjs.cloudflare.com/ajax/libs/jquery/2.1.1/jquery.js"></script>" 

Here we go through a similar example, passing a list of urls for a
script tag to a gate that produces a `<script>` tag. Since we produce
multiple tags, we use `many:poxo` to print our result as XML.
