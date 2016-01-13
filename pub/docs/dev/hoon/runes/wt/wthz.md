wuthaz, %wthz
======================

[Short description]

`wuthaz`, `[%wthz p=wing q=tine]` is a synthetic rune that selects a
case in `q` for the actual type of `p`.

Produces
--------

[Twig or tile]

Sample
------

[`p` is a \* `q` is a \*]

Tall form
---------

Kingside:

    ?-  p
      p.i.q      q.i.q
      p.i.t.q    q.i.t.q
      p.i.t.t.q  q.i.t.t.q
    ==

Queenside:

    ?-    p
        p.i.q      
      q.i.q
        p.i.t.q    
      q.i.t.q
        p.i.t.t.q  
      q.i.t.t.q
    ==

Wide form
---------

?-(p p.i.q q.i.q, p.i.t.q q.i.t.q, p.i.t.t.q q.i.t.t.q)

Irregular form
--------------

None

Examples
--------
